//! [Language Server Protocol] server frontend, handling I/O operations
//!
//! [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/

use log::info;

use super::LanguageServer;
use crate::lsp::connection::Connection;
use crate::lsp::dispatcher::{notification, request};
use crate::lsp::message::notification::Notification;
use crate::lsp::message::request::Request;
use crate::lsp::message::response::{Error, ErrorCode};
use crate::lsp::message::Message;

/// [Language Server Protocol] server frontend.
///
/// [`Server`] is the frontend server for [Language Server Protocol] handling I/O operations and dispatch requests towards the
/// user-defined backend server implementing [`LanguageServer`].
///
/// [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
pub struct Server<'server, T: LanguageServer> {
    /// User-defined backend server
    backend: &'server mut T,

    /// Connection
    connection: &'server Connection,

    /// Actual state of the server.
    state: State,
}

/// State of the [`Server`].
///
/// Depending of the state, the server will dispatch [requests] and [notifications] according to the [specification].
///
/// [notifications]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
/// [specification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#lifeCycleMessages
/// [requests]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#requestMessage
enum State {
    /// Waiting for [`initialize`] request.
    ///
    /// [`initialize`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    WaitingForInitialisation,

    /// Got [`initialize`] request: Waiting for [`initialized`] notification.
    ///
    /// [`initialize`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    /// [`initialized`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialized
    Initialised,

    /// Got [`initialized`] request: The [`Server`] is now running.
    ///
    /// [`initialized`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialized
    Running,

    /// Got [`shutdown`] request: The [`Server`] will gracefully shut down.
    ///
    /// [`shutdown`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#shutdown
    Closing,
}

impl<'server, T: LanguageServer> Server<'server, T> {
    /// Creates a new [`Server`], without starting it.
    pub fn new(backend: &'server mut T, connection: &'server Connection) -> Self {
        Server {
            backend,
            connection,
            state: State::WaitingForInitialisation,
        }
    }

    /// Starts the [`Server`].
    ///
    /// This method will block the current thread until the server is closed.
    pub fn serve(&mut self) {
        info!("Server launched");

        while let Ok(msg) = self.connection.receiver.recv() {
            match msg {
                Message::Request(request) => self.dispatch_request(request),
                Message::Notification(notification) => self.dispatch_notification(notification),
                _ => unreachable!(),
            }
        }
    }

    /// Dispatches a received [`Request`] towards the user-defined [`LanguageServer`] backend's methods.
    fn dispatch_request(&mut self, request: Request) {
        #[allow(clippy::wildcard_imports)]
        use lsp_types::request::*;

        let mut dispatcher = request::Dispatcher::new(request, self.backend, &self.connection.sender);

        match self.state {
            State::WaitingForInitialisation => dispatcher
                .handle_callback::<Initialize, _>(T::initialize, |_| self.state = State::Initialised)
                .handle_fallthrough(Error {
                    code: ErrorCode::ServerNotInitialized,
                    message: "Server not initialised".to_owned(),
                    data: None,
                }),

            State::Initialised => dispatcher.handle_fallthrough(Error {
                code: ErrorCode::ServerNotInitialized,
                message: "Server not initialised".to_owned(),
                data: None,
            }),

            State::Running => dispatcher
                .handle_callback::<Shutdown, _>(T::shutdown, |_| self.state = State::Closing)
                .handle_fallthrough(Error {
                    code: ErrorCode::MethodNotFound,
                    message: "Method not found".to_owned(),
                    data: None,
                }),

            State::Closing => (),
        }
    }

    /// Dispatches a received [`Notification`] towards the user-defined [`LanguageServer`] backend's methods.
    fn dispatch_notification(&mut self, notification: Notification) {
        #[allow(clippy::wildcard_imports)]
        use lsp_types::notification::*;

        let mut dispatcher = notification::Dispatcher::new(notification, self.backend);

        match self.state {
            State::WaitingForInitialisation => dispatcher.handle_fallthrough("Server not initialised"),

            State::Initialised => dispatcher
                .handle_callback::<Initialized, _>(T::initialized, || self.state = State::Running)
                .handle_fallthrough("Server not initialised"),

            State::Running => dispatcher
                .handle::<DidOpenTextDocument>(T::text_document_did_open)
                .handle::<DidChangeTextDocument>(T::text_document_did_change)
                .handle::<DidCloseTextDocument>(T::text_document_did_close)
                .handle_fallthrough("Unknown notification received"),

            State::Closing => (),
        }
    }
}
