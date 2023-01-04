//! [Language Server Protocol] server frontend, handling I/O operations
//!
//! [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/

use log::info;

use super::{connection, LanguageServer};
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
pub struct Server<'server, S: LanguageServer, C: connection::Server> {
    /// User-defined backend server
    backend: S,

    /// Connection
    connection: &'server C,

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

impl<'server, S, C> Server<'server, S, C>
where
    S: LanguageServer,
    C: connection::Server,
{
    /// Creates a new [`Server`], without starting it.
    pub const fn new(backend: S, connection: &'server C) -> Self {
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

        while let Ok(msg) = self.connection.receive() {
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

        let mut dispatcher = request::Dispatcher::new(request, &mut self.backend, self.connection);

        match self.state {
            State::WaitingForInitialisation => dispatcher
                .handle_callback::<Initialize, _>(S::initialize, |_| self.state = State::Initialised)
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
                .handle_callback::<Shutdown, _>(S::shutdown, |_| self.state = State::Closing)
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

        let mut dispatcher = notification::Dispatcher::new(notification, &mut self.backend);

        match self.state {
            State::WaitingForInitialisation => dispatcher.handle_fallthrough("Server not initialised"),

            State::Initialised => dispatcher
                .handle_callback::<Initialized, _>(S::initialized, || self.state = State::Running)
                .handle_fallthrough("Server not initialised"),

            State::Running => dispatcher
                .handle::<DidOpenTextDocument>(S::text_document_did_open)
                .handle::<DidChangeTextDocument>(S::text_document_did_change)
                .handle::<DidCloseTextDocument>(S::text_document_did_close)
                .handle_fallthrough("Unknown notification received"),

            State::Closing => (),
        }
    }
}
