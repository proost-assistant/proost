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

#[cfg(test)]
mod tests {
    use lsp_types::notification::*;
    use lsp_types::request::*;
    use lsp_types::*;
    use mockall::Sequence;

    use super::*;
    use crate::lsp::message::notification::Notification;
    use crate::lsp::message::request::Request;
    use crate::lsp::message::response::Response;
    use crate::lsp::MockLanguageServer;

    macro_rules! read {
        ($connection:expr, $sequence:expr, $request:expr) => {{
            $connection.expect_receive().times(1).in_sequence($sequence).return_const($request);
        }};
    }

    macro_rules! call {
        ($backend:expr, $method:path, $sequence:expr, $type:ident, $request:expr, $response:expr) => {{
            $method($backend)
                .with(mockall::predicate::eq($request))
                .times(1)
                .in_sequence($sequence)
                .return_const($response);
        }};
    }

    macro_rules! write {
        ($connection:expr, $sequence:expr, $response:expr) => {{
            $connection
                .expect_send()
                .with(mockall::predicate::eq($response))
                .times(1)
                .in_sequence($sequence)
                .return_const(Ok(()));
        }};
    }

    macro_rules! make {
        // Request with response
        ($connection:expr, $backend:expr, $method:path, $sequence:expr, $type:ident, $request:expr, $response:expr) => {{
            let request: <$type as lsp_types::request::Request>::Params = $request.clone();
            let response: <$type as lsp_types::request::Request>::Result = $response.clone();

            let payload_request = Ok(Message::Request(Request {
                id: 0,
                method: <$type as lsp_types::request::Request>::METHOD.to_owned(),
                params: serde_json::to_value(request).unwrap(),
            }));

            let payload_response = Message::Response(Response {
                id: 0,
                result: Some(serde_json::to_value(response).unwrap()),
                error: None,
            });

            read!($connection, $sequence, payload_request);
            call!($backend, $method, $sequence, $type, $request, $response);
            write!($connection, $sequence, payload_response);
        }};

        // Notification
        ($connection:expr, $backend:expr, $method:path, $sequence:expr, $type:ident, $notification:expr) => {{
            let notification: <$type as lsp_types::notification::Notification>::Params = $notification.clone();

            let payload_notification = Ok(Message::Notification(Notification {
                method: <$type as lsp_types::notification::Notification>::METHOD.to_owned(),
                params: serde_json::to_value(notification).unwrap(),
            }));

            read!($connection, $sequence, payload_notification);
            call!($backend, $method, $sequence, $type, $notification, ());
        }};
    }

    macro_rules! close {
        ($connection:expr, $sequence:expr) => {{
            $connection
                .expect_receive()
                .times(1)
                .in_sequence(&mut $sequence)
                .return_const(Err(crossbeam_channel::RecvError));
        }};
    }

    #[test]
    fn connection_closed() {
        let backend = MockLanguageServer::new();
        let mut connection = connection::MockServer::new();
        let mut sequence = Sequence::new();

        close!(connection, sequence);

        Server::new(backend, &connection).serve();
    }

    #[test]
    fn expected_flow() {
        let mut backend = MockLanguageServer::new();
        let mut connection = connection::MockServer::new();
        let mut sequence = Sequence::new();

        let request = InitializeParams::default();
        let response = InitializeResult::default();
        make!(&mut connection, &mut backend, MockLanguageServer::expect_initialize, &mut sequence, Initialize, request, response);

        let notification = InitializedParams {};
        make!(&mut connection, &mut backend, MockLanguageServer::expect_initialized, &mut sequence, Initialized, notification);

        make!(&mut connection, &mut backend, MockLanguageServer::expect_shutdown, &mut sequence, Shutdown, (), ());

        close!(connection, sequence);

        Server::new(backend, &connection).serve();
    }
}
