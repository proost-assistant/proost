use log::info;

use super::connection::Connection;
use super::payload::notification::Notification;
use super::payload::request::Request;
use super::LanguageServerBackend;
use crate::server::dispatcher::{notification, request};
use crate::server::payload::message::Message;
use crate::server::payload::response::{ErrorCode, ResponseError};

/// LSP server.
pub struct LspServer<'a, T: LanguageServerBackend> {
    backend: &'a mut T,
    connection: &'a Connection,
    state: LspServerState,
}

enum LspServerState {
    Initializing,
    Running,
    Closing,
}

impl<'a, T: LanguageServerBackend> LspServer<'a, T> {
    pub fn new(backend: &'a mut T, connection: &'a Connection) -> Self {
        LspServer {
            backend,
            connection,
            state: LspServerState::Initializing,
        }
    }

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

    fn dispatch_request(&mut self, request: Request) {
        use lsp_types::request::*;

        let mut dispatcher = request::Dispatcher::new(request, self.backend, &self.connection.sender);

        match self.state {
            LspServerState::Initializing => {
                dispatcher
                    .handle_callback::<_, Initialize>(T::initialize, |_| self.state = LspServerState::Running)
                    .handle_fallthrough(ResponseError {
                        code: ErrorCode::ServerNotInitialized,
                        message: "Server not initialized".to_string(),
                        data: None,
                    });
            },

            // LspServerState::Running => dispatcher.handle::<Initialize>(T::initialize).handle_fallthrough(),

            // LspServerState::Closing => dispatcher.handle::<Initialize>(T::initialize).handle_fallthrough(),
            _ => (),
        }
    }

    fn dispatch_notification(&mut self, notification: Notification) {
        use lsp_types::notification::*;

        let mut dispatcher = notification::Dispatcher::new(notification, self.backend);

        match self.state {
            LspServerState::Initializing => (),

            LspServerState::Running => dispatcher.handle::<DidOpenTextDocument>(T::did_open_text_document).handle_fallthrough(),

            LspServerState::Closing => (),
        }
    }
}
