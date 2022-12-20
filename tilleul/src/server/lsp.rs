use log::info;

use super::connection::Connection;
use super::payload::request::Request;
use super::LanguageServerBackend;
use crate::server::dispatcher::RequestDispatcher;
use crate::server::payload::message::Message;

/// LSP server.
pub struct LspServer<'a, T: LanguageServerBackend> {
    backend: &'a mut T,
    connection: Connection,
}

impl<'a, T: LanguageServerBackend> LspServer<'a, T> {
    pub fn new(backend: &'a mut T) -> Self {
        LspServer {
            backend,
            connection: Connection::new(),
        }
    }

    pub fn serve(&mut self) {
        info!("Server launched");

        while let Ok(msg) = self.connection.receiver.recv() {
            match msg {
                Message::Request(request) => self.dispatch_request(request),

                // Should be dropped
                Message::Notification(_) => (),

                _ => unreachable!(),
            }
        }
    }

    fn dispatch_request(&mut self, request: Request) {
        use lsp_types::request::*;

        let mut dispatcher = RequestDispatcher::new(request, self.backend, &self.connection.sender);

        dispatcher.handle::<Initialize>(T::initialize).handle_fallthrough();
    }
}
