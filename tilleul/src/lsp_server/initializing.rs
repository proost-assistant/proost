use lsp_types::*;

use crate::lsp_server::dispatcher::RequestDispatcher;
use crate::lsp_server::{Initializing, LspServer, Serve, Serving};
use crate::payload::message::Message;
use crate::payload::request::Request;

impl LspServer<Initializing> {
    pub fn initialize(self) -> LspServer<Serving> {
        self.connection.receiver.iter().for_each(|message| match message {
            Message::Request(request) => self.dispatch_request(request),
            Message::Response(_) => {},
            Message::Notification(_) => {},
        });

        LspServer(Serve {
            connection: self.0.connection,
        })
    }

    fn dispatch_request(&self, request: Request) {
        use lsp_types::request::*;

        let mut dispatcher = RequestDispatcher::new(request, self.connection.sender.clone());

        dispatcher.handle::<Initialize>(|params| self.handler_initialize(params)).handle_fallthrough();
    }

    fn handler_initialize(&self, _params: InitializeParams) -> InitializeResult {
        // TODO: Handle `initialize` request
        InitializeResult::default()
    }
}
