use log::info;
use lsp_types::*;

use crate::lsp_server::dispatcher::RequestDispatcher;
use crate::lsp_server::LspServer;
use crate::payload::message::Message;
use crate::payload::request::Request;
use crate::payload::response::{ErrorCode, ResponseError};

impl<'a> LspServer<'a> {
    pub fn launch(self) {
        info!("Server launched");

        for msg in self.connection.receiver.iter() {
            match msg {
                Message::Request(request) => self.dispatch_request(request),

                // Should be dropped
                Message::Notification(_) => (),

                _ => unreachable!(),
            }
        }
    }

    fn dispatch_request(&self, request: Request) {
        use lsp_types::request::*;

        let mut dispatcher = RequestDispatcher::new(request, self.connection.sender.clone());

        dispatcher.handle::<Initialize>(|params| self.handler_initialize(params)).handle_fallthrough(|request| ResponseError {
            code: ErrorCode::ServerNotInitialized,
            message: format!("Server not yet initialized, ignore {} method.", request.method),
            data: None,
        })
    }

    fn handler_initialize(&self, params: InitializeParams) -> InitializeResult {
        // TODO: Improve
        if let Some(root_uri) = params.root_uri {
            self.state.file_path.set(root_uri.to_file_path().unwrap()).unwrap();
        }

        InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: crate::NAME.to_string(),
                version: Some(crate::VERSION.to_string()),
            }),
        }
    }
}
