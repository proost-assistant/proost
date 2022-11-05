use derive_more::{Constructor, Deref, DerefMut};
use log::warn;

use crate::lsp_server::LspServer;
use crate::payload::Request;

#[derive(Constructor, Deref, DerefMut)]
pub struct RequestDispatcher(Option<Request>);

impl RequestDispatcher {
    pub fn handle<P, R>(&mut self, server: &LspServer<P>, fct: fn(&LspServer<P>, R::Params) -> R::Result) -> &mut Self
    where
        P: crate::lsp_server::Phase,
        R: lsp_types::request::Request,
    {
        let request = match &mut self.0 {
            Some(request) => request,
            _ => return self,
        };

        if request.method != R::METHOD {
            return self;
        }

        let params = serde_json::from_value::<R::Params>(request.params.take()).unwrap();

        serde_json::to_value::<R::Result>(fct(server, params)).unwrap();

        self
    }

    pub fn handle_fallthrough(&mut self) {
        if let Some(request) = &self.0 {
            warn!("Unhandled request: {:?}", request.method);
        }
    }
}
