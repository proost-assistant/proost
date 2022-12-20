use crossbeam_channel::Sender;
use log::warn;

use super::payload::message::Message;
use super::payload::request::Request;
use super::payload::response::Response;
use super::LanguageServerBackend;
use crate::server::payload::response::{ErrorCode, ResponseError};

pub struct RequestDispatcher<'a, T: LanguageServerBackend> {
    request: Option<Request>,

    backend: &'a mut T,
    sender: &'a Sender<Message>,
}

impl<'a, T: LanguageServerBackend> RequestDispatcher<'a, T> {
    pub fn new(request: Request, backend: &'a mut T, sender: &'a Sender<Message>) -> Self {
        Self {
            request: Some(request),

            backend,
            sender,
        }
    }

    pub fn handle<R>(&mut self, closure: fn(&T, R::Params) -> R::Result) -> &mut Self
    where
        R: lsp_types::request::Request,
    {
        let Some(ref request) = self.request else { return self; };

        if request.method != R::METHOD {
            return self;
        }

        let mut request = self.request.take().unwrap();

        let params = serde_json::from_value::<R::Params>(request.params.take()).unwrap();

        let result = closure(self.backend, params);

        let msg = Message::Response(Response {
            id: request.id,
            result: Some(serde_json::to_value(result).unwrap()),
            error: None,
        });

        self.sender.send(msg).unwrap();

        self
    }

    pub fn handle_fallthrough(&mut self) {
        let Some(ref request) = self.request else { return; };

        warn!("Method {} not implemented", request.method);

        let response = Message::Response(Response {
            id: request.id,
            result: None,
            error: Some(ResponseError {
                code: ErrorCode::MethodNotFound,
                message: format!("Method {} not implemented", request.method),
                data: None,
            }),
        });

        self.sender.send(response).unwrap();
    }
}
