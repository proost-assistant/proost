use crossbeam_channel::Sender;
use log::warn;

use crate::payload::message::Message;
use crate::payload::request::Request;
use crate::payload::response::{Response, ResponseError};

pub struct RequestDispatcher {
    request: Option<Request>,
    sender: Sender<Message>,
}

impl RequestDispatcher {
    pub fn new(request: Request, sender: Sender<Message>) -> Self {
        Self {
            request: Some(request),
            sender,
        }
    }

    pub fn handle<R>(&mut self, closure: impl FnOnce(R::Params) -> R::Result) -> &mut Self
    where
        R: lsp_types::request::Request,
    {
        let Some(ref request) = self.request else { return self; };

        if request.method != R::METHOD {
            return self;
        }

        let mut request = self.request.take().unwrap();

        let params = serde_json::from_value::<R::Params>(request.params.take()).unwrap();

        let result = closure(params);

        let msg = Message::Response(Response {
            id: request.id,
            result: Some(serde_json::to_value(result).unwrap()),
            error: None,
        });

        self.sender.send(msg).unwrap();

        self.request = None;
        self
    }

    pub fn handle_fallthrough(&mut self, closure: impl FnOnce(&Request) -> ResponseError) {
        let Some(ref request) = self.request else { return; };

        warn!("Method {} not implemented", request.method);

        let msg = Message::Response(Response {
            id: request.id,
            result: None,
            error: Some(closure(request)),
        });

        self.sender.send(msg).unwrap();
    }
}
