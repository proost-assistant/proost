use crossbeam_channel::Sender;
use log::warn;

use crate::server::payload::message::Message;
use crate::server::payload::request::Request;
use crate::server::payload::response::{Response, ResponseError};
use crate::server::LanguageServerBackend;

pub struct Dispatcher<'a, T: LanguageServerBackend> {
    request: Option<Request>,

    backend: &'a mut T,
    sender: &'a Sender<Message>,
}

impl<'a, T: LanguageServerBackend> Dispatcher<'a, T> {
    pub fn new(request: Request, backend: &'a mut T, sender: &'a Sender<Message>) -> Self {
        Self {
            request: Some(request),

            backend,
            sender,
        }
    }

    pub fn handle<R>(&mut self, closure: fn(&mut T, R::Params) -> R::Result) -> &mut Self
    where
        R: lsp_types::request::Request,
    {
        self.handle_callback::<R, _>(closure, |_| ())
    }

    pub fn handle_callback<R, C>(&mut self, handler: fn(&mut T, R::Params) -> R::Result, callback: C) -> &mut Self
    where
        C: FnOnce(&R::Result) -> (),
        R: lsp_types::request::Request,
    {
        let Some(ref request) = self.request else { return self; };

        if request.method != R::METHOD {
            return self;
        }

        let mut request = self.request.take().unwrap();

        let params = serde_json::from_value::<R::Params>(request.params.take()).unwrap();

        let result = handler(self.backend, params);

        callback(&result);

        let msg = Message::Response(Response {
            id: request.id,
            result: Some(serde_json::to_value(result).unwrap()),
            error: None,
        });

        self.sender.send(msg).unwrap();

        self
    }

    pub fn handle_fallthrough(&mut self, error_response: ResponseError) {
        let Some(ref request) = self.request else { return; };

        warn!("{}", error_response.message);

        let response = Message::Response(Response {
            id: request.id,
            result: None,
            error: Some(error_response),
        });

        self.sender.send(response).unwrap();
    }
}
