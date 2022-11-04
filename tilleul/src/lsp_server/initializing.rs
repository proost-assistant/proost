use crate::lsp_server::{Initializing, LspServer, Serve, Serving};
use crate::payload::Message;
use lazy_static::lazy_static;
use log::info;
use lsp_types::{InitializeParams, InitializeResult};
use serde_json::Value;
use std::collections::HashMap;
use sugars::hmap;

type LspServerHandler = fn(&LspServer<Initializing>, Value) -> Value;

macro_rules! register {
    ($name:ident) => {
        LspServer::<Initializing>::$name as LspServerHandler
    };
}

lazy_static! {
    static ref METHOD_REQUEST: HashMap<&'static str, LspServerHandler> = {
        hmap! {
            "initialize" => register!(handler_initialize),
        }
    };
}

impl LspServer<Initializing> {
    pub fn initialize(self) -> LspServer<Serving> {
        self.connection.receiver.iter().for_each(|message| {
            info!("Received: {:?}", message);

            match message {
                Message::Request(request) => {
                    if let Some(handler) = METHOD_REQUEST.get(request.method.as_str()) {
                        handler(&self, request.params);
                    }
                }
                Message::Response(_) => {}
                Message::Notification(_) => {}
            }
        });

        LspServer(Serve {
            connection: self.0.connection,
        })
    }

    fn handler_initialize(&self, params: Value) -> Value {
        let params = serde_json::from_value::<InitializeParams>(params).unwrap();

        info!("{:?}", params);

        serde_json::to_value::<InitializeResult>(InitializeResult::default()).unwrap()
    }
}
