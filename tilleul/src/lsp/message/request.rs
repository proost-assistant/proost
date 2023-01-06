//! A [request] sent from a client to a server.
//!
//! [request]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#requestMessage

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// [Request] message.
///
/// [Request]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#requestMessage
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Request {
    /// The request id.
    pub id: u64,

    /// The method to be invoked.
    pub method: String,

    /// The method parameters.
    #[serde(default)]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}

impl Request {
    /// Creates a new [`Request`].
    pub fn new<R: lsp_types::request::Request>(id: u64, params: R::Params) -> Self {
        Self {
            id,
            method: R::METHOD.to_owned(),
            params: serde_json::to_value(params).unwrap_or_else(|_| unreachable!("lsp_types crate is assumed to be correct")),
        }
    }
}
