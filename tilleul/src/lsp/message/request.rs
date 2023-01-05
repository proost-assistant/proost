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

    /// The method's params.
    #[serde(default)]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}
