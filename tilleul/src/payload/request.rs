use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Clone, Debug, Deserialize, Serialize)]
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
