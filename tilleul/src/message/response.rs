use lsp_types::error_codes;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Response {
    /// The request id.
    pub id: u64,

    /// The result of a request.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,

    /// The error object in case a request fails.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<ResponseError>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ResponseError {
    /// A number indicating the error type that occurred.
    pub code: ErrorCode,

    /// A string providing a short description of the error.
    pub message: String,

    /// A primitive or structured value that contains additional information about the error.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ErrorCode {
    MethodNotFound = -32601,
    RequestCancelled = error_codes::REQUEST_CANCELLED as isize,
    ContentModified = error_codes::CONTENT_MODIFIED as isize,
}
