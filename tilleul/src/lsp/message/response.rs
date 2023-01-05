//! A [response] sent from a server to a client.
//!
//! [response]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage

use lsp_types::error_codes;
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// [Response] message.
///
/// [Response]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Response {
    /// The request id.
    pub id: u64,

    /// The result of a request.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,

    /// The error object in case a request fails.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<Error>,
}

/// [Response error] message.
///
/// [Response error]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseError
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Error {
    /// A number indicating the error type that occurred.
    pub code: ErrorCode,

    /// A string providing a short description of the error.
    pub message: String,

    /// A primitive or structured value that contains additional information about the error.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

/// [Error code] message.
///
/// [Error code]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#errorCodes
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
pub enum ErrorCode {
    ServerNotInitialized = -32002,

    MethodNotFound = -32601,

    RequestCancelled = error_codes::REQUEST_CANCELLED as isize,
    ContentModified = error_codes::CONTENT_MODIFIED as isize,
}
