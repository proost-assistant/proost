use log::info;
use lsp_types::error_codes;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::io::{self, BufRead, Result, Write};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

impl Message {
    pub fn read(reader: &mut dyn BufRead) -> Result<Message> {
        let mut buffer = String::new();

        reader.read_line(&mut buffer)?;

        if !(buffer.starts_with("Content-Length: ") && buffer.ends_with("\r\n")) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Malformed header",
            ));
        }

        let size = buffer[16..buffer.len() - 2]
            .parse::<usize>()
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Malformed header"))?;

        let mut buffer = buffer.into_bytes();

        buffer.resize(size + 1, 0);

        reader.read_exact(&mut buffer)?;

        let buffer = String::from_utf8(buffer)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Malformed payload"))?;

        Ok(serde_json::from_str(&buffer)?)
    }

    pub fn write(self, _writer: &mut dyn Write) -> Result<()> {
        info!("Write: {:?}", self);

        Ok(())
    }
}

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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Notification {
    /// The method to be invoked.
    pub method: String,

    /// The notification's params.
    #[serde(default)]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}
