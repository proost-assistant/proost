//! Message handling when communicating with an [Language Server Protocol] client.
//!
//! [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/

pub mod notification;
pub mod request;
pub mod response;

use std::io::{BufRead, Write};

use anyhow::{bail, Result};
use notification::Notification;
use request::Request;
use response::Response;
use serde::{Deserialize, Serialize};

/// A message sent to or received from a [Language Server Protocol] client.
///
/// [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Message {
    /// A [`Request`] sent from a client to a server.
    Request(Request),

    /// A [`Response`] sent from a server to a client.
    Response(Response),

    /// A [`Notification`] sent from a client to a server, or from a server to a client.
    Notification(Notification),
}

/// Tiny wrapper sending [Language Server Protocol] messages to a client.
///
/// [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
#[derive(Serialize)]
struct JsonRPC {
    /// JSON-RPC version.
    jsonrpc: &'static str,

    /// The message.
    #[serde(flatten)]
    msg: Message,
}

impl Message {
    /// Convert a [`Message`] from a [`BufRead`] according to the [specification].
    ///
    /// This function will block until a complete message is received.
    ///
    /// # Errors
    /// Returns an error if the message is not conformed to the [specification], or if one of `read` operation failed.
    ///
    /// [specification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#baseProtocol
    pub fn read(reader: &mut dyn BufRead) -> Result<Self> {
        let mut buffer = String::new();

        if reader.read_line(&mut buffer)? == 0 {
            return Ok(Self::Notification(Notification::new::<lsp_types::notification::Exit>(())));
        };

        if !(buffer.starts_with("Content-Length: ") && buffer.ends_with("\r\n")) {
            bail!("Missing Content-Length header");
        }

        let size = buffer
            .get(16..buffer.len() - 2)
            .unwrap_or_else(|| unreachable!("UTF-8 is checked by read_line"))
            .parse::<usize>()?;

        let mut buffer = buffer.into_bytes();

        buffer.resize(size + 2, 0);

        reader.read_exact(&mut buffer)?;

        let buffer = String::from_utf8(buffer)?;

        Ok(serde_json::from_str(&buffer)?)
    }

    /// Send a [`Message`] to a [`Write`] according to the [specification].
    ///
    /// This function will block until the complete message is sent.
    ///
    /// # Errors
    /// Returns an error if one of `write` operation failed.
    ///
    /// [specification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#baseProtocol
    #[no_coverage]
    pub fn write(self, writer: &mut dyn Write) -> Result<()> {
        let response = JsonRPC {
            jsonrpc: "2.0",
            msg: self,
        };

        let payload = serde_json::to_string(&response).unwrap_or_else(|_| unreachable!("lsp_types crate is assumed to be correct"));

        write!(writer, "Content-Length: {}\r\n\r\n", payload.len())?;

        writer.write_all(payload.as_bytes())?;
        writer.flush()?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::io::BufReader;

    use lsp_types::*;

    use super::*;

    #[test]
    fn expected_read() {
        let data = b"Content-Length: 41\r\n\r\n{ \"method\": \"initialized\", \"params\": {} }";

        assert_eq!(
            Message::read(&mut BufReader::new(&data[..])).unwrap(),
            Message::Notification(Notification::new::<lsp_types::notification::Initialized>(InitializedParams {}))
        );
    }

    #[test]
    fn expected_write() {
        let mut output = Vec::new();

        Message::Response(Response::new::<lsp_types::request::Shutdown>(1, ()))
            .write(&mut output)
            .unwrap();

        assert_eq!(String::from_utf8(output).unwrap(), "Content-Length: 38\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":null}");
    }

    #[test]
    fn end_of_stream() {
        let data = b"";

        assert_eq!(
            Message::read(&mut BufReader::new(&data[..])).unwrap(),
            Message::Notification(Notification::new::<lsp_types::notification::Exit>(()))
        );
    }

    #[test]
    fn missing_content_length() {
        let data = b"{ \"method\": \"initialized\", \"params\": {} }";

        assert_eq!(Message::read(&mut BufReader::new(&data[..])).unwrap_err().to_string(), "Missing Content-Length header");
    }

    #[test]
    fn invalid_separator_header() {
        let data = b"Content-Length: 41{ \"method\": \"initialized\", \"params\": {} }";

        assert_eq!(Message::read(&mut BufReader::new(&data[..])).unwrap_err().to_string(), "Missing Content-Length header");
    }

    #[test]
    fn invalid_too_much_content_length() {
        let data = b"Content-Length: 100\r\n\r\n{ \"method\": \"initialized\", \"params\": {} }";

        assert_eq!(Message::read(&mut BufReader::new(&data[..])).unwrap_err().to_string(), "failed to fill whole buffer");
    }

    #[test]
    fn unexpected_content_length_value() {
        let data = b"Content-Length: ???\r\n\r\n{ \"method\": \"initialized\", \"params\": {} }";

        assert_eq!(Message::read(&mut BufReader::new(&data[..])).unwrap_err().to_string(), "invalid digit found in string");
    }

    #[test]
    fn corrupt_content_length_utf8() {
        let mut data: Vec<u8> = vec![];

        data.extend_from_slice(b"Content-Length: ");
        data.push(0xFF);
        data.extend_from_slice(b"\r\n\r\n{ \"method\": \"initialized\", \"params\": {} }");

        assert_eq!(Message::read(&mut BufReader::new(&*data)).unwrap_err().to_string(), "stream did not contain valid UTF-8");
    }

    #[test]
    fn corrupt_payload() {
        let data = b"Content-Length: 38\r\n\r\n{ \"method\": \"initialized\", \"params\": {";

        assert_eq!(
            Message::read(&mut BufReader::new(&data[..])).unwrap_err().to_string(),
            "EOF while parsing an object at line 2 column 38"
        );
    }

    #[test]
    fn corrupt_payload_utf8() {
        let mut data: Vec<u8> = vec![];

        data.extend_from_slice(b"Content-Length: 41\r\n\r\n{ \"method\": \"init");
        data.push(0xFF);
        data.extend_from_slice(b"alized\", \"params\": {} }");

        assert_eq!(
            Message::read(&mut BufReader::new(&*data)).unwrap_err().to_string(),
            "invalid utf-8 sequence of 1 bytes from index 19"
        );
    }
}
