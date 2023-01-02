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
#[derive(Debug, Deserialize, Serialize)]
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
    /// [specification]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol
    pub fn read(reader: &mut dyn BufRead) -> Result<Self> {
        let mut buffer = String::new();

        if reader.read_line(&mut buffer)? == 0 {
            return Ok(Self::Notification(Notification::new::<lsp_types::notification::Exit>(())));
        };

        if !(buffer.starts_with("Content-Length: ") && buffer.ends_with("\r\n")) {
            bail!("Missing Content-Length header");
        }

        let Some(size) = buffer.get(16..buffer.len() - 2) else {
            bail!("Unexcepted UTF-8 character found while parsing Content-Length header");
        };

        let size = size.parse::<usize>()?;

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
    /// [specification]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol
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
