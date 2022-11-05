use std::io::{BufRead, Write};

use anyhow::{bail, Result};
use log::info;
use lsp_types::notification;
use serde::{Deserialize, Serialize};

use super::notification::Notification;
use super::request::Request;
use super::response::Response;

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

impl Message {
    pub fn read(reader: &mut dyn BufRead) -> Result<Self> {
        let mut buffer = String::new();

        if reader.read_line(&mut buffer)? == 0 {
            return Ok(Message::Notification(Notification::new::<notification::Exit>(())));
        };

        if !(buffer.starts_with("Content-Length: ") && buffer.ends_with("\r\n")) {
            bail!("Missing Content-Length header");
        }

        let size = buffer[16..buffer.len() - 2].parse::<usize>()?;

        let mut buffer = buffer.into_bytes();

        buffer.resize(size + 2, 0);

        reader.read_exact(&mut buffer)?;

        let buffer = String::from_utf8(buffer)?;

        Ok(serde_json::from_str(&buffer)?)
    }

    pub fn write(self, _writer: &mut dyn Write) {
        info!("Write: {:?}", self);
    }
}
