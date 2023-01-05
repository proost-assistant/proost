//! Handlers for [`Request`]s or [`Notification`]s that can be received by the client.
//!
//! [`Notification`]: crate::lsp::message::notification::Notification
//! [`Request`]: crate::lsp::message::request::Request

#![allow(clippy::wildcard_imports)]

use lsp_types::*;

use super::Tilleul;
use crate::lsp::{connection, LanguageServer};

impl<C: connection::LanguageServer> LanguageServer for Tilleul<'_, '_, C> {
    fn initialize(&mut self, _: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: crate::NAME.to_owned(),
                version: Some(crate::VERSION.to_owned()),
            }),
        }
    }
}
