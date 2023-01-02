//! Handlers for [`Request`]s or [`Notification`]s that can be received by the client.
//!
//! [`Notification`]: crate::lsp::message::Notification
//! [`Request`]: crate::lsp::message::Request

#![allow(clippy::wildcard_imports)]

use lsp_types::*;

use super::Tilleul;
use crate::lsp::LanguageServer;

impl LanguageServer for Tilleul<'_, '_> {
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
