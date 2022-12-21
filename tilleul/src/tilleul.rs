use std::cell::OnceCell;
use std::path::PathBuf;

use kernel::term::arena::Arena;
use lsp_types::*;

use crate::server::LanguageServerBackend;

pub struct Tilleul<'a, 'arena> {
    arena: &'a mut Arena<'arena>,
}

impl<'a, 'arena> Tilleul<'a, 'arena> {
    pub fn new(arena: &'a mut Arena<'arena>) -> Self {
        Self { arena }
    }
}

impl LanguageServerBackend for Tilleul<'_, '_> {
    fn initialize(&self, _: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: crate::NAME.to_string(),
                version: Some(crate::VERSION.to_string()),
            }),
        }
    }
}
