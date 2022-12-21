pub mod connection;
pub mod dispatcher;
pub mod lsp;
pub mod payload;

use log::warn;
use lsp_types::*;

pub trait LanguageServerBackend {
    fn initialize(&self, params: InitializeParams) -> InitializeResult;

    fn did_open_text_document(&self, params: DidOpenTextDocumentParams) {
        let _ = params;
        warn!("textDocument/didOpen not handled");
    }
}
