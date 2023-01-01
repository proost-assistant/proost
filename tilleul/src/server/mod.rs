pub mod connection;
pub mod dispatcher;
pub mod lsp;
pub mod payload;

use log::info;
use lsp_types::*;

pub trait LanguageServerBackend {
    fn initialize(&mut self, params: InitializeParams) -> InitializeResult;

    fn initialized(&mut self, params: InitializedParams) {
        let _ = params;
        info!("initialized not handled");
    }

    fn did_open_text_document(&mut self, params: DidOpenTextDocumentParams) {
        let _ = params;
        info!("textDocument/didOpen not handled");
    }
}
