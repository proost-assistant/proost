pub mod connection;
pub mod dispatcher;
pub mod lsp;
pub mod payload;

use lsp_types::*;

pub trait LanguageServerBackend {
    fn initialize(&self, params: InitializeParams) -> InitializeResult;
}
