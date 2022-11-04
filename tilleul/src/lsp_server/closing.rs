use crate::lsp_server::{Closing, LspServer};

impl LspServer<Closing> {
    pub fn closing(self) {}
}
