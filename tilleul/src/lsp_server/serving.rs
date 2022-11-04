use crate::lsp_server::{Closing, LspServer, Serving};

impl LspServer<Serving> {
    pub fn serving(self) -> LspServer<Closing> {
        LspServer(self.0)
    }
}
