use crate::connection::Connection;
use crate::lsp_server::{Initialize, Initializing, Instantiated, LspServer};

impl LspServer<Instantiated> {
    pub fn new(connection: Connection) -> LspServer<Initializing> {
        LspServer(Initialize { connection })
    }
}
