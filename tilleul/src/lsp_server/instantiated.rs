use threadpool::Builder;

use crate::connection::Connection;
use crate::lsp_server::{Initialize, Initializing, Instantiated, LspServer};

const STACK_SIZE: usize = 8 * 1024 * 1024;

impl LspServer<Instantiated> {
    pub fn new(connection: Connection) -> LspServer<Initializing> {
        let thread_pool = Builder::new().thread_stack_size(STACK_SIZE).build();

        LspServer(Initialize {
            connection,
            thread_pool,
        })
    }
}
