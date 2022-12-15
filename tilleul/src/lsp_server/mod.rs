pub mod dispatcher;
pub mod server;

use std::cell::OnceCell;
use std::path::PathBuf;

use derive_more::Constructor;

use crate::connection::Connection;

/// LSP server.
#[derive(Constructor)]
pub struct LspServer<'a> {
    connection: &'a Connection,
    state: &'a LspState,
}

#[derive(Default, Debug)]
pub struct LspState {
    file_path: OnceCell<PathBuf>,
}
