#![feature(once_cell)]

mod connection;
mod lsp_server;
mod payload;

use anyhow::Result;
use connection::Connection;
use log::info;
use lsp_server::LspServer;

use crate::lsp_server::LspState;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");

fn main() -> Result<()> {
    env_logger::init();

    info!("Starting {} {}", NAME, VERSION);

    let connection = Connection::new();
    let state = LspState::default();

    LspServer::new(&connection, &state).launch();

    info!("Exiting {}", NAME);

    Ok(())
}
