mod server;
mod tilleul;

use anyhow::Result;
use log::info;

use crate::server::connection::Connection;
use crate::server::lsp::LspServer;
use crate::tilleul::Tilleul;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");

fn main() -> Result<()> {
    env_logger::init();

    info!("Starting {} {}", NAME, VERSION);

    let connection = Connection::new();

    kernel::memory::arena::use_arena(|arena| {
        let mut backend = Tilleul::new(arena, &connection);

        LspServer::new(&mut backend, &connection).serve();
    });

    info!("Exiting {}", NAME);

    Ok(())
}
