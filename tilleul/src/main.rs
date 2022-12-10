mod connection;
mod lsp_server;
mod payload;

use connection::Connection;
use log::info;
use lsp_server::LspServer;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

fn main() {
    env_logger::init();

    info!("Starting {} {}", NAME, VERSION);

    LspServer::new(Connection::new()).initialize().serving().closing();

    info!("Exiting {}", NAME);
}
