mod connection;
mod message;

use clap::Parser;
use connection::{Connection, ConnectionMode};
use log::info;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[clap(subcommand)]
    connection_mode: Option<ConnectionMode>,
}

fn main() -> std::io::Result<()> {
    std::env::set_var("RUST_LOG", "info");
    env_logger::init();

    let args = Args::parse();

    info!("Starting {} {}", NAME, VERSION);

    let (connection, threads) = Connection::new(args.connection_mode);

    main_loop(connection);

    threads.join()?;

    Ok(())
}

fn main_loop(_connection: Connection) {
    loop {
        std::thread::sleep(std::time::Duration::from_secs(1));
    }
}
