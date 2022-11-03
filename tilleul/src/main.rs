mod connection;
mod message;

use connection::Connection;
use log::info;
use message::Message;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

fn main() {
    std::env::set_var("RUST_LOG", "info");
    env_logger::init();

    info!("Starting {} {}", NAME, VERSION);

    let (connection, threads) = Connection::new();

    main_loop(connection);

    threads.join();

    info!("Exiting {}", NAME);
}

fn main_loop(connection: Connection) {
    for msg in connection.receiver {
        match msg {
            Message::Request(request) => {
                info!("Received request: {:?}", request.method);
            }

            Message::Response(response) => {
                info!("Received response: {:?}", response);
            }

            Message::Notification(notification) => {
                info!("Received notification: {:?}", notification);
            }
        }
    }
}
