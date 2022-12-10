pub mod closing;
pub mod dispatcher;
pub mod initializing;
pub mod instantiated;
pub mod serving;

use derive_more::Deref;
use threadpool::ThreadPool;

use crate::connection::Connection;

/// Sealed prevent others from implementing the trait `Phase`.
mod private {
    pub trait Sealed {}
}

/// Represents a phase of the LSP server.
pub trait Phase: private::Sealed {
    type State;
}

macro_rules! phase {
    ($name:ident ($state:ident)) => {
        pub enum $name {}

        impl Phase for $name {
            type State = $state;
        }

        impl private::Sealed for $name {}
    };
}

type Unit = ();

phase!(Instantiated(Unit));
phase!(Initializing(Initialize));
phase!(Serving(Serve));
phase!(Closing(Serve));

/// LSP server.
#[derive(Deref)]
pub struct LspServer<P: Phase>(pub P::State);

pub struct Initialize {
    connection: Connection,
    thread_pool: ThreadPool,
}

pub struct Serve {
    connection: Connection,
}
