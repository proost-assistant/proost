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

phase!(Instantiated(Instantiate));
phase!(Serving(Serve));
phase!(Closing(Serve));

/// LSP server.
pub struct LspServer<P: Phase>(pub(crate) P::State);

pub struct Instantiate {
    pub connection: Connection,
}

pub struct Serve {
    pub connection: Connection,
}

impl LspServer<Instantiated> {
    pub fn new(connection: Connection) -> Self {
        LspServer(Instantiate { connection })
    }

    pub fn initialize(self) -> LspServer<Serving> {
        LspServer(Serve {
            connection: self.0.connection,
        })
    }
}

impl LspServer<Serving> {
    pub fn serving(self) -> LspServer<Closing> {
        LspServer(self.0)
    }
}

impl LspServer<Closing> {
    pub fn closing(self) {}
}
