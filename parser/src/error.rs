use derive_more::Display;
use kernel::location::Location;

/// Type representing parser errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{kind}")]
pub struct Error {
    /// The kind of form error that occurred.
    pub kind: Kind,

    /// The location of the error.
    pub location: Location,
}

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Kind {
    CannotParse(String),
}

impl std::error::Error for Error {}

pub type Result<T> = core::result::Result<T, Error>;
