use kernel::derive_more::Display;
use kernel::Location;

/// Type representing parser errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}", kind)]
pub struct Error {
    /// The kind of form error that occurred.
    pub kind: ErrorKind,

    /// The location of the error.
    pub location: Location,
}

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind {
    #[display(fmt = "cannot parse: {}", _0)]
    CannotParse(String),
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
