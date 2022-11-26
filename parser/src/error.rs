use derive_more::Display;
use kernel::Location;

/// Type representing parser errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}", kind)]
pub struct Error<'arena> {
    /// The kind of form error that occurred.
    pub kind: ErrorKind<'arena>,

    /// The location of the error.
    pub location: Location,
}

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind<'arena> {
    #[display(fmt = "cannot parse: {}", _0)]
    CannotParse(String),
    EarlyKernelError(kernel::Error<'arena>),
}

impl std::error::Error for Error<'_> {}

pub type Result<'arena, T> = std::result::Result<T, Error<'arena>>;
