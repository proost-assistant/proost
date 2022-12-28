//! Errors that can be yielded by the parser.

use derive_more::Display;
use utils::location::Location;

/// The type representing [parser errors](Kind) with associated [`Location`].
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{kind}")]
pub struct Error {
    /// The kind of form error that occurred.
    pub kind: Kind,

    /// The trace.
    pub loc: Location,
}

/// The type of errors that can be encountered by the parser.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Kind {
    /// An error that occurs when the parser encounters an unexpected token.
    CannotParse(String),
}

/// Specify the [`Result`](core::result::Result) type for the parser.
pub type Result<T> = core::result::Result<T, Error>;
