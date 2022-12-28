//! Location of terms in a text interface.
//!
//! Be aware that this portion of the code will be refactored (see issue #38 on GitLab).

use derive_more::{Constructor, Display, From};

/// Line and column position.
#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From, Ord, PartialOrd)]
#[display(fmt = "{line}:{column}")]
pub struct Position {
    /// the corresponding line number
    pub line: usize,

    /// the corresponding column number
    pub column: usize,
}

/// Span of position.
#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From, Ord, PartialOrd)]
#[display(fmt = "{start}-{end}")]
pub struct Location {
    /// The starting position of the location
    #[from(forward)]
    pub start: Position,

    /// The ending position of the location
    #[from(forward)]
    pub end: Position,
}
