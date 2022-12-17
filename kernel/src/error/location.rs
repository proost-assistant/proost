//! Location of terms in a text interface.
//!
//! Be aware that this portion of the code will be refactored (see issue #38 on GitLab).

use derive_more::{Constructor, Display, From};

/// Line and column position.
#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From, Ord, PartialOrd)]
#[display(fmt = "{line}:{column}")]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

/// Span of position.
#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From, Ord, PartialOrd)]
#[display(fmt = "{start}-{end}")]
pub struct Location {
    #[from(forward)]
    pub start: Position,

    #[from(forward)]
    pub end: Position,
}
