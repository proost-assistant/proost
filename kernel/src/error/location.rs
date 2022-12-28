//! Location of elements in a text interface.

use derive_more::{Constructor, Display};

/// Line and column position.
#[derive(Clone, Copy, Constructor, Debug, Default, Display, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{line}:{column}")]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

/// Span of position.
#[derive(Clone, Copy, Debug, Default, Display, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{start}-{end}")]
pub struct Location {
    pub start: Position,

    pub end: Position,
}

impl Location {
    /// Create a new location.
    #[inline]
    #[must_use]
    pub fn new((start_line, start_column): (usize, usize), (end_line, end_column): (usize, usize)) -> Self {
        Self {
            start: Position::new(start_line, start_column),
            end: Position::new(end_line, end_column),
        }
    }
}
