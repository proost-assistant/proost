//! Location of elements in a text interface.

use derive_more::{Constructor, Display};

/// Line and column position.
#[derive(Clone, Copy, Constructor, Debug, Default, Display, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{line}:{column}")]
pub struct Position {
    /// Line number, starts at 1.
    pub line: usize,

    /// Column number, starts at 1.
    pub column: usize,
}

/// Span of position.
#[derive(Clone, Copy, Debug, Default, Display, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{start}-{end}")]
pub struct Location {
    /// Start position.
    pub start: Position,

    /// End position.
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
