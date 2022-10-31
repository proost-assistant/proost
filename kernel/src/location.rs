// TODO: Waiting for #15 to handle correctly location inside Terms
use derive_more::{Constructor, Display, From};

/// Line and column position.
#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From, Ord, PartialOrd)]
#[display(fmt = "{}:{}", line, column)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

/// Span of position.
#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From, Ord, PartialOrd)]
#[display(fmt = "{}-{}", start, end)]
pub struct Location {
    #[from(forward)]
    pub start: Position,

    #[from(forward)]
    pub end: Position,
}
