use derive_more::{Constructor, Display, From};

#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From)]
#[display(fmt = "{}:{}", line, column)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Constructor, Debug, Default, Display, Eq, PartialEq, From)]
#[display(fmt = "{}-{}", start, end)]
pub struct Location {
    pub start: Position,
    pub end: Position,
}
