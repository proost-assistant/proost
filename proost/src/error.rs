use derive_more::{Display, From};

#[derive(Clone, Debug, Display, From, Eq, PartialEq)]
pub enum Error<'arena> {
    Parser(parser::error::Error<'arena>),
    Kernel(kernel::error::Error<'arena>),
}

impl std::error::Error for Error<'_> {}

pub type Result<'arena, T> = std::result::Result<T, Error<'arena>>;
