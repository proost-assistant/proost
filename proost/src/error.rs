use derive_more::{Display, From};

/// The type of errors encountered by Proost during an interactive session.
///
/// Please note that some traits like `Clone` or `PartialEq` cannot be implemented here because
/// [`std::io::Error`] does not implement them.
#[derive(Debug, Display, From)]
pub enum Error<'arena> {
    Parser(parser::error::Error),
    Kernel(kernel::error::Error<'arena>),
    Toplevel(crate::command_processor::Error),

    IO(std::io::Error),
    RustyLine(rustyline::error::ReadlineError),
}

impl std::error::Error for Error<'_> {}

pub type Result<'arena, T> = std::result::Result<T, Error<'arena>>;
