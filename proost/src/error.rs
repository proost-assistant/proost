//! Error management

use derive_more::{Display, From};

use crate::evaluator;

/// The type of errors encountered by Proost during an interactive session.
///
/// Please note that some traits like `Clone` or `PartialEq` cannot be implemented here because
/// [`std::io::Error`] does not implement them.
#[derive(Display, From)]
pub enum Error<'arena, 'build> {
    /// An error raised by the [`kernel`].
    #[display(fmt = "{_1}")]
    Kernel(&'build dyn utils::trace::Traceable, kernel::error::Error<'arena>),

    /// An error raised by the [`parser`].
    Parser(parser::error::Error),

    /// An error raised by the [evaluator](crate::evaluator).
    TopLevel(evaluator::Error),

    /// An input/output error (see [`std::io::Error`]).
    Io(std::io::Error),

    /// A RustyLine error (see [`rustyline::error::ReadlineError`]).
    RustyLine(rustyline::error::ReadlineError),
}

impl core::fmt::Debug for Error<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for Error<'_, '_> {}

/// The type of results yielded by the toplevel.
pub type Result<'arena, 'build, T> = core::result::Result<T, Error<'arena, 'build>>;
