use derive_more::{Display, From};

/// The type of errors encountered by Proost during an interactive session.
///
/// Please note that some traits like `Clone` or `PartialEq` cannot be implemented here because
/// [`std::io::Error`] does not implement them.
#[derive(Display, From)]
pub enum Error<'arena, 'build> {
    #[display(fmt = "{_1}")]
    Kernel(Box<dyn kernel::error::trace::Traceable + 'build>, kernel::error::Error<'arena>),

    Parser(parser::error::Error),

    Toplevel(crate::evaluator::Error),

    Io(std::io::Error),

    RustyLine(rustyline::error::ReadlineError),
}

impl core::fmt::Debug for Error<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for Error<'_, '_> {}

pub type Result<'arena, 'build, T> = core::result::Result<T, Error<'arena, 'build>>;
