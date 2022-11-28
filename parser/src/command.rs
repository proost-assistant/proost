//! High-level which constitute the end-user language.
//!
//! This complements low-level commands defined in the [`kernel::type_checker`] module.

use kernel::error::Result;
use kernel::term::arena::{Arena, Term};

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build, 'arena> {
    /// Define a new term and optionally check that it's type match the given one.
    Define(&'build str, Option<Term<'arena>>, Term<'arena>),
    /// Infer the type of a term and check that it match the given one.
    CheckType(Term<'arena>, Term<'arena>),
    /// Infer the type of a term.
    GetType(Term<'arena>),
}

pub trait CommandProcessor<'build, 'arena> {
    /// Processes a command in a given arena.
    fn process(
        &self,
        command: Command<'build, 'arena>,
        arena: &mut Arena<'arena>,
    ) -> Result<'arena, Option<Term<'arena>>> {
        match command {
            Command::Define(s, None, term) => {
                arena.infer(term)?;
                arena.bind(s, term);
                Ok(None)
            }

            Command::Define(s, Some(t), term) => {
                arena.check(term, t)?;
                arena.bind(s, term);
                Ok(None)
            }

            Command::CheckType(t1, t2) => {
                arena.check(t1, t2)?;
                Ok(None)
            }

            Command::GetType(t) => arena.infer(t).map(Some),
        }
    }
}

#[cfg(test)]
// Waiting for new kernel interactions (#44)
mod tests {}
