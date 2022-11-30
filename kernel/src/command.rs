//! High-level commands that the kernel may receive.
//!
//! This complements low-level commands defined in the [`crate::type_checker`] module.
//!
//! Be aware that this portion of the code will be refactored (see issue #44 on GitLab).

use crate::error::Result;
use crate::term::arena::{Arena, Term};

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build, 'arena> {
    Define(&'build str, Option<Term<'arena>>, Term<'arena>),

    CheckType(Term<'arena>, Term<'arena>),

    GetType(Term<'arena>),
}

impl<'build, 'arena> Command<'build, 'arena> {
    /// Processes a command in a given arena.
    pub fn process(self, arena: &mut Arena<'arena>) -> Result<'arena, Option<Term<'arena>>> {
        match self {
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
