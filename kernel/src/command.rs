use crate::error::Result;
use crate::{Arena, Term};

#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build, 'arena> {
    Define(&'build str, Option<Term<'arena>>, Term<'arena>),

    CheckType(Term<'arena>, Term<'arena>),

    GetType(Term<'arena>),
}

impl<'build, 'arena> Command<'build, 'arena> {
    pub fn process(self, env: &mut Arena<'arena>) -> Result<'arena, Option<Term<'arena>>> {
        match self {
            Command::Define(s, None, term) => {
                env.infer(term)?;
                env.bind(s, term);
                Ok(None)
            }

            Command::Define(s, Some(t), term) => {
                env.check(term, t)?;
                env.bind(s, term);
                Ok(None)
            }

            Command::CheckType(t1, t2) => {
                env.check(t1, t2)?;
                Ok(None)
            }

            Command::GetType(t) => env.infer(t).map(Some),
        }
    }
}

#[cfg(test)]
mod tests {}
