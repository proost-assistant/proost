use crate::{Environment, KernelError, Term};
use derive_more::Display;

#[derive(Debug, Display, Clone, Eq, PartialEq)]
pub enum Command {
    #[display(fmt = "define {} := {}.", _0, _1)]
    Define(String, Term),

    #[display(fmt = "check {} : {}.", _0, _1)]
    CheckType(Term, Term),

    #[display(fmt = "type {}.", _0)]
    GetType(Term),

    #[display(fmt = "define {} : {} := {}.", _0, _1, _2)]
    DefineCheckType(String, Term, Term),
}

impl Command {
    pub fn process(self, env: &mut Environment) -> Result<Option<Term>, KernelError> {
        match self {
            Command::Define(s, t1) => match t1.infer(env) {
                Ok(t2) => match env.insert(s, t1, t2) {
                    Ok(()) => Ok(None),
                    Err(err) => Err(err),
                },
                Err(err) => Err(err),
            },
            Command::CheckType(t1, t2) => match t1.check(&t2, env) {
                Ok(_) => Ok(None),
                Err(err) => Err(err),
            },
            Command::GetType(t) => match t.infer(env) {
                Ok(t) => Ok(Some(t)),
                Err(err) => Err(err),
            },
            Command::DefineCheckType(_, t1, t2) => match t2.check(&t1, env) {
                Ok(_) => Ok(None),
                Err(err) => Err(err),
            },
        }
    }
}
