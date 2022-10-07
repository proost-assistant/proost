use crate::Term;
use derive_more::Display;

#[derive(Debug, Display, Eq, PartialEq)]
pub enum Command {
    #[display(fmt = "Define {} := {}.", _0, _1)]
    Define(String, Term),

    #[display(fmt = "Check {} : {}.", _0, _1)]
    CheckType(Term, Term),

    #[display(fmt = "Type {}.", _0)]
    GetType(Term),
}
