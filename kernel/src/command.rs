use crate::Term;
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
