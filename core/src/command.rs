use crate::Term;

#[derive(Clone, Debug)]
pub enum Command {
    Define(String, Term),
    CheckType(Term, Term),
    GetType(Term),
}
