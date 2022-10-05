use crate::Term;
//use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Command {
    Define(String, Term),
    CheckType(Term, Term),
    GetType(Term),
}
