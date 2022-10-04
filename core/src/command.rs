use crate::ClassicTerm;
//use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Command {
    Define(String, ClassicTerm),
    CheckType(ClassicTerm, ClassicTerm),
    GetType(ClassicTerm),
}
