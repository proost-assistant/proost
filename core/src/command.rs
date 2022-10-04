use crate::ClassicTerm;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Command {
    Define(String, ClassicTerm),
    CheckType(ClassicTerm, ClassicTerm),
    GetType(ClassicTerm),
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Command::Define(s, t) => write!(f, "Define {} := {}.", s, t),
            Command::CheckType(t1, t2) => write!(f, "Check {} : {}.", t1, t2),
            Command::GetType(t) => write!(f, "Type {}.", t),
        }
    }
}
