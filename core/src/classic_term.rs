use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum ClassicTerm {
    Prop,
    Var(String),
    Type(usize),
    App(Box<ClassicTerm>, Box<ClassicTerm>),
    Abs(String, Box<ClassicTerm>, Box<ClassicTerm>),
    Prod(String, Box<ClassicTerm>, Box<ClassicTerm>),
}

impl Display for ClassicTerm {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            ClassicTerm::Prop => write!(f, "\u{02119}"),
            ClassicTerm::Var(i) => write!(f, "{}", i),
            ClassicTerm::Type(i) => write!(f, "\u{1D54B}({})", i),
            ClassicTerm::App(t1, t2) => write!(f, "({}) {}", t1, t2),
            ClassicTerm::Abs(s, t1, t2) => write!(f, "\u{003BB}{} : {} \u{02192} {}", s, t1, t2),
            ClassicTerm::Prod(s, t1, t2) => write!(f, "\u{02200}{} : {} \u{02192} {}", s, t1, t2),
        }
    }
}
