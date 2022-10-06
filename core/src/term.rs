use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Prop,
    Var(usize),
    Type(usize),
    App(Box<Term>, Box<Term>),
    Abs(Box<Term>, Box<Term>),
    Prod(Box<Term>, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Term::Prop => write!(f, "\u{02119}"),
            Term::Var(i) => write!(f, "{}", i),
            Term::Type(i) => write!(f, "\u{1D54B}({})", i),
            Term::App(t1, t2) => write!(f, "({}) {}", t1, t2),
            Term::Abs(t1, t2) => write!(f, "\u{003BB}{} \u{02192} {}", t1, t2),
            Term::Prod(t1, t2) => write!(f, "\u{02200}{} \u{02192} {}", t1, t2),
        }
    }
}
