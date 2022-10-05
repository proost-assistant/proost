use crate::ClassicTerm;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
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

fn try_from_assign(t: ClassicTerm, a: &mut Vec<String>) -> Result<Term, String> {
    match t {
        ClassicTerm::Prop => Ok(Term::Prop),
        ClassicTerm::Type(i) => Ok(Term::Type(i)),
        ClassicTerm::App(t1, t2) => {
            let t1 = box try_from_assign(*t1, a)?;
            let t2 = box try_from_assign(*t2, a)?;
            Ok(Term::App(t1, t2))
        }
        ClassicTerm::Var(s) => match a.iter().position(|r| *r == s) {
            Some(i) => Ok(Term::Var(i + 1)),
            None => Err(s),
        },
        ClassicTerm::Abs(s, t1, t2) => {
            a.push(s);
            let t1 = box try_from_assign(*t1, a)?;
            let t2 = box try_from_assign(*t2, a)?;
            a.pop();
            Ok(Term::Abs(t1, t2))
        }
        ClassicTerm::Prod(s, t1, t2) => {
            a.push(s);
            let t1 = box try_from_assign(*t1, a)?;
            let t2 = box try_from_assign(*t2, a)?;
            a.pop();
            Ok(Term::Prod(t1, t2))
        }
    }
}

impl TryFrom<ClassicTerm> for Term {
    type Error = String;
    fn try_from(t: ClassicTerm) -> Result<Self, Self::Error> {
        try_from_assign(t, &mut Vec::new())
    }
}
