use core::Term;
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
