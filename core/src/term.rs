use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Prop,
    Var(usize),
    Type(usize),
    App(Box<Term>, Box<Term>),
    Abs(Box<Term>, Box<Term>),
    Prod(Box<Term>, Box<Term>),
}

use Term::*;

impl Term {
    pub fn beta_reduction(self) -> Term {
        match self {
            App(box Abs(_, box mut t1), box t2) => {
                t1.substitute(t2, 1);
                t1
            }
            Abs(x, box t) => Abs(x, box t.beta_reduction()),
            _ => self,
        }
    }

    fn shift(&mut self, offset: usize) -> Term {
        match self {
            Var(x) => Var(*x + offset),
            App(box t1, box t2) => App(box t1.shift(offset), box t2.shift(offset)),
            Abs(x, box t) => Abs(x.clone(), box t.shift(offset)),
            Prod(x, box t) => Prod(x.clone(), box t.shift(offset)),
            _ => self.clone(),
        }
    }

    fn substitute(&mut self, rhs: Term, depth: usize) {
        match self {
            Var(i) if *i == depth => *self = rhs.clone().shift(depth - 1),
            Var(i) if *i != depth => *self = Var(*i - 1),
            App(l, r) => {
                l.substitute(rhs.clone(), depth);
                r.substitute(rhs, depth);
            }
            Abs(_, t) | Prod(_, t) => {
                t.substitute(rhs, depth + 1);
            }
            _ => {}
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Prop => write!(f, "\u{02119}"),
            Var(i) => write!(f, "{}", i),
            Type(i) => write!(f, "\u{1D54B}({})", i),
            App(t1, t2) => write!(f, "({} {})", t1, t2),
            Abs(t1, t2) => write!(f, "\u{003BB}{} \u{02192} {}", t1, t2),
            Prod(t1, t2) => write!(f, "\u{02200}{} \u{02192} {}", t1, t2),
        }
    }
}

#[test]
fn subst() {
    let term = Abs(
        box Prop,
        box App(
            box Abs(box Prop, box App(box Var(2), box Var(1))),
            box Var(1),
        ),
    );

    let reduced = Abs(box Prop, box App(box Var(1), box Var(1)));

    assert_eq!(term.beta_reduction(), reduced);
}
