use kernel::derive_more::Display;
use kernel::num_bigint::BigUint;
use kernel::Term;

#[derive(Clone, Debug, Display)]
pub enum ClassicTerm {
    #[display(fmt = "{}", _0)]
    Var(String),

    #[display(fmt = "\u{02119}")]
    Prop,

    #[display(fmt = "Type({})", _0)]
    Type(BigUint),

    #[display(fmt = "({} {})", _0, _1)]
    App(Box<ClassicTerm>, Box<ClassicTerm>),

    #[display(fmt = "\u{003BB}{} \u{02192} {}", _0, _1)]
    Abs(String, Box<ClassicTerm>, Box<ClassicTerm>),

    #[display(fmt = "\u{02200}{} \u{02192} {}", _0, _1)]
    Prod(String, Box<ClassicTerm>, Box<ClassicTerm>),
}

fn try_from_assign(term: ClassicTerm, known_ids: &mut Vec<String>) -> Result<Term, String> {
    match term {
        ClassicTerm::Prop => Ok(Term::Prop),
        ClassicTerm::Type(i) => Ok(Term::Type(i.into())),
        ClassicTerm::App(t1, t2) => {
            let t1 = box try_from_assign(*t1, known_ids)?;
            let t2 = box try_from_assign(*t2, known_ids)?;

            Ok(Term::App(t1, t2))
        }
        ClassicTerm::Var(s) => match known_ids.iter().rev().position(|r| *r == s) {
            Some(i) => Ok(Term::Var((i + 1).into())),
            None => Err(s),
        },
        ClassicTerm::Abs(s, t1, t2) => {
            known_ids.push(s);
            let t1 = box try_from_assign(*t1, known_ids)?;
            let t2 = box try_from_assign(*t2, known_ids)?;
            known_ids.pop();

            Ok(Term::Abs(s, t1, t2))
        }
        ClassicTerm::Prod(s, t1, t2) => {
            known_ids.push(s);
            let t1 = box try_from_assign(*t1, known_ids)?;
            let t2 = box try_from_assign(*t2, known_ids)?;
            known_ids.pop();

            Ok(Term::Prod(s, t1, t2))
        }
    }
}

impl TryFrom<ClassicTerm> for Term {
    type Error = String;

    fn try_from(t: ClassicTerm) -> Result<Self, Self::Error> {
        try_from_assign(t, &mut Vec::new())
    }
}
