use derive_more::Display;

use crate::memory::arena::Arena;
use crate::memory::term::Term;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Axiom {
    False,
    FalseRec,

    Nat,
    NatRec,
    Zero,
    Succ,
    // TODO add new axioms here
}

use crate::axiom::Axiom::*;

impl Axiom {
    /// returns the type of a given axiom
    pub fn get_type<'arena>(&self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self {
            False => Term::sort_usize(0, arena),
            FalseRec => unreachable!("TODO"),

            Nat => Term::sort_usize(1, arena),
            NatRec => unreachable!("TODO"),
            Zero => Term::axiom(Nat, arena),
            Succ => Term::prod(Term::axiom(Nat, arena), Term::axiom(Nat, arena), arena),
        }
    }

    pub fn is_recursor(&self) -> bool {
        matches!(self, FalseRec | NatRec)
    }
}
