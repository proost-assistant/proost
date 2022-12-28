use derive_more::Display;

use crate::memory::arena::Arena;
use crate::memory::declaration::Declaration;
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
use crate::memory::level::Level;
impl Axiom {
    /// returns the type of a given axiom
    pub fn get_type<'arena>(&self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self {
            False => Term::sort_usize(0, arena),
            FalseRec => {
                // False
                let term_false = Term::axiom(False, arena);
                // Sort u
                let sort_u = Term::sort(Level::var(0, arena), arena);
                // False -> Sort u
                let motive = Term::prod(term_false, sort_u, arena);
                // motive t
                let app_motive = Term::app(Term::var(2.into(), motive, arena), Term::var(1.into(), term_false, arena), arena);
                // (t : False) -> motive t
                let prod_app_motive = Term::prod(term_false, app_motive, arena);
                // (motive : False -> Sort u) -> (t : False) -> motive t
                Term::prod(motive, prod_app_motive, arena)
            },

            Nat => Term::sort_usize(1, arena),
            NatRec => {
                // Nat
                let term_nat = Term::axiom(Nat, arena);
                // Sort u
                let sort_u = Term::sort(Level::var(0, arena), arena);
                // Nat -> Sort u
                let motive = Term::prod(term_nat, sort_u, arena);
                // motive 0
                let motive_0 = Term::app(Term::var(1.into(), motive, arena), Term::axiom(Zero, arena), arena);
                // (n : Nat) -> motive n -> motive (succ n)
                let motive_succ = Term::prod(
                    Term::axiom(Nat, arena),
                    Term::prod(
                        Term::app(Term::var(3.into(), motive, arena), Term::var(1.into(), term_nat, arena), arena),
                        Term::app(
                            Term::var(4.into(), motive, arena),
                            Term::app(Term::axiom(Succ, arena), Term::var(2.into(), term_nat, arena), arena),
                            arena,
                        ),
                        arena,
                    ),
                    arena,
                );
                // motive t
                let app_motive = Term::app(Term::var(4.into(), motive, arena), Term::var(1.into(), term_nat, arena), arena);
                // (t : Nat) -> motive t
                let prod_app_motive = Term::prod(term_nat, app_motive, arena);
                // (motive : Nat -> Sort u) -> motive 0 -> ((n : Nat) -> motive n -> motive (succ n)) -> (t : False) -> motive t
                Term::prod(motive, Term::prod(motive_0, Term::prod(motive_succ, prod_app_motive, arena), arena), arena)
            },
            Zero => Term::axiom(Nat, arena),
            Succ => Term::prod(Term::axiom(Nat, arena), Term::axiom(Nat, arena), arena),
        }
    }

    pub fn add_named_axioms<'arena>(arena: &mut Arena<'arena>) {
        let false_decl = Declaration(Term::axiom(False, arena), 0);
        let false_rec_decl = Declaration(Term::axiom(FalseRec, arena), 1);
        arena.bind_decl("False", false_decl);
        arena.bind_decl("False_rec", false_rec_decl);

        let nat_decl = Declaration(Term::axiom(Nat, arena), 0);
        let nat_rec_decl = Declaration(Term::axiom(NatRec, arena), 1);
        let zero_decl = Declaration(Term::axiom(Zero, arena), 0);
        let succ_decl = Declaration(Term::axiom(Succ, arena), 0);
        arena.bind_decl("Nat", nat_decl);
        arena.bind_decl("Nat_rec", nat_rec_decl);
        arena.bind_decl("Zero", zero_decl);
        arena.bind_decl("Succ", succ_decl);
    }
}
