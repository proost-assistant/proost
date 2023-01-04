//! A set of axioms hardcoded in the kernel.
//!
//! This is mostly used in order to provide inductive types to the user.
//! For now, no new axiom can be dynamically added by the user.

use derive_more::{Display, Deref};
use core::marker::PhantomData;

use crate::memory::arena::Arena;
use crate::memory::declaration::Declaration;
use crate::memory::level::Level;
use crate::memory::term::Term;

/// An enumeration representing the different kind of axioms hardcoded in the kernel.
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Atom {
    /// False
    False,

    /// The recursor over False
    FalseRec,

    /// Natural numbers
    Nat,

    /// Recursor over natural numbers
    NatRec,

    /// Zero (in the natural numbers)
    Zero,

    /// The successor function in the natural numbers
    Succ,
    // TODO add new axioms here
}

impl Atom {
    fn n_of_uargs(&self) -> usize {
        match self {
            Self::False => 0,
            Self::FalseRec => 1,
            Self::Nat => 0,
            Self::NatRec => 1,
            Self::Zero => 0,
            Self::Succ => 0,
        }
    }
}

struct Axiom<'arena>(Atom, &'arena [Level<'arena>]);

use Atom::{False, FalseRec, Nat, NatRec, Zero, Succ};

impl<'arena> Axiom<'arena> {

    fn new(atom: Atom, arena: &mut Arena<'arena>) -> Self {
        let n_of_uargs = atom.n_of_uargs();
        Self(atom, params)
    }

    /// Returns the type of a given axiom
    ///
    /// Because of memoisation, this is typically performed once per axiom.
    #[inline]
    pub fn get_type(&self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self.0 {
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
                let term_nat = Term::axiom(Self::Nat, arena);
                // Sort u
                let sort_u = Term::sort(Level::var(0, arena), arena);
                // Nat -> Sort u
                let motive = Term::prod(term_nat, sort_u, arena);
                // motive 0
                let motive_0 = Term::app(Term::var(1.into(), motive, arena), Term::axiom(Self::Zero, arena), arena);
                // (n : Nat) -> motive n -> motive (succ n)
                let motive_succ = Term::prod(
                    Term::axiom(Self::Nat, arena),
                    Term::prod(
                        Term::app(Term::var(3.into(), motive, arena), Term::var(1.into(), term_nat, arena), arena),
                        Term::app(
                            Term::var(4.into(), motive, arena),
                            Term::app(Term::axiom(Self::Succ, arena), Term::var(2.into(), term_nat, arena), arena),
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
            Zero => Term::axiom(Self::Nat, arena),
            Succ => Term::prod(Term::axiom(Self::Nat, arena), Term::axiom(Self::Nat, arena), arena),
        }
    }

    /// Adds with default names the list of hardcoded axioms to the given arena.
    #[inline]
    pub fn add_named_axioms(arena: &mut Arena) {
        let false_decl = Declaration(Term::axiom(Self::False, arena), 0);
        let false_rec_decl = Declaration(Term::axiom(Self::FalseRec, arena), 1);
        arena.bind_decl("False", false_decl);
        arena.bind_decl("False_rec", false_rec_decl);

        let nat_decl = Declaration(Term::axiom(Self::Nat, arena), 0);
        let nat_rec_decl = Declaration(Term::axiom(Self::NatRec, arena), 1);
        let zero_decl = Declaration(Term::axiom(Self::Zero, arena), 0);
        let succ_decl = Declaration(Term::axiom(Self::Succ, arena), 0);
        arena.bind_decl("Nat", nat_decl);
        arena.bind_decl("Nat_rec", nat_rec_decl);
        arena.bind_decl("Zero", zero_decl);
        arena.bind_decl("Succ", succ_decl);
    }
}
