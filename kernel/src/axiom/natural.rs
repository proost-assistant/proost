//! Set of axioms, typing and reduction rules for the `Nat`ural numbers

use derive_more::Display;

use super::{Axiom, AxiomKind};
use crate::memory::arena::Arena;
use crate::memory::declaration::Declaration;
use crate::memory::level::Level;
use crate::memory::term::Term;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
/// Axioms regarding `Nat`ural numbers
pub enum Natural {
    /// The natural numbers
    Nat,

    /// The recursor over natural numbers
    NatRec,

    /// Zero (in the natural numbers)
    Zero,

    /// The successor function in the natural numbers
    Succ,
}

impl<'arena> AxiomKind<'arena> for Natural {
    fn append_to_named_axioms(arena: &mut Arena<'arena>) {
        let var0 = Level::var(0, arena);

        let decl = Term::axiom(Axiom::Natural(Self::Nat), &[], arena);
        arena.bind("Nat", decl);

        let decl = Declaration(Term::axiom(Axiom::Natural(Self::NatRec), &[var0], arena), 1);
        arena.bind_decl("Nat_rec", decl);

        let decl = Term::axiom(Axiom::Natural(Self::Zero), &[], arena);
        arena.bind("Zero", decl);

        let decl = Term::axiom(Axiom::Natural(Self::Succ), &[], arena);
        arena.bind("Succ", decl);
    }

    fn get_type(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self {
            Self::Nat => Term::sort_usize(1, arena),
            Self::NatRec => Self::type_nat_rec(arena),
            Self::Zero => Term::axiom(Axiom::Natural(Self::Nat), &[], arena),
            Self::Succ => Term::prod(
                Term::axiom(Axiom::Natural(Self::Nat), &[], arena),
                Term::axiom(Axiom::Natural(Self::Nat), &[], arena),
                arena,
            ),
        }
    }

    fn reduce(term: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        use crate::memory::term::Payload::{App, Axiom};

        // The multiple `let` statements can be easily rewritten as a pattern match
        // if https://github.com/rust-lang/rfcs/issues/2099 is solved.

        let App(f, n) = *term else { return None; };
        let App(f, motive_succ) = *f.whnf(arena) else { return None; };
        let App(f, motive_0) = *f.whnf(arena) else { return None; };
        let App(f, motive) = *f.whnf(arena) else { return None; };
        let Axiom(super::Axiom::Natural(Self::NatRec), lvl) = *f.unfold(arena).whnf(arena) else { return None; };

        match *n.whnf(arena) {
            Axiom(super::Axiom::Natural(Self::Zero), _) => Some(motive_0),
            App(f, n) => {
                let Axiom(super::Axiom::Natural(Self::Succ), _) = *f.unfold(arena).whnf(arena) else { return None; };

                let new_rec = Term::app(
                    Term::app(
                        Term::app(
                            Term::app(Term::axiom(super::Axiom::Natural(Self::NatRec), lvl, arena), motive, arena),
                            motive_0,
                            arena,
                        ),
                        motive_succ,
                        arena,
                    ),
                    n,
                    arena,
                );

                Some(Term::app(Term::app(motive_succ, n, arena), new_rec, arena))
            },
            _ => None,
        }
    }
}

impl Natural {
    /// Type of the recursor over natural numbers
    fn type_nat_rec<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        // Nat
        let term_nat = Term::axiom(Axiom::Natural(Self::Nat), &[], arena);

        // Sort u
        let sort_u = Term::sort(Level::var(0, arena), arena);

        // Nat -> Sort u
        let motive = Term::prod(term_nat, sort_u, arena);

        // motive 0
        let motive_0 = Term::app(Term::var(1.into(), motive, arena), Term::axiom(Axiom::Natural(Self::Zero), &[], arena), arena);

        // (n : Nat) -> motive n -> motive (succ n)
        let motive_succ = Term::prod(
            Term::axiom(Axiom::Natural(Self::Nat), &[], arena),
            Term::prod(
                Term::app(Term::var(3.into(), motive, arena), Term::var(1.into(), term_nat, arena), arena),
                Term::app(
                    Term::var(4.into(), motive, arena),
                    Term::app(Term::axiom(Axiom::Natural(Self::Succ), &[], arena), Term::var(2.into(), term_nat, arena), arena),
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

        // (motive : Nat -> Sort u) -> motive 0 -> ((n : Nat) -> motive n -> motive (succ n)) -> (t : Nat) -> motive t
        Term::prod(motive, Term::prod(motive_0, Term::prod(motive_succ, prod_app_motive, arena), arena), arena)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_well_typedness() {
        crate::memory::arena::use_arena(|arena| {
            assert!(Term::axiom(Axiom::Natural(Natural::Nat), &[], arena).infer(arena).is_ok());
            assert!(Term::axiom(Axiom::Natural(Natural::Zero), &[], arena).infer(arena).is_ok());
            assert!(Term::axiom(Axiom::Natural(Natural::Succ), &[], arena).infer(arena).is_ok());
        });
    }

    #[test]
    fn reduce_nat() {
        crate::memory::arena::use_arena(|arena| {
            let lvl_one = Level::succ(Level::zero(arena), arena);
            let nat = Term::axiom(Axiom::Natural(Natural::Nat), &[], arena);
            let zero = Term::axiom(Axiom::Natural(Natural::Zero), &[], arena);
            let one = Term::app(Term::axiom(Axiom::Natural(Natural::Succ), &[], arena), zero, arena);
            let to_zero = Term::app(
                Term::app(
                    Term::app(
                        Term::axiom(Axiom::Natural(Natural::NatRec), arena.store_level_slice(&[lvl_one]), arena),
                        Term::abs(nat, nat, arena),
                        arena,
                    ),
                    zero,
                    arena,
                ),
                Term::abs(nat, Term::abs(nat, zero, arena), arena),
                arena,
            );
            let zero_to_zero = Term::app(to_zero, zero, arena);
            let one_to_zero = Term::app(to_zero, one, arena);
            let nat_to_zero = Term::app(to_zero, nat, arena);

            assert_eq!(zero_to_zero.normal_form(arena), zero);
            assert_eq!(one_to_zero.normal_form(arena), zero);
            assert_eq!(nat_to_zero.whnf(arena), nat_to_zero);
        });
    }
}
