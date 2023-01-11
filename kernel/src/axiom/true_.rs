//! Set of axioms and typing rules for the `True` type

use derive_more::Display;

use super::{Axiom, AxiomKind};
use crate::memory::arena::Arena;
use crate::memory::declaration::Declaration;
use crate::memory::level::Level;
use crate::memory::term::Term;

/// Axioms regarding `True`
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
pub enum True {
    /// True
    True,

    /// The default inhabitant of True
    Tt,

    /// The recursor over True
    TrueRec,
}

impl<'arena> AxiomKind<'arena> for True {
    fn append_to_named_axioms(arena: &mut Arena<'arena>) {
        let var0 = Level::var(0, arena);

        let decl = Term::axiom(Axiom::True(Self::True), &[], arena);
        arena.bind("True", decl);

        let decl = Term::axiom(Axiom::True(Self::Tt), &[], arena);
        arena.bind("Tt", decl);

        let decl = Declaration(Term::axiom(Axiom::True(Self::TrueRec), &[var0], arena), 1);
        arena.bind_decl("True_rec", decl);
    }

    fn get_type(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self {
            Self::True => Term::sort_usize(0, arena),
            Self::Tt => Term::axiom(Axiom::True(Self::True), &[], arena),
            Self::TrueRec => Self::type_true_rec(arena),
        }
    }
}

impl True {
    /// Type of the recursor over `False` proof witnesses
    fn type_true_rec<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        // True
        let term_true = Term::axiom(True, &[], arena);
        // Sort u
        let sort_u = Term::sort(Level::var(0, arena), arena);
        // True -> Sort u
        let motive = Term::prod(term_true, sort_u, arena);
        // motive t
        let app_motive = Term::app(Term::var(3.into(), motive, arena), Term::var(1.into(), term_true, arena), arena);
        // (t: True) -> motive t
        let prod_app_motive = Term::prod(term_true, app_motive, arena);
        // tt
        let term_tt = Term::axiom(Tt, &[], arena);
        // motive tt
        let motive_tt = Term::app(Term::var(1.into(), motive, arena), term_tt, arena);
        // (motive tt) -> (t: True) -> motive t
        let prod_app_motive_tt = Term::prod(motive_tt, prod_app_motive, arena);
        // (motive: True -> Sort u) -> (motive tt) -> (t: True) -> motive t
        Term::prod(motive, prod_app_motive_tt, arena)
    }
}
