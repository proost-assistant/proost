//! Set of axioms and typing rules for the `False` type

use derive_more::Display;

use super::{Axiom, AxiomKind};
use crate::memory::arena::Arena;
use crate::memory::declaration::Declaration;
use crate::memory::level::Level;
use crate::memory::term::Term;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
/// Axioms regarding `False`
pub enum False {
    /// False
    False,

    /// The recursor over False
    FalseRec,
}

impl<'arena> AxiomKind<'arena> for False {
    #[inline]
    fn append_to_named_axioms(arena: &mut Arena<'arena>) {
        let var0 = Level::var(0, arena);

        let decl = Term::axiom(Axiom::False(Self::False), &[], arena);
        arena.bind("False", decl);

        let decl = Declaration(Term::axiom(Axiom::False(Self::FalseRec), &[var0], arena), 1);
        arena.bind_decl("False_rec", decl);
    }

    #[inline]
    fn get_type(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self {
            Self::False => Term::sort_usize(0, arena),
            Self::FalseRec => Self::type_false_rec(arena),
        }
    }
}

impl False {
    /// Type of the recursor over `False` proof witnesses
    fn type_false_rec<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        // False
        let term_false = Term::axiom(Axiom::False(Self::False), &[], arena);

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
    }
}
