//! Set of axioms, typing and reduction rules for the `Exists` type

use derive_more::Display;

use super::{Axiom, AxiomKind};
use crate::memory::arena::Arena;
use crate::memory::declaration::Declaration;
use crate::memory::level::Level;
use crate::memory::term::Term;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
/// Axioms regarding `Nat`ural numbers
pub enum Exists {
    /// The natural numbers
    Exists,

    /// The recursor over natural numbers
    ExistsIntro,

    /// Zero (in the natural numbers)
    Fst,

    /// The successor function in the natural numbers
    Snd,
}

impl<'arena> AxiomKind<'arena> for Exists {
    fn append_to_named_axioms(arena: &mut Arena<'arena>) {
        let var0 = Level::var(0, arena);

        let decl = Declaration(Term::axiom(Axiom::Exists(Self::Exists), &[var0], arena),1);
        arena.bind_decl("Exists", decl);

        let decl = Declaration(Term::axiom(Axiom::Exists(Self::ExistsIntro), &[var0], arena), 1);
        arena.bind_decl("Exists_intro", decl);

        let decl = Declaration(Term::axiom(Axiom::Exists(Self::Fst), &[var0], arena), 1);
        arena.bind_decl("fst", decl);

        let decl = Declaration(Term::axiom(Axiom::Exists(Self::Snd), &[var0], arena), 1);
        arena.bind_decl("snd", decl);
    }

    fn get_type(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self {
            Self::Exists => Self::type_exists(arena),
            Self::ExistsIntro => Self::type_exists_intro(arena),
            Self::Fst => Self::type_fst(arena),
            Self::Snd => Self::type_snd(arena),
        }
    }
}

impl Exists {

    fn type_exists<'arena>(arena:&mut Arena<'arena>) -> Term<'arena> {
        Term::sort(Level::var(0, arena), arena)
        .prod(
            Term::var(0.into(),Term::sort(Level::var(0, arena), arena),arena)
                .prod(Term::sort_usize(0,arena),arena),arena)
        .prod(Term::sort_usize(0,arena),arena)
    }

    /// Type of the recursor over natural numbers
    fn type_exists_intro<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        unreachable!("todo")
    }

    fn type_fst<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        unreachable!("todo")
    }

    fn type_snd<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        unreachable!("todo")
    }

}