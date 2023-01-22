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
    Exists_,

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

        let decl = Declaration(Term::axiom(Axiom::Exists(Self::Exists_), &[var0], arena), 1);
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
            Self::Exists_ => Self::type_exists(arena),
            Self::ExistsIntro => Self::type_exists_intro(arena),
            Self::Fst => Self::type_fst(arena),
            Self::Snd => Self::type_snd(arena),
        }
    }
}

impl Exists {
    /// Type of `Exists`
    fn type_exists<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        // (A : Sort u) -> (A -> Prop) -> Prop
        Term::sort(Level::var(0, arena), arena).prod(
            Term::var(1.into(), Term::sort(Level::var(0, arena), arena), arena)
                .prod(Term::sort_usize(0, arena), arena)
                .prod(Term::sort_usize(0, arena), arena),
            arena,
        )
    }

    /// Type of the introduction rule for the Exists type
    fn type_exists_intro<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        let var0 = Level::var(0, arena);

        // (A : Sort u) -> (B : A -> Prop) -> (x : A) -> (y : B x) -> Exists A B
        // (A : Sort u)
        Term::sort(var0, arena)
            // (B : A -> Prop) -> (x : A) -> (y : B x) -> Exists A B
            .prod(
                // (B : A -> Prop)
                Term::var(1.into(), Term::sort(var0, arena), arena)
                    .prod(Term::sort_usize(0, arena), arena)
                    // (x : A) -> (y : B x) -> Exists A B
                    .prod(
                        // (x : A)
                        Term::var(2.into(), Term::sort(var0, arena), arena)
                            // (y : B x) -> Exists A B
                            .prod(
                                // B x
                                Term::app(
                                    Term::var(
                                        2.into(),
                                        Term::var(3.into(), Term::sort(var0, arena), arena).prod(Term::sort_usize(0, arena), arena),
                                        arena,
                                    ),
                                    Term::var(1.into(), Term::var(3.into(), Term::sort(var0, arena), arena), arena),
                                    arena,
                                )
                                .prod(
                                    // Exists A B
                                    Term::axiom(super::Axiom::Exists(Self::Exists_), &[var0], arena)
                                        .app(Term::var(4.into(), Term::sort(var0, arena), arena), arena)
                                        .app(
                                            Term::var(
                                                3.into(),
                                                Term::var(4.into(), Term::sort(var0, arena), arena)
                                                    .prod(Term::sort_usize(0, arena), arena),
                                                arena,
                                            ),
                                            arena,
                                        ),
                                    arena,
                                ),
                                arena,
                            ),
                        arena,
                    ),
                arena,
            )
    }

    /// First projection of a dependant pair
    fn type_fst<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        // (A : Sort u) -> (A -> Prop) -> Exists A B -> A
        let var0 = Level::var(0, arena);

        // (A : Sort u) -> (B : A -> Prop) -> Exists A B -> A
        // (A : Sort u)
        Term::sort(var0, arena)
            // (B : A -> Prop) -> Exists A B -> A
            .prod(
                // (B : A -> Prop)
                Term::var(1.into(), Term::sort(var0, arena), arena)
                    .prod(Term::sort_usize(0, arena), arena)
                    .prod(
                        // Exists A B
                        Term::axiom(super::Axiom::Exists(Self::Exists_), &[var0], arena)
                            .app(Term::var(2.into(), Term::sort(var0, arena), arena), arena)
                            .app(
                                Term::var(
                                    1.into(),
                                    Term::var(2.into(), Term::sort(var0, arena), arena).prod(Term::sort_usize(0, arena), arena),
                                    arena,
                                ),
                                arena,
                            )
                            .prod(Term::var(3.into(), Term::sort(var0, arena), arena), arena),
                        arena,
                    ),
                arena,
            )
    }

    /// Second projection of a dependant pair
    fn type_snd<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        // (A : Sort u) -> (A -> Prop) -> (p : Exists A B) -> B (fst A B p)
        let var0 = Level::var(0, arena);
        // fst A B p
        let fst = Term::axiom(super::Axiom::Exists(Self::Fst), &[var0], arena)
            .app(Term::var(3.into(), Term::sort(var0, arena), arena), arena)
            .app(
                Term::var(
                    2.into(),
                    Term::var(3.into(), Term::sort(var0, arena), arena).prod(Term::sort_usize(0, arena), arena),
                    arena,
                ),
                arena,
            )
            .app(
                Term::var(
                    1.into(),
                    Term::axiom(super::Axiom::Exists(Self::Exists_), &[var0], arena)
                        .app(Term::var(3.into(), Term::sort(var0, arena), arena), arena)
                        .app(
                            Term::var(
                                2.into(),
                                Term::var(3.into(), Term::sort(var0, arena), arena).prod(Term::sort_usize(0, arena), arena),
                                arena,
                            ),
                            arena,
                        ),
                    arena,
                ),
                arena,
            );

        // (A : Sort u) -> (B : A -> Prop) -> (p : Exists A B) -> B (fst A B p)
        // (A : Sort u)
        Term::sort(var0, arena)
            // (B : A -> Prop) -> (p : Exists A B) -> B (fst A B p)
            .prod(
                // (B : A -> Prop)
                Term::var(1.into(), Term::sort(var0, arena), arena)
                    .prod(Term::sort_usize(0, arena), arena)
                    .prod(
                        // (p : Exists A B)
                        Term::axiom(super::Axiom::Exists(Self::Exists_), &[var0], arena)
                            .app(Term::var(2.into(), Term::sort(var0, arena), arena), arena)
                            .app(
                                Term::var(
                                    1.into(),
                                    Term::var(2.into(), Term::sort(var0, arena), arena).prod(Term::sort_usize(0, arena), arena),
                                    arena,
                                ),
                                arena,
                            )
                            // B (fst A B p)
                            .prod(
                                Term::var(
                                    2.into(),
                                    Term::var(3.into(), Term::sort(var0, arena), arena).prod(Term::sort_usize(0, arena), arena),
                                    arena,
                                )
                                .app(fst, arena),
                                arena,
                            ),
                        arena,
                    ),
                arena,
            )
    }
}
