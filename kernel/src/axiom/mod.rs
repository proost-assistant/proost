//! A set of axioms hardcoded in the kernel.
//!
//! This is mostly used in order to provide inductive types to the user.
//! For now, no new axiom can be dynamically added by the user.

use derive_more::Display;

use crate::memory::arena::Arena;
use crate::memory::term::Term;

pub mod equality;
pub mod false_;
pub mod natural;

#[derive(Copy, Clone, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "_0")]
pub enum Axiom {
    Equality(equality::Equality),
    False(false_::False),
    Natural(natural::Natural),
}

impl Axiom {
    #[inline]
    pub fn add_named_axioms(arena: &mut Arena<'_>) {
        self::equality::Equality::append_to_named_axioms(arena);
        self::false_::False::append_to_named_axioms(arena);
        self::natural::Natural::append_to_named_axioms(arena);
    }

    #[inline]
    pub fn get_type<'arena>(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        use Axiom::{Equality, False, Natural};

        match self {
            Equality(axiom) => axiom.get_type(arena),
            False(axiom) => axiom.get_type(arena),
            Natural(axiom) => axiom.get_type(arena),
        }
    }

    /// Reduces a term if possible, returns None otherwise.
    #[inline]
    pub fn reduce_recursor<'arena>(term: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        let recursors = [equality::Equality::reduce, natural::Natural::reduce];

        recursors.into_iter().find_map(|f| f(term, arena))
    }
}

/// Trait that defines kind of axioms hardcoded in the kernel.
trait AxiomKind<'arena> {
    /// Adds axioms to the given arena.
    fn append_to_named_axioms(arena: &mut Arena<'arena>);

    /// Returns the type of a given axiom
    ///
    /// Because of memoisation, this is typically performed once per axiom.
    fn get_type(self, arena: &mut Arena<'arena>) -> Term<'arena>;

    /// Reduces a [`Term`] if it is an instance of the reducer, returns [`None`] otherwise.
    #[allow(unused_variables)]
    fn reduce(term: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        None
    }
}
