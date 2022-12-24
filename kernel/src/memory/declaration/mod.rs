//! universe-polymorphic declarations.

use core::fmt;
use std::cell::OnceCell;

use derive_more::Display;

use super::arena::Arena;
use super::level::Level;
use super::term::Term;
use crate::error::ResultTerm;

pub mod builder;

/// A declaration is a term where some of its constituting universe levels may contain
/// universe-polymorphic variables.
///
/// None of these variables may be "free".
///
/// Declarations can be instantiated to create [`InstantiatedDeclaration`]s, which can in turn be
/// incorporated into [`Term`]s.
#[derive(Copy, Clone, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "{_0}")]
pub struct Declaration<'arena>(pub(crate) Term<'arena>, pub(crate) usize);

super::arena::new_dweller!(InstantiatedDeclaration, Header, Payload);

/// An instantiated declaration.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Payload<'arena> {
    /// The declaration being instantiated
    pub(crate) decl: Declaration<'arena>,

    /// The parameters used to instantiate it
    pub(crate) params: &'arena [Level<'arena>],
}

struct Header<'arena> {
    /// The corresponding term, where levels have been substituted.
    term: OnceCell<Term<'arena>>,
}

impl<'arena> fmt::Display for InstantiatedDeclaration<'arena> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(term) = self.0.header.term.get() {
            write!(f, "{term}")
        } else {
            write!(f, "({}).{{", self.0.payload.decl)?;

            let mut iter = self.0.payload.params.iter();

            iter.next().map_or(Ok(()), |level| write!(f, "{level}"))?;
            iter.try_for_each(|level| write!(f, ", {level}"))?;

            write!(f, "}}")
        }
    }
}

impl<'arena> Declaration<'arena> {
    pub(crate) const fn new(term: Term<'arena>, vars: usize) -> Self {
        Self(term, vars)
    }
}

impl<'arena> InstantiatedDeclaration<'arena> {
    /// Creates a new instantiated declaration from its base components. It is not verified that
    /// the provided slice matches in length the number of expected Levels.
    pub(crate) fn instantiate(decl: Declaration<'arena>, params: &[Level<'arena>], arena: &mut Arena<'arena>) -> Self {
        let new_node = Node {
            header: Header {
                term: OnceCell::new(),
            },
            payload: Payload {
                decl,
                params: arena.store_level_slice(params),
            },
        };

        if let Some(addr) = arena.hashcons_decls.get(&new_node) {
            Self::new(addr)
        } else {
            let addr = arena.alloc.alloc(new_node);
            arena.hashcons_decls.insert(addr);
            Self::new(addr)
        }
    }

    /// Returns the term linked to a definition in a given environment.
    #[inline]
    pub fn get_term(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        *self.0.header.term.get_or_init(|| self.0.payload.decl.0.substitute_univs(self.0.payload.params, arena))
    }

    /// Tries to type the generic underlying declaration. If it works, returns the type
    /// corresponding to the instantiated declaration, via a universe-variable substitution.
    pub(crate) fn get_type_or_try_init<F>(self, f: F, arena: &mut Arena<'arena>) -> ResultTerm<'arena>
    where
        F: FnOnce(Term<'arena>, &mut Arena<'arena>) -> ResultTerm<'arena>,
    {
        let term = self.0.payload.decl.0;
        term.get_type_or_try_init(|| f(term, arena)).map(|type_| type_.substitute_univs(self.0.payload.params, arena))
    }
}
