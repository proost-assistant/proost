//! Declarations constructed through commands.
//!
//! A declaration describes a constant in the
//! environment, whether it's a definition with a corresponding term, or an axiom with only a type.
//! univ_vars corresponds to the number of universe variables bound to the declaration.
//! No universe variable can be "free" in a term, meaning that for all Var(i) in ty or term,
//! `i < univ_vars`. Additionally, ty and term *should* in theory always have the same number of
//! universe variables, and as such, only a single method is needed. However, additional checks to
//! ensure this invariant will have to be put in place. For now, when constructing declarations,
//! only the number of universes in ty are counted.

use std::cell::OnceCell;
use std::fmt;

use derive_more::Display;

use super::arena::Arena;
use super::level::Level;
use super::term::Term;

pub mod builder;

#[derive(Copy, Clone, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "{_0}")]
pub struct Declaration<'arena>(Term<'arena>, usize);

super::arena::new_dweller!(InstantiatedDeclaration, Header, Payload);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Payload<'arena> {
    pub decl: Declaration<'arena>,
    pub params: &'arena [Level<'arena>],
}

struct Header<'arena> {
    term: OnceCell<Term<'arena>>,
}

impl<'arena> fmt::Display for InstantiatedDeclaration<'arena> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.header.term.get() {
            Some(term) => write!(f, "{term}"),
            None => {
                write!(f, "({}).{{", self.0.payload.decl)?;
                self.0.payload.params.iter().try_for_each(|level| write!(f, "{level}, "))?;
                write!(f, "}}")
            },
        }
    }
}

impl<'arena> Declaration<'arena> {
    pub(crate) fn new(term: Term<'arena>, vars: usize) -> Self {
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

        match arena.hashcons_decls.get(&new_node) {
            Some(addr) => Self::new(addr),
            None => {
                let addr = arena.alloc.alloc(new_node);
                arena.hashcons_decls.insert(addr);
                Self::new(addr)
            },
        }
    }

    /// Returns the term linked to a definition in a given environment.
    pub fn get_term(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        *self.0.header.term.get_or_init(|| self.0.payload.decl.0.substitute_univs(self.0.payload.params, arena))
    }
}
