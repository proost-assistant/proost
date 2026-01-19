//! universe-polymorphic declarations.

use core::fmt;
use std::cell::OnceCell;

use derive_more::Display;

use super::arena::Arena;
use super::level::Level;
use super::term::Term;
use crate::error::ResultTerm;

pub mod builder;

#[derive(Clone,Copy, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "{ty}")]
pub struct Constant<'arena> {
    // name: String,
    ty : Term<'arena>,
    n_levels : usize
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "{constant}")]
pub struct Definition<'arena> {
    constant : Constant<'arena>,
    term : Term<'arena>,
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "{constant}")]
pub struct Inductive<'arena> {
    constant : Constant<'arena>,
    constructor : Vec<Constructor<'arena>>,
    n_params : usize,
    n_indices :  usize,
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "{constant}")]
pub struct Constructor<'arena> {
    constant : Constant<'arena>,
    ind : Inductive<'arena>,
    constructor_num : usize,
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
#[display(fmt = "{constant}")]
// TODO to handle mutual/nested types, we will need to add two fields `num_motives` and `num_minors`
pub struct Recursor<'arena> {
    constant : Constant<'arena>,
    ind : Inductive<'arena>,
    reduction_rules : Vec<Term<'arena>>,
}

/// A declaration is a term where some of its constituting universe levels may contain
/// universe-polymorphic variables.
///
/// Declarations can be instantiated to create [`InstantiatedDeclaration`]s, which can in turn be
/// incorporated into [`Term`]s. No variable may remain in the instantiated declaration.
#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
pub enum Declaration<'arena> {
    #[display(fmt = "{_0}")]
    Definition(Definition<'arena>),
    #[display(fmt = "{_0}")]
    Inductive(Inductive<'arena>),
    #[display(fmt = "{_0}")]
    Constructor(Constructor<'arena>),
    #[display(fmt = "{_0}")]
    Recursor(Recursor<'arena>)
}

super::arena::new_dweller!(InstantiatedDeclaration, Header, Payload);

/// An instantiated declaration.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Payload<'arena> {
    /// The declaration being instantiated.
    pub(crate) decl: Declaration<'arena>,

    /// The parameters used to instantiate it
    pub(crate) params: &'arena [Level<'arena>],
}

/// The header of an instantiated declaration.
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
    /// Creates a declaration from a pair of arguments.
    pub(crate) const fn new(term: Term<'arena>, vars: usize) -> Self {
        // Declaration::Definition(term, vars)
        todo!()
    }
    
    pub(crate) fn to_constant(self) -> Constant<'arena> {
        match self {
            Declaration::Definition(d)   => return d.constant,
            Declaration::Inductive(d)     => return d.constant,
            Declaration::Constructor(d) => return d.constant,
            Declaration::Recursor(d)       => return d.constant,
        }
    }

    pub(crate) fn get_type(decl : Declaration) -> Term {
        return decl.to_constant().ty 
    }

    pub(crate) fn try_get_term(decl : Declaration) -> Option<Term> {
        match decl {
            Declaration::Definition(d) => return Some(d.term),
            _ => return None
        }
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
    pub fn get_term(self, arena: &mut Arena<'arena>) -> ResultTerm<'arena> {
        todo!()
        // self
        //     .0
        //     .header
        //     .term
        //     .get_or_try_init(|| todo!())
    }

    pub(crate) fn get_type(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        return self.0.payload.decl.to_constant().ty.substitute_univs(self.0.payload.params, arena)
    }

    /// Tries to type the generic underlying declaration. If it works, returns the type
    /// corresponding to the instantiated declaration, via a universe-variable substitution.
    pub(crate) fn get_type_or_try_init<F>(self, f: F, arena: &mut Arena<'arena>) -> Term<'arena>
    where
        F: FnOnce(Term<'arena>, &mut Arena<'arena>) -> Term<'arena>,
    {
        todo!()
        // f(Declaration::try_get_term(self.0.payload.decl), arena).map(|type_| type_.substitute_univs(self.0.payload.params, arena))
    }
}
