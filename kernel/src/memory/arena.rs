//! A comprehensive memory management unit for terms.
//!
//! This module defines the core functions used to manipulate an arena and its terms.

use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use bumpalo::Bump;

use super::declaration::Declaration;
use super::level::Level;
use super::term::Term;

/// A comprehensive memory management unit for terms.
///
/// An arena is a location in memory where a group of terms with several properties is stored. Most
/// importantly, it ensures that all terms living in the arena are syntactically unique, which
/// accelerates many algorithms. In particular, this property allows for *memoizing* easily
/// operations on terms like substitution, shifting, type checking, etc. It also facilitates the
/// [building of terms](super::builders) which are named or use named terms.
///
/// This paradigm of memory management is akin to what is usually lectured for Binary Decision
/// Diagrams (BDD) management. Additionally, it makes use of Rust features to provide a clean
/// interface: the arena type is invariant over its lifetime argument (usually called `'arena`),
/// which together with the [`use_arena`] function, enforces strong guarantees on how the arena can
/// be used, particularly if several of them are used simultaneously.
///
/// Early versions of this system are freely inspired by an assignment designed by
/// [Jacques-Henri Jourdan](<https://jhjourdan.mketjh.fr>).
pub struct Arena<'arena> {
    pub(super) alloc: &'arena Bump,

    // enforces invariances over lifetime parameter
    _phantom: PhantomData<*mut &'arena ()>,

    // Hashconsing of terms, levels and declarations, at the heart of the uniqueness property
    // Please note that [`Level`](super::level::Level) behave differently because it has an
    // additional *reduced form* invariant.
    pub(super) hashcons_terms: HashSet<&'arena super::term::Node<'arena>>,
    pub(super) hashcons_decls: HashSet<&'arena super::declaration::Node<'arena>>,
    pub(super) hashcons_levels: HashMap<&'arena super::level::Node<'arena>, super::level::Level<'arena>>,

    named_decls: HashMap<&'arena str, Declaration<'arena>>,
    named_terms: HashMap<&'arena str, Term<'arena>>,

    // Hash maps used to speed up certain algorithms. See also `OnceCell`s in [`Term`]
    pub(super) mem_subst: HashMap<(Term<'arena>, Term<'arena>, usize), Term<'arena>>,
    // TODO shift hashmap (see #45)
    // requires the design of an additional is_certainly_closed predicate in terms.
}

/// This function is the main function that the kernel exports. Most importantly, it is the only
/// one to provide an entry point for Arena objects, by means of a closure provided by the end
/// user.
///
/// Such an interface is the most elegant way to ensure the one-to-one correspondence between
/// lifetime parameters and [`Arena`] objects.
///
/// To generate the `alloc` object in this function is necessary, as this is the main way to
/// "create" a lifetime variable which makes sense. That way, `'arena` is valid exactly during
/// the execution of the function `f`.
pub fn use_arena<F, T>(f: F) -> T
where
    F: for<'arena> FnOnce(&mut Arena<'arena>) -> T,
{
    let alloc = Bump::new();
    let mut arena = Arena::new(&alloc);
    f(&mut arena)
}

impl<'arena> Arena<'arena> {
    /// Allocates a new memory arena. As detailed in the [`use_arena`] function, it is necessary to
    /// externalise the generation of the [`bumpalo::Bump`] object.
    fn new(alloc: &'arena Bump) -> Self {
        Arena {
            alloc,
            _phantom: PhantomData,

            hashcons_terms: HashSet::new(),
            hashcons_decls: HashSet::new(),
            hashcons_levels: HashMap::new(),

            named_decls: HashMap::new(),
            named_terms: HashMap::new(),

            mem_subst: HashMap::new(),
        }
    }

    pub(crate) fn store_level_slice(&mut self, slice: &[Level<'arena>]) -> &'arena [Level<'arena>] {
        self.alloc.alloc_slice_copy(slice)
    }

    /// Stores a string in the arena.
    ///
    /// This is typically done to ensure strings live long enough when manipulating them.
    pub(crate) fn store_name(&mut self, name: &str) -> &'arena str {
        self.alloc.alloc_str(name)
    }

    /// Binds a term to a given name.
    pub fn bind(&mut self, name: &str, t: Term<'arena>) {
        let name = self.store_name(name);
        self.named_terms.insert(name, t);
    }

    /// Binds a declaration to a given name.
    pub fn bind_decl(&mut self, name: &str, t: Declaration<'arena>) {
        let name = self.store_name(name);
        self.named_decls.insert(name, t);
    }

    /// Retrieves the binding of a given name, if one exists.
    pub fn get_binding(&self, name: &str) -> Option<Term<'arena>> {
        self.named_terms.get(name).copied()
    }

    /// Retrieves the declaration binding of a given name, if one exists.
    pub fn get_binding_decl(&self, name: &str) -> Option<Declaration<'arena>> {
        self.named_decls.get(name).copied()
    }
}

/// This macro generates two types, $dweller and Node, parametrised by a lifetime. These types are
/// associated to a set of traits that they are expected to have by living in an arena.
macro_rules! new_dweller {
    ($dweller: ident, $header: ident, $payload: ident) => {
        /// A term of the calculus of constructions.
        ///
        /// This type is associated, through its lifetime argument, to an [`Arena`], where it lives. There,
        /// it is guaranteed to be unique, which accelerates many algorithms. It is fundamentally a pointer
        /// to an internal term structure, called a Node, which itself contains the core term, [`Payload`],
        /// which is what can be expected of a term.
        ///
        /// Additionally, the Node contains lazy structures which indicate the result of certain
        /// transformation on the term, namely type checking and term reduction. Storing it directly here
        /// is both faster and takes overall less space than storing the result in a separate hash table.
        #[derive(Clone, Copy)]
        pub struct $dweller<'arena>(&'arena Node<'arena>, std::marker::PhantomData<*mut &'arena ()>);

        pub(super) struct Node<'arena> {
            header: $header<'arena>,
            payload: $payload<'arena>,
        }

        impl<'arena> $dweller<'arena> {
            fn new(node: &'arena Node<'arena>) -> Self {
                $dweller(node, std::marker::PhantomData)
            }
        }

        /// ${dweller}s are smart pointers, and as such, can be directly dereferenced to their associated
        /// payload.
        ///
        /// This is done for convenience, as it allows to manipulate the terms relatively seamlessly.
        ///
        /// # Example
        ///
        /// For a [`Term`](super::term::Term), it is possible to write:
        /// ```
        /// # use kernel::memory::arena::use_arena;
        /// # use kernel::memory::term::Payload::*;
        /// # use kernel::memory::term::builder::prop;
        /// # use_arena(|arena| {
        /// # let t = arena.build(prop()).unwrap();
        /// match *t {
        ///     Abs(_, t2) => t2.beta_reduction(arena),
        ///     App(t1, _) => t1,
        ///     _ => t,
        /// }
        /// # ;})
        /// ```
        /// Please note that this trait has some limits. For instance, the notations used to match against
        /// a *pair* of terms still requires some convolution.
        impl<'arena> std::ops::Deref for $dweller<'arena> {
            type Target = $payload<'arena>;

            fn deref(&self) -> &Self::Target {
                &self.0.payload
            }
        }

        /// Debug mode only prints the payload of a ${dweller}
        ///
        /// Apart from enhancing the debug readability, this reimplementation is surprisingly necessary: in
        /// the case of terms for instance, and because they may refer to themselves in the payload, the
        /// default debug implementation recursively calls itself until the stack overflows.
        impl<'arena> std::fmt::Debug for $dweller<'arena> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                self.0.payload.fmt(f)
            }
        }

        /// Because ${dweller}s are unique in the arena, it is sufficient to compare their locations in memory to
        /// test equality.
        impl<'arena> PartialEq<Self> for $dweller<'arena> {
            fn eq(&self, rhs: &Self) -> bool {
                std::ptr::eq(self.0, rhs.0)
            }
        }

        impl<'arena> Eq for $dweller<'arena> {}

        /// Because ${dweller}s are unique in the arena, it is sufficient to compare their locations in memory to
        /// test equality. In particular, hash can also be computed from the location.
        impl<'arena> std::hash::Hash for $dweller<'arena> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                std::ptr::hash(self.0, state)
            }
        }

        impl<'arena> PartialEq<Self> for Node<'arena> {
            fn eq(&self, x: &Self) -> bool {
                self.payload == x.payload
            }
        }

        impl<'arena> Eq for Node<'arena> {}

        /// Nodes are not guaranteed to be unique. Nonetheless, only the payload matters and characterises
        /// the value. Which means computing the hash for nodes can be restricted to hashing their
        /// payloads.
        impl<'arena> std::hash::Hash for Node<'arena> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.payload.hash(state);
            }
        }
    };
}

pub(super) use new_dweller;
