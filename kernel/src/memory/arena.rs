//! A comprehensive memory management unit for terms.
//!
//! This module defines the core functions used to manipulate an arena and its terms.

use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Deref;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use bumpalo::Bump;

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

    named_terms: HashMap<&'arena str, Term<'arena>>,

    // Hash maps used to speed up certain algorithms. See also `OnceCell`s in [`Term`]
    mem_subst: HashMap<(Term<'arena>, Term<'arena>, usize), Term<'arena>>,
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
            hashcons_levels: HashSet::new(),
            hashcons_decls: HashSet::new(),

            named_terms: HashMap::new(),

            mem_subst: HashMap::new(),
        }
    }

    /// Stores a string in the arena.
    ///
    /// This is typically done to ensure strings live long enough when manipulating them.
    pub(crate) fn store_name(&mut self, name: &str) -> &'arena str {
        self.alloc.alloc_str(name)
    }

    /// Binds a term to a certain name.
    pub fn bind(&mut self, name: &str, t: Term<'arena>) {
        let name = self.store_name(name);
        self.named_terms.insert(name, t);
    }

    /// Retrieves the binding of a certain name, if one exists.
    pub fn get_binding(&self, name: &str) -> Option<Term<'arena>> {
        self.named_terms.get(name).copied()
    }
}

/// Trait of objects living in the arena.
/// These objects are pointers to data, which have uniqueness properties.
pub(super) trait ArenaDweller<'arena, Node, Payload>
    where Self: Copy
{
    fn to_payload(&self) -> &Payload;
    fn to_addr(self) -> &'arena Node;

    fn node_to_payload(n: &Node) -> &Payload;
}

/// Arena dwellers are smart pointers, and as such, can be directly dereferenced to its associated
/// payload.
///
/// This is done for convenience, as it allows to manipulate the terms relatively seamlessly.
///
/// # Example
///
/// A [`Term`](super::term::Term) is an arena dweller, and it is possible to write:
/// ```
/// # use kernel::term::arena::{use_arena, Payload::*};
/// # use kernel::term::builders::prop;
/// # use_arena(|arena| {
/// # let t = arena.build(prop()).unwrap();
/// match *t {
///     Abs(_, t2) => arena.beta_reduction(t2),
///     App(t1, _) => t1,
///     _ => t,
/// }
/// # ;})
/// ```
/// Please note that this trait has some limits. For instance, the notations used to match against
/// a *pair* of terms still requires some convolution.
impl<'arena, T, U, Payload> Deref for T
    where T: ArenaDweller<'arena, U, Payload> {
    type Target = Payload;

    fn deref(&self) -> &Self::Target {
        self.to_payload()
    }
}

/// Debug mode only prints the payload of a dweller
///
/// Apart from enhancing the debug readability, this reimplementation is surprisingly necessary: in
/// the case of terms for instance, and because they may refer to themselves in the payload, the
/// default debug implementation recursively calls itself until the stack overflows.
impl<'arena, T, U, Payload> Debug for T
    where T: ArenaDweller<'arena, U, Payload>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.to_payload().fmt(f)
    }
}
///
/// Because dwellers are unique in the arena, it is sufficient to compare their locations in memory to
/// test equality.
impl<'arena, T, U, V> PartialEq<T> for T
where T: ArenaDweller<'arena, U, V> {
    fn eq(&self, rhs: &T) -> bool {
        std::ptr::eq(self.to_node(), rhs.to_node())
    }
}

/// Because dwellers are unique in the arena, it is sufficient to compare their locations in memory to
/// test equality. In particular, hash can also be computed from the location.
impl<'arena, T, U, Payload> Hash for T
    where T: ArenaDweller<'arena, U, Payload>
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.to_addr(), state)
    }
}

impl<'arena, T, Node, Payload> PartialEq<Node> for Node
    where T: ArenaDweller<'arena, Node, Payload>
{
    fn eq(&self, x: &Node<'arena>) -> bool {
        self.payload == x.payload
    }
}

/// Nodes are not guaranteed to be unique. Nonetheless, only the payload matters and characterises
/// the value. Which means computing the hash for nodes can be restricted to hashing their
/// payloads.
impl<'arena> Hash for Node<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.payload.hash(state);
    }
}
