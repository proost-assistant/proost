//! A comprehensive memory management unit for terms.
//!
//! This module defines the core functions used to manipulate an arena and its terms.

use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

use bumpalo::Bump;
use derive_more::{Add, Display, From, Into, Sub};
use num_bigint::BigUint;

use crate::error::ResultTerm;

/// An index used to designate bound variables.
#[derive(Add, Copy, Clone, Debug, Default, Display, PartialEq, Eq, Hash, From, Into, PartialOrd, Ord, Sub)]
pub struct DeBruijnIndex(usize);

/// A level of universe, used to build terms of the form `Type i`.
///
/// In type theory, this corresponds to the construction of universes "à la Russell", the purpose
/// of which is to give a hierarchy to these types, so as to preserve soundness against paradoxes
/// akin to Russell's. Universe levels can be arbitrarily large, and, with good faith, they are
/// represented with *big unsigned integers*, limited only by the memory of the operating computer.
#[derive(Add, Clone, Debug, Default, Display, PartialEq, Eq, From, Hash, PartialOrd, Ord, Sub)]
pub struct UniverseLevel(BigUint);

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Namespace<'arena>(&'arena [&'arena str]);

#[derive(Clone, PartialEq, Eq, Debug, From)]
pub struct NamespaceSet<'arena>(Vec<Namespace<'arena>>);

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
    alloc: &'arena Bump,

    // enforces invariances over lifetime parameter
    _phantom: PhantomData<*mut &'arena ()>,

    // Hashconsing of terms, at the heart of the uniqueness property
    hashcons: HashSet<&'arena Node<'arena>>,

    // Hashconsing strings to prevent having duplicates in the arena. Unlike terms, this is only
    // done for space-efficiency purposes. Further improvements might include an appropriate,
    // arena-friendly, structure for namespaces (e.g. a list of strings living in the arena)
    hashcons_strings: HashSet<&'arena str>,
    // the values store the keys. This is due to a rather subtle lifetime issue that appears when
    // functions like HashMap::get_key_value are needed.
    named_terms: HashMap<&'arena [&'arena str], (Namespace<'arena>, Term<'arena>)>,

    // Hash maps used to speed up certain algorithms. See also `OnceCell`s in [`Term`]
    mem_subst: HashMap<(Term<'arena>, Term<'arena>, usize), Term<'arena>>,
    // TODO shift hashmap (see #45)
    // requires the design of an additional is_certainly_closed predicate in terms.
}

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
#[derive(Clone, Copy, Display, Eq)]
#[display(fmt = "{}", "_0.payload")]
pub struct Term<'arena>(
    &'arena Node<'arena>,
    // This marker ensures invariance over the 'arena lifetime.
    PhantomData<*mut &'arena ()>,
);

#[derive(Debug, Eq)]
struct Node<'arena> {
    payload: Payload<'arena>,

    // Lazy and aliasing-compatible structures for memoizing
    head_normal_form: OnceCell<Term<'arena>>,
    type_: OnceCell<Term<'arena>>,
    // TODO is_certainly_closed: boolean underapproximation of whether a term is closed.
    // This may greatly improve performance in shifting, along with a mem_shift hash map.
}

/// The essence of a term.
///
/// This enumeration has the same shape as the algebraic type of terms in the calculus of
/// constructions.
///
/// There is one true exception, which is the variable (Var)[`Payload::Var`]. Along with its de
/// Bruijn index, the variable also stores its type, which is unique, and also ensures two
/// variables with a different type do not share the same term in memory.
#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
pub enum Payload<'arena> {
    /// A variable, with its de Bruijn index and its type
    #[display(fmt = "{}", _0)]
    Var(DeBruijnIndex, Term<'arena>),

    /// The type of propositions
    #[display(fmt = "Prop")]
    Prop,

    /// Type i, as described in [`UniverseLevel`]
    #[display(fmt = "Type {}", _0)]
    Type(UniverseLevel),

    /// The application of two terms
    #[display(fmt = "{} {}", _0, _1)]
    App(Term<'arena>, Term<'arena>),

    /// The lambda-abstraction of a term: the argument type is on the left, the body on the right.
    #[display(fmt = "\u{003BB} {} \u{02192} {}", _0, _1)]
    Abs(Term<'arena>, Term<'arena>),

    /// The dependant product of the term on the right over all elements of the type on the left.
    #[display(fmt = "\u{03A0} {} \u{02192} {}", _0, _1)]
    Prod(Term<'arena>, Term<'arena>),
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

use Payload::*;

impl<'arena> Arena<'arena> {
    /// Allocates a new memory arena. As detailed in the [`use_arena`] function, it is necessary to
    /// externalise the generation of the [`bumpalo::Bump`] object.
    fn new(alloc: &'arena Bump) -> Self {
        Arena {
            alloc,
            _phantom: PhantomData,

            hashcons: HashSet::new(),

            hashcons_strings: HashSet::new(),
            named_terms: HashMap::new(),

            mem_subst: HashMap::new(),
        }
    }

    /// Stores a string in the arena.
    ///
    /// This is typically done to ensure strings live long enough when manipulating them.
    /// It also ensures strings are unique in the arena.
    pub(crate) fn store_string(&mut self, s: &str) -> &'arena str {
        match self.hashcons_strings.get(s) {
            Some(s) => s,
            None => {
                let s = self.alloc.alloc_str(s);
                self.hashcons_strings.insert(s);
                s
            },
        }
    }

    pub(crate) fn store_name<'a, T: IntoIterator<Item = &'a &'a str>, U: IntoIterator<Item = &'a &'a str>>(
        &mut self,
        prefix: T,
        suffix: U,
    ) -> Namespace<'arena> {
        let name = prefix.into_iter().chain(suffix).map(|s| self.store_string(s)).collect::<Vec<&'arena str>>();
        Namespace(self.alloc.alloc_slice_copy(&name))
    }

    pub(crate) fn store_name_1<'a, T: IntoIterator<Item = &'a &'a str>>(&mut self, name: T) -> Namespace<'arena> {
        self.store_name([], name)
    }

    /// Binds a term to a certain name.
    pub fn bind(&mut self, prefix: &[&str], suffix: &str, t: Term<'arena>) {
        let name = self.store_name(prefix, &[suffix]);
        self.named_terms.insert(name.0, (name, t));
    }

    pub fn bind_root(&mut self, name: &str, t: Term<'arena>) {
        self.bind(&[], name, t)
    }

    /// Retrieves the binding of a certain name, if one exists.
    pub fn get_binding(&self, name: &[&str]) -> Option<Term<'arena>> {
        self.named_terms.get(name).map(|(_, t)| t).copied()
    }

    /// Retrieves the binding of a certain name, if one exists.
    pub fn get_binding_with_name(&self, name: &[&str]) -> Option<(Namespace<'arena>, Term<'arena>)> {
        self.named_terms.get(name).map(|(key, value)| (*key, *value))
    }

    /// This function is the base low-level function for creating terms.
    ///
    /// It enforces the uniqueness property of terms in the arena.
    fn hashcons(&mut self, n: Payload<'arena>) -> Term<'arena> {
        // There are concurrent designs here. hashcons could also take a node, which gives
        // subsequent function some liberty in providing the other objects of the header: WHNF,
        // type_ (unlikely, because not always desirable), is_certainly_closed.
        let new_node = Node {
            payload: n,
            head_normal_form: OnceCell::new(),
            type_: OnceCell::new(),
        };

        match self.hashcons.get(&new_node) {
            Some(addr) => Term(addr, PhantomData),
            None => {
                let addr = self.alloc.alloc(new_node);
                self.hashcons.insert(addr);
                Term(addr, PhantomData)
            },
        }
    }

    /// Returns a variable term with the given index and type
    pub(crate) fn var(&mut self, index: DeBruijnIndex, type_: Term<'arena>) -> Term<'arena> {
        self.hashcons(Var(index, type_))
    }

    /// Returns the term corresponding to a proposition
    pub(crate) fn prop(&mut self) -> Term<'arena> {
        self.hashcons(Prop)
    }

    /// Returns the term corresponding to Type(level)
    pub(crate) fn type_(&mut self, level: UniverseLevel) -> Term<'arena> {
        self.hashcons(Type(level))
    }

    /// Returns the term corresponding to Type(level), casting level appropriately first
    pub(crate) fn type_usize(&mut self, level: usize) -> Term<'arena> {
        self.hashcons(Type(BigUint::from(level).into()))
    }

    /// Returns the application of one term to the other
    pub(crate) fn app(&mut self, func: Term<'arena>, arg: Term<'arena>) -> Term<'arena> {
        self.hashcons(App(func, arg))
    }

    /// Returns the lambda-abstraction of the term `body`, with an argument of type `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn abs(&mut self, arg_type: Term<'arena>, body: Term<'arena>) -> Term<'arena> {
        self.hashcons(Abs(arg_type, body))
    }

    /// Returns the dependant product of the term `body`, over elements of `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn prod(&mut self, arg_type: Term<'arena>, body: Term<'arena>) -> Term<'arena> {
        self.hashcons(Prod(arg_type, body))
    }

    /// Returns the result of the substitution described by the key, lazily computing the closure `f`.
    pub(crate) fn get_subst_or_init<F>(&mut self, key: &(Term<'arena>, Term<'arena>, usize), f: F) -> Term<'arena>
    where
        F: FnOnce(&mut Self) -> Term<'arena>,
    {
        match self.mem_subst.get(key) {
            Some(res) => *res,
            None => {
                let res = f(self);
                self.mem_subst.insert(*key, res);
                res
            },
        }
    }
}

impl<'arena> Term<'arena> {
    /// Returns the weak head normal form of the term, lazily computing the closure `f`.
    pub(crate) fn get_whnf_or_init<F>(self, f: F) -> Term<'arena>
    where
        F: FnOnce() -> Term<'arena>,
    {
        *self.0.head_normal_form.get_or_init(f)
    }

    /// Returns the type of the term, lazily computing the closure `f`.
    pub(crate) fn get_type_or_try_init<F>(self, f: F) -> ResultTerm<'arena>
    where
        F: FnOnce() -> ResultTerm<'arena>,
    {
        self.0.type_.get_or_try_init(f).copied()
    }
}

/// A Term is arguably a smart pointer, and as such, can be directly dereferenced to its associated
/// payload.
///
/// This is done for convenience, as it allows to manipulate the terms relatively seamlessly.
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
impl<'arena> Deref for Term<'arena> {
    type Target = Payload<'arena>;

    fn deref(&self) -> &Self::Target {
        &self.0.payload
    }
}

/// Debug mode only prints the payload of a term.
///
/// Apart from enhancing the debug readability, this reimplementation is surprisingly necessary:
/// because terms may refer to themselves in the payload, the default debug implementation
/// recursively calls itself and provokes a stack overflow.
impl<'arena> Debug for Term<'arena> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.payload.fmt(f)
    }
}

/// Because terms are unique in the arena, it is sufficient to compare their locations in memory to
/// test equality.
impl<'arena> PartialEq<Term<'arena>> for Term<'arena> {
    fn eq(&self, x: &Term<'arena>) -> bool {
        std::ptr::eq(self.0, x.0)
    }
}

/// Because terms are unique in the arena, it is sufficient to compare their locations in memory to
/// test equality. In particular, hash can also be computed from the location.
impl<'arena> Hash for Term<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

impl<'arena> PartialEq<Node<'arena>> for Node<'arena> {
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

impl std::fmt::Display for Namespace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut namespace = self.0.into_iter();
        write!(f, "{}", namespace.next().unwrap())?;
        namespace.try_for_each(|s| write!(f, "::{s}"))
    }
}

impl std::fmt::Display for NamespaceSet<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut set = self.0.iter();
        write!(f, "\n\t{}", set.next().unwrap())?;
        set.try_for_each(|namespace| write!(f, ",\n\t{namespace}"))
    }
}
