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
#[derive(
    Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash,
)]
pub struct DeBruijnIndex(usize);

/// A level of universe, used to build termes of the form `Type i`.
///
/// In type theory, this corresponds to the construction of universes ``à la Russell'', the purpose
/// of which is to give a hierarchy to these types, so as to preserve soundness against paradoxes
/// akin to Russell's. Universe levels can be arbitrarily large, and, with good faith, they are
/// represented with *big unsigned integers*, limited only to the memory of the operating computer.
#[derive(Add, Clone, Debug, Default, Display, Eq, From, Sub, PartialEq, PartialOrd, Ord, Hash)]
pub struct UniverseLevel(BigUint);

/// A comprehensive memory management unit for terms.
///
/// An arena is a location in memory where a group of terms with several properties is stored. Most
/// importantly, it ensures that all terms living in the arena are syntaxically unique, which
/// accelerates many algorithms. In particular, this property allows for *memoizing* easily
/// operations on terms like substitution, shifting, type checking, etc. It also facilitates the
/// (building of terms)[./builder.rs] which are named or use named terms.
///
/// This paradigm of memory management is akin to what is usually lectured for Binary Decision
/// Diagrams (BDD) management. Additionally, it makes use of Rust features to provide a clean
/// interface: the arena type is invariant over its lifetime argument (usually called `'arena`),
/// which together with the [`use_arena`] function, enforces strong guarantees on how the arena can
/// be used, particularily if several of them are used simultaneously.
///
/// Early versions of this system are freely inspired by an assignment designed by
/// (Jacques-Henri Jourdan)[https://jhjourdan.mketjh.fr].
pub struct Arena<'arena> {
    alloc: &'arena Bump,

    // enforces invariances over lifetime parameter
    _phantom: PhantomData<*mut &'arena ()>,

    // Hashconsing of terms, at the heart of the uniqueness property
    hashcons: HashSet<&'arena Node<'arena>>,
    named_terms: HashMap<&'arena str, Term<'arena>>,

    // Hash maps used to speed up certain algorithms. See also `OnceCell`s in [`Term`]
    mem_subst: HashMap<(Term<'arena>, Term<'arena>, usize), Term<'arena>>,
    // TODO shift hashmap (see #45)
    // requires the design of an additional is_certainly_closed predicate in terms.
>>>>>>> 994770f (doc(kernel/arena): WIP)
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
    PhantomData<*mut &'arena ()>
);

// no name storage here: meaning consts are known and can be found, but no pretty printing is
// possible so far.
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
    /// (TODO DOC.) Allocate a new memory arena.
    fn new(alloc: &'arena Bump) -> Self {
        Arena {
            alloc,
            _phantom: PhantomData,

            hashcons: HashSet::new(),
            named_terms: HashMap::new(),

            mem_subst: HashMap::new(),
        }
    }

    pub(crate) fn store_name(&mut self, name: &str) -> &'arena str {
        self.alloc.alloc_str(name)
    }

    pub fn bind(&mut self, name: &str, t: Term<'arena>) {
        let name = self.store_name(name);
        self.named_terms.insert(name, t);
    }

    pub fn get_binding(&self, name: &str) -> Option<Term<'arena>> {
        self.named_terms.get(name).copied()
    }

    /// (TODO DOC.) there are concurrent designs here: either hashcons take a Node and functions
    /// which use it immediatly intend to compute the type and/or WHNF of the term, or this is
    /// postponed and computed lazily. This iteration chooses the second approach.
    fn hashcons(&mut self, n: Payload<'arena>) -> Term<'arena> {
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
            }
        }
    }

    pub(crate) fn var(&mut self, index: DeBruijnIndex, type_: Term<'arena>) -> Term<'arena> {
        self.hashcons(Var(index, type_))
    }

    pub(crate) fn prop(&mut self) -> Term<'arena> {
        self.hashcons(Prop)
    }

    pub(crate) fn type_(&mut self, level: UniverseLevel) -> Term<'arena> {
        self.hashcons(Type(level))
    }

    pub(crate) fn type_usize(&mut self, level: usize) -> Term<'arena> {
        self.hashcons(Type(BigUint::from(level).into()))
    }

    pub(crate) fn app(&mut self, u1: Term<'arena>, u2: Term<'arena>) -> Term<'arena> {
        self.hashcons(App(u1, u2))
    }

    pub(crate) fn abs(&mut self, arg_type: Term<'arena>, u: Term<'arena>) -> Term<'arena> {
        self.hashcons(Abs(arg_type, u))
    }

    pub(crate) fn prod(&mut self, arg_type: Term<'arena>, u: Term<'arena>) -> Term<'arena> {
        self.hashcons(Prod(arg_type, u))
    }

    pub(crate) fn get_subst_or_init<F>(
        &mut self,
        key: &(Term<'arena>, Term<'arena>, usize),
        f: F,
    ) -> Term<'arena>
    where
        F: FnOnce(&mut Self) -> Term<'arena>,
    {
        match self.mem_subst.get(key) {
            Some(res) => *res,
            None => {
                let res = f(self);
                self.mem_subst.insert(*key, res);
                res
            }
        }
    }
}

impl<'arena> Term<'arena> {
    pub(crate) fn get_whnf_or_init<F>(self, f: F) -> Term<'arena>
    where
        F: FnOnce() -> Term<'arena>,
    {
        *self.0.head_normal_form.get_or_init(f)
    }

    pub(crate) fn get_type_or_try_init<F>(self, f: F) -> ResultTerm<'arena>
    where
        F: FnOnce() -> ResultTerm<'arena>,
    {
        self.0.type_.get_or_try_init(f).copied()
    }
}

impl<'arena> Deref for Term<'arena> {
    type Target = Payload<'arena>;

    fn deref(&self) -> &Self::Target {
        &self.0.payload
    }
}

// the rest of the struct is very verbose and useless for debugging
impl<'arena> Debug for Term<'arena> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.payload.fmt(f)
    }
}

/// (TODO PRECISE DOCUMENTATION) make use of unicity invariant to speed up equality test
impl<'arena> PartialEq<Term<'arena>> for Term<'arena> {
    fn eq(&self, x: &Term<'arena>) -> bool {
        std::ptr::eq(self.0, x.0)
    }
}

/// (TODO PRECISE DOCUMENTATION) make use of unicity invariant to speed up equality test
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

/// (TODO PRECISE DOCUMENTATION) Only the payload matters and caracterises the value. Changing
/// OnceCells is *guaranteed* to have no impact on that.
impl<'arena> Hash for Node<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.payload.hash(state);
    }
}
