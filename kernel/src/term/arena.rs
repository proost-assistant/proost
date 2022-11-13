use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

use bumpalo::Bump;
use derive_more::{Add, Display, From, Into, Sub};
use im_rc::hashmap::HashMap as ImHashMap;
use num_bigint::BigUint;

use crate::error::ResultTerm;

#[derive(
    Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash,
)]
pub struct DeBruijnIndex(usize);

#[derive(Add, Clone, Debug, Default, Display, Eq, From, Sub, PartialEq, PartialOrd, Ord, Hash)]
pub struct UniverseLevel(BigUint);

pub struct Arena<'arena> {
    alloc: &'arena Bump,
    _phantom: PhantomData<*mut &'arena ()>,

    hashcons: HashSet<&'arena Node<'arena>>,
    named_terms: HashMap<&'arena str, Term<'arena>>,

    mem_subst: HashMap<(Term<'arena>, Term<'arena>, usize), Term<'arena>>,
    // a shift hashmap may also be added when the is_certainly_closed also is
}

#[derive(Clone, Copy, Display, Eq)]
#[display(fmt = "{}", "_0.payload")]
// PhantomData is a marker to ensure invariance over the 'arena lifetime.
pub struct Term<'arena>(&'arena Node<'arena>, PhantomData<*mut &'arena ()>);

// the rest of the struct is very verbose and useless for debugging
impl<'arena> Debug for Term<'arena> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.payload.fmt(f)
    }
}

// no name storage here: meaning consts are known and can be found, but no pretty printing is
// possible so far.
#[derive(Debug)]
struct Node<'arena> {
    payload: Payload<'arena>,

    head_normal_form: OnceCell<Term<'arena>>,
    type_: OnceCell<Term<'arena>>,
    //
    // is_certainly_closed: boolean underapproximation of whether a term is closed, which can
    // greatly improve performance in shifting
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
pub enum Payload<'arena> {
    #[display(fmt = "{}", _0)]
    Var(DeBruijnIndex, Term<'arena>),

    #[display(fmt = "Prop")]
    Prop,

    #[display(fmt = "Type {}", _0)]
    Type(UniverseLevel),

    #[display(fmt = "{} {}", _0, _1)]
    App(Term<'arena>, Term<'arena>),

    #[display(fmt = "\u{003BB} {} \u{02192} {}", _0, _1)]
    Abs(Term<'arena>, Term<'arena>),

    #[display(fmt = "\u{03A0} {} \u{02192} {}", _0, _1)]
    Prod(Term<'arena>, Term<'arena>),
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

impl<'arena> Eq for Node<'arena> {}

/// (TODO PRECISE DOCUMENTATION) Only the payload matters and caracterises the value. Changing
/// OnceCells is *guaranteed* to have no impact on that.
impl<'arena> Hash for Node<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.payload.hash(state);
    }
}

pub fn use_arena<F, T>(f: F) -> T
where
    F: for<'arena> FnOnce(&mut Arena<'arena>) -> T,
{
    let alloc = Bump::new();
    let mut arena = Arena::new(&alloc);
    f(&mut arena)
}

use super::builders::extern_::Builder;
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

    pub fn bind(&mut self, name: &'arena str, t: Term<'arena>) {
        self.named_terms.insert(name, t);
    }

    pub fn get_binding(&self, name: &str) -> Option<Term<'arena>> {
        self.named_terms.get(name).copied()
    }

    /// (TODO DOC.) there are concurrent designs here: either hashcons take a Node and functions
    /// which use it immediatly intend to compute the type and/or WHNF of the term, or this is
    /// postponed and computed lazily. This iteration chooses the second approach.
    fn hashcons(&mut self, n: Payload<'arena>) -> Term<'arena> {
        let nn = Node {
            payload: n,
            head_normal_form: OnceCell::new(),
            type_: OnceCell::new(),
        };
        match self.hashcons.get(&nn) {
            Some(addr) => Term(addr, PhantomData),
            None => {
                let addr = self.alloc.alloc(nn);
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

    #[inline]
    pub fn build_from_extern<F: Builder<'arena>>(&mut self, f: F) -> ResultTerm<'arena> {
        f(self, &ImHashMap::new(), 0.into())
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
