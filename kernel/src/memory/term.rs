//! A comprehensive memory management unit for terms.
//!
//! This module defines the core functions used to manipulate an arena and its terms.

use core::fmt;
use std::cell::OnceCell;
use std::fmt::Debug;

use derive_more::{Add, Display, From, Into, Sub};

use super::declaration::InstantiatedDeclaration;
use super::level::Level;
use crate::error::ResultTerm;

/// An index used to designate bound variables.
#[derive(Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash)]
pub struct DeBruijnIndex(usize);

super::arena::new_dweller!(Term, Header, Payload);

struct Header<'arena> {
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
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Payload<'arena> {
    /// A variable, with its de Bruijn index and its type
    //#[display(fmt = "{_0}")]
    Var(DeBruijnIndex, Term<'arena>),

    /// Sort i, the encoding of Prop and Type i type
    Sort(Level<'arena>),

    /// The application of two terms
    //#[display(fmt = "{_0} {_1}")]
    App(Term<'arena>, Term<'arena>),

    /// The lambda-abstraction of a term: the argument type is on the left, the body on the right.
    Abs(Term<'arena>, Term<'arena>),

    /// The dependant product of the term on the right over all elements of the type on the left.
    //#[display(fmt = "\u{03A0} {_0} \u{02192} {_1}")]
    Prod(Term<'arena>, Term<'arena>),

    /// An instantiated universe-polymorphic declaration
    Decl(InstantiatedDeclaration<'arena>),
}

impl<'arena> fmt::Display for Payload<'arena> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Var(index, _) => write!(f, "{index}"),
            Sort(level) => match level.to_numeral() {
                Some(n) => match n {
                    0 => write!(f, "Prop"),
                    1 => write!(f, "Type"),
                    _ => write!(f, "Type {n}"),
                },
                None => write!(f, "Type {{{level}}}"),
            },
            App(fun, arg) => write!(f, "{fun} {arg}"),
            Abs(argtype, body) => write!(f, "\u{003BB} {argtype} \u{02192} {body}"),
            Prod(argtype, body) => write!(f, "\u{003A0} {argtype} \u{02192} {body}"),
            Decl(decl) => write!(f, "{decl}"),
        }
    }
}

impl<'arena> fmt::Display for Term<'arena> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.payload)
    }
}

use Payload::*;

// TODO make this a Term-impl. This will impact most files from the project, but should not be so
// hard.
impl<'arena> super::arena::Arena<'arena> {
    /// This function is the base low-level function for creating terms.
    ///
    /// It enforces the uniqueness property of terms in the arena.
    fn hashcons_term(&mut self, n: Payload<'arena>) -> Term<'arena> {
        // There are concurrent designs here. hashcons could also take a node, which gives
        // subsequent function some liberty in providing the other objects of the header: WHNF,
        // type_ (unlikely, because not always desirable), is_certainly_closed.
        let new_node = Node {
            payload: n,
            header: Header {
                head_normal_form: OnceCell::new(),
                type_: OnceCell::new(),
            },
        };

        match self.hashcons_terms.get(&new_node) {
            Some(addr) => Term::new(addr),
            None => {
                let addr = self.alloc.alloc(new_node);
                self.hashcons_terms.insert(addr);
                Term::new(addr)
            },
        }
    }

    /// Returns a variable term with the given index and type
    pub(crate) fn var(&mut self, index: DeBruijnIndex, type_: Term<'arena>) -> Term<'arena> {
        self.hashcons_term(Var(index, type_))
    }

    /// Returns the term corresponding to a proposition
    pub(crate) fn prop(&mut self) -> Term<'arena> {
        let zero = Level::zero(self);
        self.hashcons_term(Sort(zero))
    }

    pub(crate) fn sort(&mut self, level: Level<'arena>) -> Term<'arena> {
        self.hashcons_term(Sort(level))
    }

    /// Returns the term corresponding to Type(level), casting level appropriately first
    pub(crate) fn type_usize(&mut self, level: usize) -> Term<'arena> {
        let level = Level::from(level + 1, self);
        self.hashcons_term(Sort(level))
    }

    /// Returns the application of one term to the other
    pub(crate) fn app(&mut self, func: Term<'arena>, arg: Term<'arena>) -> Term<'arena> {
        self.hashcons_term(App(func, arg))
    }

    /// Returns the lambda-abstraction of the term `body`, with an argument of type `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn abs(&mut self, arg_type: Term<'arena>, body: Term<'arena>) -> Term<'arena> {
        self.hashcons_term(Abs(arg_type, body))
    }

    /// Returns the dependant product of the term `body`, over elements of `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn prod(&mut self, arg_type: Term<'arena>, body: Term<'arena>) -> Term<'arena> {
        self.hashcons_term(Prod(arg_type, body))
    }

    /// TODO(doc)
    pub(crate) fn decl(&mut self, decl: InstantiatedDeclaration<'arena>) -> Term<'arena> {
        self.hashcons_term(Decl(decl))
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
        *self.0.header.head_normal_form.get_or_init(f)
    }

    /// Returns the type of the term, lazily computing the closure `f`.
    pub(crate) fn get_type_or_try_init<F>(self, f: F) -> ResultTerm<'arena>
    where
        F: FnOnce() -> ResultTerm<'arena>,
    {
        self.0.header.type_.get_or_try_init(f).copied()
    }
}
