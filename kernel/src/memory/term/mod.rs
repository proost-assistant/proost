//! Terms in the calculus of construction.
//!
//! This module defines the core functions used to create and manipulate terms.

use core::fmt;
use core::fmt::Debug;
use std::cell::OnceCell;

use derive_more::{Add, Display, From, Into, Sub};

use super::declaration::InstantiatedDeclaration;
use super::level::Level;
use crate::axiom;
use crate::error::ResultTerm;
use crate::memory::arena::Arena;

pub mod builder;

/// An index used to designate bound variables.
#[derive(Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash)]
pub struct DeBruijnIndex(usize);

super::arena::new_dweller!(Term, Header, Payload);

/// The header of a term.
struct Header<'arena> {
    /// Lazy structure to store the weak-head normal form of a term.
    head_normal_form: OnceCell<Term<'arena>>,

    /// Lazy structure to store the type of a term.
    type_: OnceCell<Term<'arena>>,

    /// The relevance of a given term.
    is_relevant: OnceCell<bool>,

    /// Whether the term is *known* to be closed.
    is_certainly_closed: OnceCell<()>,
}

impl<'arena> Header<'arena> {
    /// Creates a new default header, indicating whether it is *sure* that the term is closed, by
    /// `is_certainly_closed`. Should it be understood later that the term is closed, it will be
    /// modified accordingly.
    fn new(is_certainly_closed: bool) -> Self {
        Header {
            head_normal_form: OnceCell::new(),
            type_: OnceCell::new(),
            is_relevant: OnceCell::new(),
            is_certainly_closed: if is_certainly_closed { OnceCell::from(()) } else { OnceCell::new() },
        }
    }
}

/// A term.
///
/// This enumeration has almost the same shape as the algebraic type of terms in the calculus of
/// constructions.
///
/// One exception is the [variable](`Payload::Var`). Along with its de Bruijn index, the
/// variable also stores its type, which is unique, and also ensures two variables with a different
/// type do not share the same term in memory.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Payload<'arena> {
    /// A variable, with its de Bruijn index and its type.
    Var(DeBruijnIndex, Term<'arena>),

    /// Sort i, the encoding of Prop and Type i type.
    Sort(Level<'arena>),

    /// The application of two terms.
    App(Term<'arena>, Term<'arena>),

    /// The lambda-abstraction of a term: the argument type is on the left, the body on the right.
    Abs(Term<'arena>, Term<'arena>),

    /// The dependant product of the term on the right over all elements of the type on the left.
    Prod(Term<'arena>, Term<'arena>),

    /// An instance of a universe-polymorphic declaration.
    Decl(InstantiatedDeclaration<'arena>),

    /// An axiom.
    Axiom(axiom::Axiom, &'arena [Level<'arena>]),
}

impl<'arena> fmt::Display for Term<'arena> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_print(f, 0, 0, false)
    }
}

use Payload::{Abs, App, Axiom, Decl, Prod, Sort, Var};

/// A term that is ready to be pretty printed with named variables. This term must be closed.
#[allow(clippy::module_name_repetitions)]
pub struct PrettyTerm<'arena>(pub Term<'arena>);

impl<'arena> fmt::Display for PrettyTerm<'arena> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.pretty_print(f, 0, 0, true)
    }
}

impl<'arena> Term<'arena> {
    /// This function generates the pretty print of a term.
    ///
    /// `depth` parameter indicates the number of existing variables (number of Abs/Prop from the root of the term to the
    /// considered subterm).
    /// `distance` parameter corresponds to the number of Abs/Prop between the environement where the variables live and these
    /// variables.
    /// `is_root_closed` indicates if the root is certain to be closed. If true, the De Bruijn indexes will not appear but
    /// transformed in named variables instead.
    // TODO #45 Once is_certainly_closed is implemented, use it to supersede is_root_closed when true.
    #[no_coverage]
    fn pretty_print(self, f: &mut fmt::Formatter, depth: usize, distance: usize, is_root_closed: bool) -> fmt::Result {
        match *self {
            Var(index, _) => {
                if is_root_closed {
                    write!(f, "x{}", depth - distance - index.0)
                } else {
                    write!(f, "{index}")
                }
            },
            Sort(level) => match level.to_numeral() {
                Some(n) => match n {
                    0 => write!(f, "Prop"),
                    1 => write!(f, "Type"),
                    _ => write!(f, "Type {}", n - 1),
                },
                None => write!(f, "Sort {level}"),
            },
            App(fun, arg) => {
                write!(f, "(")?;
                fun.pretty_print(f, depth, distance, is_root_closed)?;
                write!(f, ") (")?;
                arg.pretty_print(f, depth, distance, is_root_closed)?;
                write!(f, ")")
            },
            Abs(argtype, body) => {
                write!(f, "\u{003BB} ")?;
                if is_root_closed {
                    write!(f, "x{depth}: ")?;
                };
                argtype.pretty_print(f, depth + 1, distance + 1, is_root_closed)?;
                write!(f, " => ")?;
                body.pretty_print(f, depth + 1, distance, is_root_closed)
            },
            Prod(argtype, body) => {
                if is_root_closed {
                    write!(f, "(x{depth}: ")?;
                };
                argtype.pretty_print(f, depth + 1, distance + 1, is_root_closed)?;
                if is_root_closed {
                    write!(f, ")")?;
                };
                write!(f, " -> ")?;
                body.pretty_print(f, depth + 1, distance, is_root_closed)
            },
            Decl(decl) => write!(f, "{decl}"),
            Axiom(s, _) => write!(f, "{s}"),
        }
    }

    /// This function is the base low-level function for creating terms.
    ///
    /// It enforces the uniqueness property of terms in the arena.
    fn hashcons(node: Node<'arena>, arena: &mut Arena<'arena>) -> Self {
        if let Some(addr) = arena.hashcons_terms.get(&node) {
            Term::new(addr)
        } else {
            let addr = arena.alloc.alloc(node);
            arena.hashcons_terms.insert(addr);
            Term::new(addr)
        }
    }

    /// Returns a variable term with the given index and type.
    pub(crate) fn var(index: DeBruijnIndex, type_: Term<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(false);
        let payload = Var(index, type_);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns an axiom term with the given axiom.
    pub(crate) fn axiom(axiom: axiom::Axiom, lvl: &[Level<'arena>], arena: &mut Arena<'arena>) -> Self {
        let lvl = arena.store_level_slice(lvl);
        let header = Header::new(true);
        let payload = Axiom(axiom, lvl);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the term corresponding to a proposition.
    pub(crate) fn prop(arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(true);
        let payload = Sort(Level::zero(arena));

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the term associated to the sort of the given level.
    pub(crate) fn sort(level: Level<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(true);
        let payload = Sort(level);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the term corresponding to Type(level), casting level appropriately first.
    pub(crate) fn type_usize(level: usize, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(true);
        let payload = Sort(Level::from(level + 1, arena));

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the term corresponding to Sort(level), casting level appropriately first.
    pub(crate) fn sort_usize(level: usize, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(true);
        let payload = Sort(Level::from(level, arena));

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the application of one term to the other.
    pub(crate) fn app(self, arg: Self, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(self.is_certainly_closed() && arg.is_certainly_closed());
        let payload = App(self, arg);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the lambda-abstraction of the term `body`, with an argument of type `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn abs(self, body: Self, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(body.is_certainly_closed() && self.is_certainly_closed());
        let payload = Abs(self, body);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the dependant product of the term `body`, over elements of `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn prod(self, body: Self, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(body.is_certainly_closed() && self.is_certainly_closed());
        let payload = Prod(self, body);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the term associated to the given instantiated declaration.
    pub(crate) fn decl(decl: InstantiatedDeclaration<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new(false);
        let payload = Decl(decl);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the weak head normal form of the term, lazily computing the closure `f`.
    pub(crate) fn get_whnf_or_init<F>(self, f: F) -> Self
    where
        F: FnOnce() -> Self,
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

    /// Returns the relevance of the term, lazily computing the closure `f`.
    pub(crate) fn get_relevance_or_try_init<F>(self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        *self.0.header.is_relevant.get_or_init(f)
    }

    /// Indicates whether the term is known to be closed.
    #[inline]
    #[must_use]
    pub fn is_certainly_closed(self) -> bool {
        self.0.header.is_certainly_closed.get().is_some()
    }

    /// Set the term as closed.
    pub(crate) fn set_as_closed(self) {
        self.0.header.is_certainly_closed.set(()).ok();
    }
}

impl<'arena> Arena<'arena> {
    /// Returns the result of the substitution described by the key, lazily computing the closure `f`.
    pub(crate) fn get_subst_or_init<F>(&mut self, key: &(Term<'arena>, Term<'arena>, usize), f: F) -> Term<'arena>
    where
        F: FnOnce(&mut Self) -> Term<'arena>,
    {
        if let Some(res) = self.mem_subst.get(key) {
            *res
        } else {
            let res = f(self);
            self.mem_subst.insert(*key, res);
            res
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::memory::arena::use_arena;
    use crate::memory::declaration::{Declaration, InstantiatedDeclaration};
    use crate::memory::level::builder::raw as level;
    use crate::memory::term::builder::raw::*;
    use crate::memory::term::{PrettyTerm, Term};

    #[test]
    fn display_1() {
        use_arena(|arena| {
            let decl = InstantiatedDeclaration::instantiate(Declaration(Term::prop(arena), 0), &Vec::new(), arena);
            let prop = Term::decl(decl, arena);

            assert_eq!(prop.to_string(), "(Prop).{}");
        });
    }

    #[test]
    fn display_2() {
        use_arena(|arena| {
            let lvl = level::max(level::succ(level::var(0)), level::succ(level::var(1)));

            let term = arena.build_term_raw(abs(
                sort_(lvl),
                abs(
                    type_usize(0),
                    abs(
                        type_usize(1),
                        prod(var(1.into(), type_usize(1)), app(var(1.into(), type_usize(1)), var(2.into(), type_usize(0)))),
                    ),
                ),
            ));

            assert_eq!(term.to_string(), "λ Sort max (u0) (u1) + 1 => λ Type => λ Type 1 => 1 -> (1) (2)");
            assert_eq!(
                PrettyTerm(term).to_string(),
                "λ x0: Sort max (u0) (u1) + 1 => λ x1: Type => λ x2: Type 1 => (x3: x2) -> (x3) (x2)"
            );
        });
    }
}
