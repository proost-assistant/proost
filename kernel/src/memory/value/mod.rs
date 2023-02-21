use std::cell::OnceCell;

use derive_more::{Add, Display, From, Into, Sub};

use super::arena::Arena;
use super::declaration::InstantiatedDeclaration;
use super::level::Level;
use super::term::Term;
use crate::axiom;

/// An index used to designate bound variables.
#[derive(Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash)]
pub struct DeBruijnLevel(usize);

super::arena::new_dweller!(Value, Header, Payload);

/// The header of a term.
struct Header<'arena> {
    to_term: OnceCell<Term<'arena>>,
}

pub type Spine<'arena> = Vec<Value<'arena>>;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Head<'arena> {
    /// A variable, with its de Bruijn **level** and its type.
    Var(DeBruijnLevel, Term<'arena>),

    /// An axiom.
    Axiom(axiom::Axiom, &'arena [Level<'arena>]),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppClosure<'arena> {
    pub term: Term<'arena>,

    pub values: Vec<Value<'arena>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Payload<'arena> {
    /// A stuck computation of the form `h a1 ... an`, where `h` is a head
    Neutral(Head<'arena>, Spine<'arena>),

    /// Sort i, the encoding of Prop and Type i type.
    Sort(Level<'arena>),

    /// The lambda-abstraction of a term: the argument type is on the left, the body on the right.
    Abs(Value<'arena>, AppClosure<'arena>),

    /// The dependant product of the term on the right over all elements of the type on the left.
    Prod(Value<'arena>, AppClosure<'arena>),

    /// An instance of a universe-polymorphic declaration.
    Decl(InstantiatedDeclaration<'arena>),
}

impl<'arena> Header<'arena> {
    /// Creates a new default header, indicating whether it is *sure* that the term is closed, by
    /// `is_certainly_closed`. Should it be understood later that the term is closed, it will be
    /// modified accordingly.
    fn new() -> Self {
        Header {
            to_term: OnceCell::new(),
        }
    }
}

impl<'arena> AppClosure<'arena> {
    pub fn new(term: Term<'arena>, values: Vec<Value<'arena>>) -> AppClosure<'arena> {
        AppClosure { term, values }
    }
}

use Payload::{Abs, Decl, Neutral, Prod, Sort};

impl<'arena> Head<'arena> {
    // TODO add var/axiom intro funcions
}

impl<'arena> Value<'arena> {
    /// This function is the base low-level function for creating terms.
    ///
    /// It enforces the uniqueness property of terms in the arena.
    fn hashcons(node: Node<'arena>, arena: &mut Arena<'arena>) -> Self {
        if let Some(addr) = arena.hashcons_values.get(&node) {
            Value::new(addr)
        } else {
            let addr = arena.alloc.alloc(node);
            arena.hashcons_values.insert(addr);
            Value::new(addr)
        }
    }

    pub(crate) fn neutral(head: Head<'arena>, spine: Spine<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new();
        let payload = Neutral(head, spine);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the term associated to the sort of the given level.
    pub(crate) fn sort(level: Level<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new();
        let payload = Sort(level);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the lambda-abstraction of the term `body`, with an argument of type `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn abs(self, body: AppClosure<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new();
        let payload = Abs(self, body);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the dependant product of the term `body`, over elements of `arg_type`.
    ///
    /// Please note that no verification is done that occurrences of this variable in `body` have
    /// the same type.
    pub(crate) fn prod(self, body: AppClosure<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new();
        let payload = Prod(self, body);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the term associated to the given instantiated declaration.
    pub(crate) fn decl(decl: InstantiatedDeclaration<'arena>, arena: &mut Arena<'arena>) -> Self {
        let header = Header::new();
        let payload = Decl(decl);

        Self::hashcons(Node { header, payload }, arena)
    }

    /// Returns the weak head normal form of the term, lazily computing the closure `f`.
    pub(crate) fn get_term_or_init<F>(self, f: F) -> Term<'arena>
    where
        F: FnOnce() -> Term<'arena>,
    {
        *self.0.header.to_term.get_or_init(f)
    }
}
