//! Errors that can be yielded by the kernel.

use derive_more::{Display, From};

use crate::memory::{declaration, level, term};
use crate::trace::Trace;
use crate::type_checker;

/// The kind of errors that can be encountered by the kernel.
///
/// This disambiguates between the different sections of the kernel, where the errors are respectively defined.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq, From)]
pub enum Kind<'arena> {
    /// Kind of errors raised during type checking phase.
    TypeChecker(type_checker::ErrorKind<'arena>),

    /// Kind of errors raised during [`Term`](term::Term) building.
    Term(term::builder::ErrorKind<'arena>),

    /// Kind of errors raised during [`Level`](level::Level) building.
    Level(level::builder::ErrorKind<'arena>),

    /// Kind of errors raised during [`Declaration`](declaration::Declaration) building.
    Declaration(declaration::builder::ErrorKind<'arena>),
}

/// The type representing errors and the trace to find the specific element that yield the error.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{kind}")]
pub struct Error<'arena> {
    /// The kind of form error that occurred.
    pub kind: Kind<'arena>,

    /// The trace.
    pub trace: Vec<Trace>,
}

impl<'arena> Error<'arena> {
    /// Creates a new error from a `kind` and a `trace`.
    #[inline]
    #[must_use]
    pub const fn new(kind: Kind<'arena>) -> Self {
        let trace = vec![];

        Self { kind, trace }
    }
}

/// The type of results yielded by the kernel.
pub type Result<'arena, T> = core::result::Result<T, Error<'arena>>;

/// The type of results yielded by the kernel (specialised to terms).
pub type ResultTerm<'arena> = Result<'arena, term::Term<'arena>>;

/// The type of results yielded by the kernel (specialised to levels).
pub type ResultLevel<'arena> = Result<'arena, level::Level<'arena>>;

/// The type of results yielded by the kernel (specialised to declarations).
pub type ResultDecl<'arena> = Result<'arena, declaration::Declaration<'arena>>;

/// The type of results yielded by the kernel (specialised to instantiated declarations).
pub type ResultInstantiatedDecl<'arena> = Result<'arena, declaration::InstantiatedDeclaration<'arena>>;
