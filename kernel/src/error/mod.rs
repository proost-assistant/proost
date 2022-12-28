//! Errors that can be yielded by the kernel

pub mod location;
pub mod trace;

use derive_more::{Display, From};
use trace::Trace;

use crate::memory::{declaration, level, term};
use crate::type_checker::TypeCheckerError;

/// Type representing kernel errors.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{kind}")]
pub struct Error<'arena> {
    /// The kind of form error that occurred.
    pub kind: Kind<'arena>,

    /// Trace that generated the error.
    pub trace: Vec<Trace>,
}

impl<'arena> Error<'arena> {
    /// Creates a new error from a kind and a trace.
    #[inline]
    #[must_use]
    pub const fn new(kind: Kind<'arena>) -> Self {
        Self {
            kind,
            trace: Vec::new(),
        }
    }
}

/// The kind of the error. This disambiguate between the different sections of the kernel, where
/// the errors are respectively defined.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq, From)]
pub enum Kind<'arena> {
    TypeChecker(TypeCheckerError<'arena>),
    Term(term::builder::TermError<'arena>),
    Level(level::builder::LevelError<'arena>),
    Declaration(declaration::builder::DeclarationError<'arena>),
}

impl<'arena> std::error::Error for Error<'arena> {}

pub type Result<'arena, T> = core::result::Result<T, Error<'arena>>;
pub type ResultTerm<'arena> = Result<'arena, term::Term<'arena>>;
pub type ResultLevel<'arena> = Result<'arena, level::Level<'arena>>;
pub type ResultDecl<'arena> = Result<'arena, declaration::Declaration<'arena>>;
pub type ResultInstantiatedDecl<'arena> = Result<'arena, declaration::InstantiatedDeclaration<'arena>>;
