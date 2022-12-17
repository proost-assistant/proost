//! Errors that can be yielded by the kernel

use derive_more::{Display, From};

use crate::memory::{declaration, level, term};
use crate::type_checker::TypeCheckerError;

/// Type representing kernel errors.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error<'arena> {
    /// The kind of form error that occurred.
    pub kind: Kind<'arena>,

    /// Trace that generated the error.
    pub trace: Vec<i32>,
}

impl<'a> Error<'a> {
    /// Creates a new error from a kind and a trace.
    pub fn new(kind: Kind<'a>) -> Self {
        Self { kind, trace: Vec::new() }
    }
}

impl core::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.kind)
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
