//! Errors that can be yielded by the kernel

use derive_more::{Display, From};

use crate::memory::*;
use crate::type_checker::TypeCheckerError;

/// Type representing kernel errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub struct Error<'arena> {
    /// The kind of form error that occurred.
    pub kind: ErrorKind<'arena>,
    // This struct might contains more fields in the future (waiting for #15)
}

/// The kind of the error. This disambiguate between the different sections of the kernel, where
/// the errors are respectively defined.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq, From)]
pub enum ErrorKind<'arena> {
    TypeChecker(TypeCheckerError<'arena>),
    Term(term::builder::TermError<'arena>),
    Level(level::builder::LevelError<'arena>),
    Declaration(declaration::builder::DeclarationError<'arena>),
}

impl<'arena> std::error::Error for Error<'arena> {}

pub type Result<'arena, T> = std::result::Result<T, Error<'arena>>;
pub type ResultTerm<'arena> = Result<'arena, term::Term<'arena>>;
pub type ResultLevel<'arena> = Result<'arena, level::Level<'arena>>;
pub type ResultDecl<'arena> = Result<'arena, declaration::Declaration<'arena>>;
pub type ResultInstantiatedDecl<'arena> = Result<'arena, declaration::InstantiatedDeclaration<'arena>>;
