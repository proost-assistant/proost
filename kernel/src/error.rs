//! Errors that can be yielded by the kernel

use derive_more::{Display, From};

use crate::memory::{declaration, level, term};
use crate::type_checker;

/// The kind of errors that can be encountered by the kernel.
///
/// This disambiguate between the different sections of the kernel, where the errors are respectively defined.
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

/// Specify the [`Error`](utils::error::Error) type for the kernel.
pub type Error<'arena> = utils::error::Error<Kind<'arena>>;

/// The type of results yielded by the kernel.
pub type Result<'arena, T> = core::result::Result<T, Error<'arena>>;

/// The type of results yielded by the kernel (specialised to terms).
pub(crate) type ResultTerm<'arena> = Result<'arena, term::Term<'arena>>;

/// The type of results yielded by the kernel (specialised to levels).
pub(crate) type ResultLevel<'arena> = Result<'arena, level::Level<'arena>>;

/// The type of results yielded by the kernel (specialised to declarations).
pub(crate) type ResultDecl<'arena> = Result<'arena, declaration::Declaration<'arena>>;

/// The type of results yielded by the kernel (specialised to instantiated declarations).
pub(crate) type ResultInstantiatedDecl<'arena> = Result<'arena, declaration::InstantiatedDeclaration<'arena>>;
