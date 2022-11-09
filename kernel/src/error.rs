use crate::term::{DefinitionError, Term};
use crate::type_checker::TypeCheckerError;
use derive_more::{Display, From};

/// Type representing kernel errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub struct Error<'arena> {
    /// The kind of form error that occurred.
    pub kind: ErrorKind<'arena>,
    // This struct might contains more fields in the future (waiting for #15)
}

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq, From)]
pub enum ErrorKind<'arena> {
    TypeChecker(TypeCheckerError<'arena>),
    Definition(DefinitionError<'arena>),
}

impl<'arena> std::error::Error for Error<'arena> {}

pub type Result<'arena, T> = std::result::Result<T, Error<'arena>>;
pub(crate) type ResultTerm<'arena> = Result<'arena, Term<'arena>>;
