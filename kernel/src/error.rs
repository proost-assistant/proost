use crate::environment::EnvironmentError;
use crate::type_checker::TypeCheckerError;
use derive_more::{Display, From};

/// Type representing kernel errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub struct Error {
    /// The kind of form error that occurred.
    pub kind: ErrorKind,
    // This struct might contains more fields in the future (waiting for #15)
}

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq, From)]
pub enum ErrorKind {
    Environment(EnvironmentError),
    TypeChecker(TypeCheckerError),
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
