use crate::term::Term;
use derive_more::Display;

// TODO #19
/// Type representing kernel errors, is used by the toplevel to pretty-print errors.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum KernelError {
    // cannot parse command
    #[display(fmt = "cannot parse: {}", _0)]
    CannotParse(String),

    // s is already defined
    #[display(fmt = "{} is already defined", _0)]
    AlreadyDefined(String),

    /// s is not defined
    #[display(fmt = "{} is not defined", _0)]
    ConstNotFound(String),

    /// t is not a universe
    #[display(fmt = "{} is not a universe", _0)]
    NotUniverse(Term),

    /// t is not a type
    #[display(fmt = "{} is not a type", _0)]
    NotType(Term),

    /// t1 and t2 are not definitionally equal
    #[display(fmt = "{} and {} are not definitionaly equal", _0, _1)]
    NotDefEq(Term, Term),

    /// f of type t1 cannot be applied to x of type t2
    #[display(fmt = "{} : {} cannot be applied to {} : {}", _0, _1, _2, _3)]
    WrongArgumentType(Term, Term, Term, Term),

    /// t1 of type ty is not a function so cannot be applied to t2
    #[display(
        fmt = "{} : {} is not a function so cannot be applied to {}",
        _0,
        _1,
        _2
    )]
    NotAFunction(Term, Term, Term),

    /// Expected ty1, found ty2
    #[display(fmt = "expected {}, found {}", _0, _1)]
    TypeMismatch(Term, Term),
}
