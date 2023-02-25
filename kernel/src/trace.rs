//! Traces to record the path taken by an algorithm over a structure.
//!
//! Traces are used by the typecheck to provide additional information about any error that it may
//! encounter in a normal execution.

use crate::error::Error;

/// An element of a trace that indicates which branch has been taken at each step of the execution
/// of an algorithm.
///
/// Please note that there are only two possible values since the structures of interest have at
/// most two children.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Trace {
    /// Application Left branch.
    AppLeft,
    /// Application Right branch.
    AppRight,

    /// Lambda-abstraction Left branch.
    AbsLeft,
    /// Lambda-abstraction Right branch.
    AbsRight,

    /// Dependent product Left branch.
    ProdLeft,
    /// Dependent product Right branch.
    ProdRight,

    /// Type of a term introduced by a let-in
    LetType,
    /// Definition of a term introduced by a let-in
    LetDef,
    /// Body in which a term introduced by a let-in is used
    LetBody,
}

/// Types that generates a trace when executed.
///
/// See also: [`TraceableError`].
pub trait Traceable<T> {
    /// Returns the specific element, given by the `trace`.
    fn apply_trace(&self, trace: &[Trace]) -> T;
}

/// Utility trait simplifying the use of [`Trace`] in error handling contexts.
pub trait TraceableError {
    /// Appends a trace from which the error comes.
    #[must_use]
    fn trace_err(self, trace: Trace) -> Self;
}

impl<T> TraceableError for Result<T, Error<'_>> {
    #[inline]
    fn trace_err(self, trace: Trace) -> Self {
        self.map_err(|mut err| {
            err.trace.push(trace);
            err
        })
    }
}
