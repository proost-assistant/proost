//! Trace to record the path taken by an algorithm over a structure.

use crate::error::Error;
use crate::location::Location;

/// An element of a trace that indicates which branch has been taken at each step of the execution of an algorithm.
///
/// Please note that there is only two possible values since our structures have at most two children.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Trace {
    /// Left branch
    Left,

    /// Right branch
    Right,
}

/// Types that generates a trace when executed.
///
/// See also: [`TraceableError`]
pub trait Traceable {
    /// Returns the location of a specific element, given by the `trace`.
    fn apply_trace(&self, trace: &[Trace]) -> Location;
}

/// Utility trait simplifying the use of [`Trace`] in error handling contexts.
pub trait TraceableError {
    /// Appends a trace from which the error comes.
    #[must_use]
    fn trace_err(self, trace: Trace) -> Self;
}

impl<K: core::fmt::Display, T> TraceableError for Result<T, Error<K>> {
    #[inline]
    fn trace_err(self, trace: Trace) -> Self {
        self.map_err(|mut err| {
            err.trace.push(trace);
            err
        })
    }
}
