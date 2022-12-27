//! Allows to recover position from the term.

use super::Result;

pub trait TraceableError {
    /// Appends a trace from which the error comes.
    #[must_use]
    fn trace_err(self, trace: Trace) -> Self;
}

impl<'arena, T> TraceableError for Result<'arena, T> {
    #[inline]
    fn trace_err(self, trace: Trace) -> Self {
        self.map_err(|mut err| {
            err.trace.push(trace);
            err
        })
    }
}

/// An element of a trace.
///
/// Indicates which branch has been taken.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Trace {
    /// Left branch
    Left,

    /// Right branch
    Right,
}
