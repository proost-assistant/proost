//! Definition of error with corresponding [`Trace`](crate::trace) to find [`Location`](crate::location).

use derive_more::Display;

use crate::trace;

/// The type representing errors and the trace to find the location.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{kind}")]
pub struct Error<K: core::fmt::Display> {
    /// The kind of form error that occurred.
    pub kind: K,

    /// The trace.
    pub trace: Vec<trace::Trace>,
}

impl<K: core::fmt::Display> Error<K> {
    /// Creates a new error from a `kind` and a `trace`.
    #[inline]
    #[must_use]
    pub const fn new(kind: K) -> Self {
        let trace = vec![];

        Self { kind, trace }
    }
}
impl<K: core::fmt::Debug + core::fmt::Display> std::error::Error for Error<K> {}
