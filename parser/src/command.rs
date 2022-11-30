//! High-level commands which constitute the end-user language.
//!
//! This complements low-level functions defined in the [`kernel::type_checker`] module.

use kernel::term::builders::Builder;
use kernel::term::arena::Arena;

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build> {
    /// Define a new term and optionally check that it's type match the given one.
    Define(&'build str, Option<Builder<'build>>, Builder<'build>),

    /// Infer the type of a term and check that it match the given one.
    CheckType(Builder<'build>, Builder<'build>),

    /// Infer the type of a term.
    GetType(Builder<'build>),

    /// Evaluate a term.
    Eval(Builder<'build>),

    /// Import a (series of) file(s).
    Import(Vec<&'build str>),

    /// Search for a variable
    Search(String),
}

pub trait CommandProcessor<'build, 'arena, T> {
    fn process(&mut self, arena: &mut Arena<'arena>, command: &Command<'build>) -> T;
}
