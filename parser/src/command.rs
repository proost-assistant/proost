//! High-level commands which constitute the end-user language.
//!
//! This complements low-level functions defined in the [`kernel::type_checker`] module.

use kernel::term::arena::{Arena, Term};

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build, 'arena> {
    /// Define a new term and optionally check that it's type match the given one.
    Define(&'build str, Option<Term<'arena>>, Term<'arena>),

    /// Infer the type of a term and check that it match the given one.
    CheckType(Term<'arena>, Term<'arena>),

    /// Infer the type of a term.
    GetType(Term<'arena>),

    /// Evaluate a term.
    Eval(Term<'arena>),

    /// Import a file.
    Import(Vec<String>),
}

pub trait CommandProcessor<'build, 'arena, T> {
    fn process(&mut self, arena: &mut Arena<'arena>, command: &Command<'build, 'arena>) -> T;
}
