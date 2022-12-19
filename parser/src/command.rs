//! High-level commands which constitute the end-user language.
//!
//! This complements low-level functions defined in the [`kernel::type_checker`] module.

use core::fmt;

use kernel::memory::builders::TermBuilder;

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build> {
    /// Define a new term and optionally check that it's type match the given one.
    Define(&'build str, Box<[String]>, Option<TermBuilder<'build>>, TermBuilder<'build>),

    /// Infer the type of a term and check that it match the given one.
    CheckType(TermBuilder<'build>,TermBuilder<'build>),

    /// Infer the type of a term.
    GetType(TermBuilder<'build>),

    /// Evaluate a term.
    Eval(TermBuilder<'build>),

    /// Import a (series of) file(s).
    Import(Vec<&'build str>),

    /// Search for a variable
    Search(&'build str),
}

impl<'build> fmt::Display for Command<'build> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Command::*;

        match self {
            Define(name,_, None, t) => write!(f, "def {} := {}", name, t),

            Define(name,_,  Some(ty), t) => write!(f, "def {} : {} := {}", name, ty, t),

            CheckType(t, ty) => write!(f, "check {}: {}", t, ty),

            GetType(t) => write!(f, "check {}", t),

            Eval(t) => write!(f, "eval {}", t),

            Import(files) => {
                write!(f, "imports")?;
                files.iter().try_for_each(|file| write!(f, " {file}"))
            },

            Search(name) => write!(f, "search {}", name),
        }
    }
}
