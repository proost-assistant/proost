//! High-level commands which constitute the end-user language.
//!
//! This complements low-level functions defined in the [`kernel::type_checker`] module.

use core::fmt;

use kernel::memory::declaration::builder as declaration;
use kernel::memory::term::builder::Builder;

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build> {
    /// Define the given term
    Define(&'build str, Option<Builder<'build>>, Builder<'build>),

    /// Define the given declaration
    Declaration(&'build str, Option<declaration::Builder<'build>>, declaration::Builder<'build>),

    /// Infer the type of a term and check that it matches the given one.
    CheckType(Builder<'build>, Builder<'build>),

    /// Infer the type of a term.
    GetType(Builder<'build>),

    /// Evaluate a term.
    Eval(Builder<'build>),

    /// Import a (series of) file(s).
    Import(Vec<&'build str>),

    /// Search for a variable
    Search(&'build str),
}

impl<'build> fmt::Display for Command<'build> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Command::*;

        match self {
            Define(name, None, t) => write!(f, "def {name} := {t}"),

            Define(name, Some(ty), t) => write!(f, "def {name}: {ty} := {t}"),

            Declaration(name, None, t) => write!(f, "def {name} := {t}"),

            Declaration(name, Some(ty), t) => write!(f, "def {name}: {ty} := {t}"),

            CheckType(t, ty) => write!(f, "check {t}: {ty}"),

            GetType(t) => write!(f, "check {t}"),

            Eval(t) => write!(f, "eval {t}"),

            Import(files) => {
                write!(f, "imports")?;
                files.iter().try_for_each(|file| write!(f, " {file}"))
            },

            Search(name) => write!(f, "search {name}"),
        }
    }
}
