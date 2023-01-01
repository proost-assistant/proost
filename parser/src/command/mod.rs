//! High-level commands which constitute the end-user language.
//!
//! This complements low-level functions defined in the [`kernel::type_checker`] module.

pub mod parse;

use core::fmt;

use kernel::memory::declaration::builder as declaration;
use kernel::memory::term::builder::Builder;
use utils::location::Location;

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build> {
    /// Define the given term
    Define((Location, &'build str), Option<Builder<'build>>, Builder<'build>),

    /// Define the given declaration
    Declaration((Location, &'build str), Option<declaration::Builder<'build>>, declaration::Builder<'build>),

    /// Infer the type of a term and check that it matches the given one.
    CheckType(Builder<'build>, Builder<'build>),

    /// Infer the type of a term.
    GetType(Builder<'build>),

    /// Evaluate a term.
    Eval(Builder<'build>),

    /// Import a (series of) file(s).
    Import(Vec<(Location, &'build str)>),

    /// Search for a variable
    Search(&'build str),
}

impl<'build> fmt::Display for Command<'build> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Command::{CheckType, Declaration, Define, Eval, GetType, Import, Search};

        match *self {
            Define((_, name), None, ref t) => write!(f, "def {name} := {t}"),

            Define((_, name), Some(ref ty), ref t) => write!(f, "def {name}: {ty} := {t}"),

            Declaration((_, name), None, ref t) => write!(f, "def {name} := {t}"),

            Declaration((_, name), Some(ref ty), ref t) => write!(f, "def {name}: {ty} := {t}"),

            CheckType(ref t, ref ty) => write!(f, "check {t}: {ty}"),

            GetType(ref t) => write!(f, "check {t}"),

            Eval(ref t) => write!(f, "eval {t}"),

            Import(ref files) => {
                write!(f, "imports")?;
                files.iter().try_for_each(|(_, file)| write!(f, " {file}"))
            },

            Search(name) => write!(f, "search {name}"),
        }
    }
}
