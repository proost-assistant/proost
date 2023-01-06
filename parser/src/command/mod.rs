//! High-level commands which constitute the end-user language.
//!
//! This complements low-level functions defined in the [`kernel::type_checker`] module.

pub mod parse;

use core::fmt;

use elaboration::builder::declaration;
use elaboration::builder::term::Builder;
use elaboration::location::Location;

/// The type of commands that can be received by the kernel.
#[derive(Debug, Eq, PartialEq)]
pub enum Command<'build> {
    /// Define the given term
    Define(bool, (Location, &'build str), Option<Builder<'build>>, Builder<'build>),

    /// Define the given declaration
    Declaration(bool, (Location, &'build str), Option<declaration::Builder<'build>>, declaration::Builder<'build>),

    /// Infer the type of a term and check that it matches the given one.
    CheckType(Builder<'build>, Builder<'build>),

    /// Infer the type of a term.
    GetType(Builder<'build>),

    /// Evaluate a term.
    Eval(Builder<'build>),

    /// Import a (series of) file(s).
    Import(Vec<(Location, &'build str)>),

    /// Search for a variable
    Search(Vec<&'build str>),

    /// Begin a module
    BeginModule(bool, &'build str),

    /// End a module
    EndModule(),

    /// Open a module
    UseModule(bool, Vec<&'build str>),
}

impl<'build> fmt::Display for Command<'build> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Command::{BeginModule, CheckType, Declaration, Define, EndModule, Eval, GetType, Import, Search, UseModule};

        match *self {
            Define(public, (_, name), None, ref t) => {
                if public {
                    write!(f, "mod ")?;
                }
                write!(f, "def {name} := {t}")
            },

            Define(public, (_, name), Some(ref ty), ref t) => {
                if public {
                    write!(f, "mod ")?;
                }
                write!(f, "def {name}: {ty} := {t}")
            },

            Declaration(public, (_, name), None, ref t) => {
                if public {
                    write!(f, "mod ")?;
                }
                write!(f, "def {name} := {t}")
            },

            Declaration(public, (_, name), Some(ref ty), ref t) => {
                if public {
                    write!(f, "mod ")?;
                }
                write!(f, "def {name}: {ty} := {t}")
            },

            CheckType(ref t, ref ty) => write!(f, "check {t}: {ty}"),

            GetType(ref t) => write!(f, "check {t}"),

            Eval(ref t) => write!(f, "eval {t}"),

            Import(ref files) => {
                write!(f, "imports")?;
                files.iter().try_for_each(|&(_, file)| write!(f, " {file}"))
            },

            Search(ref name) => write!(f, "search {}", name.join("::")),

            BeginModule(public, name) => {
                if public {
                    write!(f, "mod ")?;
                }
                write!(f, "mod {name}")
            },

            EndModule() => write!(f, "end"),

            UseModule(public, ref name) => {
                if public {
                    write!(f, "mod ")?;
                }
                write!(f, "use {}", name.join("::"))
            },
        }
    }
}
