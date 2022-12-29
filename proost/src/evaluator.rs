//! Tools to evaluate commands, as provided by the parser

use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::PathBuf;

use derive_more::Display;
use kernel::memory::arena::Arena;
use kernel::memory::term::Term;
use parser::command::{parse, Command};
use path_absolutize::Absolutize;
use utils::location::Location;

use crate::error::Error::{Kernel, TopLevel};
use crate::error::Result;

/// Type representing parser errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{kind}")]
pub struct Error {
    /// The kind of form error that occurred.
    pub kind: ErrorKind,

    /// The location of the error.
    pub location: Location,
}

/// Type representing errors from the evaluator
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind {
    /// This file cannot be found
    #[display(fmt = "{_0} is not a file")]
    FileNotFound(String),

    /// The given file could not be imported
    #[display(fmt = "errors occurred while reading file")]
    FileError,

    /// These files have a cyclic dependency
    #[display(fmt = "cyclic dependency:\n{_0}")]
    CyclicDependencies(String),

    /// This variable is already defined
    #[display(fmt = "identifier {_0} already defined")]
    BoundVariable(String),
}

impl std::error::Error for Error {}

/// An evaluator.
/// Responds to commands and use the kernel for side-effects.
pub struct Evaluator {
    /// The current path
    path: PathBuf,

    /// The set of all imported paths
    imported: HashSet<PathBuf>,

    /// Whether the evaluator should be verbose in designated contexts
    verbose: bool,
}

impl<'arena> Evaluator {
    /// Creates a new evaluator
    pub fn new(path: PathBuf, verbose: bool) -> Self {
        Self {
            path,
            imported: HashSet::new(),
            verbose,
        }
    }

    /// Create a new path from a relative path
    fn create_path<'build>(
        &self,
        location: Location,
        relative_path: String,
        importing: &[PathBuf],
    ) -> Result<'arena, 'build, PathBuf> {
        let file_path = importing
            .last()
            .and_then(|path| path.parent())
            .unwrap_or(&self.path)
            .join(relative_path)
            .absolutize()?
            .to_path_buf();

        if file_path.is_file() {
            Ok(file_path)
        } else {
            Err(TopLevel(Error {
                kind: ErrorKind::FileNotFound(file_path.to_string_lossy().to_string()),
                location,
            }))
        }
    }

    /// Begin a new file importation.
    ///
    /// `file_path` must be an absolute path.
    fn import_file<'build>(
        &mut self,
        arena: &mut Arena<'arena>,
        location: Location,
        file_path: PathBuf,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, 'build, ()> {
        if self.imported.contains(&file_path) {
            return Ok(());
        }

        if let Some(i) = importing.iter().position(|path| path == &file_path) {
            return Err(TopLevel(Error {
                kind: ErrorKind::CyclicDependencies(
                    importing[i..]
                        .iter()
                        .map(|path| path.to_string_lossy())
                        .collect::<Vec<_>>()
                        .join(" \u{2192}\n"),
                ),
                location,
            }));
        }

        // add file to the list of files to import
        importing.push(file_path.clone());
        // read it
        let file = read_to_string(file_path)?;
        // try to import it
        let result = self.process_file(arena, &file, importing);
        // remove it from the list of files to import
        let file_path = importing.pop().unwrap_or_else(|| unreachable!());

        // if importation failed, display the associated errors now (the imported file is discarded
        // right after, and errors may depend on it), and return an error about the command itself.
        result.map_err(|err| {
            crate::display(Err(err));

            Error {
                kind: ErrorKind::FileError,
                location,
            }
        })?;

        self.imported.insert(file_path);

        Ok(())
    }

    /// Processes a given line.
    ///
    /// # Errors
    /// Transmits any error that may occur during the overall process
    pub fn process_line<'build>(
        &mut self,
        arena: &mut Arena<'arena>,
        line: &'build str,
    ) -> Result<'arena, 'build, Option<Term<'arena>>> {
        let command = parse::line(line)?;

        self.process(arena, &command, &mut vec![])
    }

    /// Processes a given file.
    ///
    /// # Errors
    /// Transmits any error that may occur during the overall process
    pub fn process_file<'build>(
        &mut self,
        arena: &mut Arena<'arena>,
        file: &'build str,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, 'build, Option<Term<'arena>>> {
        let commands = parse::file(file)?;

        commands
            .iter()
            .try_for_each(|command| {
                if self.verbose {
                    println!("{command}");
                }
                self.process(arena, command, importing).map(|_| ())
            })
            .map(|_| None)
    }

    /// Processes a command.
    /// This is where most interaction with the kernel happens.
    ///
    /// # Errors
    /// Transmits any error from the kernel. Also signals any variable being defined twice.
    fn process<'build>(
        &mut self,
        arena: &mut Arena<'arena>,
        command: &Command<'build>,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, 'build, Option<Term<'arena>>> {
        match *command {
            Command::Define((location, s), ref type_builder, ref term_builder) => {
                if arena.get_binding(s).is_some() {
                    return Err(TopLevel(Error {
                        kind: ErrorKind::BoundVariable(s.to_owned()),
                        location,
                    }));
                }

                let term = term_builder.realise(arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;

                match *type_builder {
                    None => {
                        term.infer(arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;
                    },
                    Some(ref type_builder) => {
                        let type_ = type_builder.realise(arena).map_err(|err| Kernel(Box::new(type_builder.clone()), err))?;

                        term.check(type_, arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;
                    },
                }

                arena.bind(s, term);
                Ok(None)
            },

            Command::Declaration((location, s), ref type_builder, ref decl_builder) => {
                if arena.get_binding_decl(s).is_some() {
                    return Err(TopLevel(Error {
                        kind: ErrorKind::BoundVariable(s.to_owned()),
                        location,
                    }));
                }

                let decl = decl_builder.realise(arena).map_err(|err| Kernel(Box::new(decl_builder.clone()), err))?;

                match *type_builder {
                    None => {
                        decl.infer(arena).map_err(|err| Kernel(Box::new(decl_builder.clone()), err))?;
                    },
                    Some(ref type_builder) => {
                        let type_ = type_builder.realise(arena).map_err(|err| Kernel(Box::new(type_builder.clone()), err))?;

                        decl.check(type_, arena).map_err(|err| Kernel(Box::new(decl_builder.clone()), err))?;
                    },
                }

                arena.bind_decl(s, decl);
                Ok(None)
            },

            Command::CheckType(ref term_builder, ref type_builder) => {
                let term = term_builder.realise(arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;
                let type_ = type_builder.realise(arena).map_err(|err| Kernel(Box::new(type_builder.clone()), err))?;

                term.check(type_, arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;
                Ok(None)
            },

            Command::GetType(ref term_builder) => {
                let term = term_builder.realise(arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;

                Ok(term.infer(arena).map(Some).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?)
            },

            Command::Eval(ref term_builder) => {
                let term = term_builder.realise(arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;
                let _ = term.infer(arena).map_err(|err| Kernel(Box::new(term_builder.clone()), err))?;

                Ok(Some(term.normal_form(arena)))
            },

            Command::Search(s) => Ok(arena.get_binding(s)), // TODO (see #49)

            Command::Import(ref files) => files
                .iter()
                .try_for_each(|&(loc, relative_path)| {
                    let file_path = self.create_path(loc, relative_path.to_owned(), importing)?;

                    self.import_file(arena, loc, file_path, importing)
                })
                .map(|_| None),
        }
    }
}
