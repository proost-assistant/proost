//! Tools to evaluate commands, as provided by the parser

use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

use derive_more::Display;
use elaboration::builder::Buildable;
use elaboration::location::Location;
use kernel::memory::arena::Arena;
use parser::command::{parse, Command};
use path_absolutize::Absolutize;

use crate::error::Error::{Kernel, TopLevel};
use crate::error::{Result, ResultProcess};
use crate::module_tree::ModuleTree;

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
    #[display(fmt = "errors occurred while reading file {_0}")]
    FileError(String),

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

    /// Whether the evaluator should be verbose in designated contexts
    verbose: bool,
    
    /// The set of all imported paths
    imported: HashSet<PathBuf>,

    /// The tree containing the module inclusion information
    modtree: ModuleTree,
}

impl<'arena> Evaluator {
    /// Creates a new evaluator
    pub fn new(path: PathBuf, verbose: bool) -> Self {
        Self {
            path,
            verbose,
            imported: HashSet::new(),
            modtree: ModuleTree::new(),
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
        file_path: &PathBuf,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, 'build, ()> {
        if self.imported.contains(file_path) {
            return Ok(());
        }

        if let Some(i) = importing.iter().position(|path| path == file_path) {
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
        let result = self.process_file(arena, location, &file, file_path, importing);
        // remove it from the list of files to import
        let file_path = importing.pop().unwrap_or_else(|| unreachable!());

        result?;

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
        command: &'build Command<'build>,
    ) -> ResultProcess<'arena, 'build> {
        self.process(arena, command, &mut vec![])
    }

    /// Processes a given file.
    ///
    /// # Errors
    /// Transmits any error that may occur during the overall process
    pub fn process_file<'build>(
        &mut self,
        arena: &mut Arena<'arena>,
        location: Location,
        file: &'build str,
        file_path: &Path,
        importing: &mut Vec<PathBuf>,
    ) -> ResultProcess<'arena, 'static> {
        let commands = parse::file(file)?;

        commands
            .iter()
            .try_for_each(|command| {
                if self.verbose {
                    println!("{command}");
                }
                self.process(arena, command, importing).map(|_| ()).map_err(|err| {
                    // if importation failed, display the associated errors now (the imported file is discarded
                    // right after, and errors may depend on it), and return an error about the command itself.
                    crate::display(Err(err), false);

                    Error {
                        kind: ErrorKind::FileError(file_path.to_string_lossy().to_string()),
                        location,
                    }
                    .into()
                })
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
        command: &'build Command<'build>,
        importing: &mut Vec<PathBuf>,
    ) -> ResultProcess<'arena, 'build> {
        match *command {
            // TODO
            Command::Define(_, (location, s), ref type_builder, ref term_builder) => {
                if arena.get_binding(s).is_some() {
                    return Err(TopLevel(Error {
                        kind: ErrorKind::BoundVariable(s.to_owned()),
                        location,
                    }));
                }

                let term = term_builder.realise(arena).map_err(|err| Kernel(term_builder, err))?;

                if let Some(ref type_builder) = *type_builder {
                    let type_ = type_builder.realise(arena).map_err(|err| Kernel(type_builder, err))?;

                    term.check(type_, arena).map_err(|err| Kernel(term_builder, err))?;
                } else {
                    term.infer(arena).map_err(|err| Kernel(term_builder, err))?;
                }

                arena.bind(s, term);
                Ok(None)
            },

            // TODO
            Command::Declaration(_, (location, s), ref type_builder, ref decl_builder) => {
                if arena.get_binding_decl(s).is_some() {
                    return Err(TopLevel(Error {
                        kind: ErrorKind::BoundVariable(s.to_owned()),
                        location,
                    }));
                }

                let decl = decl_builder.realise(arena).map_err(|err| Kernel(decl_builder, err))?;

                if let Some(ref type_builder) = *type_builder {
                    let type_ = type_builder.realise(arena).map_err(|err| Kernel(type_builder, err))?;

                    decl.check(type_, arena).map_err(|err| Kernel(decl_builder, err))?;
                } else {
                    decl.infer(arena).map_err(|err| Kernel(decl_builder, err))?;
                }

                arena.bind_decl(s, decl);
                Ok(None)
            },

            Command::CheckType(ref term_builder, ref type_builder) => {
                let term = term_builder.realise(arena).map_err(|err| Kernel(term_builder, err))?;
                let type_ = type_builder.realise(arena).map_err(|err| Kernel(type_builder, err))?;

                term.check(type_, arena).map_err(|err| Kernel(term_builder, err))?;
                Ok(None)
            },

            Command::GetType(ref term_builder) => {
                let term = term_builder.realise(arena).map_err(|err| Kernel(term_builder, err))?;

                Ok(term.infer(arena).map(Some).map_err(|err| Kernel(term_builder, err))?)
            },

            Command::Eval(ref term_builder) => {
                let term = term_builder.realise(arena).map_err(|err| Kernel(term_builder, err))?;
                let _ = term.infer(arena).map_err(|err| Kernel(term_builder, err))?;

                Ok(Some(term.normal_form(arena)))
            },

            Command::Search(ref s) => Ok(arena.get_binding(s.last().unwrap())), // TODO (see #49)

            Command::Import(ref files) => files
                .iter()
                .try_for_each(|&(loc, relative_path)| {
                    let file_path = self.create_path(loc, relative_path.to_owned(), importing)?;

                    self.import_file(arena, loc, &file_path, importing)
                })
                .map(|_| None),

            Command::BeginModule(ref s) => {
                self.modtree.begin_module(s);
                Ok(None)
            },

            Command::EndModule() => {
                self.modtree.end_module()?;
                Ok(None)
            },

            Command::UseModule(ref name) => Ok(None), //TODO
        }
    }
}
