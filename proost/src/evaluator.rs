use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::PathBuf;

use derive_more::Display;
use kernel::location::Location;
use kernel::memory::arena::Arena;
use kernel::memory::term::Term;
use parser::command::Command;
use parser::{parse_file, parse_line};
use path_absolutize::Absolutize;

use crate::error::Error::Toplevel;
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

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind {
    #[display(fmt = "{_0} is not a file")]
    FileNotFound(String),
    #[display(fmt = "cyclic dependency:\n{_0}")]
    CyclicDependencies(String),
    #[display(fmt = "identifier {_0} already defined")]
    BoundVariable(String),
}

impl std::error::Error for Error {}

pub struct Evaluator {
    path: PathBuf,
    imported: HashSet<PathBuf>,
    verbose: bool,
}

impl<'arena> Evaluator {
    pub fn new(path: PathBuf, verbose: bool) -> Self {
        Self {
            path,
            imported: HashSet::new(),
            verbose,
        }
    }

    /// Create a new path from a relative path
    fn create_path(&self, location: Location, relative_path: String, importing: &[PathBuf]) -> Result<'arena, PathBuf> {
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
            Err(Toplevel(Error {
                kind: ErrorKind::FileNotFound(file_path.to_string_lossy().to_string()),
                location,
            }))
        }
    }

    /// Begin a new file importation.
    ///
    /// `file_path` must be absolute
    fn import_file(
        &mut self,
        arena: &mut Arena<'arena>,
        location: Location,
        file_path: PathBuf,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, ()> {
        if self.imported.contains(&file_path) {
            return Ok(());
        }

        if let Some(i) = importing.iter().position(|path| path == &file_path) {
            return Err(Toplevel(Error {
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
        // if importation failed, return error, else add file to imported files
        result?;
        self.imported.insert(file_path);
        Ok(())
    }

    pub fn process_line(&mut self, arena: &mut Arena<'arena>, line: &str) -> Result<'arena, Option<Term<'arena>>> {
        let command = parse_line(line)?;
        self.process(arena, &command, &mut Vec::new())
    }

    pub fn process_file(
        &mut self,
        arena: &mut Arena<'arena>,
        file: &str,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, Option<Term<'arena>>> {
        let commands = parse_file(file)?;
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

    fn process(
        &mut self,
        arena: &mut Arena<'arena>,
        command: &Command,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, Option<Term<'arena>>> {
        match *command {
            Command::Define(s, ref ty, ref term) => {
                if arena.get_binding(s).is_some() {
                    return Err(Toplevel(Error {
                        kind: ErrorKind::BoundVariable(s.to_owned()),
                        location: Location::default(), // TODO (see #38)
                    }));
                }
                let term = term.realise(arena)?;
                match *ty {
                    None => {
                        term.infer(arena)?;
                    },
                    Some(ref ty) => term.check(ty.realise(arena)?, arena)?,
                }
                arena.bind(s, term);
                Ok(None)
            },

            Command::Declaration(s, ref ty, ref decl) => {
                if arena.get_binding_decl(s).is_some() {
                    return Err(Toplevel(Error {
                        kind: ErrorKind::BoundVariable(s.to_owned()),
                        location: Location::default(), // TODO (see #38)
                    }));
                }
                let decl = decl.realise(arena)?;
                match *ty {
                    None => {
                        decl.infer(arena)?;
                    },
                    Some(ref ty) => decl.check(ty.realise(arena)?, arena)?,
                }
                arena.bind_decl(s, decl);
                Ok(None)
            },

            Command::CheckType(ref t1, ref t2) => {
                let t1 = t1.realise(arena)?;
                let t2 = t2.realise(arena)?;
                t1.check(t2, arena)?;
                Ok(None)
            },

            Command::GetType(ref t) => {
                let t = t.realise(arena)?;
                Ok(t.infer(arena).map(Some)?)
            },

            Command::Eval(ref t) => {
                let t = t.realise(arena)?;
                let _ = t.infer(arena)?;
                Ok(Some(t.normal_form(arena)))
            },

            Command::Search(s) => Ok(arena.get_binding(s)), // TODO (see #49)

            Command::Import(ref files) => files
                .iter()
                .try_for_each(|relative_path| {
                    let file_path = self.create_path(Location::default(), (*relative_path).to_owned(), importing)?;
                    self.import_file(arena, Location::default(), file_path, importing)
                })
                .map(|_| None),
        }
    }
}
