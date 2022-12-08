use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::PathBuf;

use colored::Colorize;
use derive_more::Display;
use kernel::location::Location;
use kernel::term::arena::{Arena, Term};
use parser::command::Command;
use parser::{parse_file, parse_line};
use path_absolutize::Absolutize;

use crate::error::Error::*;
use crate::error::Result;

/// Type representing parser errors.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}", kind)]
pub struct Error {
    /// The kind of form error that occurred.
    pub kind: ErrorKind,

    /// The location of the error.
    pub location: Location,
}

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind {
    #[display(fmt = "{} is not a file", _0)]
    FileNotFound(String),
    #[display(fmt = "cyclic dependency:\n{}", _0)]
    CyclicDependencies(String),
    #[display(fmt = "identifier {} already defined", _0)]
    BoundVariable(String),
    #[display(fmt = "no module to close")]
    NoModuleToClose(),
}

impl std::error::Error for Error {}

pub struct Evaluator {
    path: PathBuf,
    imported_files: HashSet<PathBuf>,
    verbose: bool,
    module_stack: Vec<String>,
    defined_modules: HashSet<String>,
}

impl<'arena> Evaluator {
    pub fn new(path: PathBuf, verbose: bool) -> Evaluator {
        Evaluator {
            path,
            imported_files: HashSet::new(),
            verbose,
            module_stack: Vec::new(),
            defined_modules: HashSet::new(),
        }
    }

    /// Create a new path from a relative path
    fn create_path(&self, location: Location, relative_path: String, importing: &[PathBuf]) -> Result<'arena, PathBuf> {
        let file_path = importing
            .last()
            .and_then(|path| path.parent())
            .unwrap_or(&self.path)
            .join(relative_path)
            .absolutize()
            .unwrap()
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
    /// file_path must be absolute
    fn import_file(
        &mut self,
        arena: &mut Arena<'arena>,
        location: Location,
        file_path: PathBuf,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, ()> {
        if self.imported_files.contains(&file_path) {
            return Ok(());
        }

        if let Some(i) = importing.iter().position(|path| path == &file_path) {
            return Err(Toplevel(Error {
                kind: ErrorKind::CyclicDependencies(
                    importing[i..].iter().map(|path| path.to_string_lossy()).collect::<Vec<_>>().join(" \u{2192}\n"),
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
        let file_path = importing.pop().unwrap();
        // if importation failed, return error, else add file to imported_files files
        result?;
        self.imported_files.insert(file_path);
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
                    println!("{}", command);
                }
                self.process(arena, command, importing).map(|_| ())
            })
            .map(|_| None)
    }

    fn process<'build>(
        &mut self,
        arena: &mut Arena<'arena>,
        command: &Command<'build>,
        importing: &mut Vec<PathBuf>,
    ) -> Result<'arena, Option<Term<'arena>>> {
        match command {
            Command::Define(_, s, None, term) => {
                //TODO
                let term = term.realise(arena)?;
                if arena.get_binding(s).is_none() {
                    arena.infer(term)?;
                    arena.bind(s, term);
                    Ok(None)
                } else {
                    Err(Toplevel(Error {
                        kind: ErrorKind::BoundVariable(s.to_string()),
                        location: Location::default(), // TODO (see #38)
                    }))
                }
            },

            Command::Define(_, s, Some(t), term) => {
                //TODO
                let term = term.realise(arena)?;
                let t = t.realise(arena)?;
                arena.check(term, t)?;
                arena.bind(s, term);
                Ok(None)
            },

            Command::CheckType(t1, t2) => {
                let t1 = t1.realise(arena)?;
                let t2 = t2.realise(arena)?;
                arena.check(t1, t2)?;
                Ok(None)
            },

            Command::GetType(t) => {
                let t = t.realise(arena)?;
                Ok(arena.infer(t).map(Some)?)
            },

            Command::Eval(t) => {
                let t = t.realise(arena)?;
                Ok(Some(arena.normal_form(t)))
            },

            Command::Search(s) => Ok(arena.get_binding(s)), // TODO (see #49)

            Command::Import(files) => files
                .iter()
                .try_for_each(|relative_path| {
                    let file_path = self.create_path(Location::default(), relative_path.to_string(), importing)?;
                    self.import_file(arena, Location::default(), file_path, importing)
                })
                .map(|_| None),

            Command::BeginModule(s) => {
                self.module_stack.push(s.to_string());
                println!("{:?}, {:?}", self.module_stack, self.defined_modules);
                Ok(None)
            },

            Command::EndModule() => {
                if let Some(suffix) = self.module_stack.pop() {
                    let prefix = self.module_stack.join("::");
                    let module = if prefix.is_empty() { suffix } else { prefix + "::" + &suffix };
                    self.defined_modules.insert(module);
                    Ok(None)
                } else {
                    Err(Toplevel(Error {
                        kind: ErrorKind::NoModuleToClose(),
                        location: Location::default(), // TODO (see #38)
                    }))
                }
            },
        }
    }

    pub fn display(&self, res: Result<'arena, Option<Term<'arena>>>) {
        match res {
            Ok(None) => println!("{}", "\u{2713}".green()),
            Ok(Some(t)) => {
                for line in t.to_string().lines() {
                    println!("{} {}", "\u{2713}".green(), line)
                }
            },
            Err(err) => {
                let string = match err {
                    Parser(parser::error::Error {
                        kind: parser::error::ErrorKind::CannotParse(message),
                        location: loc,
                    }) => {
                        if loc.start.column == loc.end.column {
                            format!("{:0w1$}^\n{m}", "", w1 = loc.start.column - 1, m = message)
                        } else {
                            format!(
                                "{:0w1$}^{:-<w2$}^\n{m}",
                                "",
                                "",
                                w1 = loc.start.column - 1,
                                w2 = loc.end.column - loc.start.column - 1,
                                m = message
                            )
                        }
                    },

                    _ => err.to_string(),
                };

                for line in string.lines() {
                    println!("{} {}", "\u{2717}".red(), line)
                }
            },
        }
    }
}
