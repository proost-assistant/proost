use std::{
    collections::HashSet,
    fs::read_to_string,
    path::{Path, PathBuf},
};

use derive_more::Display;
use path_absolutize::Absolutize;

use crate::error::{Error::*, Result};
use kernel::location::Location;
use kernel::term::arena::{Arena, Term};
use parser::command::{Command, CommandProcessor};
use parser::{parse_file, parse_line};

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
}

impl std::error::Error for Error {}

pub struct Processor {
    importing: Vec<PathBuf>,
    imported: HashSet<PathBuf>,
}

impl<'arena> Processor {
    pub fn new(current_path: PathBuf) -> Processor {
        Processor {
            importing: vec![current_path],
            imported: HashSet::new(),
        }
    }

    /// Return the current working path
    fn path(&self) -> &Path {
        let path = self.importing.last().unwrap();
        if path.is_file() {
            path.parent().unwrap()
        } else {
            path
        }
    }

    /// Create a new path from a relative path
    fn create_path(&self, location: Location, relative_path: String) -> Result<'arena, PathBuf> {
        let file_path = self
            .path()
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
    ) -> Result<'arena, ()> {
        if self.imported.contains(&file_path) {
            return Ok(());
        }

        if let Some(i) = self.importing.iter().position(|path| path == &file_path) {
            return Err(Toplevel(Error {
                kind: ErrorKind::CyclicDependencies(
                    self.importing[i..]
                        .iter()
                        .map(|path| path.to_string_lossy())
                        .collect::<Vec<_>>()
                        .join(" \u{2192}\n"),
                ),
                location,
            }));
        }

        self.importing.push(file_path.clone());

        let file = read_to_string(file_path)?;
        self.process_file(arena, &file)?;

        let file_path = self.importing.pop().unwrap();
        self.imported.insert(file_path);

        Ok(())
    }

    pub fn process_line(
        &mut self,
        arena: &mut Arena<'arena>,
        line: &str,
    ) -> Result<'arena, Option<Term<'arena>>> {
        let command = parse_line(line)?;
        self.process(arena, &command)
    }

    pub fn process_file(
        &mut self,
        arena: &mut Arena<'arena>,
        file: &str,
    ) -> Result<'arena, Option<Term<'arena>>> {
        let commands = parse_file(file)?;
        commands
            .iter()
            .try_for_each(|command| {
                println!("{}", command);
                self.process(arena, command).map(|_| ())
            })
            .map(|_| None)
    }
}

impl<'build, 'arena> CommandProcessor<'build, 'arena, Result<'arena, Option<Term<'arena>>>>
    for Processor
{
    fn process(
        &mut self,
        arena: &mut Arena<'arena>,
        command: &Command<'build>,
    ) -> Result<'arena, Option<Term<'arena>>> {
        match command {
            Command::Define(s, None, term) => {
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
            }

            Command::Define(s, Some(t), term) => {
                let term = term.realise(arena)?;
                let t = t.realise(arena)?;
                arena.check(term, t)?;
                arena.bind(s, term);
                Ok(None)
            }

            Command::CheckType(t1, t2) => {
                let t1 = t1.realise(arena)?;
                let t2 = t2.realise(arena)?;
                arena.check(t1, t2)?;
                Ok(None)
            }

            Command::GetType(t) => {
                let t = t.realise(arena)?;
                Ok(arena.infer(t).map(Some)?)
            }

            Command::Eval(t) => {
                let t = t.realise(arena)?;
                Ok(Some(arena.normal_form(t)))
            }

            Command::Search(s) => Ok(arena.get_binding(s)), // TODO

            Command::Import(files) => files
                .iter()
                .map(|relative_path| {
                    let file_path =
                        self.create_path(Location::default(), relative_path.to_string())?;
                    self.import_file(arena, Location::default(), file_path)
                })
                .collect::<Result<'arena, Vec<()>>>()
                .map(|_| None),
        }
    }
}
