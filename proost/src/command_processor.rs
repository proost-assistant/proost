use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use colored::Colorize;
use derive_more::Display;
use path_absolutize::Absolutize;

use crate::error::{Error::*, Result};
use kernel::location::Location;
use kernel::term::arena::{Arena, Term};
use parser::command::{Command, CommandProcessor};
use parser::parse_line;

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
    #[display(fmt = "cyclic dependency")]
    CyclicDependencies(String),
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
        let path = &self.importing.last().unwrap();
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
            Err(IO(Error {
                kind: ErrorKind::FileNotFound(file_path.to_string_lossy().to_string()),
                location,
            }))
        }
    }

    /// Begin a new file importation
    /// file_path must be absolutize
    fn import(&mut self, location: Location, file_path: PathBuf) -> Result<'arena, ()> {
        if !self.imported.contains(&file_path) {
            match self.importing.iter().position(|path| path == &file_path) {
                None => {
                    self.importing.push(file_path);

                    Ok(())
                }
                Some(i) => Err(IO(Error {
                    kind: ErrorKind::CyclicDependencies(
                        self.importing[i..]
                            .iter()
                            .map(|path| path.to_string_lossy())
                            .collect::<Vec<_>>()
                            .join("->\n"),
                    ),
                    location,
                })),
            }
        } else {
            Ok(())
        }
    }

    pub fn process_line(
        &mut self,
        arena: &mut Arena<'arena>,
        line: &str,
    ) -> Result<'arena, Option<Term<'arena>>> {
        let command = parse_line(arena, line)?;
        Ok(self.process(arena, command)?)
    }
}

impl<'build, 'arena> CommandProcessor<'build, 'arena, Result<'arena, Option<Term<'arena>>>>
    for Processor
{
    fn process(
        &mut self,
        arena: &mut Arena<'arena>,
        command: Command<'build, 'arena>,
    ) -> Result<'arena, Option<Term<'arena>>> {
        match command {
            Command::Define(s, None, term) => {
                arena.infer(term)?;
                arena.bind(s, term);
                Ok(None)
            }

            Command::Define(s, Some(t), term) => {
                arena.check(term, t)?;
                arena.bind(s, term);
                Ok(None)
            }

            Command::CheckType(t1, t2) => {
                arena.check(t1, t2)?;
                Ok(None)
            }

            Command::GetType(t) => {
                arena.infer(t).map(Some)?;
                Ok(Some(t))
            }

            Command::Eval(t) => Ok(Some(arena.whnf(t))),

            Command::Import(files) => files
                .iter()
                .map(|relative_path| {
                    let file_path =
                        self.create_path(Location::default(), relative_path.to_string())?;
                    self.import(Location::default(), file_path)
                })
                .collect::<Result<'arena, Vec<()>>>()
                .map(|_| None),
        }
    }
}

pub fn print_repl<'arena>(res: Result<'arena, Option<Term<'arena>>>) {
    match res {
        Ok(None) => println!("{}", "\u{2713}".green()),
        Ok(Some(t)) => {
            for line in t.to_string().lines() {
                println!("{} {}", "\u{2713}".green(), line)
            }
        }
        Err(err) => {
            let string = match err {
                Parser(parser::error::Error {
                    kind: parser::error::ErrorKind::EarlyKernelError(err),
                    ..
                }) => err.to_string(),

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
                }

                _ => err.to_string(),
            };

            for line in string.lines() {
                println!("{} {}", "\u{2717}".red(), line)
            }
        }
    }
}
