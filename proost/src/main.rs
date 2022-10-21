#![feature(box_syntax)]

use clap::Parser;
use colored::*;
use kernel::{Command, Term, TypeCheckingError};
use parser::*;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;
use std::{fmt, fs};

// clap configuration
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    files: Vec<String>,
}

// constants fetching
const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

// managing commands results
struct CommandResult(Result<Option<Term>, TypeCheckingError>);
impl fmt::Display for CommandResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Ok(Some(t)) => write!(f, "{} {}", "\u{2713}".green(), t),
            Ok(None) => write!(f, "{}", "\u{2713}".green()),
            Err(err) => write!(f, "{} {}", "\u{2717}".red(), err),
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if !args.files.is_empty() {
        for path in args.files.iter() {
            match fs::read_to_string(path) {
                Ok(contents) => {
                    let _ = parse_file(&contents);
                }
                Err(_) => {
                    println!("Error: No such file or directory: {}", path);
                }
            }
        }
        return Ok(());
    }

    let mut rl_err: Option<ReadlineError> = None;
    let mut rl = Editor::<()>::new()?;

    println!("Welcome to {} {}", NAME, VERSION);

    loop {
        let readline = rl.readline("\u{00BB} ");
        match readline {
            Ok(line) if !line.is_empty() => {
                rl.add_history_entry(line.as_str());

                match parse_command(line.as_str()) {
                    Ok(command) => {
                        let res = match command {
                            Command::Define(_, _) => CommandResult(Ok(None)),
                            Command::CheckType(t1, t2) => match t1.check(t2) {
                                Ok(_) => CommandResult(Ok(None)),
                                Err(err) => CommandResult(Err(err)),
                            },
                            Command::GetType(t) => match t.infer() {
                                Ok(t) => CommandResult(Ok(Some(t))),
                                Err(err) => CommandResult(Err(err)),
                            },
                            Command::DefineCheckType(_, t1, t2) => match t2.check(t1) {
                                Ok(_) => CommandResult(Ok(None)),
                                Err(err) => CommandResult(Err(err)),
                            },
                        };
                        println!("{}", res);
                    }
                    Err(err) => println!("{}", *err),
                };
            }

            Ok(_) => (),
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                rl_err = Some(err);
                break;
            }
        }
    }
    match rl_err {
        None => Ok(()),
        Some(err) => Err(box err),
    }
}
