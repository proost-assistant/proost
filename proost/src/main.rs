#![feature(box_syntax)]

use clap::Parser;
use colored::*;
use kernel::{Environment, KernelError, Term};
use parser::*;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;
use std::fs;

// clap configuration
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    files: Vec<String>,
}

// constants fetching
const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

// usefull functions
fn process_line(line: &str, env: &mut Environment) -> Result<Option<Term>, KernelError> {
    parse_line(line)?.process(env)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if !args.files.is_empty() {
        for path in args.files.iter() {
            match fs::read_to_string(path) {
                Ok(_contents) => (),
                Err(_) => {
                    println!("no such file or directory: {}", path);
                }
            }
        }
        return Ok(());
    }

    let mut rl_err: Option<ReadlineError> = None;
    let mut rl = Editor::<()>::new()?;
    println!("Welcome to {} {}", NAME, VERSION);

    let mut env = Environment::new();

    loop {
        let readline = rl.readline("\u{00BB} ");
        match readline {
            Ok(line) if !line.is_empty() => {
                rl.add_history_entry(line.as_str());
                match process_line(line.as_str(), &mut env) {
                    Ok(Some(t)) => {
                        for line in t.to_string().lines() {
                            println!("{} {}", "\u{2713}".green(), line)
                        }
                    }
                    Ok(None) => println!("{}", "\u{2713}".green()),
                    Err(err) => {
                        for line in err.to_string().lines() {
                            println!("{} {}", "\u{2717}".red(), line)
                        }
                    }
                }
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
