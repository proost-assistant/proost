#![feature(box_syntax)]

mod process;

use crate::process::process_command;
use clap::Parser;
use kernel::Environment;
use parser::{parse_command, parse_file};
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

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if !args.files.is_empty() {
        for path in args.files.iter() {
            match fs::read_to_string(path) {
                Ok(contents) => {
                    let _ = parse_file(&contents);
                }
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

                match parse_command(line.as_str()) {
                    Ok(command) => {
                        println!("{}", process_command(command, &mut env));
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
