#![feature(box_syntax)]

use clap::Parser;
use parser::{parse_command, parse_file};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;
use std::fs;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    files: Vec<String>,
    #[arg(short, long, action)]
    banner: bool,
}

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
                    println!("Error: No such file or directory: {}", path);
                }
            }
        }
        return Ok(());
    }

    let mut rl_err: Option<ReadlineError> = None;
    let mut rl = Editor::<()>::new()?;

    if args.banner {
        println!("#Insert banner here#\n#  This is a test  #")
    }
    println!("Welcome to {} {}", NAME, VERSION);

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match parse_command(line.as_str()) {
                    Ok(command) => println!("{}", command),
                    Err(err) => println!("{}", *err),
                }
            }
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
