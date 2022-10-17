#![feature(box_syntax)]

use clap::Parser;
use parser::parse_commands;
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
                    let _ = parse_commands(&contents);
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
            Ok(line) if !line.is_empty() => {
                rl.add_history_entry(line.as_str());
                match parse_commands(line.as_str()) {
                    Ok(commands) => {
                        for command in commands {
                            println!("{}", command);
                        }
                    }
                    Err(err) => println!("{}", *err),
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
