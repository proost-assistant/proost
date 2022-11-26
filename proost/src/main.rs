#![feature(box_syntax)]
#![feature(let_chains)]

mod process;
mod rustyline_helper;

use std::fs;

use atty::Stream;
use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::{Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers, Result};
use rustyline_helper::*;

use kernel::Environment;
use process::*;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// some .mdln files
    files: Vec<String>,
    /// remove syntax highlighting
    #[arg(long)]
    no_color: bool,
}

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

fn main() -> Result<()> {
    let args = Args::parse();

    // check if files are inputed
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

    // check if we are in a terminal
    if atty::isnt(Stream::Stdout) || atty::isnt(Stream::Stdin) {
        return Ok(());
    }

    let helper = RustyLineHelper::new(!args.no_color);
    let mut rl = Editor::<RustyLineHelper>::new()?;
    rl.set_helper(Some(helper));
    rl.bind_sequence(
        KeyEvent::from('\t'),
        EventHandler::Conditional(Box::new(TabEventHandler)),
    );
    rl.bind_sequence(
        KeyEvent(KeyCode::Enter, Modifiers::ALT),
        EventHandler::Simple(Cmd::Newline),
    );
    println!("Welcome to {} {}", NAME, VERSION);

    let mut env = Environment::new();

    loop {
        let readline = rl.readline("\u{00BB} ");
        match readline {
            Ok(line) if !line.is_empty() => {
                rl.add_history_entry(line.as_str());
                print_repl(process_line(line.as_str(), &mut env));
            }
            Ok(_) => (),
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => break,
            Err(err) => return Err(err),
        }
    }
    Ok(())
}
