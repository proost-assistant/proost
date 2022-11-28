#![feature(let_chains)]

mod command_processor;
mod error;
mod rustyline_helper;

use std::fs;

use atty::Stream;
use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::{Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers, Result};
use rustyline_helper::*;

use command_processor::{print_repl, Processor};

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

    // check if files are provided as command-line arguments
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

    kernel::term::arena::use_arena(|arena| {
        let processor = Processor::new();

        println!("Welcome to {} {}", NAME, VERSION);

        loop {
            let readline = rl.readline("\u{00BB} ");
            match readline {
                Ok(line) if is_command(&line) => {
                    rl.add_history_entry(line.as_str());
                    print_repl(processor.process_line(line.as_str(), arena));
                }
                Ok(_) => (),
                Err(ReadlineError::Interrupted) => {}
                Err(ReadlineError::Eof) => break,
                Err(err) => return Err(err),
            }
        }
        Ok(())
    })
}

fn is_command(input: &String) -> bool {
    input
        .chars()
        .position(|c| !c.is_whitespace())
        .map(|pos| input[pos..pos + 2] != *"//")
        .unwrap_or_else(|| false)
}
