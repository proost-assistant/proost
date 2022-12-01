#![feature(let_chains)]

mod error;
mod repl;
mod rustyline_helper;

use std::env::current_dir;
use std::fs;

use atty::Stream;
use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::{Cmd, Config, Editor, EventHandler, KeyCode, KeyEvent, Modifiers};
use rustyline_helper::*;

use crate::error::Result;
use repl::Repl;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// some .mdln files
    files: Vec<String>,
    /// remove syntax highlighting
    #[arg(long)]
    no_color: bool,
    /// print the content of imported files
    #[arg(short, long)]
    verbose: bool,
}

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

fn main() -> Result<'static, ()> {
    let args = Args::parse();

    // check if files are provided as command-line arguments
    if !args.files.is_empty() {
        return args
            .files
            .iter()
            .try_for_each(|path| fs::read_to_string(path).map(|_| ()))
            .map_err(error::Error::from);
    }

    // check if we are in a terminal
    if atty::isnt(Stream::Stdout) || atty::isnt(Stream::Stdin) {
        return Ok(());
    }

    let helper = RustyLineHelper::new(!args.no_color);
    let config = Config::builder()
        .completion_type(rustyline::CompletionType::List)
        .build();
    let mut rl = Editor::with_config(config)?;
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
        let current_path = current_dir()?;
        let mut repl = Repl::new(current_path, args.verbose);

        println!("Welcome to {} {}", NAME, VERSION);

        loop {
            let readline = rl.readline("\u{00BB} ");
            match readline {
                Ok(line) if is_command(&line) => {
                    rl.add_history_entry(line.as_str());
                    let result = repl.process_line(arena, line.as_str());
                    repl.display(result);
                }
                Ok(_) => (),
                Err(ReadlineError::Interrupted) => {}
                Err(ReadlineError::Eof) => break,
                Err(err) => return Err(err.into()),
            }
        }
        Ok(())
    })
}

fn is_command(input: &str) -> bool {
    input
        .chars()
        .position(|c| !c.is_whitespace())
        .map(|pos| input[pos..pos + 2] != *"//")
        .unwrap_or_else(|| false)
}
