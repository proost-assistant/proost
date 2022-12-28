#![doc(html_logo_url = "https://gitlab.crans.org/loutr/proost/-/raw/main/docs/media/logo.png")]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(let_chains)]
#![deny(
    clippy::complexity,
    clippy::correctness,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::restriction,
    clippy::style,
    clippy::suspicious
)]
#![allow(
    clippy::arithmetic_side_effects,
    clippy::blanket_clippy_restriction_lints,
    clippy::else_if_without_else,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::implicit_return,
    clippy::integer_arithmetic,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::missing_trait_methods,
    clippy::mod_module_files,
    clippy::panic_in_result_fn,
    clippy::pattern_type_mismatch,
    clippy::separated_literal_suffix,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
    clippy::unreachable,
    clippy::wildcard_enum_match_arm,
    // Due to clap dependency
    clippy::std_instead_of_core,
    // Due to this crate is a binary manipulating string
    clippy::indexing_slicing,
    clippy::print_stdout,
    clippy::string_slice
)]
#![warn(clippy::missing_docs_in_private_items)]
#![cfg_attr(
    test,
    allow(
        clippy::assertions_on_result_states,
        clippy::enum_glob_use,
        clippy::indexing_slicing,
        clippy::non_ascii_literal,
        clippy::too_many_lines,
        clippy::unwrap_used,
        clippy::wildcard_imports,
    )
)]

extern crate alloc;

mod error;
mod evaluator;
mod rustyline_helper;

use std::env::current_dir;
use std::fs;

use atty::Stream;
use clap::Parser;
use colored::Colorize;
use evaluator::Evaluator;
use kernel::memory::term::Term;
use rustyline::error::ReadlineError;
use rustyline::{Cmd, Config, Editor, EventHandler, KeyCode, KeyEvent, Modifiers};
use rustyline_helper::{RustyLineHelper, TabEventHandler};

use crate::error::{Error, Result};

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

fn main() -> Result<'static, 'static, ()> {
    let args = Args::parse();

    // check if files are provided as command-line arguments
    if !args.files.is_empty() {
        return args
            .files
            .iter()
            .try_for_each(|path| fs::read_to_string(path).map(|_| ()))
            .map_err(Error::from);
    }

    // check if we are in a terminal
    if atty::isnt(Stream::Stdout) || atty::isnt(Stream::Stdin) {
        return Ok(());
    }

    let helper = RustyLineHelper::new(!args.no_color);
    let config = Config::builder().completion_type(rustyline::CompletionType::List).build();
    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(helper));
    rl.bind_sequence(KeyEvent::from('\t'), EventHandler::Conditional(Box::new(TabEventHandler)));
    rl.bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), EventHandler::Simple(Cmd::Newline));

    kernel::memory::arena::use_arena(|arena| {
        let current_path = current_dir()?;
        let mut evaluator = Evaluator::new(current_path, args.verbose);

        println!("Welcome to {NAME} {VERSION}");

        loop {
            let readline = rl.readline("\u{00BB} ");
            match readline {
                Ok(line) if is_command(&line) => {
                    rl.add_history_entry(line.as_str());

                    display(evaluator.process_line(arena, line.as_str()));
                },
                Ok(_) => (),
                Err(ReadlineError::Interrupted) => {},
                Err(ReadlineError::Eof) => break,
                Err(err) => return Err(err.into()),
            }
        }

        Ok(())
    })
}

pub fn display<'arena>(res: Result<'arena, '_, Option<Term<'arena>>>) {
    match res {
        Ok(None) => println!("{}", "\u{2713}".green()),

        Ok(Some(t)) => {
            for line in t.to_string().lines() {
                println!("{} {line}", "\u{2713}".green());
            }
        },

        Err(err) => {
            let location = match err {
                Error::Kernel(ref builder, ref err) => Some(builder.apply_trace(&err.trace)),
                Error::Parser(ref err) => Some(err.loc),

                _ => None,
            };

            if let Some(loc) = location {
                let indicator = if loc.start.column == loc.end.column {
                    format!("{:0w1$}^", "", w1 = loc.start.column - 1)
                } else {
                    format!("{:0w1$}^{:-<w2$}^", "", "", w1 = loc.start.column - 1, w2 = loc.end.column - loc.start.column - 1)
                };

                println!("{} {indicator}", "\u{2717}".red());
            };

            println!("{} {err}", "\u{2717}".red());
        },
    }
}

fn is_command(input: &str) -> bool {
    input
        .chars()
        .position(|c| !c.is_whitespace())
        .map_or(false, |pos| input.len() < 2 || input[pos..pos + 2] != *"//")
}

#[cfg(test)]
mod tests {

    #[test]
    fn is_command_no_crash() {
        assert!(!super::is_command(""));
        assert!(super::is_command("a"));
        assert!(super::is_command("aa"));
        assert!(super::is_command("aaa"));
        assert!(super::is_command("aaaa"));
    }

    #[test]
    fn is_command_false() {
        assert!(!super::is_command("    "));
        assert!(!super::is_command(" "));
        assert!(!super::is_command("// comment"));
    }

    #[test]
    fn is_command_true() {
        assert!(super::is_command("     check x"));
        assert!(super::is_command("  check x"));
        assert!(super::is_command("check x // comment"));
    }
}
