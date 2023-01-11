//! Proost, a small proof assistant written in Rust.
//!
//! `proost` denotes the toplevel executable. Please refer to the manual for detailed usage
//! instructions.

#![doc(html_logo_url = "https://gitlab.crans.org/loutr/proost/-/raw/main/docs/media/logo.png")]
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
    // Allowed because of the `clap` crate
    clippy::std_instead_of_core,
    // Allowed because this crate is a binary manipulating string
    clippy::indexing_slicing,
    clippy::print_stdout,
    clippy::string_slice
)]
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

use std::cmp::max;
use std::env::current_dir;

use atty::Stream;
use clap::Parser;
use colored::Colorize;
use evaluator::Evaluator;
use parser::command::{self, Command};
use rustyline::error::ReadlineError;
use rustyline::{Cmd, Config, Editor, EventHandler, KeyCode, KeyEvent, Modifiers};
use rustyline_helper::{RustyLineHelper, TabEventHandler};
use utils::location::Location;

use crate::error::{Error, Result, ResultProcess};

/// Command line arguments, interpreted with `clap`.
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

/// The version of the program
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The name of the program
const NAME: &str = env!("CARGO_PKG_NAME");

fn main() -> Result<'static, 'static, ()> {
    let args = Args::parse();

    let current_path = current_dir()?;
    let mut evaluator = Evaluator::new(current_path, args.verbose);

    // check if files are provided as command-line arguments
    if !args.files.is_empty() {
        return kernel::memory::arena::use_arena(|arena| {
            let command = Command::Import(args.files.iter().map(|file| (Location::default(), file.as_str())).collect());

            display(evaluator.process_line(arena, &command), false);
            Ok(())
        });
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

    kernel::memory::arena::use_arena_with_axioms(|arena| {
        println!("Welcome to {NAME} {VERSION}");

        loop {
            let readline = rl.readline("\u{00BB} ");
            match readline {
                Ok(line) if is_command(&line) => {
                    rl.add_history_entry(line.as_str());

                    match command::parse::line(line.as_str()) {
                        Ok(command) => display(evaluator.process_line(arena, &command), true),
                        Err(err) => display(Err(Error::Parser(err)), true),
                    }
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

/// Toplevel function to display a result, as yielded by the toplevel processing of a command
///
/// The `toggle_location` indicates whether or not to display a hint for the location of the error
pub fn display(res: ResultProcess, toggle_location: bool) {
    match res {
        Ok(None) => println!("{}", "\u{2713}".green()),

        Ok(Some(t)) => {
            for line in t.to_string().lines() {
                println!("{} {line}", "\u{2713}".green());
            }
        },

        Err(err) => {
            let location = match err {
                Error::Kernel(builder, ref err) => Some(builder.apply_trace(&err.trace)),
                Error::Parser(ref err) => Some(err.location),

                Error::TopLevel(evaluator::Error {
                    kind: evaluator::ErrorKind::FileError(_),
                    ..
                }) => None,
                Error::TopLevel(ref err) => Some(err.location),

                _ => None,
            };

            if toggle_location && let Some(loc) = location {
                println!("{} {}", "\u{2717}".red(), pretty_print_loc(loc));
            };

            println!("{} {err}", "\u{2717}".red());
        },
    }
}

/// Pretty print a location as underscores
fn pretty_print_loc(loc: Location) -> String {
    if loc.start.line == loc.end.line {
        if loc.start.column + 1 >= loc.end.column {
            format!("{:0w$}^", "", w = loc.start.column - 1)
        } else {
            format!("{:0w1$}^{:-<w2$}^", "", "", w1 = loc.start.column - 1, w2 = loc.end.column - loc.start.column - 2)
        }
    } else {
        format!(" {:-<w$}^", "", w = max(loc.start.column, loc.end.column) - 1)
    }
}

/// Tests whether the string corresponds to a command (here, not a comment)
fn is_command(input: &str) -> bool {
    input
        .chars()
        .position(|c| !c.is_whitespace())
        .map_or(false, |pos| input.len() < 2 || input[pos..pos + 2] != *"//")
}

#[cfg(test)]
mod tests {
    use utils::location::Location;

    use crate::pretty_print_loc;

    #[test]
    fn correct_pretty_print_loc() {
        assert_eq!(pretty_print_loc(Location::new((1, 3), (1, 3))), "  ^".to_string());
        assert_eq!(pretty_print_loc(Location::new((1, 3), (1, 4))), "  ^".to_string());
        assert_eq!(pretty_print_loc(Location::new((1, 3), (1, 5))), "  ^^".to_string());
        assert_eq!(pretty_print_loc(Location::new((1, 3), (1, 6))), "  ^-^".to_string());
        assert_eq!(pretty_print_loc(Location::new((1, 3), (1, 7))), "  ^--^".to_string());
    }

    /// Robustness against multilines
    #[test]
    fn robust_pretty_print_loc() {
        pretty_print_loc(Location::new((2, 3), (2, 3)));
        pretty_print_loc(Location::new((1, 3), (2, 3)));
        pretty_print_loc(Location::new((1, 3), (2, 1)));
    }

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
