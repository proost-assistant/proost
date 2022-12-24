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
    clippy::default_numeric_fallback,
    clippy::else_if_without_else,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::implicit_return,
    clippy::integer_arithmetic,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::missing_trait_methods,
    clippy::mod_module_files,
    clippy::module_name_repetitions,
    clippy::panic_in_result_fn,
    clippy::pattern_type_mismatch,
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
#![warn(clippy::missing_errors_doc, clippy::missing_docs_in_private_items, clippy::self_named_module_files)]
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

mod error;
mod evaluator;
mod rustyline_helper;

use std::env::current_dir;
use std::fs;

use atty::Stream;
use clap::Parser;
use evaluator::Evaluator;
use rustyline::error::ReadlineError;
use rustyline::{Cmd, Config, Editor, EventHandler, KeyCode, KeyEvent, Modifiers};
use rustyline_helper::*;

use crate::error::Result;

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
        return args.files.iter().try_for_each(|path| fs::read_to_string(path).map(|_| ())).map_err(error::Error::from);
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

        println!("Welcome to {} {}", NAME, VERSION);

        loop {
            let readline = rl.readline("\u{00BB} ");
            match readline {
                Ok(line) if is_command(&line) => {
                    rl.add_history_entry(line.as_str());
                    let result = evaluator.process_line(arena, line.as_str());
                    evaluator.display(result);
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

fn is_command(input: &str) -> bool {
    input
        .chars()
        .position(|c| !c.is_whitespace())
        .map(|pos| input.len() < 2 || input[pos..pos + 2] != *"//")
        .unwrap_or_else(|| false)
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
        assert!(!super::is_command("// comment"))
    }

    #[test]
    fn is_command_true() {
        assert!(super::is_command("     check x"));
        assert!(super::is_command("  check x"));
        assert!(super::is_command("check x // comment"))
    }
}
