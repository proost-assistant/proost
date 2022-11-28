use colored::Colorize;

use crate::error::{Error::*, Result};
use kernel::term::arena::{Arena, Term};
use parser::parse_line;

pub fn process_line<'arena>(
    line: &str,
    arena: &mut Arena<'arena>,
) -> Result<'arena, Option<Term<'arena>>> {
    Ok(parse_line(arena, line)?.process(arena)?)
}

pub fn print_repl<'arena>(res: Result<'arena, Option<Term<'arena>>>) {
    match res {
        Ok(None) => println!("{}", "\u{2713}".green()),
        Ok(Some(t)) => {
            for line in t.to_string().lines() {
                println!("{} {}", "\u{2713}".green(), line)
            }
        }
        Err(err) => {
            let string = match err {
                Parser(parser::error::Error {
                    kind: parser::error::ErrorKind::EarlyKernelError(err),
                    ..
                }) => err.to_string(),

                Parser(parser::error::Error {
                    kind: parser::error::ErrorKind::CannotParse(message),
                    location: loc,
                }) => {
                    if loc.start.column == loc.end.column {
                        format!("{:0w1$}^\n{m}", "", w1 = loc.start.column - 1, m = message)
                    } else {
                        format!(
                            "{:0w1$}^{:-<w2$}^\n{m}",
                            "",
                            "",
                            w1 = loc.start.column - 1,
                            w2 = loc.end.column - loc.start.column - 1,
                            m = message
                        )
                    }
                }

                _ => err.to_string(),
            };

            for line in string.lines() {
                println!("{} {}", "\u{2717}".red(), line)
            }
        }
    }
}
