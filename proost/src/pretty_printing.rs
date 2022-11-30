use colored::Colorize;

use kernel::term::arena::Term;

use crate::error::{Error::*, Result};

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
