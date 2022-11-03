use colored::Colorize;
use kernel::{Environment, Term};
use parser::parse_line;

pub fn process_line(line: &str, env: &mut Environment) -> anyhow::Result<Option<Term>> {
    Ok(parse_line(line)?.process(env)?)
}

pub fn print_repl(res: anyhow::Result<Option<Term>>) {
    match res {
        Ok(None) => println!("{}", "\u{2713}".green()),
        Ok(Some(t)) => {
            for line in t.to_string().lines() {
                println!("{} {}", "\u{2713}".green(), line)
            }
        }
        Err(err) => {
            let string = match err.downcast_ref::<parser::Error>() {
                Some(parser::Error {
                    kind: parser::ErrorKind::CannotParse(message),
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
