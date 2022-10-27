use colored::*;
use kernel::{Environment, KernelError, Term};
use parser::*;

pub fn process_line(line: &str, env: &mut Environment) -> Result<Option<Term>, KernelError> {
    parse_line(line)?.process(env)
}

pub fn print_repl(res: Result<Option<Term>, KernelError>) {
    match res {
        Ok(None) => println!("{}", "\u{2713}".green()),
        Ok(Some(t)) => {
            for line in t.to_string().lines() {
                println!("{} {}", "\u{2713}".green(), line)
            }
        }
        Err(err) => {
            let string = match err {
                KernelError::CannotParse(loc, message) => {
                    if loc.column1 == loc.column2 {
                        format!("{:0w1$}^\n{m}", "", w1 = loc.column1 - 1, m = message)
                    } else {
                        format!(
                            "{:0w1$}^{:-<w2$}^\n{m}",
                            "",
                            "",
                            w1 = loc.column1 - 1,
                            w2 = loc.column2 - loc.column1 - 1,
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
