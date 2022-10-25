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
                KernelError::CannotParse(pos, message) => {
                    format!("{:0w$}^\n{m}", "", w = pos.column - 1, m = message)
                }
                _ => err.to_string(),
            };
            for line in string.lines() {
                println!("{} {}", "\u{2717}".red(), line)
            }
        }
    }
}
