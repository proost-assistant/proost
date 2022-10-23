use colored::*;
use kernel::{Command, Environment, KernelError, Term};
use std::fmt;

pub struct CommandResult(Result<Option<Term>, KernelError>);

impl fmt::Display for CommandResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Ok(Some(t)) => write!(f, "{} {}", "\u{2713}".green(), t),
            Ok(None) => write!(f, "{}", "\u{2713}".green()),
            Err(err) => write!(f, "{} {}", "\u{2717}".red(), err),
        }
    }
}

pub fn process_command(command: Command, env: &mut Environment) -> CommandResult {
    match command {
        Command::Define(s, t1) => match t1.infer(env) {
            Ok(t2) => match env.insert(s, t1, t2) {
                Ok(()) => CommandResult(Ok(None)),
                Err(err) => CommandResult(Err(err)),
            },
            Err(err) => CommandResult(Err(err)),
        },
        Command::CheckType(t1, t2) => match t1.check(&t2, env) {
            Ok(_) => CommandResult(Ok(None)),
            Err(err) => CommandResult(Err(err)),
        },
        Command::GetType(t) => match t.infer(env) {
            Ok(t) => CommandResult(Ok(Some(t))),
            Err(err) => CommandResult(Err(err)),
        },
        Command::DefineCheckType(_, t1, t2) => match t2.check(&t1, env) {
            Ok(_) => CommandResult(Ok(None)),
            Err(err) => CommandResult(Err(err)),
        },
    }
}
