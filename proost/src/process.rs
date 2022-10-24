use kernel::{Command, Environment, KernelError, Term};
use parser::*;

fn process_command(command: Command, env: &mut Environment) -> Result<Option<Term>, KernelError> {
    match command {
        Command::Define(s, t1) => match t1.infer(env) {
            Ok(t2) => match env.insert(s, t1, t2) {
                Ok(()) => Ok(None),
                Err(err) => Err(err),
            },
            Err(err) => Err(err),
        },
        Command::CheckType(t1, t2) => match t1.check(&t2, env) {
            Ok(_) => Ok(None),
            Err(err) => Err(err),
        },
        Command::GetType(t) => match t.infer(env) {
            Ok(t) => Ok(Some(t)),
            Err(err) => Err(err),
        },
        Command::DefineCheckType(_, t1, t2) => match t2.check(&t1, env) {
            Ok(_) => Ok(None),
            Err(err) => Err(err),
        },
    }
}

pub fn process_line(line: &str, env: &mut Environment) -> Result<Option<Term>, KernelError> {
    process_command(parse_line(line)?, env)
}

// TODO
// pub fn process_file(file: &str, env: &mut Environment) ->  {
//     parse_file(file)?.map(|command| process_command(command, env))
// }
