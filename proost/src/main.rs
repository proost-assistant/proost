use parser::parse_command;
use rustyline::error::ReadlineError;
use rustyline::{Editor, Result};
use std::env;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        //TODO
        let file_path = &args[1];
        println!("Fichier {}", file_path);
        Ok(())
    } else {
        let mut rl = Editor::<()>::new()?;
        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    match parse_command(line.as_str()) {
                        Ok(command) => println!("{}", command),
                        Err(err) => println!("{}", *err),
                    }
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    println!("CTRL-C");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }
        Ok(())
    }
}
