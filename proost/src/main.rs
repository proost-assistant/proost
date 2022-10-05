use parser::{parse_command, parse_file};
use rustyline::error::ReadlineError;
use rustyline::{Editor, Result};
use std::{env, fs};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let file_path = &args[1];
        let contents: &str =
            &fs::read_to_string(file_path).expect("Should have been able to read the file");
        // TODO
        // unsatisfactory behavior
        let _ = parse_file(contents);
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
