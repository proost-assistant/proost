#[macro_use]
extern crate pest_derive;

mod parser;
pub use parser::{parse_term, parse_command, parse_file};

