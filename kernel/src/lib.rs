//! TODO: Make a documentation (#15)

#![feature(box_patterns)]
#![feature(box_syntax)]

mod command;
mod environment;
mod error;
mod term;
mod type_checker;
mod universe;
mod declaration;

pub use derive_more;
pub use num_bigint;

pub use command::Command;
pub use environment::Environment;
pub use error::{KernelError, Loc};
pub use term::Term;
