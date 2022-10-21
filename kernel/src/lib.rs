//! TODO: Make a documentation (#15)

#![feature(box_patterns)]
#![feature(box_syntax)]

mod command;
mod environment;
mod term;
mod type_checker;

pub use command::Command;
pub use derive_more;
pub use num_bigint;
pub use term::Term;
pub use type_checker::TypeCheckingError;
