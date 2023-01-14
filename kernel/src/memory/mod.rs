//! Abstracted memory manipulation primitives.
//!
//! This module provides a paradigm for building and manipulating [terms](term::Term) in the
//! calculus of construction, centered around the notion of [arena](`arena::Arena`). Terms also
//! rely on other structures like [declarations](declaration::Declaration) and [universe
//! levels](level::Level).

pub mod arena;
pub mod declaration;
pub mod level;
pub mod term;
