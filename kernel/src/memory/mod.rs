//! Abstracted memory manipulation primitives
//!
//! This module provides a paradigm for building and manipulating terms in the calculus of
//! construction, centered around the notion of [arena](`arena::Arena`). Terms also rely on other
//! structures like

pub mod arena;
pub mod builders;
pub mod levelBuilders;
pub mod declaration;
pub mod level;
pub mod term;

