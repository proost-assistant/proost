#![doc(
    html_logo_url = "https://gitlab.crans.org/loutr/proost/-/raw/48-first-release-preparations/docs/media/logo.png"
)]

//! A kernel for the calculus of constructions.
//!
//! Terms can be built with functions from the [`term`] module. This module also provides essential
//! manipulation functions from lambda-calculus, while the [`type_checker`] module provides typed
//! interactions.

#![feature(no_coverage)]
#![feature(once_cell)]
#![feature(trait_alias)]

pub mod error;
pub mod location;
pub mod term;
pub mod type_checker;
