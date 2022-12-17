#![doc(html_logo_url = "https://gitlab.crans.org/loutr/proost/-/raw/main/docs/media/logo.png")]

//! A kernel for the calculus of constructions.
//!
//! Terms can be built with functions from the [`memory::term`] module. The [`calculus`] module
//! provides essential manipulation functions from lambda-calculus, while the [`type_checker`]
//! module provides typed interactions.

#![feature(no_coverage)]
#![feature(once_cell)]
#![feature(trait_alias)]
#![feature(if_let_guard)]
#![deny(
    clippy::complexity,
    clippy::correctness,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::restriction,
    clippy::style,
    clippy::suspicious
)]
#![allow(
    clippy::blanket_clippy_restriction_lints,
    clippy::else_if_without_else,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::implicit_return,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::missing_trait_methods,
    clippy::mod_module_files,
    clippy::panic_in_result_fn,
    clippy::separated_literal_suffix,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
    clippy::unreachable,
    clippy::wildcard_enum_match_arm
)]
#![warn(
    clippy::arithmetic_side_effects,
    clippy::integer_arithmetic,
    clippy::missing_errors_doc,
    clippy::missing_docs_in_private_items
)]
#![cfg_attr(
    test,
    allow(
        clippy::assertions_on_result_states,
        clippy::enum_glob_use,
        clippy::indexing_slicing,
        clippy::non_ascii_literal,
        clippy::too_many_lines,
        clippy::unwrap_used,
        clippy::wildcard_imports,
    )
)]

pub mod calculus;
pub mod error;
pub mod memory;
pub mod type_checker;
