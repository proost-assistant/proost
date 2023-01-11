#![doc(html_logo_url = "https://gitlab.crans.org/loutr/proost/-/raw/main/docs/media/logo.png")]

//! Utilities defining, in a uniformal way, errors and location of elements.

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
    clippy::pattern_type_mismatch,
    clippy::separated_literal_suffix,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
    clippy::unreachable,
    clippy::wildcard_enum_match_arm
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
pub mod location;
