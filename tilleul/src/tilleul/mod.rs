//! [Language Server Protocol] implementation for `Madelaine` language.
//!
//! This crate defines [`Tilleul`] which implements the [`LanguageServer`] trait.
//!
//! [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/

use kernel::memory::arena::Arena;

use crate::lsp::connection;

pub mod handler;

/// The [`LanguageServer`] implementation for `Madelaine` language.
#[allow(dead_code)]
pub struct Tilleul<'tilleul, 'arena, C: connection::LanguageServer> {
    /// [Memory] of the [`kernel`].
    ///
    /// [Memory]: kernel::memory
    arena: &'tilleul mut Arena<'arena>,

    /// [`Connection`] to the [Language Server Protocol] client.
    ///
    /// [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
    connection: &'tilleul C,
}

impl<'tilleul, 'arena, C: connection::LanguageServer> Tilleul<'tilleul, 'arena, C> {
    /// Creates a new [`Tilleul`] instance.
    pub fn new(arena: &'tilleul mut Arena<'arena>, connection: &'tilleul C) -> Self {
        Self { arena, connection }
    }
}