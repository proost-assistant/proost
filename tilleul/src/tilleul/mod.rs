//! [Language Server Protocol] implementation for `Madelaine` language.
//!
//! This crate defines [`Tilleul`] which implements the [`LanguageServer`] trait.
//!
//! [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/

use kernel::memory::arena::Arena;

use crate::lsp::connection::Connection;

pub mod handler;

/// The [`LanguageServer`] implementation for `Madelaine` language.
pub struct Tilleul<'tilleul, 'arena> {
    /// [Memory] of the [`kernel`].
    ///
    /// [Memory]: kernel::memory
    arena: &'tilleul mut Arena<'arena>,

    /// [`Connection`] to the [Language Server Protocol] client.
    ///
    /// [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
    connection: &'tilleul Connection,
}

impl<'tilleul, 'arena> Tilleul<'tilleul, 'arena> {
    /// Creates a new [`Tilleul`] instance.
    pub fn new(arena: &'tilleul mut Arena<'arena>, connection: &'tilleul Connection) -> Self {
        Self { arena, connection }
    }
}
