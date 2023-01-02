//! [Language Server Protocol] server wrapper implementation for Rust.
//!
//! This crate defines a [`Server`], communicating through a [`Connection`] to a [Language Server Protocol] client.
//! The [`Server`] uses a user-defined backend server implementing [`LanguageServer`] which defines the behavior of the server.
//!
//! [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
//! [`Connection`]: connection::Connection
//! [`Server`]: server::Server

#![allow(clippy::wildcard_imports)]

pub mod connection;
pub mod dispatcher;
pub mod message;
pub mod server;

use log::info;
use lsp_types::*;

/// A trait for a [Language Server Protocol] backend.
///
/// This trait defines the behavior of the [`Server`], according to [request] or [notification] according to the [specification].
/// The [`Server`] will call the appropriate method of the [`LanguageServer`] when a request or notification is received.
///
/// An example implementation of this trait can be found in the [`crate::tilleul`] directory.
///
/// [Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
/// [notifications]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage
/// [specification]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages
/// [requests]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage
/// [`Server`]: server::Server
pub trait LanguageServer {
    /// [`initialize`] request.
    ///
    /// This method is guaranteed to be called only once.
    ///
    /// [`initialize`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    fn initialize(&mut self, params: InitializeParams) -> InitializeResult;

    /// [`initialized`] notification.
    ///
    /// [`initialized`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialized
    fn initialized(&mut self, params: InitializedParams) {
        let _ = params;
        info!("initialized not handled");
    }

    /// [`textDocument/didOpen`] notification.
    ///
    /// [`textDocument/didOpen`]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
    fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        let _ = params;
        info!("textDocument/didOpen not handled");
    }
}
