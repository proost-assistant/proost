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
/// [notifications]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
/// [specification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#lifeCycleMessages
/// [requests]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#requestMessage
/// [`Server`]: server::Server
#[allow(unused_variables)]
#[cfg_attr(test, mockall::automock)]
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
        info!("initialized not handled");
    }

    /// [`shutdown`] request.
    ///
    /// This method is guaranteed to be called only once.
    ///
    /// [`shutdown`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#shutdown
    fn shutdown(&mut self, params: ()) {
        info!("shutdown not handled");
    }

    /// [`textDocument/didOpen`] notification.
    ///
    /// [`textDocument/didOpen`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didOpen
    fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        info!("textDocument/didOpen not handled");
    }

    /// [`textDocument/didChange`] notification.
    ///
    /// [`textDocument/didChange`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
    fn text_document_did_change(&mut self, params: DidChangeTextDocumentParams) {
        info!("textDocument/didChange not handled");
    }

    /// [`textDocument/didClose`] notification.
    ///
    /// [`textDocument/didClose`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didClose
    fn text_document_did_close(&mut self, params: DidCloseTextDocumentParams) {
        info!("textDocument/didClose not handled");
    }
}
