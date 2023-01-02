//! Handlers for [`Request`]s or [`Notification`]s that can be received by the client.
//!
//! [`Notification`]: crate::lsp::message::Notification
//! [`Request`]: crate::lsp::message::Request

#![allow(clippy::wildcard_imports)]

use lsp_types::notification::PublishDiagnostics;
use lsp_types::*;
use parser::command::parse;

use super::Tilleul;
use crate::lsp::message::notification::Notification;
use crate::lsp::message::Message;
use crate::lsp::LanguageServer;

impl LanguageServer for Tilleul<'_, '_> {
    fn initialize(&mut self, _: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: crate::NAME.to_owned(),
                version: Some(crate::VERSION.to_owned()),
            }),
        }
    }

    fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        for line in params.text_document.text.lines() {
            let command = parse::line(line);

            // TODO: Handle errors correctly
            if let Err(err) = command {
                let diagnostic = Diagnostic::new_simple(
                    Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    },
                    err.to_string(),
                );

                self.connection
                    .send(Message::Notification(Notification::new::<PublishDiagnostics>(PublishDiagnosticsParams {
                        uri: params.text_document.uri.clone(),
                        diagnostics: vec![diagnostic],
                        version: None,
                    })));
            };
        }
    }
}
