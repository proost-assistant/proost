//! Handlers for [`Request`]s or [`Notification`]s that can be received by the client.
//!
//! [`Notification`]: crate::lsp::message::notification::Notification
//! [`Request`]: crate::lsp::message::request::Request
//!
//! Currently, this checks the syntax continuously, and runs the
//! Proost checker whenever the file is saved.

#![allow(clippy::wildcard_imports)]
use elaboration::location::{Location as ParserLocation, Position as ParserPosition};
use lsp_types::notification::PublishDiagnostics;
use lsp_types::*;
use parser::command::parse::file as parse_file;
use parser::error::{Error as ParserError, Kind as ParserErrorKind};

use super::Tilleul;
use crate::lsp::message::notification::Notification;
use crate::lsp::{connection, LanguageServer};

/// Convert between parser and `lsp_types` position types.
#[allow(clippy::unwrap_used)]
fn parser_position2lsp(pos: ParserPosition) -> Position {
    let ParserPosition { line, column } = pos;
    // parser uses usize while lsp_types uses u32.
    Position {
        line: u32::try_from(line).unwrap() - 1,
        character: u32::try_from(column).unwrap() - 1,
    }
}

/// Convert between parser and `lsp_types` location types.
fn parser_location2lsp(loc: ParserLocation) -> Range {
    let ParserLocation { start, end } = loc;
    Range {
        start: parser_position2lsp(start),
        end: parser_position2lsp(end),
    }
}

impl<C: connection::LanguageServer> Tilleul<'_, '_, C> {
    /// Use the parser to perform a syntax check on the file.
    fn check_syntax_and_publish(&mut self, new_content: &str, uri: Url, version: i32) {
        let res = parse_file(new_content);
        let diagnostics = match res {
            Err(ParserError { kind, location }) => {
                let ParserErrorKind::CannotParse(msg) = kind else {
                    error!("Unknown error kind: {}", kind);
                    return;
                };
                let range = parser_location2lsp(location);
                // One single diagnostic here since the parser stops
                // on the first error.
                let diagnostic = Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some(String::from("parser")),
                    message: msg,
                    ..Default::default()
                };
                vec![diagnostic]
            },
            Ok(_commands) => {
                // We don't do any real checking here, only a syntax check.
                vec![]
            },
        };
        let params = PublishDiagnosticsParams {
            uri,
            version: Some(version),
            diagnostics,
        };
        let notif = Notification::new::<PublishDiagnostics>(params);
        self.connection.send(notif);
    }

    //    fn check_file_and_publish(&mut self, _filename: PathBuf) {}
}

impl<C: connection::LanguageServer> LanguageServer for Tilleul<'_, '_, C> {
    fn initialize(&mut self, _params: InitializeParams) -> InitializeResult {
        info!("Initializing Tilleul...");
        InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                    open_close: Some(true), // TODO: useful?
                    // Send the whole document on each edit, no incremental changes
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: Some(false),            // Not needed
                    will_save_wait_until: Some(false), // Not needed
                    // Notify whenever the document is saved
                    save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                })),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: crate::NAME.to_owned(),
                version: Some(crate::VERSION.to_owned()),
            }),
        }
    }

    fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        info!("Handling open in Tilleul...");
        let doc = params.text_document;
        self.check_syntax_and_publish(&doc.text, doc.uri, doc.version);
    }

    fn text_document_did_change(&mut self, params: DidChangeTextDocumentParams) {
        info!("Handling change in Tilleul...");
        let doc = params.text_document;
        let [ref change] = params.content_changes[..] else {
            error!("Expected only one content change, as we are using full synchronization");
            return;
        };
        let TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            ref text
        } = *change else
        {
            error!("Expected no range or range length, as we are using full synchronization");
            return;
        };
        self.check_syntax_and_publish(text, doc.uri, doc.version);
    }

    fn text_document_did_save(&mut self, _params: DidSaveTextDocumentParams) {
        info!("Handling save in Tilleul...");
    }
}
