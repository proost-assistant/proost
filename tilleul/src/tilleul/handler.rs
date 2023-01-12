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

// Convert between parser and lsp_types locations
struct LspLocation(ParserLocation); // Wrapper struct to respect trait consistency

impl TryFrom<LspLocation> for Range {
    type Error = <u32 as TryFrom<usize>>::Error;

    fn try_from(pos: LspLocation) -> Result<Self, Self::Error> {
        let LspLocation(ParserLocation {
            start: ParserPosition {
                line: start_line,
                column: start_column,
            },
            end: ParserPosition {
                line: end_line,
                column: end_column,
            },
        }) = pos;
        Ok(Self {
            start: Position {
                line: u32::try_from(start_line)? - 1,
                character: u32::try_from(start_column)? - 1,
            },
            end: Position {
                line: u32::try_from(end_line)? - 1,
                character: u32::try_from(end_column)? - 1,
            },
        })
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
                let Ok(range) = Range::try_from(LspLocation(location)) else {
                    error!("File too big to represent lines/columns as i32");
                    return;
                };
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
