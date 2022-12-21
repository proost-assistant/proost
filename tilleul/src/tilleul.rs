use kernel::term::arena::Arena;
use log::info;
use lsp_types::notification::PublishDiagnostics;
use lsp_types::*;
use parser::parse_line;

use crate::server::connection::Connection;
use crate::server::payload::message::Message;
use crate::server::payload::notification::Notification;
use crate::server::LanguageServerBackend;

pub struct Tilleul<'a, 'arena> {
    arena: &'a mut Arena<'arena>,
    connection: &'a Connection,
}

impl<'a, 'arena> Tilleul<'a, 'arena> {
    pub fn new(arena: &'a mut Arena<'arena>, connection: &'a Connection) -> Self {
        Self { arena, connection }
    }
}

impl LanguageServerBackend for Tilleul<'_, '_> {
    fn initialize(&self, _: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: crate::NAME.to_string(),
                version: Some(crate::VERSION.to_string()),
            }),
        }
    }

    fn did_open_text_document(&self, params: DidOpenTextDocumentParams) {
        for line in params.text_document.text.lines() {
            let command = parse_line(line);

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

                self.connection.send(Message::Notification(Notification::new::<PublishDiagnostics>(PublishDiagnosticsParams {
                    uri: params.text_document.uri.clone(),
                    diagnostics: vec![diagnostic],
                    version: None,
                })));
            };
        }
    }
}
