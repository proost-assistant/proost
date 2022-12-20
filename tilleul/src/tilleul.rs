use std::cell::OnceCell;
use std::path::PathBuf;

use lsp_types::*;

use crate::server::LanguageServerBackend;

#[derive(Default, Debug)]
pub struct Tilleul {
    file_path: OnceCell<PathBuf>,
}

impl LanguageServerBackend for Tilleul {
    fn initialize(&self, params: InitializeParams) -> InitializeResult {
        // TODO: Improve
        if let Some(root_uri) = params.root_uri {
            self.file_path.set(root_uri.to_file_path().unwrap()).unwrap();
        }

        InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: crate::NAME.to_string(),
                version: Some(crate::VERSION.to_string()),
            }),
        }
    }
}
