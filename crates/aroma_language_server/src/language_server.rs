//! aroma language server implementation

use async_trait::async_trait;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{info, instrument};

/// Aroma language server implementation
#[derive(Debug)]
pub struct AromaLanguageServer {
    client: Client
}

impl AromaLanguageServer {
    /// Create a new aroma language server with the given client
    pub fn new(client: Client) -> Self {
        Self { client }
    }
}

#[async_trait]
impl LanguageServer for AromaLanguageServer {
    #[instrument]
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        info!("initializing server...");
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                ..ServerCapabilities::default()
            },
            ..InitializeResult::default()
        })
    }



    #[instrument]
    async fn shutdown(&self) -> Result<()> {
        info!("shutting down server...");
        Ok(())
    }
}
