#![doc = include_str!("../README.md")]

mod language_server;

pub use language_server::AromaLanguageServer;
use tokio::io::{AsyncRead, AsyncWrite};
use tower_lsp::{ClientSocket, LspService, Server};

/// Creates a new lsp service and associated client socket
pub fn new() -> (LspService<AromaLanguageServer>, ClientSocket) {
    LspService::new(|client| AromaLanguageServer::new(client))
}

/// Runs the server, serving the LSP for aroma with the given input and output
pub async fn serve<I: AsyncRead + Unpin, O: AsyncWrite>(input: I, output: O) {
    let (service, socket) = new();
    Server::new(input, output, socket).serve(service).await;
}
