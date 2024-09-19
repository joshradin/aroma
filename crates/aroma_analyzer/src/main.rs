use aroma_cli_common::LoggingArgs;
use aroma_language_server::serve;
use clap::Parser;
use tokio::net::{TcpListener, TcpStream};
use tracing::trace;
use tracing_error::ErrorLayer;
use tracing_subscriber::fmt::format;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::{Layer, Registry};

#[derive(Debug, Parser)]
struct Args {
    #[command(flatten)]
    logging: LoggingArgs,
    /// If this should listen for an incoming client
    #[arg(long)]
    listen: bool,
    /// The port to connect to/with
    #[clap(long, default_value_t = 9257)]
    port: u16
}

const LSP_ADDR: &'static str = "127.0.0.1";

#[tokio::main]
async fn main() -> eyre::Result<()> {
    color_eyre::install()?;

    let args = Args::parse();
    let log_level_filter = args.logging.log_level_filter();

    let registry = Registry::default()
        .with(
            tracing_subscriber::fmt::layer()
                .event_format(format().with_thread_ids(true))
                .with_filter(log_level_filter),
        )
        .with(ErrorLayer::default());

    tracing::subscriber::set_global_default(registry).expect("could not initialize tracing");

    let stream = if args.listen {
        trace!("listening on {}:{}", LSP_ADDR, args.port);
        let listener = TcpListener::bind(
            (LSP_ADDR, args.port)
        ).await?;
        let (stream, _) = listener.accept().await?;
        stream
    } else {
        trace!("connecting to {}:{}", LSP_ADDR, args.port);
        TcpStream::connect((LSP_ADDR, args.port)).await?
    };
    let (read, write) = stream.into_split();

    serve(read, write).await;

    Ok(())
}

