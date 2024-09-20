use aroma_cli_common::LoggingArgs;
use aroma_language_server::serve;
use clap::Parser;
use tokio::net::{TcpListener, TcpStream};
use tracing::trace;
use tracing_error::ErrorLayer;
use tracing_subscriber::fmt::format;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::{Layer, Registry};

const LSP_ADDR: &'static str = "127.0.0.1";
const LSP_PORT: u16 = 9257;

#[derive(Debug, Parser)]
struct Args {
    #[command(flatten)]
    logging: LoggingArgs,
    /// If this should listen for an incoming client
    #[arg(long)]
    listen: bool,
    /// The port to connect to/with
    #[clap(long, default_value_t = LSP_PORT, alias = "socket")]
    port: u16,
    /// If no ansi, uses no ansi code
    #[clap(long = "no-ansi")]
    no_ansi: bool
}


#[tokio::main]
async fn main() -> eyre::Result<()> {
    color_eyre::install()?;

    let args = Args::parse();
    let log_level_filter = args.logging.log_level_filter();

    let registry = Registry::default()
        .with(
            tracing_subscriber::fmt::layer()
                .with_ansi(!args.no_ansi)
                .with_writer(std::io::stderr)
                .event_format(format().with_thread_ids(true))
                .with_filter(log_level_filter),
        )
        .with(ErrorLayer::default());

    tracing::subscriber::set_global_default(registry).expect("could not initialize tracing");

    let stream = if args.listen {

        let listener = TcpListener::bind((LSP_ADDR, args.port)).await?;
        trace!("listening on {}:{}", LSP_ADDR, listener.local_addr()?.port());
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
