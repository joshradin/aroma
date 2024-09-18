use crate::args::Args;
use aroma_files::prelude::*;
use aromac::{AromaC, AromaCBuilder};
use cfg_if::cfg_if;
use chrono::{DateTime, NaiveDateTime};
use clap::Parser;
use eyre::eyre;
use std::collections::HashSet;
use std::io;
use std::io::{stderr, stdout, Stderr, StderrLock, Stdout, StdoutLock};
use std::path::{Path, PathBuf};
use tracing::metadata::LevelFilter;
use tracing::{debug, error, trace, warn, Level};
use tracing::{Metadata, Subscriber};
use tracing_error::ErrorLayer;
use tracing_subscriber::fmt::{format, MakeWriter};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::prelude::*;
use tracing_subscriber::{Layer, Registry};

mod args;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    let args = Args::parse();
    init_logging(args.log_level_filter())?;
    trace!("starting aromac with args: {args:?}");
    debug!("aromac version: {}", env!("CARGO_PKG_VERSION"));

    let aroma_compiler_builder = AromaC::builder();

    let included = args
        .included()
        .iter()
        .map(|file| file_tree(file))
        .flat_map(|b| b.files())
        .collect::<Vec<_>>();

    let to_compile = args
        .files
        .iter()
        .map(|file| file_tree(file))
        .flat_map(|b| b.files())
        .collect::<Vec<_>>();

    debug!("paths to compile: {to_compile:#?}");

    let mut aroma_c = aroma_compiler_builder.build()?;
    aroma_c
        .compile_all(to_compile.iter().map(|path| path.clone()).collect())
        .await?;

    Ok(())
}

struct MyWriter {
    stdout: Stdout,
    stderr: Stderr,
}

enum StdioLock<'a> {
    Stdout(StdoutLock<'a>),
    Stderr(StderrLock<'a>),
}

impl<'a> io::Write for StdioLock<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            StdioLock::Stdout(stdout) => stdout.write(buf),
            StdioLock::Stderr(stderr) => stderr.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            StdioLock::Stdout(stdout) => stdout.flush(),
            StdioLock::Stderr(stderr) => stderr.flush(),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        match self {
            StdioLock::Stdout(stdout) => stdout.write_all(buf),
            StdioLock::Stderr(stderr) => stderr.write_all(buf),
        }
    }
}

impl<'a> MakeWriter<'a> for MyWriter {
    type Writer = StdioLock<'a>;

    fn make_writer(&'a self) -> Self::Writer {
        StdioLock::Stdout(self.stdout.lock())
    }

    fn make_writer_for(&'a self, meta: &Metadata<'_>) -> Self::Writer {
        if meta.level() < &Level::WARN {
            StdioLock::Stderr(self.stderr.lock())
        } else {
            StdioLock::Stdout(self.stdout.lock())
        }
    }
}

fn init_logging(level_filter: LevelFilter) -> eyre::Result<()> {
    let registry = Registry::default()
        .with(
            tracing_subscriber::fmt::layer()
                .event_format(format().with_thread_ids(true))
                .with_writer(MyWriter {
                    stdout: stdout(),
                    stderr: stderr(),
                })
                .with_filter(level_filter),
        )
        .with(ErrorLayer::default());

    tracing::subscriber::set_global_default(registry)?;

    Ok(())
}
