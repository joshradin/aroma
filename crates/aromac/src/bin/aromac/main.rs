use crate::args::Args;
use aroma_files::prelude::*;
use aromac::{AromaC, AromaCBuilder};
use cfg_if::cfg_if;
use clap::Parser;
use fern::Dispatch;
use log::{debug, error, trace, warn, Level, LevelFilter};
use owo_colors::OwoColorize;
use owo_colors::Stream::Stdout;
use std::collections::HashSet;
use std::io::{stderr, stdout};
use std::path::{Path, PathBuf};

mod args;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    let args = Args::parse();
    init_logging(args.log_level_filter())?;
    trace!("starting aromac with args: {args:?}");
    debug!("aromac version: {}", env!("CARGO_PKG_VERSION"));

    let mut aroma_compiler_builder = AromaC::builder();

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
    aroma_c.compile_all(
        to_compile.iter().map(|path| path.as_ref()),
    )?;

    Ok(())
}


fn init_logging(level_filter: LevelFilter) -> eyre::Result<()> {
    Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{}]: {}",
                record.level().if_supports_color(Stdout, |text| match text {
                    Level::Error => {
                        text.bright_red().to_string()
                    }
                    Level::Warn => {
                        text.bright_yellow().to_string()
                    }
                    Level::Info => {
                        text.green().to_string()
                    }
                    Level::Debug => {
                        text.blue().to_string()
                    }
                    Level::Trace => {
                        text.purple().to_string()
                    }
                }),
                message
            ))
        })
        .level(level_filter)
        .chain(
            Dispatch::new()
                .filter(|l| l.level() > Level::Error)
                .chain(stdout()),
        )
        .chain(
            Dispatch::new()
                .filter(|l| l.level() == Level::Error)
                .chain(stderr()),
        )
        .apply()?;
    Ok(())
}
