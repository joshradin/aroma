use std::collections::HashSet;
use std::io::{stderr, stdout};
use std::path::Path;
use cfg_if::cfg_if;
use clap::Parser;
use fern::Dispatch;
use log::{debug, error, trace, warn, Level, LevelFilter};
use owo_colors::OwoColorize;
use owo_colors::Stream::Stdout;
use aromac::{AromaC, AromaCBuilder};
use crate::args::Args;

mod args;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    let args = Args::parse();
    init_logging(args.log_level_filter())?;
    trace!("starting aromac with args: {args:?}");

    let mut aroma_compiler_builder = AromaC::builder();

    debug!("included paths:");
    for included in args.included() {
        debug!("  - {included:?}");
        aroma_compiler_builder.included.push(included.to_path_buf());
    }
    let mut to_compile = vec![];
    debug!("paths to compile:");
    for file in &args.files {
        debug!("  - {file:?}");

    }

    let mut aroma_c = aroma_compiler_builder.build()?;


    Ok(())
}

/// flattens the given path
fn files(path: &Path) -> HashSet<Path> {
    
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
