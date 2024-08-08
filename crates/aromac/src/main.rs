#![doc = include_str!("../README.md")]

use std::io::{stderr, stdout};

use cfg_if::cfg_if;
use clap::Parser;
use fern::Dispatch;
use log::{debug, error, Level, LevelFilter, trace, warn};
use owo_colors::OwoColorize;
use owo_colors::Stream::Stdout;

use crate::args::Args;

mod args;
mod frontend;
mod common;

cfg_if! {
    if #[cfg(windows)] {
        #[path = "native/windows.rs"]
        #[doc(hidden)]
        mod windows;
        use windows as os;
    } else if #[cfg(target_os = "linux")] {
        #[path = "native/linux.rs"]
        #[doc(hidden)]
        mod linux;
        use linux as os;
    } else if #[cfg(target_os = "macos")] {
        #[path = "native/macos.rs"]
        #[doc(hidden)]
        mod macos;
        use macos as os;
    } else {
        compile_error!("unsupported OS for compiling")
    }
}

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    let args = Args::parse();
    init_logging(args.log_level_filter())?;
    trace!("starting aromac with args: {args:?}");
    debug!("included paths:");
    for included in args.included() {
        debug!("  - {included:?}")
    }
    debug!("paths to compile:");
    for file in &args.files {
        debug!("  - {file:?}")
    }

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
