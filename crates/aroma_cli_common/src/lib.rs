#![doc = include_str!("../README.md")]

use clap::{Args, value_parser, ArgAction};
/// Common way to set logging levels
#[derive(Debug, Clone, Copy, Args)]
pub struct LoggingArgs {
    #[clap(short = 'v', value_parser = value_parser!(u8).range(0..=2), action=ArgAction::Count, conflicts_with="quiet")]
    verbose: u8,
    #[clap(short = 'q', value_parser = value_parser!(u8).range(0..=2), action=ArgAction::Count, conflicts_with="verbose")]
    quiet: u8,
}

impl LoggingArgs {
    /// Gets the logging level based on whether `-v[v]` or `-q[q]` has been used,
    #[cfg(feature = "tracing")]
    pub fn log_level_filter(&self) -> tracing::level_filters::LevelFilter {
        use tracing::level_filters::LevelFilter;
        let sum = self.verbose as i8 - self.quiet as i8;
        match sum {
            -2 => LevelFilter::OFF,
            -1 => LevelFilter::ERROR,
            0 => LevelFilter::INFO,
            1 => LevelFilter::DEBUG,
            2 => LevelFilter::TRACE,
            _ => unreachable!(),
        }
    }
}
