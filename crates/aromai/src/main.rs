use aroma_vm::vm::AromaVm;
use chrono::{DateTime, Utc};
use clap::Parser;
use log::{trace, Level, LevelFilter};
use owo_colors::OwoColorize;
use owo_colors::Stream::Stdout;
use std::io::{stderr, stdout};
use std::process::ExitCode;

fn main() -> eyre::Result<ExitCode> {
    color_eyre::install()?;
    let app = App::parse();
    init_logging(app.log.unwrap_or(LevelFilter::Info))?;
    trace!("starting aromi with args {app:?}");

    let vm = AromaVm::new();
    trace!("Created aroma vm: {:?}", vm);

    Ok(ExitCode::SUCCESS)
}

fn init_logging(level_filter: LevelFilter) -> eyre::Result<()> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "{} {:>5} {} --- [{:>16}] {:<32} : {}",
                Utc::now().format("%Y-%m-%d %H:%M:%S%.3f"),
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
                sysinfo::get_current_pid().unwrap(),
                std::thread::current()
                    .name()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                record.target(),
                message
            ))
        })
        .level(level_filter)
        .chain(stdout())
        .apply()?;
    Ok(())
}

#[derive(Debug, Parser)]
struct App {
    #[clap(long = "log-level", env = "RUST_LOG")]
    log: Option<LevelFilter>,
}
