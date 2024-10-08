use std::io::stdout;
use std::process::ExitCode;

use chrono::Utc;
use clap::Parser;
use log::{trace, Level, LevelFilter};
use owo_colors::OwoColorize;
use owo_colors::Stream::Stdout;

use aroma_vm::function;
use aroma_vm::vm::AromaVm;

fn main() -> eyre::Result<ExitCode> {
    color_eyre::install()?;
    let app = Args::parse();
    init_logging(app.log.unwrap_or(LevelFilter::Off))?;
    trace!("starting aromi with args {app:?}");
    let mut vm = AromaVm::new();
    trace!("Created aroma vm: {:?}", vm);
    let fibonacci = aroma_vm::examples::fibonacci();
    let main = function!(
        name "main",
        params (),
        ret,
        variables (),
        consts {
            function_ref 1
            utf8 "fibonacci"
            long 47
            function_ref 4
            utf8 "print"
        },
        bytecode {
            const(2_u8)
            const(0_u8)
            call(1_u8)
            const(3_u8)
            call(1_u8)
            ret
        }
    );

    vm.load(main).expect("could not add main");
    vm.load(fibonacci).expect("could not add main");

    let ret = vm.start("main")?;
    Ok(ExitCode::from(ret as u8))
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
        .level_for("cranelift_codegen", LevelFilter::Off)
        .level_for("cranelift_jit", LevelFilter::Off)
        .level_for("regalloc2", LevelFilter::Off)
        .chain(stdout())
        .apply()?;
    Ok(())
}

#[derive(Debug, Parser)]
struct Args {
    #[clap(long = "log-level", env = "RUST_LOG")]
    log: Option<LevelFilter>,
}
