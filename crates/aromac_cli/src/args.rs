//! the args for running aromac

use aroma_cli_common::LoggingArgs;
use aromac::os::PATH_DELIMITER;
use std::path::{Path, PathBuf};

/// The args struct
#[derive(Debug, clap::Parser)]
#[clap(author, version, about = "Compiles aroma code into aroma bytecode")]
pub struct Args {
    #[command(flatten)]
    logging: LoggingArgs,

    /// Specify which source files to compile
    #[clap(required = true, value_name="source file", value_hint=clap::ValueHint::FilePath)]
    pub files: Vec<PathBuf>,
    /// Specify where to place generated aroma bytecode files
    #[clap(short = 'd', default_value = ".")]
    pub output_directory: PathBuf,
    /// Specify files to include in the compilation process.
    ///
    /// Can use multiple includes for many files, a native PATH-like string, or both.
    #[clap(short = 'i', long = "include")]
    include: Vec<String>,
}

impl Args {


    /// Gets files to be included in the compilation process
    pub fn included(&self) -> Vec<&Path> {
        self.include
            .iter()
            .flat_map(|s| s.split(PATH_DELIMITER))
            .map(|s| Path::new(s))
            .collect()
    }

    pub fn logging(&self) -> &LoggingArgs {
        &self.logging
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[test]
    fn test_args_parsing() {
        let test = "aromac file.aroma";
        let args = Args::try_parse_from(test.split(" ")).expect("could not parse test string");
        assert_eq!(args.files[0], Path::new("file.aroma"));
    }

    #[test]
    #[cfg(any(target_os = "macos", target_os = "linux"))]
    fn test_include_parsing_colon() {
        let test = "aromac file.aroma -i objects/object.ao:zip.ab";
        let args = Args::try_parse_from(test.split(" ")).expect("could not parse test string");
        let included = args.included();
        assert_eq!(included[0], Path::new("objects/object.ao"));
        assert_eq!(included[1], Path::new("zip.ab"));
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn test_include_parsing_semi_colon() {
        let test = "aromac file.aroma -i object.ao;zip.ab";
        let args = Args::try_parse_from(test.split(" ")).expect("could not parse test string");
        let included = args.included();
        assert_eq!(included[0], Path::new("object.ao"));
        assert_eq!(included[1], Path::new("zip.ab"));
    }
}
