//! Responsible with compiling aroma files into aroma object files

use crate::resolution::TranslationData;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_ast_parsing::parse_file;
use aroma_ast_parsing::parser::SyntaxError;
use itertools::Itertools;
use prelude::*;
use std::io;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::task::JoinSet;
use tracing::{error, error_span, info_span, Instrument};

pub mod error;
mod passes;
pub mod virtual_header;

/// Prelude that can be used in inner modules.
mod prelude {
    use super::*;
    pub use error::{AromaCError, AromaCErrorKind, AromaCResult};
}

/// Responsible with compiling aroma files into aroma object files.
///
/// Must be configured using an [AromaCBuilder].
#[derive(Debug)]
pub struct AromaC {
    max_jobs: usize,
    output_directory: PathBuf,
}

impl AromaC {
    /// Creates the default AromaCBuilder
    #[inline]
    pub fn builder() -> AromaCBuilder {
        AromaCBuilder::new()
    }

    /// Compile a file at a given path
    #[inline]
    pub async fn compile(&mut self, path: &Path) -> AromaCResult<()> {
        self.compile_all(vec![path.to_path_buf()]).await
    }

    /// Compile a file at a given path
    pub async fn compile_all(&mut self, paths: Vec<PathBuf>) -> AromaCResult<()> {
        let mut translation_units = {
            let mut join_set = JoinSet::new();
            for path in paths {
                let path_clone = path.clone();
                join_set.spawn(
                    async move { parse_file(&path_clone) }
                        .instrument(error_span!("parse", path=?path)),
                );
            }
            join_set.join_all().await.into_iter().try_fold(
                Vec::new(),
                |mut accum, next| -> Result<Vec<TranslationUnit>, SyntaxError> {
                    match next? {
                        None => {}
                        Some(tu) => {
                            accum.push(tu);
                        }
                    }
                    Ok(accum)
                },
            )
        }?;

        Ok(())
    }
}

/// Builder for creating a [AromaC] instance.
#[derive(Debug)]
pub struct AromaCBuilder {
    /// Number of jobs to run at once
    pub jobs: usize,
    pub included: Vec<PathBuf>,
    pub output_directory: PathBuf,
}

impl AromaCBuilder {
    /// Creates an AromaCbuilder with default settings
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the output directory for build object files
    pub fn output_directory<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.output_directory = path.as_ref().to_path_buf();
        self
    }

    /// Builds an [AromaC] instance from this builder
    pub fn build(self) -> Result<AromaC, BuildAromaCError> {
        if self.jobs == 0 {
            return Err(BuildAromaCError::ZeroJobs);
        }

        let output_dir_meta = std::fs::metadata(&self.output_directory).map_err(|e| {
            BuildAromaCError::OutputDirectoryDoesNotExist(self.output_directory.clone(), e)
        })?;
        if !output_dir_meta.is_dir() {
            return Err(BuildAromaCError::OutputDirectoryIsNotADirectory(
                self.output_directory,
            ));
        }
        Ok(AromaC {
            max_jobs: self.jobs,
            output_directory: self.output_directory,
        })
    }
}

impl Default for AromaCBuilder {
    fn default() -> Self {
        Self {
            jobs: num_cpus::get(),
            included: vec![],
            output_directory: PathBuf::from("."),
        }
    }
}

/// An error occurred while building an [AromaC] instance
#[derive(Debug, Error)]
pub enum BuildAromaCError {
    #[error("{0:?} does not exist: {1}")]
    OutputDirectoryDoesNotExist(PathBuf, io::Error),
    #[error("{0:?} is not a directory")]
    OutputDirectoryIsNotADirectory(PathBuf),
    #[error("Compilation can't occur if no jobs are allowed")]
    ZeroJobs,
}
