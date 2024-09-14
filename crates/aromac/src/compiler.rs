//! Responsible with compiling aroma files into aroma object files

use crate::compiler::passes::fully_qualify;
use crate::error::CollectAromaCResults;
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
use tracing::{debug, error, error_span, info_span, Instrument};

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
        let translation_units = self.create_translation_units(paths).await?;
        let full_qualified = self.fully_qualify(translation_units).await?;

        Ok(())
    }

    /// Creates translation units
    async fn create_translation_units(
        &mut self,
        paths: Vec<PathBuf>,
    ) -> AromaCResult<Vec<TranslationUnit>> {
        let result = async {
            let mut join_set = JoinSet::new();
            for path in paths {
                let path_clone = path.clone();
                join_set.spawn(
                    async move { parse_file(&path_clone).await.map_err(|e| e.into()) }
                        .instrument(error_span!("to_mir", path=?path)),
                );
            }

            finish_join_set(join_set)
                .await
                .map(|i| i.into_iter().flatten().collect())
        }
        .instrument(error_span!("translation-units"))
        .await?;
        Ok(result)
    }

    async fn fully_qualify(
        &mut self,
        translation_units: Vec<TranslationUnit>,
    ) -> AromaCResult<Vec<TranslationUnit>> {
        async {
            let mut join_set = JoinSet::new();
            for mut tu in translation_units {
                join_set.spawn(
                    async move {
                        fully_qualify(&mut tu)?;
                        Ok(tu)
                    }
                    .instrument(error_span!("full-qualify")),
                );
            }
            finish_join_set(join_set).await
        }
        .instrument(error_span!("fully-qualify"))
        .await
    }
}

async fn finish_join_set<T: 'static>(
    mut join_set: JoinSet<AromaCResult<T>>,
) -> AromaCResult<Vec<T>> {
    let mut finished = vec![];
    while let Some(next) = join_set.join_next().await {
        match next {
            Ok(Ok(o)) => {
                finished.push(Ok(o));
            }
            Ok(Err(e)) => {
                finished.push(Err(e));
            }
            Err(e) => finished.push(Err(e.into())),
        }
    }
    finished.into_iter().collect_aroma_c_results()
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
