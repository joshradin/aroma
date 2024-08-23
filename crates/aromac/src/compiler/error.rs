use crate::compiler::compile_job::{CompileError, CompileJobCommand};
use itertools::Itertools as _;
use std::io;
use std::sync::mpsc::SendError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AromaCError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    CompileError(CompileError),
    #[error(transparent)]
    CompileJobLost(#[from] SendError<CompileJobCommand>),
    #[error("{}", .0.iter().join("\n"))]
    Errors(Vec<AromaCError>),
}

impl From<CompileError> for AromaCError {
    fn from(value: CompileError) -> Self {
        Self::CompileError(value)
    }
}

pub type Result<'a, T> = std::result::Result<T, AromaCError>;
