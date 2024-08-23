use crate::compiler::compile_job::{CompileError, CompileJobCommand};
use itertools::Itertools as _;
use std::io;
use std::sync::mpsc::SendError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AromaCError<'p> {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    CompileError(CompileError<'p>),
    #[error(transparent)]
    CompileJobLost(#[from] SendError<CompileJobCommand>),
    #[error("{}", .0.iter().join("\n"))]
    Errors(Vec<AromaCError<'p>>),
}

impl<'p> From<CompileError<'p>> for AromaCError<'p> {
    fn from(value: CompileError<'p>) -> Self {
        Self::CompileError(value)
    }
}

pub type Result<'a, T> = std::result::Result<T, AromaCError<'a>>;
