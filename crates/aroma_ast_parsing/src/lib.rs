#![doc = include_str!("../README.md")]

use crate::parser::SyntacticParser;
use parser::expr::Expr;
use std::path::Path;

pub mod lexer;
pub mod parser;
pub mod type_resolution;

/// Parses a path syntactically, performing no analysis transformations.
///
/// This creates the baseline AST for aroma source code.
pub fn syntactic_parse(path: &Path) -> Result<(), parser::Error> {
    let mut parser = SyntacticParser::with_file(path)?;

    Ok(())
}
