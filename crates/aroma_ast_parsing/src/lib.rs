#![doc = include_str!("../README.md")]

use crate::parser::{Err, Parsable, SyntacticParser};
use aroma_ast::translation_unit::TranslationUnit;
use parser::transforms::{transform, Transformer};
use std::path::Path;
use tracing::trace;

pub mod lexer;
pub mod parser;
pub mod type_resolution;

/// Parses a path syntactically, performing no analysis transformations.
///
/// This creates the baseline AST for aroma source code.
pub async fn parse_file(path: &Path) -> Result<Option<TranslationUnit>, parser::SyntaxError> {
    trace!("parsing {path:?}");
    let mut parser = SyntacticParser::with_file(path).await?;
    trace!("created parser for {path:?}");
    let unit = parser
        .parse(parser::translation_unit::TranslationUnit::parse)
        .map_err(|e| match e {
            Err::Error(e) => e,
            Err::Failure(e) => e,
        })?;

    if unit.is_empty() {
        return Ok(None);
    }
    let translated = transform(unit)?;

    Ok(Some(translated))
}
