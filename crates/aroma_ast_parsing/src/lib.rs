#![doc = include_str!("../README.md")]

use crate::parser::items::TranslationUnit;
use crate::parser::{Err, Parsable, SyntacticParser};
use std::path::Path;

pub mod lexer;
pub mod parser;
pub mod type_resolution;

/// Parses a path syntactically, performing no analysis transformations.
///
/// This creates the baseline AST for aroma source code.
pub fn parse_file(path: &Path) -> Result<TranslationUnit, parser::SyntaxError> {
    let mut parser = SyntacticParser::with_file(path)?;
    let unit = parser.parse(TranslationUnit::parse)
        .map_err(|e|
            match e {
                Err::Error(e ) => {e }
                Err::Failure(e) => {e }
            }
        )?;
    Ok(unit)
}
