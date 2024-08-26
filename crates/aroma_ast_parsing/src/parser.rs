//! parsers used for creating the AST

mod syntactic_parser;

pub(crate) use syntactic_parser::hir::*;
pub use syntactic_parser::{
    transforms, CouldParse, Err, ErrorKind, Parsable, Parser, SyntacticParser, SyntaxError,
    SyntaxResult,
};
