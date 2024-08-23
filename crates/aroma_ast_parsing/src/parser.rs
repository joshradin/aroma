//! parsers used for creating the AST

mod syntactic_parser;

pub use syntactic_parser::{
    hir::*, CouldParse, Err, ErrorKind, Parsable, Parser, SyntaxResult, SyntacticParser,
    SyntaxError,
};
