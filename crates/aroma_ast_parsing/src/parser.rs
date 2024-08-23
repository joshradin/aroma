//! parsers used for creating the AST

mod syntactic_parser;

pub use syntactic_parser::{
    syntax_tree::*, CouldParse, Err, ErrorKind, Parsable, Parser, Result, SyntacticParser,
    SyntaxError,
};
