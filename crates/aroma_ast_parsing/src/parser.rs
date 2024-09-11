//! parsers used for creating the AST

mod hir_parser;
pub mod error;
pub mod hir;
pub mod transforms;
mod traits;

pub use hir_parser::{
    CouldParse, Err, ErrorKind, Parsable,
    Parser,
    SyntaxError,
    SyntaxResult,
    blocking,

};

#[cfg(test)] pub use hir_parser::tests::test_parser;


