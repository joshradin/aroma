//! parsers used for creating the AST

mod hir_parser;
pub mod error;
pub mod hir;
pub mod transforms;

pub use hir_parser::{
    blocking, Err,
    ErrorKind,
    SyntaxError,
    SyntaxResult,
};

#[cfg(test)] pub use hir_parser::tests::test_parser;


