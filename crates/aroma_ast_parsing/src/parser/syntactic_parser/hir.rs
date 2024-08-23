//! Syntax tree

use crate::parser::syntactic_parser::error::{ErrorKind, SyntaxError};
use crate::parser::SyntacticParser;
use aroma_ast::token::ToTokens;
use std::fmt::Debug;
use std::io::Read;

pub mod binding;
pub mod expr;
mod helpers;
pub mod items;
pub mod singletons;
pub mod statement;
pub mod constants;

use crate::parser::syntactic_parser::{Err, Parsable};
pub use helpers::*;

