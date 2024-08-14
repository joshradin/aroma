//! Syntax tree

use aroma_ast::identifier::Id;
use aroma_ast::token::{ToTokens, Token, TokenStream};

/// Parse a syntax tree part
pub trait ParseSyntaxTree<'p, T>
    where T : SyntaxTree<'p>
{
    type Err;

    /// Attempt to parse some syntax tree part
    fn parse_tree(&mut self) -> Result<T, Self::Err>;
}

/// A syntax tree part
pub trait SyntaxTree<'p> : ToTokens<'p> {
}


#[derive(Debug)]
pub enum ConstantKind {
    Float(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
}

#[derive(Debug)]
pub struct Constant<'p> {
    pub kind: ConstantKind,
    pub tok: Token<'p>
}

impl<'p> SyntaxTree<'p> for Constant<'p> {}

impl<'p> ToTokens<'p> for Constant<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        TokenStream::from_iter([self.tok.clone()])
    }
}


impl<'p> SyntaxTree<'p> for Id<'p> {}


/// An expression
#[derive(Debug)]
pub enum PrimaryExpression<'p> {
    Identifier(Id<'p>),
    Constant(Constant<'p>)
}

impl<'p> ToTokens<'p> for PrimaryExpression<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        match self {
            PrimaryExpression::Identifier(i) => { i.to_tokens()}
            PrimaryExpression::Constant(c) => { c.to_tokens()}
        }
    }
}

impl<'p> SyntaxTree<'p> for PrimaryExpression<'p> {

}