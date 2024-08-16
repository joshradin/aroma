use crate::lexer::Lexer;
use aroma_ast::id::Id;
use aroma_ast::token::{Token, TokenKind};
use std::fmt::{Display, Pointer};
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub mod error;
pub mod syntax_tree;

use crate::parser::{Constant, ConstantKind, CouldParse, Parse};
pub use error::*;

#[derive(Debug, Default)]
enum State<'p> {
    #[default]
    Uninit,
    Lookahead(Token<'p>),
    Eof,
    Poisoned,
}

/// Creates the syntactic parse tree from a token stream.
///
/// This is the first pass of the parsing sequence, and should assign no meaning to the tree
/// it creates, instead this created tree should just be bare minimum structure.
///
/// # Examples
/// Create from an existing lexer
/// ```
/// # use std::path::Path;
/// # use aroma_ast_parsing::lexer::Lexer;
/// # use aroma_ast_parsing::parser::SyntacticParser;
/// let mut buffer = vec![0_u8; 0];
/// let mut lexer = Lexer::new(Path::new("test_path"), &*buffer).unwrap();
/// let parser = SyntacticParser::from(lexer);
/// ```
#[derive(Debug)]
pub struct SyntacticParser<'p, R: Read> {
    lexer: Lexer<'p, R>,
    state: State<'p>,
}

impl<'p, R: Read> SyntacticParser<'p, R> {
    /// Creates a new parser with a given lexer
    pub fn new(lexer: Lexer<'p, R>) -> Self {
        Self {
            lexer,
            state: Default::default(),
        }
    }

    fn next_token(&mut self) -> Result<'p, ()> {
        match self.lexer.next() {
            None => {
                self.state = State::Eof;
            }
            Some(res) => {
                let token = res?;
                self.state = State::Lookahead(token);
            }
        }
        Ok(())
    }

    /// peak the current lookahead
    fn peek(&mut self) -> Result<'p, Option<&Token<'p>>> {
        if matches!(self.state, State::Uninit) {
            let _ = self.next_token()?;
        }
        match &self.state {
            State::Lookahead(tok) => Ok(Some(tok)),
            State::Uninit => {
                unreachable!("next_token() should init parser")
            }
            State::Eof => Ok(None),
            State::Poisoned => Err(self.error(ErrorKind::ParserPoisoned, None)),
        }
    }

    fn consume(&mut self) -> Result<'p, Option<Token<'p>>> {
        if matches!(self.state, State::Uninit) {
            let _ = self.next_token()?;
        }
        let swapped = std::mem::replace(&mut self.state, State::Poisoned);
        match swapped {
            State::Uninit => {
                unreachable!("next_token() should init parser")
            }
            State::Lookahead(token) => {
                self.next_token()?;
                Ok(Some(token))
            }
            State::Eof => Ok(None),
            State::Poisoned => Err(self.error(ErrorKind::ParserPoisoned, None)),
        }
    }

    /// consumes if predicate matches
    fn consume_if<F>(&mut self, predicate: F) -> Result<'p, Option<Token<'p>>>
    where
        F: FnOnce(&Token<'p>) -> bool,
    {
        let should_consume = {
            let peek = self.peek()?;
            if let Some(peek) = peek {
                predicate(peek)
            } else {
                false
            }
        };
        if should_consume {
            self.consume()
        } else {
            Ok(None)
        }
    }

    fn error<E1, E2>(&self, error: E1, cause: E2) -> Error<'p>
    where
        E1: Into<ErrorKind<'p>>,
        E2: Into<Option<Error<'p>>>,
    {
        Error::new(error.into(), None, cause)
    }

    /// Wrapper function for parsing an item
    pub fn parse<T>(&mut self) -> std::result::Result<T, T::Err>
    where
        T: Parse<'p>,
    {
        T::parse(self)
    }
}

impl<'p> SyntacticParser<'p, File> {
    /// Creates a new parser for a given file
    pub fn with_file(path: &'p Path) -> error::Result<Self> {
        let file = File::open(path)?;
        let lexer = Lexer::new(path, file)?;
        Ok(Self::new(lexer))
    }
}

impl<'p> Parse<'p> for Id<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> std::result::Result<Self, Self::Err> {
        if let Some(tok) =
            parser.consume_if(|token| matches!(token.kind(), TokenKind::Identifier(_)))?
        {
            match tok.kind() {
                TokenKind::Identifier(_) => Ok(Id::new([tok]).unwrap()),
                _ => unreachable!(),
            }
        } else {
            let kind = ErrorKind::expected_token(["constant".to_string()], parser.consume()?);
            Err(parser.error(kind, None))
        }
    }
}
impl<'p> CouldParse<'p> for Id<'p> {
    fn could_parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> std::result::Result<bool, Self::Err> {
        Ok(parser
            .peek()?
            .map(|t| matches!(t.kind(), TokenKind::Identifier(_)))
            .unwrap_or(false))
    }
}

impl<'p, R: Read> From<Lexer<'p, R>> for SyntacticParser<'p, R> {
    fn from(value: Lexer<'p, R>) -> Self {
        Self::new(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::syntactic_parser::syntax_tree::expr::Expr;
    use crate::parser::ConstantKind;
    use aroma_ast::spanned::{Span, Spanned};
    use aroma_ast::token::{ToTokens, TokenKind};
    use std::io::Write as _;
    use tempfile::NamedTempFile;

    fn test_parser<F>(s: &str, callback: F)
    where
        F: FnOnce(&mut SyntacticParser<File>, &Path),
    {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "{}", s).expect("could not write");
        let path = temp_file.path();
        let mut parser = SyntacticParser::with_file(path).unwrap();
        callback(&mut parser, path)
    }
    #[test]
    fn test_create_parser_from_file() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "let x = 1.0;").expect("could not write");
        let path = temp_file.path();
        let mut parser = SyntacticParser::with_file(path).unwrap();
        let token = parser.peek().unwrap().unwrap();
        assert_eq!(token.kind(), &TokenKind::Let);
        assert_eq!(token.span(), Span::new(temp_file.path(), 0, 3));
    }

    #[test]
    fn test_consume() {
        test_parser("let x = 1.0;", |parser, path| {
            let consumed_token = parser.consume().unwrap().unwrap();
            assert_eq!(consumed_token.kind(), &TokenKind::Let);
            assert_eq!(consumed_token.span(), Span::new(path, 0, 3));
            let token = parser.peek().unwrap().unwrap();
            assert_eq!(token.kind(), &TokenKind::Identifier("x".to_string()));
            assert_eq!(token.span(), Span::new(path, 3, 1));
        });
    }

    #[test]
    fn test_consume_if() {
        test_parser("let x = 1.0;", |parser, _| {
            assert!(parser
                .consume_if(|tok| tok.kind() == &TokenKind::MultAssign)
                .unwrap()
                .is_none());
            assert!(parser
                .consume_if(|tok| tok.kind() == &TokenKind::Let)
                .unwrap()
                .is_some());
        });
    }

    #[test]
    fn test_parse_constant() {
        test_parser("3.0", |parser, _| {
            let constant = parser.parse::<Constant>().unwrap();
            assert!(matches!(constant.kind, ConstantKind::Float(3.0)));
            println!(
                "{constant:?} -> {:#?}",
                constant.to_tokens().collect::<Vec<_>>()
            );
        });
        test_parser("3", |parser, _| {
            let constant = parser.parse::<Constant>().unwrap();
            assert!(matches!(constant.kind, ConstantKind::Integer(3)));
            println!(
                "{constant:?} -> {:#?}",
                constant.to_tokens().collect::<Vec<_>>()
            );
        })
    }
    #[test]
    fn test_parse_identifier() {
        test_parser("helloWorld", |parser, _| {
            let identifier = parser.parse::<Id>().unwrap();
            assert!(matches!(identifier.most_specific(), "helloWorld"));
        })
    }

    #[test]
    fn test_parse_identifier_single_letter() {
        test_parser("i", |parser, _| {
            let identifier = parser.parse::<Id>().unwrap();
            assert!(matches!(identifier.most_specific(), "i"));
        })
    }

    #[test]
    fn test_parse_unary() {
        test_parser("--helloWorld", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
        })
    }

    #[test]
    fn test_parse_expressions() {
        test_parser("helloWorld + 3*2/5 - 1 << 3 * -2.0", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>())
        })
    }

    #[test]
    fn test_parse_method_calls() {
        test_parser("foo(bar() + 2, bar(1+2))", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>())
        })
    }

    #[test]
    fn test_parse_fields() {
        test_parser("foo.blah.get(1)", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>())
        })
    }

    #[test]
    fn test_parse_index() {
        test_parser("foo[0]", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>());
        })
    }

    #[test]
    fn test_closure() {
        test_parser("{ -> }", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>());
        })
    }

    #[test]
    fn test_closure_with_bindings() {
        test_parser("{ i -> }", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>());
        })
    }

    #[test]
    fn test_closure_after_method_no_parens() {
        test_parser("forEach { -> }", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>());
        })
    }

    #[test]
    fn test_closure_after_method_no_args() {
        test_parser("forEach() { -> }", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            assert!(
                matches!(expr, Expr::Call(call) if call.parameters.is_some() && call.parameters.as_ref().unwrap().punctuated.len() == 1)
            );
        })
    }

    #[test]
    fn test_closure_after_method_mixed_args() {
        test_parser("repeat(10){ -> }", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");
            assert!(
                matches!(expr, Expr::Call(call) if call.parameters.is_some() && call.parameters.as_ref().unwrap().punctuated.len() == 2)
            );
        })
    }

    #[test]
    fn test_closure_after_method_mixed_args_then_expr() {
        test_parser("(repeat(10)) { -> } ", |parser, _| {
            let expr = parser.parse::<Expr>().unwrap();
            println!("{expr:#?}");

        })
    }
}
