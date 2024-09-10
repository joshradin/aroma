use crate::lexer::Lexer;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::{Span, Spanned};
use aroma_tokens::token::{ToTokens, Token, TokenKind};
use std::any::type_name;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::result;

pub mod error;
pub mod hir;
pub mod transforms;

use crate::parser::expr::remove_nl;
use crate::parser::singletons::{Dot, VarId};
use crate::parser::{map, Punctuated1};
pub use error::*;
use log::trace;

/// Parser for syntax tree items
pub trait Parser<R: Read, O, E = SyntaxError>: Clone
where
    E: std::error::Error,
{
    fn non_terminal(&self) -> &'static str;
    fn parse(&mut self, parser: &mut SyntacticParser<'_, R>) -> result::Result<O, Err<E>>;
}

impl<R, O, E, F> Parser<R, O, E> for F
where
    R: Read,
    F: FnMut(&mut SyntacticParser<'_, R>) -> result::Result<O, Err<E>>,
    F: Clone,
    E: std::error::Error,
{
    fn non_terminal(&self) -> &'static str {
        type_name::<F>()
    }

    fn parse(&mut self, parser: &mut SyntacticParser<'_, R>) -> result::Result<O, Err<E>> {
        (self)(parser)
    }
}

/// Parse a syntax tree part
pub trait Parsable: ToTokens + Sized {
    type Err;

    /// Attempt to parse some syntax tree part
    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, Err<Self::Err>>;
}

/// A sub trait that determines if this type could be parsed without doing the parsing
pub trait CouldParse: Parsable {
    /// Attempt to parse some syntax tree part
    fn could_parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<bool, Err<Self::Err>>;
}

impl<P: Parsable + CouldParse> Parsable for Vec<P>
where
    P::Err: Error,
{
    type Err = P::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, Err<Self::Err>> {
        let mut result = Vec::new();
        while let Some(item) = parser.parse_opt::<P>()? {
            result.push(item);
        }

        Ok(result)
    }
}

impl<P: Parsable + CouldParse> Parsable for Option<P>
where
    P::Err: Error,
{
    type Err = P::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, Err<Self::Err>> {
        parser.parse_opt::<P>()
    }
}

#[derive(Debug, Default)]
enum State {
    #[default]
    Uninit,
    Lookahead(Token),
    Buffered(VecDeque<Token>),
    Eof,
    Poisoned,
}

#[derive(Debug, Default)]
struct StateFrame {
    used: VecDeque<Token>,
    ignore_nl_prev: bool,
    non_terminals_prev: Vec<&'static str>,
    last_span_prev: Option<Span>,
}

/// Err enum used to represent recoverable and non-recoverable errors
#[derive(Debug)]
pub enum Err<E> {
    /// a recoverable error
    Error(E),
    /// a non-recoverable error
    Failure(E),
}

impl<E> Err<E> {
    pub fn cut(self) -> Self {
        match self {
            Err::Error(e) => Err::Failure(e),
            e @ Err::Failure(_) => e,
        }
    }
}

impl<E: std::error::Error> Display for Err<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Err::Error(e) => {
                write!(f, "recoverable error: {e}")
            }
            Err::Failure(e) => {
                write!(f, "unrecoverable error: {e}")
            }
        }
    }
}

impl<E: std::error::Error> std::error::Error for Err<E> {}

impl<E> Err<E> {
    pub fn convert<E2>(self) -> Err<E2>
    where
        E: Into<E2>,
    {
        match self {
            Err::Error(e) => Err::Error(e.into()),
            Err::Failure(e) => Err::Failure(e.into()),
        }
    }
}

impl From<SyntaxError> for Err<SyntaxError> {
    fn from(value: SyntaxError) -> Self {
        Err::Error(value)
    }
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
    state: State,
    last_span: Option<Span>,
    frames: Vec<StateFrame>,
    non_terminals: Vec<&'static str>,
    ignore_nl: bool,
}

impl<'p, R: Read> SyntacticParser<'p, R> {
    /// Creates a new parser with a given lexer
    pub fn new(lexer: Lexer<'p, R>) -> Self {
        Self {
            lexer,
            state: Default::default(),
            last_span: None,
            frames: vec![],
            non_terminals: vec![],
            ignore_nl: true,
        }
    }

    fn next_token(&mut self) -> SyntaxResult<()> {
        loop {
            match &self.state {
                // nothing needs to be done to get the next token
                State::Buffered(v) if !v.is_empty() => {}
                _ => match self.lexer.next() {
                    None => {
                        self.state = State::Eof;
                        return Ok(());
                    }
                    Some(res) => {
                        let token = res.map_err(|e| Err::Failure(ErrorKind::from(e).into()))?;
                        self.state = State::Lookahead(token);
                    }
                },
            }

            if self.ignore_nl {
                if let Some(o) = self.peek()? {
                    if !matches!(o.kind(), TokenKind::Nl) {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    /// peak the current lookahead
    fn peek(&mut self) -> SyntaxResult<Option<&Token>> {
        if matches!(self.state, State::Uninit) {
            self.next_token()?;
        }
        match &self.state {
            State::Lookahead(tok) => Ok(Some(tok)),
            State::Uninit => {
                unreachable!("next_token() should init parser")
            }
            State::Eof => Ok(None),
            State::Poisoned => Err(self.error(ErrorKind::ParserPoisoned, None)),
            State::Buffered(buf) => Ok(buf.front()),
        }
    }

    fn consume(&mut self) -> SyntaxResult<Option<Token>> {
        if matches!(self.state, State::Uninit) {
            self.next_token()?;
        }
        trace!("starting consume, state={:?}", self.state);
        let swapped = std::mem::replace(&mut self.state, State::Poisoned);
        let token = match swapped {
            State::Uninit => {
                unreachable!("next_token() should init parser")
            }
            State::Buffered(mut buffer) => {
                let token = buffer
                    .pop_front()
                    .expect("buffer should always have >0 len");
                if buffer.is_empty() {
                    self.next_token()?;
                } else {
                    self.state = State::Buffered(buffer);
                }
                trace!("consumed {token:?}, state ={:?}", self.state);
                if let Some(state_frame) = self.state_frame_mut() {
                    state_frame.used.push_back(token.clone());
                }
                Ok(Some(token))
            }
            State::Lookahead(token) => {
                self.next_token()?;
                trace!("consumed {token:?}, state ={:?}", self.state);
                if let Some(state_frame) = self.state_frame_mut() {
                    state_frame.used.push_back(token.clone());
                }

                Ok(Some(token))
            }
            State::Eof => {
                self.state = State::Eof;
                Ok(None)
            }
            State::Poisoned => Err(self.error(ErrorKind::ParserPoisoned, None)),
        }?;
        if let Some(token) = &token {
            self.last_span = Some(token.span());
        }
        Ok(token)
    }

    fn state_frame_mut(&mut self) -> Option<&mut StateFrame> {
        self.frames.last_mut()
    }

    fn push_backtrack_frame(&mut self) {
        let mut frame = StateFrame::default();
        frame.ignore_nl_prev = self.ignore_nl;
        frame.non_terminals_prev = self.non_terminals.clone();
        frame.last_span_prev = self.last_span.clone();
        self.frames.push(frame)
    }

    fn pop_backtrack_frame(&mut self) -> Option<StateFrame> {
        self.frames.pop()
    }

    /// Gets if this parser is at EOF
    pub fn eof(&self) -> bool {
        matches!(self.state, State::Eof)
    }

    /// Wrapper function for parsing an item
    #[inline]
    pub fn parse<O, E, P: Parser<R, O, E>>(&mut self, mut parser: P) -> result::Result<O, Err<E>>
    where
        E: std::error::Error,
    {
        trace!(
            "starting parsing {} state={:?}",
            parser.non_terminal(),
            self.state
        );
        self.non_terminals.push(parser.non_terminal());
        let r = parser.parse(self);
        trace!(
            "after parsing {} state={:?}",
            parser.non_terminal(),
            self.state
        );
        if r.is_ok() {
            self.non_terminals.pop();
        }
        r
    }

    /// Wrapper function for parsing an optional item
    pub fn parse_opt<P: Parsable<Err: std::error::Error> + CouldParse>(
        &mut self,
    ) -> Result<Option<P>, Err<P::Err>> {
        if P::could_parse(self)? {
            Ok(Some(self.parse(P::parse)?))
        } else {
            Ok(None)
        }
    }

    /// Tries to run a parser, backtracking if an errors occurs within the parser.
    ///
    /// returns `Ok(Some(parsed))` on success, `Ok(None)` on `Err::Error(_)` and `Err(e)` on `Err::Failure(e)`.
    pub fn try_parse<O, E: std::error::Error, P: Parser<R, O, E>>(
        &mut self,
        mut parser: P,
    ) -> result::Result<Option<O>, E> {
        trace!(
            "({}) starting backtracking parse with state at {:?}",
            self.frames.len(),
            self.state
        );
        self.push_backtrack_frame();
        let result = parser.parse(self);
        let pop = self.pop_backtrack_frame().unwrap();
        match result {
            Ok(ok) => Ok(Some(ok)),
            Err(Err::Error(e)) => {
                self.apply_frame(pop, e);
                Ok(None)
            }
            Err(Err::Failure(e)) => Err(e),
        }
    }

    fn apply_frame<E: std::error::Error>(&mut self, pop: StateFrame, e: E) {
        let state = std::mem::replace(&mut self.state, State::Poisoned);
        trace!(
            "({}) error occurred while state={state:?} -> {e:?}",
            self.frames.len()
        );
        let mut v = pop.used;
        let next_state = match state {
            State::Buffered(buffered) => {
                v.extend(buffered);
                State::Buffered(v)
            }
            State::Lookahead(tok) if v.is_empty() => State::Lookahead(tok),
            State::Lookahead(tok) => {
                v.push_back(tok);
                State::Buffered(v)
            }
            State::Eof if v.is_empty() => State::Eof,
            State::Eof => State::Buffered(v),
            state => panic!("unhandled state: {state:?}"),
        };
        self.state = next_state;
        trace!(
            "({}) after backtrack state={:?}",
            self.frames.len(),
            self.state
        );
        self.ignore_nl = pop.ignore_nl_prev;
        self.non_terminals = pop.non_terminals_prev;
        self.last_span = pop.last_span_prev;
    }

    /// Sets the parser into a given ignore state.
    pub fn set_ignore_nl(&mut self, state: bool) -> SyntaxResult {
        if self.ignore_nl == state {
            return Ok(());
        }
        if !self.ignore_nl {
            while self
                .peek()?
                .map(|i| matches!(i.kind(), TokenKind::Nl))
                .unwrap_or(false)
            {
                self.consume()?;
            }
        }

        self.ignore_nl = state;
        Ok(())
    }

    pub fn with_ignore_nl<F: FnOnce(&mut Self) -> SyntaxResult<O>, O>(
        &mut self,
        state: bool,
        func: F,
    ) -> SyntaxResult<O> {
        let old_state = self.ignore_nl;
        self.set_ignore_nl(state)?;
        let ret = (func)(self);
        self.set_ignore_nl(old_state)?;
        ret
    }

    /// consumes if predicate matches
    fn consume_if<F>(&mut self, predicate: F) -> SyntaxResult<Option<Token>>
    where
        F: FnOnce(&Token) -> bool,
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

    fn error<E1, E2>(&self, error: E1, cause: E2) -> Err<SyntaxError>
    where
        E1: Into<ErrorKind>,
        E2: Into<Option<SyntaxError>>,
    {
        let span = match &self.state {
            State::Lookahead(t) => Some(t.span()),
            State::Buffered(vec) => vec.front().map(|t| t.span()),
            _ => None,
        }
        .or(self.last_span.as_ref().cloned().map(|s| s.end()));
        Err::Error(SyntaxError::new(
            error.into(),
            span,
            cause,
            self.non_terminals.clone(),
        ))
    }

    fn error_with_span<E1, E2>(&self, error: E1, cause: E2, span: Span) -> Err<SyntaxError>
    where
        E1: Into<ErrorKind>,
        E2: Into<Option<SyntaxError>>,
    {
        Err::Error(SyntaxError::new(
            error.into(),
            span,
            cause,
            self.non_terminals.clone(),
        ))
    }
}

impl<'p> SyntacticParser<'p, File> {
    /// Creates a new parser for a given file
    pub fn with_file(path: &'p Path) -> result::Result<Self, SyntaxError> {
        let file = File::open(path)?;
        let lexer = Lexer::new(path, file)?;
        Ok(Self::new(lexer))
    }
}

impl Parsable for Id {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let parts = map(Punctuated1::<VarId, Dot>::parse, |ids| {
            Id::new(
                ids.punctuated
                    .into_iter()
                    .flat_map(|(var, _)| var.id.to_tokens()),
            )
            .unwrap()
        });
        parser
            .try_parse(parts)
            .map_err(Err::Error)
            .and_then(|id| id.ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None)))
    }
}
impl CouldParse for Id {
    fn could_parse<R: Read>(parser: &mut SyntacticParser<R>) -> SyntaxResult<bool> {
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
    use crate::parser::constants::{Constant, ConstantKind};
    use crate::parser::cut;
    use crate::parser::singletons::Class;
    use crate::parser::syntactic_parser::hir::expr::Expr;
    use aroma_tokens::spanned::{Span, Spanned};
    use aroma_tokens::token::{ToTokens, TokenKind};
    use std::io::Write as _;
    use tempfile::NamedTempFile;

    pub fn test_parser<F>(s: &str, callback: F)
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
            assert_eq!(token.span(), Span::new(path, 4, 1));
        });
    }

    #[test]
    fn test_try_parse() {
        test_parser("class", |parser, _| {
            let result = parser.try_parse(Expr::parse);
            assert!(matches!(result, Ok(None)), "should not be okay");
            trace!("parser: {:#?}", parser);
            let result = parser.try_parse(Class::parse);
            assert!(
                matches!(result, Ok(Some(Class { .. }))),
                "should be okay but got {result:?}"
            );
            let result = parser.try_parse(cut(Class::parse));
            assert!(matches!(result, Err(_)), "should be err but got {result:?}");
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
            let constant = parser.parse(Constant::parse).unwrap();
            assert!(matches!(constant.kind, ConstantKind::Float(3.0)));
            trace!(
                "{constant:?} -> {:#?}",
                constant.to_tokens().collect::<Vec<_>>()
            );
        });
        test_parser("3", |parser, _| {
            let constant = parser.parse(Constant::parse).unwrap();
            assert!(matches!(constant.kind, ConstantKind::Integer(3)));
            trace!(
                "{constant:?} -> {:#?}",
                constant.to_tokens().collect::<Vec<_>>()
            );
        })
    }
}
