//! blocking HIR parser

use crate::lexer::blocking::Lexer;
use crate::parser;
pub use crate::parser::hir_parser::traits::blocking::{CouldParse, Parsable, Parser};
use crate::parser::{ErrorKind, SyntaxError, SyntaxResult};
use aroma_tokens::spanned::{Span, Spanned};
use aroma_tokens::token::{Token, TokenKind};
use std::collections::VecDeque;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::result;
use tracing::trace;

#[derive(Debug, Default)]
pub(in crate::parser) enum State {
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

/// Creates the syntactic parse tree from a token stream.
///
/// This is the first pass of the parsing sequence, and should assign no meaning to the tree
/// it creates, instead this created tree should just be bare minimum structure.
///
/// # Examples
/// Create from an existing lexer
/// ```
/// # use std::path::Path;
/// # use aroma_ast_parsing::lexer::blocking::Lexer;
/// # use aroma_ast_parsing::parser::blocking::BlockingParser;
/// let mut buffer = vec![0_u8; 0];
/// let mut lexer = Lexer::new(Path::new("test_path"), &*buffer).unwrap();
/// let parser = BlockingParser::from(lexer);
/// ```
#[derive(Debug)]
pub struct BlockingParser<'p, R: Read> {
    lexer: Lexer<'p, R>,
    pub(in crate::parser) state: State,
    last_span: Option<Span>,
    frames: Vec<StateFrame>,
    non_terminals: Vec<&'static str>,
    ignore_nl: bool,
}

impl<'p, R: Read> BlockingParser<'p, R> {
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
                        let token =
                            res.map_err(|e| parser::Err::Failure(ErrorKind::from(e).into()))?;
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
    pub(in crate::parser) fn peek(&mut self) -> SyntaxResult<Option<&Token>> {
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

    pub(in crate::parser) fn consume(&mut self) -> SyntaxResult<Option<Token>> {
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
    pub fn parse<O, E, P: Parser<R, O, E>>(&mut self, mut parser: P) -> Result<O, parser::Err<E>>
    where
        E: Error,
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
    ) -> Result<Option<P>, parser::Err<P::Err>> {
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
            Err(parser::Err::Error(e)) => {
                self.apply_frame(pop, e);
                Ok(None)
            }
            Err(parser::Err::Failure(e)) => Err(e),
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
    pub(in crate::parser) fn consume_if<F>(&mut self, predicate: F) -> SyntaxResult<Option<Token>>
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

    pub(in crate::parser) fn error<E1, E2>(&self, error: E1, cause: E2) -> parser::Err<SyntaxError>
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
        parser::Err::Error(SyntaxError::new(
            error.into(),
            span,
            cause,
            self.non_terminals.clone(),
        ))
    }

    pub(in crate::parser) fn error_with_span<E1, E2>(
        &self,
        error: E1,
        cause: E2,
        span: Span,
    ) -> parser::Err<SyntaxError>
    where
        E1: Into<ErrorKind>,
        E2: Into<Option<SyntaxError>>,
    {
        parser::Err::Error(SyntaxError::new(
            error.into(),
            span,
            cause,
            self.non_terminals.clone(),
        ))
    }
}

impl<'p> BlockingParser<'p, File> {
    /// Creates a new parser for a given file
    pub fn with_file(path: &'p Path) -> result::Result<Self, SyntaxError> {
        let mut file = File::open(path)?;
        let lexer = Lexer::new(path, file)?;
        Ok(Self::new(lexer))
    }
}

impl<'p, R: Read> From<Lexer<'p, R>> for BlockingParser<'p, R> {
    fn from(value: Lexer<'p, R>) -> Self {
        Self::new(value)
    }
}

pub(crate) fn remove_nl<'p, R: Read>(
    parser: &mut BlockingParser<R>,
) -> Result<(), crate::parser::Err<SyntaxError>> {
    parser.parse(|parser: &mut BlockingParser<R>| {
        loop {
            if parser
                .consume_if(|p| matches!(p.kind(), TokenKind::Nl))?
                .is_none()
            {
                break;
            }
        }
        Ok(())
    })
}
