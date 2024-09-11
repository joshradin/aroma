use crate::lexer::token_parsing::parse_token;
use crate::lexer::{LexResult, LexingError};
use aroma_tokens::spanned::Span;
use aroma_tokens::token::{Token, TokenKind};
use async_stream::{stream, try_stream};
use std::path::{Path, PathBuf};
use tokio::fs::File;
use tokio::io;
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncReadExt, BufReader};
use tokio_stream::Stream;

/// An async lexer
#[derive(Debug)]
pub struct Lexer<R> {
    path: PathBuf,
    internal_buffer: String,
    reader: BufReader<R>,
    offset: usize,
}

impl<R: AsyncRead + Unpin> Lexer<R> {
    /// Creates a new async lexer
    pub fn new(path: impl AsRef<Path>, reader: R) -> Self {
        let reader = BufReader::new(reader);
        Self {
            path: path.as_ref().to_path_buf(),
            internal_buffer: String::new(),
            reader,
            offset: 0,
        }
    }

    /// asynchronously gets the next token
    pub async fn next(&mut self) -> LexResult<Option<Token>> {
        if self.internal_buffer.is_empty() {
            self.reader.read_line(&mut self.internal_buffer).await?;
        }
        while !self.internal_buffer.is_empty() {
            match parse_token(&self.internal_buffer) {
                Ok((_, (_, _, _, TokenKind::Eof))) => {
                    break;
                }
                Ok((rest, (l, token_len, r, token_kind))) => {
                    let offset = self.offset + l;
                    self.offset += l + token_len + r;
                    let span = Span::new(self.path.as_path(), offset, token_len);
                    self.internal_buffer = String::from(rest);
                    return Ok(Some(Token::new(span, token_kind)));
                }
                Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
                    return Err(e.into());
                }
                Err(nom::Err::Incomplete(needed)) => {
                    panic!("Incomplete: {:?}", needed);
                }
            }
        }
        Ok(None)
    }

    /// Converts this lexer into a stream
    pub fn into_stream(mut self) -> impl Stream<Item = Result<Token, LexingError>> {
        try_stream! {
            loop {
                match self.next().await {
                    Ok(Some(tok)) => { yield tok }
                    Ok(None) => { break}
                    Err(e) => { Err(e)?; }
                }
            }
        }
    }
}

impl Lexer<File> {
    /// Creates a lexer using a given file
    pub async fn open(path: impl AsRef<Path>) -> io::Result<Self> {
        let path = path.as_ref();
        let file = File::open(path).await?;
        Ok(Self::new(path, file))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aroma_tokens::token::TokenKind;
    use tokio_stream::StreamExt;

    #[tokio::test]
    async fn text_lex_once() {
        let string = b"let i: Int = 3";
        let mut lexer = Lexer::new("path", &string[..]);
        let token = lexer.next().await.unwrap().unwrap();
        assert_eq!(token.kind(), &TokenKind::Let)
    }

    #[tokio::test]
    async fn text_lex_stream() {
        let string = b"let i: Int = 3;";
        let mut lexer = Lexer::new("path", &string[..]);
        let mut token = lexer.into_stream();
        let tokens: Vec<_> = token
            .collect::<Result<Vec<_>, _>>()
            .await
            .expect("could not create a token stream");
    }
}
