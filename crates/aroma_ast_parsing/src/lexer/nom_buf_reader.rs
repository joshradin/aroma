//! A buf reader implemented for use with nom

use std::fmt::{Debug, Formatter};
use std::io;
use std::io::{BufRead, Error, ErrorKind, IoSliceMut, Read, Seek, SeekFrom};

use nom::{Err, Needed, Offset, Parser};
use thiserror::Error;

const DEFAULT_BUF_SIZE: usize = 256;
fn default_read_exact<R: Read + ?Sized>(this: &mut R, mut buf: &mut [u8]) -> io::Result<()> {
    while !buf.is_empty() {
        match this.read(buf) {
            Ok(0) => break,
            Ok(n) => {
                let tmp = buf;
                buf = &mut tmp[n..];
            }
            Err(ref e) if e.kind() == ErrorKind::Interrupted => {}
            Err(e) => return Err(e),
        }
    }
    if !buf.is_empty() {
        Err(Error::new(
            ErrorKind::UnexpectedEof,
            "failed to fill whole buffer",
        ))
    } else {
        Ok(())
    }
}
/// A buf reader implemented for use with nom. Adds a buffer to any reader.
pub struct BufReader<R> {
    inner: R,
    buf: Vec<u8>,
    pos: usize,
    cap: usize,
}

impl<R: Read> BufReader<R> {
    /// Creates a reader
    pub fn new(reader: R) -> Self {
        Self::with_capacity(reader, DEFAULT_BUF_SIZE)
    }

    /// Creates a new buf reader with a given size
    pub fn with_capacity(reader: R, capacity: usize) -> Self {
        Self {
            inner: reader,
            buf: vec![0_u8; capacity],
            pos: 0,
            cap: 0,
        }
    }
}

impl<R> BufReader<R> {
    /// Gets a reference to the underlying reader
    pub fn get_ref(&self) -> &R {
        &self.inner
    }

    /// Gets a mutable reference to the underlying reader
    pub fn get_mut(&mut self) -> &mut R {
        &mut self.inner
    }

    /// Gets the underlying buffer
    pub fn buffer(&self) -> &[u8] {
        &self.buf[self.pos..self.cap]
    }
    /// Invalidates all data in the internal buffer.
    #[inline]
    fn discard_buffer(&mut self) {
        self.pos = 0;
        self.cap = 0;
    }

    fn reset_buffer_position(&mut self) {
        if self.cap - self.pos > 0 {
            for i in 0..(self.cap - self.pos) {
                self.buf[i] = self.buf[self.pos + i];
            }
        }
        self.cap = self.cap - self.pos;
        self.pos = 0;
    }
}

impl<R: Seek> BufReader<R> {
    /// Seeks relative to the current position. If the new position lies within the buffer,
    /// the buffer will not be flushed, allowing for more efficient seeks.
    /// This method does not return the location of the underlying reader, so the caller
    /// must track this information themselves if it is required.
    pub fn seek_relative(&mut self, offset: i64) -> io::Result<()> {
        let pos = self.pos as u64;
        if offset < 0 {
            if let Some(new_pos) = pos.checked_sub((-offset) as u64) {
                self.pos = new_pos as usize;
                return Ok(());
            }
        } else {
            if let Some(new_pos) = pos.checked_add(offset as u64) {
                if new_pos <= self.cap as u64 {
                    self.pos = new_pos as usize;
                    return Ok(());
                }
            }
        }
        self.seek(SeekFrom::Current(offset)).map(drop)
    }
}

impl<R: Read> Read for BufReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // If we don't have any buffered data and we're doing a massive read
        // (larger than our internal buffer), bypass our internal buffer
        // entirely.
        if self.pos == self.cap && buf.len() >= self.buf.len() {
            self.discard_buffer();
            return self.inner.read(buf);
        }
        let nread = {
            let mut rem = self.fill_buf()?;
            rem.read(buf)?
        };
        self.consume(nread);
        Ok(nread)
    }

    // Small read_exacts from a BufReader are extremely common when used with a deserializer.
    // The default implementation calls read in a loop, which results in surprisingly poor code
    // generation for the common path where the buffer has enough bytes to fill the passed-in
    // buffer.
    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        if self.buffer().len() >= buf.len() {
            buf.copy_from_slice(&self.buffer()[..buf.len()]);
            self.consume(buf.len());
            return Ok(());
        }

        default_read_exact(self, buf)
    }

    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        let total_len = bufs.iter().map(|b| b.len()).sum::<usize>();
        if self.pos == self.cap && total_len >= self.buf.len() {
            self.discard_buffer();
            return self.inner.read_vectored(bufs);
        }
        let nread = {
            let mut rem = self.fill_buf()?;
            rem.read_vectored(bufs)?
        };
        self.consume(nread);
        Ok(nread)
    }
}

impl<R: Read> BufRead for BufReader<R> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        if self.cap == self.buf.len() {
            if self.pos == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::Interrupted,
                    "buffer completely filled",
                ));
            } else {
                self.reset_buffer_position();
            }
        }

        let read = self.inner.read(&mut self.buf[self.cap..])?;
        self.cap += read;
        Ok(&self.buf[self.pos..self.cap])
    }

    fn consume(&mut self, amt: usize) {
        self.pos = std::cmp::min(self.pos + amt, self.cap);
    }
}
impl<R: Seek> Seek for BufReader<R> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        let result: u64;
        if let SeekFrom::Current(n) = pos {
            let remainder = (self.cap - self.pos) as i64;
            // it should be safe to assume that remainder fits within an i64 as the alternative
            // means we managed to allocate 8 exbibytes and that's absurd.
            // But it's not out of the realm of possibility for some weird underlying reader to
            // support seeking by i64::MIN so we need to handle underflow when subtracting
            // remainder.
            if let Some(offset) = n.checked_sub(remainder) {
                result = self.inner.seek(SeekFrom::Current(offset))?;
            } else {
                // seek backwards by our remainder, and then by the offset
                self.inner.seek(SeekFrom::Current(-remainder))?;
                self.discard_buffer();
                result = self.inner.seek(SeekFrom::Current(n))?;
            }
        } else {
            // Seeking with Start/End doesn't care about our buffer length.
            result = self.inner.seek(pos)?;
        }
        self.discard_buffer();
        Ok(result)
    }

    fn stream_position(&mut self) -> io::Result<u64> {
        let remainder = (self.cap - self.pos) as u64;
        self.inner.stream_position().map(|pos| {
            pos.checked_sub(remainder).expect(
                "overflow when subtracting remaining buffer size from inner stream position",
            )
        })
    }
}

impl<R : Debug> Debug for BufReader<R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BufReader")
            .field("inner", &self.inner)
            .field("pos", &self.pos)
            .field("cap", &self.cap)
            .field("buffer_usage", &(self.pos.checked_div(self.cap)))
            .finish()
    }
}

pub trait Parse<O, E, P> {
    fn parse(&mut self, p: P) -> Result<O, ParseError<E>>
    where
            for<'a> P: Parser<&'a [u8], O, E>;
}

impl<R: Read, O, E, P> Parse<O, E, P> for BufReader<R> {
    fn parse(&mut self, mut p: P) -> Result<O, ParseError<E>>
    where
            for<'a> P: Parser<&'a [u8], O, E>,
    {
        let mut eof = false;
        let mut error = None;
        loop {
            let result = p.parse(self.buffer());
            let opt = match result {
                Err(Err::Error(e)) => {
                    return Err(ParseError::Error(e))
                }
                Err(Err::Failure(e)) => {
                    return Err(ParseError::Failure(e))
                }
                Err(Err::Incomplete(Needed::Size(e))) => {
                    if e.get() <= self.buffer().len() {
                        return Err(ParseError::UnexpectedStartOfToken(String::from_utf8_lossy(self.buffer()).chars().next().unwrap()));
                    }
                    None
                }
                Err(Err::Incomplete(Needed::Unknown)) => {
                    None
                }
                Ok((i, o)) => {
                    let offset = self.buffer().offset(i);
                    Some((offset, o))
                }
            };
            match opt {
                Some((sz, o)) => {
                    self.consume(sz);
                    return Ok(o);
                }
                None => {
                    if eof {
                        return Err(ParseError::Eof);
                    }
                    if let Some(e) = error.take() {
                        return Err(ParseError::Io(e));
                    }

                    let old_len = self.buffer().len();
                    match self.fill_buf() {
                        Ok(s) => {
                            if s.is_empty() || s.len() == old_len {
                                eof = true;
                            }
                        }
                        Err(e) => {
                            error = Some(e);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum ParseError<E> {
    #[error(transparent)]
    Error(E),
    #[error(transparent)]
    Failure(E),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("Unexpected Eof")]
    Eof,
    #[error("Unexpected input {0}")]
    UnexpectedStartOfToken(char),
}

