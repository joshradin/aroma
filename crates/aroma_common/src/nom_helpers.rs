//! nom helpers

use nom::character::complete::{alpha1, alphanumeric0};
use nom::combinator::{opt, peek, recognize};
use nom::error::ParseError;
use nom::sequence::{preceded, tuple};
use nom::{AsChar, IResult, InputLength, InputTakeAtPosition, Offset, Parser, Slice, Err};
use std::ops::RangeTo;

pub fn identifier_parser<I, E>() -> impl FnMut(I) -> IResult<I, I, E>
where
    I: Clone + Offset + Slice<RangeTo<usize>> + InputTakeAtPosition<Item: AsChar>,
    E: ParseError<I>,
{
    recognize(tuple((alpha1, alphanumeric0)))
}

