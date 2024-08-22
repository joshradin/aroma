//! nom helpers

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1};
use nom::combinator::recognize;
use nom::error::ParseError;
use nom::multi::many0_count;
use nom::sequence::pair;
use nom::{AsChar, IResult, InputLength, InputTake, InputTakeAtPosition, Offset, Parser, Slice};
use std::ops::{RangeFrom, RangeTo};

pub fn recognize_identifier<'a, E : ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E>
{
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

#[cfg(test)]
mod tests {
    use crate::nom_helpers::recognize_identifier;
    use nom::Finish;

    #[test]
    fn test_one_letter_variable() {
        let id = "i ";
        let (rest, parsed) = recognize_identifier::<nom::error::Error<_>>(id)
            .finish()
            .unwrap();
        assert_eq!(parsed, "i");
    }
}
