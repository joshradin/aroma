use std::str::FromStr;

use eyre::eyre;
use nom::branch::alt;
use nom::bytes::streaming::{is_not, tag, take_until, take_while_m_n};
use nom::character::complete::hex_digit1;
use nom::character::streaming::{
    alpha1, alphanumeric0, char, digit1, multispace1, newline, space1,
};
use nom::combinator::{consumed, cut, eof, map, map_opt, map_res, not, recognize, value, verify};
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{IResult, Parser};

use crate::frontend::token::TokenKind;

type Result<'a, O, E = &'a [u8]> = IResult<&'a [u8], O, nom::error::Error<E>>;

pub fn parse_token(src: &[u8]) -> Result<(usize, TokenKind), String> {
    map(
        consumed(delimited(
            parse_insignificant,
            _parse_token,
            parse_insignificant,
        )),
        |(consumed, token)| (consumed.len(), token),
    )(src)
    .map_err(|e| e.map_input(|e| String::from_utf8_lossy(e).as_ref().to_owned()))
}

fn _parse_token(src: &[u8]) -> Result<TokenKind> {
    alt((
        parse_eof,
        parse_word,
        parse_punctuation,
        parse_operator,
        parse_newline,
    ))(src)
}

fn parse_eof(src: &[u8]) -> Result<TokenKind> {
    value(TokenKind::Eof, eof)(src)
}

fn parse_operator(src: &[u8]) -> Result<TokenKind> {
    alt((
        value(TokenKind::Eq, tag("==")),
        value(TokenKind::Assign, char('=')),
    ))(src)
}

fn parse_word(src: &[u8]) -> Result<TokenKind, &[u8]> {
    alt((
        parse_keyword,
        parse_identifier,
        parse_newline,
        parse_operator,
        parse_literal,
    ))(src)
}

fn parse_keyword(src: &[u8]) -> Result<TokenKind> {
    alt((
        value(TokenKind::If, tag("if")),
        value(TokenKind::Else, tag("else")),
        value(TokenKind::While, tag("while")),
        value(TokenKind::For, tag("for")),
        value(TokenKind::Const, tag("const")),
        value(TokenKind::Let, tag("let")),
    ))(src)
}

fn parse_identifier(src: &[u8]) -> Result<TokenKind> {
    let id_parser = recognize(tuple((alpha1, alphanumeric0)));

    map(id_parser, |id: &[u8]| {
        TokenKind::Identifier(
            String::from_utf8(Vec::from(id)).expect("only [0-0a-zA-Z] should be here"),
        )
    })(src)
}

fn parse_newline(src: &[u8]) -> Result<TokenKind, &[u8]> {
    value(TokenKind::Nl, newline)(src)
}

fn parse_punctuation(src: &[u8]) -> Result<TokenKind, &[u8]> {
    alt((
        value(TokenKind::Colon, tag(":")),
        value(TokenKind::SemiColon, tag(";")),
    ))(src)
}

fn parse_literal(src: &[u8]) -> Result<TokenKind, &[u8]> {
    alt((
        map(parse_hexadecimal_value, |hex| TokenKind::Integer(hex)),
        map(parse_integer_value, |hex| TokenKind::Integer(hex)),
    ))(src)
}

fn parse_hexadecimal_value(input: &[u8]) -> IResult<&[u8], i64> {
    map_res(
        preceded(
            alt((tag("0x"), tag("0X"))),
            cut(recognize(many1(terminated(hex_digit1, many0(char('_')))))),
        ),
        |out: &[u8]| {
            std::str::from_utf8(out)
                .map_err(|s| eyre!(s))
                .and_then(|s| i64::from_str_radix(s, 16).map_err(|e| eyre!(e)))
        },
    )(input)
}

fn parse_integer_value(input: &[u8]) -> IResult<&[u8], i64> {
    map_res(
        recognize(many1(terminated(digit1, many0(char('_'))))),
        |out: &[u8]| {
            std::str::from_utf8(out)
                .map_err(|s| eyre!(s))
                .and_then(|s| i64::from_str(s).map_err(|e| eyre!(e)))
        },
    )(input)
}

#[derive(Debug, Clone)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWs,
}

fn parse_literal_str(input: &[u8]) -> Result<&str> {
    let not_quoted = map_res(is_not("\"\\"), |s: &[u8]| std::str::from_utf8(s));
    verify(not_quoted, |s: &str| s.is_empty())(input)
}

fn parse_unicode(input: &[u8]) -> Result<std::primitive::char> {
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c| char::from(c).is_ascii_hexdigit());

    // `preceded` takes a prefix parser, and if it succeeds, returns the result
    // of the body parser. In this case, it parses u{XXXX}.
    let parse_delimited_hex = preceded(
        char('u'),
        // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
        // It returns the result of the middle parser. In this case, it parses
        // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
        delimited(char('{'), parse_hex, char('}')),
    );

    // `map_res` takes the result of a parser and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to a u32.
    let parse_u32 = map_res(
        map_res(parse_delimited_hex, |s| std::str::from_utf8(s)),
        move |hex| u32::from_str_radix(hex, 16),
    );

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, std::char::from_u32).parse(input)
}
fn parse_escaped_char(input: &[u8]) -> Result<std::primitive::char> {
    preceded(
        char('\\'),
        // `alt` tries each parser in sequence, returning the result of
        // the first successful match
        alt((
            parse_unicode,
            // The `value` parser returns a fixed value (the first argument) if its
            // parser (the second argument) succeeds. In these cases, it looks for
            // the marker characters (n, r, t, etc) and returns the matching
            // character (\n, \r, \t, etc).
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )
    .parse(input)
}

fn parse_escaped_whitespace(input: &[u8]) -> Result<&[u8]> {
    preceded(char('\\'), multispace1)(input)
}

fn parse_string_fragment(input: &[u8]) -> Result<StringFragment> {
    alt((
        map(parse_literal_str, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWs, parse_escaped_whitespace),
    ))(input)
}

fn parse_string_value(input: &[u8]) -> Result<String> {
    let build_string = fold_many0(
        parse_string_fragment,
        String::new,
        |accum, next| match next {
            StringFragment::Literal(l) => {
                format!("{accum}{l}")
            }
            StringFragment::EscapedChar(c) => {
                format!("{accum}{c}")
            }
            StringFragment::EscapedWs => accum,
        },
    );

    delimited(char('"'), cut(build_string), char('"'))(input)
}

fn parse_insignificant(src: &[u8]) -> Result<&[u8], &[u8]> {
    recognize(many0(alt((
        space1,
        recognize(delimited(tag("/*"), take_until("*/"), tag("*/"))),
        recognize(tuple((tag("//"), many0(not(newline))))),
    ))))(src)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token() {
        let (rest, token) = dbg!(_parse_token(b" /* */   if  \n")).expect("could not parse");
    }
}
