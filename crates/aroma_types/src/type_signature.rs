//! Type signatures

use crate::class::ClassInst;
use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::char;
use nom::combinator::{all_consuming, cut, map, map_res, value};
use nom::error::{context, VerboseError};
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, tuple};
use nom::{Finish, IResult};
use std::error::Error as StdError;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

/// A type signature
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeSignature {
    Void,
    Boolean,
    Byte,
    Int,
    Long,
    Float,
    Double,
    Invariant(ClassInst),
    Covariant(ClassInst),
    Contravariant(ClassInst),
    Function(Vec<TypeSignature>, Box<TypeSignature>),
    Array(Box<TypeSignature>),
}

impl Display for TypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSignature::Void => {
                write!(f, "V")
            }
            TypeSignature::Boolean => {
                write!(f, "Z")
            }
            TypeSignature::Byte => {
                write!(f, "B")
            }
            TypeSignature::Int => {
                write!(f, "I")
            }
            TypeSignature::Long => {
                write!(f, "J")
            }
            TypeSignature::Float => {
                write!(f, "F")
            }
            TypeSignature::Double => {
                write!(f, "D")
            }
            TypeSignature::Invariant(o) => {
                write!(f, "L{o};")
            }
            TypeSignature::Covariant(o) => {
                write!(f, "_{o};")
            }
            TypeSignature::Contravariant(o) => {
                write!(f, "^{o};")
            }
            TypeSignature::Function(args, ret) => {
                write!(
                    f,
                    "({}){ret}",
                    args.iter().map(ToString::to_string).join(",")
                )
            }
            TypeSignature::Array(a) => {
                write!(f, "[{a}")
            }
        }
    }
}

fn parse_type_signature(input: &str) -> IResult<&str, TypeSignature, VerboseError<&str>> {
    context(
        "parse_type_signature",
        alt((
            value(TypeSignature::Void, char('V')),
            value(TypeSignature::Boolean, char('Z')),
            value(TypeSignature::Byte, char('B')),
            value(TypeSignature::Int, char('I')),
            value(TypeSignature::Long, char('J')),
            value(TypeSignature::Float, char('F')),
            value(TypeSignature::Double, char('D')),
            map(
                map_res(delimited(char('L'), cut(is_not(";")), char(';')), |path| {
                    ClassInst::from_str(path)
                }),
                |t| TypeSignature::Invariant(t),
            ),
            map(
                map_res(delimited(char('^'), cut(is_not(";")), char(';')), |path| {
                    ClassInst::from_str(path)
                }),
                |t| TypeSignature::Contravariant(t),
            ),
            map(
                map_res(delimited(char('_'), cut(is_not(";")), char(';')), |path| {
                    ClassInst::from_str(path)
                }),
                |t| TypeSignature::Covariant(t),
            ),
            map(preceded(char('['), parse_type_signature), |ts| {
                TypeSignature::Array(Box::new(ts))
            }),
            map(
                tuple((
                    context(
                        "args",
                        delimited(
                            char('('),
                            cut(separated_list0(char(','), parse_type_signature)),
                            char(')'),
                        ),
                    ),
                    context("ret-type", parse_type_signature),
                )),
                |(args, ret)| TypeSignature::Function(args, Box::new(ret)),
            ),
        )),
    )(input)
}

impl FromStr for TypeSignature {
    type Err = IllegalTypeSignature;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(parse_type_signature)(s)
            .finish()
            .map(|(_, v)| v)
            .map_err(|e| {
                IllegalTypeSignature(
                    s.to_string(),
                    Box::new(VerboseError {
                        errors: e
                            .errors
                            .into_iter()
                            .map(|(i, e)| (i.to_string(), e))
                            .collect(),
                    }),
                )
            })
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Illegal type signature {0:?}: {1}")]
pub struct IllegalTypeSignature(String, Box<dyn StdError>);

#[cfg(test)]
mod tests {
    use crate::type_signature::TypeSignature;
    use std::str::FromStr;

    #[test]
    fn test_parse_object() {
        let p = "Laroma.system.Object;";
        let signature = TypeSignature::from_str(p).unwrap_or_else(|e| panic!("{}", e));
        assert!(
            matches!(signature, TypeSignature::Invariant(_)),
            "{signature:?}"
        )
    }

    #[test]
    fn test_parse_void_function() {
        let p = "(V)V";
        let signature = TypeSignature::from_str(p).unwrap_or_else(|e| panic!("{}", e));
        assert!(matches!(signature, TypeSignature::Function(_, f) if *f == TypeSignature::Void))
    }

    #[test]
    fn test_parse_complex_function() {
        let p = "(LClass<String>;,J)I";
        let signature = TypeSignature::from_str(p).unwrap_or_else(|e| panic!("{}", e));
        assert!(
            matches!(signature, TypeSignature::Function(args, f) if *f == TypeSignature::Int && args.len() == 2)
        )
    }
}
