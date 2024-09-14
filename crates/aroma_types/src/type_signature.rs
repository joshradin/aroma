//! Type signatures

use crate::class::{ClassInst, ClassRef};
use crate::functions::FunctionSignature;
use crate::hierarchy::intrinsics::{
    ARRAY_CLASS, BOOL_CLASS, F32_CLASS, F64_CLASS, I32_CLASS, I64_CLASS, U8_CLASS, VOID_CLASS,
};
use aroma_common::nom_helpers::recognize_identifier;
use aroma_tokens::spanned::{NoSpanError, Span, Spanned, TrySpanned};
use itertools::Itertools;
use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::{all_consuming, cut, map, map_res, opt, recognize, value};
use nom::error::{context, ErrorKind, FromExternalError, VerboseError};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, tuple};
use nom::{Finish, IResult};
use std::error::Error as StdError;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

/// A type signature
#[derive(Clone, Eq, PartialEq, Hash, Default)]
pub enum TypeSignature {
    Never,
    #[default]
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
    Function(Box<FunctionSignature>),
    Array(Box<TypeSignature>),
}

impl TypeSignature {
    /// Gets if this a real type, aka not [Covariant](TypeSignature::Covariant) or [Contravariant](TypeSignature::Contravariant).
    #[inline]
    pub fn is_real(&self) -> bool {
        !matches!(
            self,
            TypeSignature::Contravariant(..) | TypeSignature::Covariant(..)
        )
    }
    /// Gets if this an abstract type, aka [Covariant](TypeSignature::Covariant) or [Contravariant](TypeSignature::Contravariant).
    #[inline]
    pub fn is_abstract(&self) -> bool {
        !self.is_real()
    }
}

impl TrySpanned for TypeSignature {
    type Error = NoSpanError;

    fn try_span(&self) -> Result<Span, Self::Error> {
        match self {
            TypeSignature::Invariant(cls)
            | TypeSignature::Covariant(cls)
            | TypeSignature::Contravariant(cls) => {
                let span = cls.as_ref().as_ref().span();
                Ok(cls
                    .generics()
                    .iter()
                    .filter_map(|i| i.try_span().ok())
                    .fold(span, |mut accum, next| accum.join(next)))
            }
            TypeSignature::Function(function) => function.try_span(),
            TypeSignature::Array(array) => array.try_span(),
            _ => Err(NoSpanError),
        }
    }
}

impl Debug for TypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSignature::Never => {
                write!(f, "!")
            }
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
            TypeSignature::Invariant(class_inst) => {
                write!(f, "L{}", class_inst.as_ref())?;
                if !class_inst.generics().is_empty() {
                    write!(
                        f,
                        "[{}]",
                        class_inst
                            .generics()
                            .iter()
                            .map(|s| s.to_string())
                            .join(",")
                    )?;
                }
                write!(f, ";")
            }
            TypeSignature::Covariant(class_inst) => {
                write!(f, "_{}", class_inst.as_ref())?;
                if !class_inst.generics().is_empty() {
                    write!(
                        f,
                        "[{}]",
                        class_inst
                            .generics()
                            .iter()
                            .map(|s| s.to_string())
                            .join(",")
                    )?;
                }
                write!(f, ";")
            }
            TypeSignature::Contravariant(class_inst) => {
                write!(f, "^{}", class_inst.as_ref())?;
                if !class_inst.generics().is_empty() {
                    write!(
                        f,
                        "[{}]",
                        class_inst
                            .generics()
                            .iter()
                            .map(|s| s.to_string())
                            .join(",")
                    )?;
                }
                write!(f, ";")
            }
            TypeSignature::Function(f_sig) => {
                write!(
                    f,
                    "({}){}",
                    f_sig.parameters().iter().map(ToString::to_string).join(","),
                    f_sig.return_type()
                )
            }
            TypeSignature::Array(a) => {
                write!(f, "[{a}")
            }
        }
    }
}

impl Display for TypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl From<ClassInst> for TypeSignature {
    fn from(t: ClassInst) -> Self {
        TypeSignature::Invariant(ClassInst::with_generics(
            t.class_ref().clone(),
            t.generics().iter().cloned(),
        ))
    }
}

impl From<&TypeSignature> for ClassInst {
    fn from(value: &TypeSignature) -> Self {
        match value {
            TypeSignature::Never => ClassInst::from("never"),
            TypeSignature::Void => ClassInst::from(VOID_CLASS.get_ref()),
            TypeSignature::Boolean => ClassInst::from(BOOL_CLASS.get_ref()),
            TypeSignature::Byte => ClassInst::from(U8_CLASS.get_ref()),
            TypeSignature::Int => I32_CLASS.get_ref().into(),
            TypeSignature::Long => I64_CLASS.get_ref().into(),
            TypeSignature::Float => F32_CLASS.get_ref().into(),
            TypeSignature::Double => F64_CLASS.get_ref().into(),
            TypeSignature::Covariant(c)
            | TypeSignature::Contravariant(c)
            | TypeSignature::Invariant(c) => c.clone(),

            TypeSignature::Function(args) => {
                todo!("function representation as ClassInst")
            }
            TypeSignature::Array(a) => {
                ClassInst::with_generics(ARRAY_CLASS.get_ref(), [(**a).clone().into()])
            }
        }
    }
}

impl From<TypeSignature> for ClassInst {
    fn from(value: TypeSignature) -> Self {
        ClassInst::from(&value)
    }
}

fn parse_class_ts(input: &str) -> IResult<&str, TypeSignature, VerboseError<&str>> {
    let fqi = recognize(separated_list1(char('.'), recognize_identifier));
    map_res(
        tuple((
            fqi,
            opt(delimited(
                char('['),
                separated_list0(char(','), parse_type_signature),
                char(']'),
            )),
        )),
        |(fqi, generics)| -> Result<_, VerboseError<&str>> {
            let fqi = ClassRef::from_str(fqi)
                .map_err(|e| VerboseError::from_external_error(fqi, ErrorKind::Verify, e))?;
            Ok(match generics {
                Some(generics) => TypeSignature::Invariant(ClassInst::with_generics(fqi, generics)),
                None => TypeSignature::Invariant(ClassInst::new(fqi)),
            })
        },
    )(input)
}

fn parse_type_signature(input: &str) -> IResult<&str, TypeSignature, VerboseError<&str>> {
    context(
        "parse_type_signature",
        alt((
            value(TypeSignature::Never, char('!')),
            value(TypeSignature::Void, char('V')),
            value(TypeSignature::Boolean, char('Z')),
            value(TypeSignature::Byte, char('B')),
            value(TypeSignature::Int, char('I')),
            value(TypeSignature::Long, char('J')),
            value(TypeSignature::Float, char('F')),
            value(TypeSignature::Double, char('D')),
            delimited(char('L'), parse_class_ts, char(';')),
            map(delimited(char('^'), parse_class_ts, char(';')), |t| {
                let TypeSignature::Invariant(i) = t else {
                    panic!()
                };
                TypeSignature::Contravariant(i)
            }),
            map(delimited(char('_'), parse_class_ts, char(';')), |t| {
                let TypeSignature::Invariant(i) = t else {
                    panic!()
                };
                TypeSignature::Covariant(i)
            }),
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
                |(args, ret)| TypeSignature::Function(Box::new(
                    FunctionSignature::new(
                        [],
                        None,
                        args,
                        ret,
                        None
                    )
                )),
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
            matches!(signature, TypeSignature::Invariant(..)),
            "{signature:?}"
        )
    }

    #[test]
    fn test_parse_void_function() {
        let p = "(V)V";
        let signature = TypeSignature::from_str(p).unwrap_or_else(|e| panic!("{}", e));
        assert!(matches!(signature, TypeSignature::Function(f) if f.return_type() == &TypeSignature::Void))
    }

    #[test]
    fn test_parse_complex_function() {
        let p = "(LClass[_String;];,J)I";
        let signature = TypeSignature::from_str(p).unwrap_or_else(|e| panic!("{}", e));
        assert!(
            matches!(signature, TypeSignature::Function(f) if *f.return_type() == TypeSignature::Int && f.parameters().len() == 2)
        )
    }

    #[test]
    fn test_parse_never_function() {
        let p = "(LClass[LString;];,J)!";
        let signature = TypeSignature::from_str(p).unwrap_or_else(|e| panic!("{}", e));
        assert!(
            matches!(signature, TypeSignature::Function(f) if *f.return_type() == TypeSignature::Never && f.parameters().len() == 2)
        )
    }
}
