//! bindings of id to type

use crate::parser;
use crate::parser::blocking::BlockingParser;
use crate::parser::hir::singletons::{
    Colon, Comma, In, LBracket, LParen, Out, QMark, RBracket, RParen, VarId,
};
use crate::parser::hir::{cut, ErrorKind, Punctuated0, Punctuated1, SyntaxError};
use crate::parser::hir_parser::blocking::{CouldParse, Parsable};
use crate::parser::SyntaxResult;
use aroma_ast::typed::TypeError;
use aroma_tokens::id::Id;
use aroma_tokens::token::{ToTokens, TokenKind};
use aroma_types::class::{ClassInst, ClassRef};
use aroma_types::hierarchy::intrinsics::OBJECT_CLASS;
use aroma_types::type_signature::TypeSignature;
use std::io::Read;
use std::str::FromStr;

/// A binding between an id to a type
#[derive(Debug, ToTokens)]
pub struct Binding {
    pub id: VarId,
    pub type_dec: TypeDec,
}

impl Parsable for Binding {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let id = parser.parse(VarId::parse)?;
        let type_dec = parser.parse(TypeDec::parse)?;
        Ok(Self { id, type_dec })
    }
}

/// A binding between an id to an optional
#[derive(Debug, ToTokens)]
pub struct OptTypeBinding {
    pub id: VarId,
    pub type_dec: Option<TypeDec>,
}

impl Parsable for OptTypeBinding {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let id = parser.parse(VarId::parse)?;
        let type_dec = parser.try_parse(TypeDec::parse)?;
        Ok(Self { id, type_dec })
    }
}

#[derive(Debug, ToTokens)]
pub struct TypeDec {
    pub colon: Colon,
    pub ty: Type,
}

impl Parsable for TypeDec {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let colon = parser.parse(Colon::parse)?;
        let ty = parser.parse(cut(Type::parse))?;
        Ok(Self { colon, ty })
    }
}

impl CouldParse for TypeDec {
    fn could_parse<R: Read>(
        parser: &mut BlockingParser<R>,
    ) -> Result<bool, parser::Err<Self::Err>> {
        Ok(matches!(parser.peek()?, Some(tok) if matches!(tok.kind(), TokenKind::Colon)))
    }
}

#[derive(Debug, ToTokens)]
pub struct ClassType {
    /// the main id of the type
    pub id: Id,
    /// Generics
    pub generics: Option<GenericParameters>,
    /// If present, this type is nullable
    pub nullable: Option<QMark>,
}

/// Type signature
#[derive(Debug, ToTokens)]
pub enum Type {
    Class(ClassType),
    Function(),
}

impl CouldParse for Type {
    fn could_parse<R: Read>(
        parser: &mut BlockingParser<R>,
    ) -> Result<bool, parser::Err<Self::Err>> {
        Ok(matches!(parser.peek()?, Some(tok) if matches!(tok.kind(), TokenKind::Identifier(_))))
    }
}

impl Type {
    /// Converts this into a class inst
    pub fn as_class_inst(&self) -> Option<ClassInst> {
        match self {
            Type::Class(class_type) => Some(ClassInst::with_generics(
                ClassRef::from(class_type.id.clone()),
                class_type
                    .generics
                    .as_ref()
                    .map(|i| {
                        i.bounds
                            .punctuated
                            .iter()
                            .map(|(param, _)| {
                                let bound = param
                                    .bound
                                    .as_class_inst()
                                    .unwrap_or_else(|| OBJECT_CLASS.generic_inst());
                                match param.variance {
                                    None => TypeSignature::Invariant(bound),
                                    Some(Variance::In(_)) => TypeSignature::Contravariant(bound),
                                    Some(Variance::Out(_)) => TypeSignature::Covariant(bound),
                                }
                            })
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default(),
            )),
            Type::Function() => None,
        }
    }
    /// Converts this into a type signature
    #[inline]
    pub fn as_type_signature(&self) -> TypeSignature {
        match self {
            Type::Class(class_type) => {
                let class_inst = ClassInst::with_generics(
                    ClassRef::from(class_type.id.clone()),
                    class_type
                        .generics
                        .as_ref()
                        .map(|i| {
                            i.bounds
                                .punctuated
                                .iter()
                                .map(|(param, _)| {
                                    let bound = param
                                        .bound
                                        .as_class_inst()
                                        .unwrap_or_else(|| OBJECT_CLASS.generic_inst());
                                    match param.variance {
                                        None => TypeSignature::Invariant(bound),
                                        Some(Variance::In(_)) => {
                                            TypeSignature::Contravariant(bound)
                                        }
                                        Some(Variance::Out(_)) => TypeSignature::Covariant(bound),
                                    }
                                })
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default(),
                );
                TypeSignature::from(class_inst)
            }
            Type::Function() => {
                todo!("complex function binding")
            }
        }
    }
}

impl TryFrom<Type> for ClassInst {
    type Error = TypeError;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        value
            .as_class_inst()
            .ok_or(TypeError::NotClassRepresentable)
    }
}

impl From<Type> for TypeSignature {
    fn from(value: Type) -> Self {
        value.as_type_signature()
    }
}

impl Parsable for Type {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let id = parser.parse(Id::parse)?;
        let generics = if GenericParameters::could_parse(parser)? {
            Some(parser.parse(GenericParameters::parse)?)
        } else {
            None
        };
        let qmark = parser.parse_opt::<QMark>()?;

        Ok(Type::Class(ClassType {
            id,
            generics,
            nullable: qmark,
        }))
    }
}

#[derive(Debug, ToTokens)]
pub struct GenericParameters {
    pub lbracket: LBracket,
    pub bounds: Punctuated0<GenericParameter, Comma>,
    pub rbracket: RBracket,
}

impl Parsable for GenericParameters {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let lbracket = parser.parse(LBracket::parse)?;
        let generics = {
            if !RBracket::could_parse(parser)? {
                let p = parser.parse(Punctuated1::parse)?;
                p.into()
            } else {
                Punctuated0::default()
            }
        };
        let rbracket = parser.parse(RBracket::parse)?;

        Ok(GenericParameters {
            lbracket,
            bounds: generics,
            rbracket,
        })
    }
}

impl CouldParse for GenericParameters {
    fn could_parse<R: Read>(
        parser: &mut BlockingParser<R>,
    ) -> Result<bool, crate::parser::Err<Self::Err>> {
        Ok(matches!(parser.peek()?, Some(tok) if matches!(tok.kind(), TokenKind::LBracket)))
    }
}

/// A generic parameter
#[derive(Debug, ToTokens)]
pub struct GenericParameter {
    pub variance: Option<Variance>,
    pub bound: Type,
}

impl Parsable for GenericParameter {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let variance =
            parser.try_parse(|parser: &mut BlockingParser<R>| match parser.peek()? {
                Some(tok) if matches!(tok.kind(), TokenKind::In) => {
                    parser.parse(cut(In::parse)).map(Variance::In)
                }
                Some(tok) if matches!(tok.kind(), TokenKind::Out) => {
                    parser.parse(cut(Out::parse)).map(Variance::Out)
                }
                _ => Err(parser.error(ErrorKind::UnexpectedEof, None)),
            })?;
        let bound = parser.parse(Type::parse)?;
        Ok(GenericParameter { variance, bound })
    }
}

/// Generic variance
#[derive(Debug, ToTokens)]
pub enum Variance {
    In(In),
    Out(Out),
}

/// Function parameters
#[derive(Debug, ToTokens)]
pub struct FnParameters {
    pub lparen: LParen,
    pub parameters: Punctuated0<Binding, Comma>,
    pub rparen: RParen,
}

impl Parsable for FnParameters {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let lparen = parser.parse(LParen::parse)?;
        let parameters = if RParen::could_parse(parser)? {
            Punctuated0::default()
        } else {
            Punctuated0::from(parser.parse(Punctuated1::parse)?)
        };
        let rparen = parser.parse(RParen::parse)?;
        Ok(FnParameters {
            lparen,
            parameters,
            rparen,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::hir::binding::{Binding, FnParameters, OptTypeBinding, Type};
    use crate::parser::hir_parser::blocking::Parsable;
    use crate::parser::hir_parser::tests::test_parser;
    use aroma_tokens::token::ToTokens;
    use test_log::test;

    #[test]
    fn parse_basic_type() {
        test_parser("Int", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
        })
    }

    #[test]
    fn parse_complex_type() {
        test_parser("Class[in Int, out Int[Float]]", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }

    #[test]
    fn parse_function_type() {
        test_parser("fn()", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }

    #[test]
    fn parse_function_type_with_return() {
        test_parser("fn() -> Int", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }

    #[test]
    fn parse_function_type_with_parameters() {
        test_parser("fn(Int, Int)", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }

    #[test]
    fn parse_function_type_with_parameters_and_return() {
        test_parser("fn(Int, Int) -> Int", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }

    #[test]
    fn parse_closure_type() {
        test_parser("Int.fn()", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }
    #[test]
    fn parse_closure_type_with_return() {
        test_parser("Int.fn() -> Int", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }
    #[test]
    fn parse_closure_type_with_parameters() {
        test_parser("Int.fn(Int)", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }
    #[test]
    fn parse_closure_type_with_parameters_and_return() {
        test_parser("Int.fn(Int, Int) -> Int", |parser, _| {
            let type_ast = parser.parse(Type::parse).expect("could not parse");
            println!(
                "type_ast: {type_ast:#?}, ts={}",
                type_ast.as_type_signature()
            );
        })
    }

    #[test]
    fn parse_binding() {
        test_parser("x: Class[out Int]", |parser, _| {
            let type_dec = parser.parse(Binding::parse).expect("could not parse");
            println!(
                "type_ast: {type_dec:#?}, ts={}",
                type_dec.type_dec.ty.as_type_signature()
            );
        })
    }

    #[test]
    fn parse_type_opt_binding() {
        test_parser("x", |parser, _| {
            let type_dec = parser
                .parse(OptTypeBinding::parse)
                .expect("could not parse");
            assert!(type_dec.type_dec.is_none());
        });
        test_parser("x: a", |parser, _| {
            let type_dec = parser
                .parse(OptTypeBinding::parse)
                .expect("could not parse");
            assert!(type_dec.type_dec.is_some());
            println!("{:#?}", type_dec.to_token_tree());
        });
    }

    #[test]
    fn parse_fn_parameters() {
        test_parser("()", |parser, _| {
            let fn_parameters = parser
                .parse(FnParameters::parse)
                .unwrap_or_else(|e| panic!("{e}"));
            assert_eq!(fn_parameters.parameters.punctuated.len(), 0);
        });
        test_parser("(x: a)", |parser, _| {
            let fn_parameters = parser
                .parse(FnParameters::parse)
                .unwrap_or_else(|e| panic!("{e}"));
            assert_eq!(fn_parameters.parameters.punctuated.len(), 1);
        });
        test_parser("(x: a, y: b)", |parser, _| {
            let fn_parameters = parser
                .parse(FnParameters::parse)
                .unwrap_or_else(|e| panic!("{e}"));
            assert_eq!(fn_parameters.parameters.punctuated.len(), 2);
        });
    }
}
