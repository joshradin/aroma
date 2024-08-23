//! bindings of id to type

use crate::parser;
use crate::parser::singletons::{
    Colon, Comma, In, LBracket, LParen, Out, QMark, RBracket, RParen, VarId,
};
use crate::parser::{
    cut, CouldParse, ErrorKind, Parsable, Punctuated0, Punctuated1, SyntacticParser, SyntaxError, SyntaxResult
};
use aroma_ast::id::Id;
use aroma_ast::token::{ToTokens, TokenKind};
use aroma_types::class::{ClassInst, ClassRef};
use aroma_types::generic::GenericParameterBound;
use aroma_types::type_signature::TypeSignature;
use std::io::Read;
use std::result;

/// A binding between an id to a type
#[derive(Debug, ToTokens)]
pub struct Binding {
    pub id: VarId,
    pub type_dec: TypeDec,
}

impl Parsable for Binding {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
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

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
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

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let colon = parser.parse(Colon::parse)?;
        let ty = parser.parse(cut(Type::parse))?;
        Ok(Self { colon, ty })
    }
}

impl CouldParse for TypeDec {
    fn could_parse<R: Read>(
        parser: &mut SyntacticParser<R>,
    ) -> Result<bool, parser::Err<Self::Err>> {
        Ok(matches!(parser.peek()?, Some(tok) if matches!(tok.kind(), TokenKind::Colon)))
    }
}

/// Type signature
#[derive(Debug, ToTokens)]
pub struct Type {
    /// the main id of the type
    pub id: Id,
    /// Generics
    pub generics: Option<GenericParameters>,
    /// If present, this type is nullable
    pub nullable: Option<QMark>,
}

impl CouldParse for Type {
    fn could_parse<R: Read>(
        parser: &mut SyntacticParser<R>,
    ) -> Result<bool, parser::Err<Self::Err>> {
        Ok(matches!(parser.peek()?, Some(tok) if matches!(tok.kind(), TokenKind::Identifier(_))))
    }
}

impl Type {
    /// Converts this into a class inst
    pub fn as_class_inst(&self) -> ClassInst {
        ClassInst::with_generics(
            ClassRef::from(self.id.to_string()),
            self.generics
                .as_ref()
                .map(|i| {
                    i.bounds
                        .punctuated
                        .iter()
                        .map(|(param, _)| {
                            let bound = param.bound.as_class_inst();
                            match param.variance {
                                None => GenericParameterBound::Invariant(bound),
                                Some(Variance::In(_)) => {
                                    GenericParameterBound::Contravariant(bound)
                                }
                                Some(Variance::Out(_)) => GenericParameterBound::Covariant(bound),
                            }
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default(),
        )
    }
    /// Converts this into a type signature
    #[inline]
    pub fn as_type_signature(&self) -> TypeSignature {
        TypeSignature::from(self.as_class_inst())
    }
}

impl From<Type> for ClassInst {
    fn from(value: Type) -> Self {
        value.as_class_inst()
    }
}

impl From<Type> for TypeSignature {
    fn from(value: Type) -> Self {
        value.as_type_signature()
    }
}

impl Parsable for Type {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let id = parser.parse(Id::parse)?;
        let generics = if GenericParameters::could_parse(parser)? {
            Some(parser.parse(GenericParameters::parse)?)
        } else {
            None
        };
        let qmark = parser.parse_opt::<QMark>()?;

        Ok(Type {
            id,
            generics,
            nullable: qmark,
        })
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

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'_, R>,
    ) -> SyntaxResult<Self> {
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
        parser: &mut SyntacticParser<R>,
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

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let variance =
            parser.try_parse(|parser: &mut SyntacticParser<R>| match parser.peek()? {
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

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
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
    use crate::parser::binding::{Binding, FnParameters, OptTypeBinding, Type};
    use crate::parser::syntactic_parser::tests::test_parser;
    use crate::parser::Parsable;
    use aroma_ast::token::ToTokens;
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
