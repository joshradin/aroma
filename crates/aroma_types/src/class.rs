use crate::field::Field;
use crate::generic::{GenericDeclaration, GenericParameterBound, GenericParameterBounds};
use crate::method::Method;
use crate::vis::{Vis, Visibility};
use aroma_common::nom_helpers::identifier_parser;
use itertools::Itertools;
use nom::character::complete::char;
use nom::combinator::{all_consuming, map, opt, recognize};
use nom::error::ParseError;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, tuple};
use nom::{Finish, IResult, Parser};
use petgraph::visit::Walker;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ClassKind {
    Interface,
    Abstract,
    Concrete,
}

/// A class is the base definition used for describing a type.
#[derive(Debug, Clone)]
pub struct Class {
    vis: Vis,
    kind: ClassKind,
    name: String,
    generics: Vec<GenericDeclaration>,
    super_class: Option<ClassInst>,
    mixins: Vec<ClassInst>,
    fields: Vec<Field>,
    methods: Vec<Method>,
}

impl Class {
    /// Creates a new class
    pub fn new<G, S, M, F, Me>(
        vis: Vis,
        kind: ClassKind,
        name: impl AsRef<str>,
        generics: G,
        super_class: S,
        mixins: M,
        fields: F,
        methods: Me,
    ) -> Self
    where
        G: IntoIterator<Item = GenericDeclaration>,
        S: Into<Option<ClassInst>>,
        M: IntoIterator<Item = ClassInst>,
        F: IntoIterator<Item = Field>,
        Me: IntoIterator<Item = Method>,
    {
        Self {
            vis,
            kind,
            name: name.as_ref().to_string(),
            generics: generics.into_iter().collect(),
            super_class: super_class.into(),
            mixins: mixins.into_iter().collect(),
            fields: fields.into_iter().collect(),
            methods: methods.into_iter().collect(),
        }
    }

    /// Creates a new class
    pub fn with_self_reference<Fn>(name: impl AsRef<str>, cb: Fn) -> Self
    where
        Fn: FnOnce(ClassRef) -> Self,
    {
        let class_ref = ClassRef(name.as_ref().to_string());
        cb(class_ref)
    }

    /// Gets a ref to this class
    pub fn get_ref(&self) -> ClassRef {
        ClassRef(self.name.clone())
    }

    pub fn vis(&self) -> Vis {
        self.vis
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn generics(&self) -> &[GenericDeclaration] {
        &self.generics
    }

    pub fn super_class(&self) -> Option<&ClassInst> {
        self.super_class.as_ref()
    }

    pub fn mixins(&self) -> &[ClassInst] {
        &self.mixins
    }

    pub fn kind(&self) -> ClassKind {
        self.kind
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    /// Gets a mutable reference to the methods of this class.
    pub fn methods_mut(&mut self) -> &mut Vec<Method> {
        &mut self.methods
    }
}

impl AsClassRef for Class {
    fn as_class_ref(&self) -> ClassRef {
        self.get_ref()
    }
}

impl Visibility for Class {
    fn visibility(&self) -> Vis {
        self.vis
    }

    fn visibility_mut(&mut self) -> &mut Vis {
        &mut self.vis
    }
}

/// A reference to a class
#[derive(Debug, Eq, PartialEq, Hash, Clone, derive_more::Display)]
pub struct ClassRef(String);

impl From<String> for ClassRef {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for ClassRef {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl AsRef<str> for ClassRef {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Get a class ref
pub trait AsClassRef {
    fn as_class_ref(&self) -> ClassRef;
}

impl<I: Into<ClassRef> + Clone> AsClassRef for I {
    fn as_class_ref(&self) -> ClassRef {
        self.clone().into()
    }
}

/// A instantiate of a class
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct ClassInst(ClassRef, GenericParameterBounds);

impl ClassInst {
    /// Creates a new class reference
    pub fn with_generics<I: IntoIterator<Item = GenericParameterBound>>(
        name: ClassRef,
        bounds: I,
    ) -> Self {
        Self(name, bounds.into_iter().collect())
    }

    /// Creates a new class reference with no generics
    pub fn new(name: ClassRef) -> Self {
        Self(name, vec![])
    }

    /// Creates a new class reference with no generics
    pub fn new_generic_param(name: &str) -> Self {
        Self(ClassRef::from(name), vec![])
    }
    pub fn class_ref(&self) -> &ClassRef {
        &self.0
    }
    pub fn generics(&self) -> &[GenericParameterBound] {
        &self.1
    }

    /// A class instant is real if all parameter bounds are invariant
    #[inline]
    pub fn is_real(&self) -> bool {
        self.generics().iter().all(|s| s.is_invariant())
    }

    /// A class instant is abstract if any parameter bounds are not invariant
    #[inline]
    pub fn is_abstract(&self) -> bool {
        self.generics().iter().any(|s| !s.is_invariant())
    }
}

pub fn class_inst_parser<'a, E: ParseError<&'a str>>(v: &'a str) -> IResult<&str, ClassInst, E> {
    let fqi = recognize(separated_list1(char('.'), identifier_parser()));
    map(
        tuple((
            fqi,
            opt(delimited(
                char('['),
                separated_list0(char(','), class_inst_parser),
                char(']'),
            )),
        )),
        |(fqi, generics)| {
            let fqi = ClassRef::from(fqi);
            match generics {
                Some(generics) => ClassInst::with_generics(
                    fqi,
                    generics.into_iter().map(GenericParameterBound::Invariant),
                ),
                None => ClassInst::new(fqi),
            }
        },
    )(v)
}

impl FromStr for ClassInst {
    type Err = ParseClassInstError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(class_inst_parser)(s)
            .finish()
            .map_err(|nom::error::Error::<_> { code, input }| {
                ParseClassInstError(Box::new(nom::error::Error::<_> {
                    code,
                    input: input.to_string(),
                }))
            })
            .map(|(_, ret)| ret)
    }
}

impl AsRef<ClassRef> for ClassInst {
    fn as_ref(&self) -> &ClassRef {
        &self.0
    }
}

impl Debug for ClassInst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if !self.1.is_empty() {
            write!(f, "[{}]", self.1.iter().map(|s| format!("{s}")).join(","))?;
        }
        Ok(())
    }
}

impl Display for ClassInst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl From<ClassRef> for ClassInst {
    fn from(value: ClassRef) -> Self {
        ClassInst::new(value)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("An error occurred while parsing a class inst: {0}")]
pub struct ParseClassInstError(Box<dyn Error>);

#[cfg(test)]
mod tests {
    use crate::class::{ClassInst, ClassRef};
    use crate::generic::GenericParameterBound;

    #[test]
    fn test_parse_basic_class_inst() {
        let t = "aroma.system.Object";
        let class_inst: ClassInst = t.parse().expect("could not parse");
        assert_eq!(
            class_inst,
            ClassInst::new(ClassRef::from("aroma.system.Object"))
        )
    }

    #[test]
    fn test_parse_basic_class_inst_with_generics1() {
        let t = "aroma.system.Class<aroma.system.Object>";
        let class_inst: ClassInst = t.parse().expect("could not parse");
        assert_eq!(
            class_inst,
            ClassInst::with_generics(
                ClassRef::from("aroma.system.Class"),
                [GenericParameterBound::Invariant(
                    ClassRef::from("aroma.system.Object").into()
                )]
            )
        )
    }

    #[test]
    fn test_parse_basic_class_inst_with_generics2() {
        let t = "aroma.system.Tuple2<aroma.system.Object,aroma.system.Object>";
        let class_inst: ClassInst = t.parse().expect("could not parse");
        assert_eq!(
            class_inst,
            ClassInst::with_generics(
                ClassRef::from("aroma.system.Tuple2"),
                [
                    GenericParameterBound::Invariant(ClassRef::from("aroma.system.Object").into()),
                    GenericParameterBound::Invariant(ClassRef::from("aroma.system.Object").into())
                ]
            )
        )
    }
}
