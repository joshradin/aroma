use crate::constructor::Constructor;
use crate::field::Field;
use crate::functions::FunctionDeclaration;
use crate::generic::{GenericDeclaration, GenericParameterBounds, GenericParameterBoundsSlice};
use crate::vis::{Vis, Visibility};
use aroma_common::nom_helpers::recognize_identifier;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::{Span, Spanned, TrySpanned};
use itertools::Itertools;
use nom::character::complete::char;
use nom::combinator::{all_consuming, map, map_res, opt, recognize};
use nom::error::{ErrorKind, FromExternalError, ParseError};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, tuple};
use nom::{Finish, IResult, Parser};
use petgraph::visit::Walker;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;
use crate::type_signature::TypeSignature;

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
    id: Id,
    generics: Vec<GenericDeclaration>,
    super_class: Option<ClassInst>,
    mixins: Vec<ClassInst>,
    fields: Vec<Field>,
    methods: Vec<FunctionDeclaration>,
    constructors: Vec<Constructor>,
    sub_classes: Vec<Class>,
}

impl Class {
    /// Creates a new class
    pub fn new<G, S, M, F, Me, Co, Sub>(
        vis: Vis,
        kind: ClassKind,
        id: Id,
        generics: G,
        super_class: S,
        mixins: M,
        fields: F,
        methods: Me,
        constructors: Co,
        sub_classes: Sub,
    ) -> Self
    where
        G: IntoIterator<Item = GenericDeclaration>,
        S: Into<Option<ClassInst>>,
        M: IntoIterator<Item = ClassInst>,
        F: IntoIterator<Item = Field>,
        Me: IntoIterator<Item = FunctionDeclaration>,
        Co: IntoIterator<Item = Constructor>,
        Sub: IntoIterator<Item = Class>,
    {
        Self {
            vis,
            kind,
            id: id,
            generics: generics.into_iter().collect(),
            super_class: super_class.into(),
            mixins: mixins.into_iter().collect(),
            fields: fields.into_iter().collect(),
            methods: methods.into_iter().collect(),
            constructors: constructors.into_iter().collect(),
            sub_classes: sub_classes.into_iter().collect(),
        }
    }

    /// Creates a new class
    pub fn with_self_reference<Fn>(name: impl AsRef<str>, cb: Fn) -> Self
    where
        Fn: FnOnce(ClassRef) -> Self,
    {
        let class_ref = ClassRef(Id::new_call_site([name.as_ref().to_string()]).unwrap());
        cb(class_ref)
    }

    /// Gets a ref to this class
    pub fn get_ref(&self) -> ClassRef {
        ClassRef(self.id.clone())
    }

    /// Checks if this is a mixin class
    #[inline]
    pub fn is_mixin(&self) -> bool {
        self.kind() == ClassKind::Interface
    }

    pub fn vis(&self) -> Vis {
        self.vis
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn generics(&self) -> &[GenericDeclaration] {
        &self.generics
    }

    pub fn super_class(&self) -> Option<&ClassInst> {
        self.super_class.as_ref()
    }
    pub fn super_class_mut(&mut self) -> Option<&mut ClassInst> {
        self.super_class.as_mut()
    }

    pub fn mixins(&self) -> &[ClassInst] {
        &self.mixins
    }
    pub fn mixins_mut(&mut self) -> &mut Vec<ClassInst> {
        &mut self.mixins
    }

    pub fn kind(&self) -> ClassKind {
        self.kind
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    /// Gets a mutable reference to the fields of this type
    pub fn fields_mut(&mut self) -> &mut Vec<Field> {
        &mut self.fields
    }

    pub fn methods(&self) -> &[FunctionDeclaration] {
        &self.methods
    }

    /// Gets a mutable reference to the methods of this class.
    pub fn methods_mut(&mut self) -> &mut Vec<FunctionDeclaration> {
        &mut self.methods
    }

    /// Creates a class inst where all generics are just their bounds
    pub fn generic_inst(&self) -> ClassInst {
        ClassInst::with_generics(
            self.get_ref(),
            self.generics
                .iter()
                .map(|i|
                    i.bound().clone()
                ),
        )
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

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}[{}]",
            self.as_class_ref(),
            self.generics().iter().join(",")
        )
    }
}

/// A reference to a class
#[derive(Debug, Eq, PartialEq, Hash, Clone, derive_more::Display)]
pub struct ClassRef(Id);

impl From<Id> for ClassRef {
    fn from(value: Id) -> Self {
        Self(value)
    }
}

impl AsRef<Id> for ClassRef {
    fn as_ref(&self) -> &Id {
        &self.0
    }
}

impl AsMut<Id> for ClassRef {
    fn as_mut(&mut self) -> &mut Id {
        &mut self.0
    }
}

impl FromStr for ClassRef {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Id::from_str(s).map(ClassRef)
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
    pub fn with_generics<I: IntoIterator<Item = TypeSignature>>(
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
        Self(ClassRef::from(Id::from(name)), vec![])
    }
    pub fn class_ref(&self) -> &ClassRef {
        &self.0
    }
    pub fn class_ref_mut(&mut self) -> &mut ClassRef {
        &mut self.0
    }

    pub fn generics(&self) -> &GenericParameterBoundsSlice {
        &self.1
    }
    pub fn generics_mut(&mut self) -> &mut GenericParameterBounds {
        &mut self.1
    }

    /// A class instant is real if all parameter bounds are invariant
    #[inline]
    pub fn is_real(&self) -> bool {
        self.generics().iter().all(|s| s.is_real())
    }

    /// A class instant is abstract if any parameter bounds are not invariant
    #[inline]
    pub fn is_abstract(&self) -> bool {
        self.generics().iter().any(|s| s.is_abstract())
    }
}

impl Spanned for ClassInst {
    fn span(&self) -> Span {
        let mut span = self.0 .0.span();
        for generic in self.generics() {
            if let Ok(inner) = generic.try_span() {
                span = span.join(inner);
            }
        }
        span
    }
}

pub fn class_inst_parser<'a, E: ParseError<&'a str> + FromExternalError<&'a str, E>>(
    v: &'a str,
) -> IResult<&str, ClassInst, E> {
    let fqi = map_res(
        recognize(separated_list1(char('.'), recognize_identifier)),
        |id_str| Id::from_str(id_str).map_err(|e| E::from_error_kind(id_str, ErrorKind::Verify)),
    );
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
                    generics.into_iter().map(|g| g.into()),
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

impl From<&str> for ClassInst {
    fn from(value: &str) -> Self {
        ClassInst::new(ClassRef::from(Id::from(value)))
    }
}

#[derive(Debug, thiserror::Error)]
#[error("An error occurred while parsing a class inst: {0}")]
pub struct ParseClassInstError(Box<dyn Error>);

#[cfg(test)]
mod tests {
    use crate::class::{ClassInst, ClassRef};
    use std::str::FromStr;
    use crate::type_signature::TypeSignature;

    #[test]
    fn test_parse_basic_class_inst() {
        let t = "aroma.system.Object";
        let class_inst: ClassInst = t.parse().expect("could not parse");
        assert_eq!(
            class_inst,
            ClassInst::from_str("aroma.system.Object").unwrap()
        )
    }

    #[test]
    fn test_parse_basic_class_inst_with_generics1() {
        let t = "aroma.system.Class[aroma.system.Object]";
        let class_inst: ClassInst = t.parse().expect("could not parse");
        assert_eq!(
            class_inst,
            ClassInst::with_generics(
                ClassRef::from_str("aroma.system.Class").unwrap(),
                [TypeSignature::Invariant(
                    ClassRef::from_str("aroma.system.Object").unwrap().into()
                )]
            )
        )
    }

    #[test]
    fn test_parse_basic_class_inst_with_generics2() {
        let t = "aroma.system.Tuple2[aroma.system.Object,aroma.system.Object]";
        let class_inst: ClassInst = t.parse().expect("could not parse");
        assert_eq!(
            class_inst,
            ClassInst::with_generics(
                ClassRef::from_str("aroma.system.Tuple2").unwrap(),
                [
                    TypeSignature::Invariant(
                        ClassRef::from_str("aroma.system.Object").unwrap().into()
                    ),
                    TypeSignature::Invariant(
                        ClassRef::from_str("aroma.system.Object").unwrap().into()
                    )
                ]
            )
        )
    }
}
