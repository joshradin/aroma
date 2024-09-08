use aroma_ast::items::ClassItem;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::Spanned;
use aroma_types::class::{AsClassRef, Class, ClassInst, ClassKind, ClassRef};
use aroma_types::field::Field;
use aroma_types::generic::{GenericDeclaration, GenericParameterBound};
use aroma_types::hierarchy::intrinsics::OBJECT_CLASS;
use crate::parser::{items, ErrorKind, Punctuated, SyntaxError};
use crate::parser::syntactic_parser::hir::items::{ClassField, ClassMember};
use crate::parser::syntactic_parser::transforms::to_mir::method_hir_to_mir;
use crate::parser::transforms::to_mir;
pub fn class_hir_to_mir(
    namespace: Option<&Id>,
    cls: items::ItemClass,
) -> Result<ClassItem, SyntaxError> {
    let id = match namespace {
        None => cls.ident.id.clone(),
        Some(namespace) => namespace.resolve(&cls.ident.id),
    };
    let span = cls.span();
    let vis = to_mir::vis_hir_to_mir(cls.vis);
    let class_kind = cls
        .abstract_tok
        .as_ref()
        .map(|a| ClassKind::Abstract)
        .unwrap_or(ClassKind::Concrete);
    let class_generics = cls
        .generics
        .map(|generics| {
            Vec::from_iter(generics.bounds.punctuated.into_iter().map(|(gen, _)| {
                GenericDeclaration::new(
                    gen.id,
                    gen.bound
                        .map(|i| i.as_class_inst())
                        .unwrap_or(ClassInst::from(OBJECT_CLASS.as_class_ref())),
                )
            }))
        })
        .unwrap_or_default();
    let super_class = cls
        .extends
        .map(|extends| extends.extended.as_class_inst())
        .unwrap_or(ClassInst::from(OBJECT_CLASS.as_class_ref()));
    let mixins = cls
        .implements
        .map(|i| {
            i.types
                .items()
                .into_iter()
                .map(|i| i.as_class_inst())
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    let class_inst = ClassInst::with_generics(
        ClassRef::from(id.clone()),
        class_generics
            .iter()
            .map(|i| GenericParameterBound::Invariant(ClassInst::new_generic_param(i.id()))),
    );

    let mut fields = vec![];
    let mut methods = vec![];
    let mut method_defs = vec![];
    let mut sub_classes = vec![];
    let mut constructors = vec![];
    cls.members.members.into_iter().try_for_each(|member| {
        match member {
            ClassMember::Method(method) => {
                let (dec, def) = method_hir_to_mir::method_hir_to_mir(&class_inst, &fields, &class_generics, method)?;
                methods.push(dec);
                method_defs.push(def);
            }
            ClassMember::AbstractMethod(ref abs) if matches!(class_kind, ClassKind::Concrete) => {
                return Err(SyntaxError::new(
                    ErrorKind::AbstractMethodInConcreteClass(
                        abs.ident.as_ref().to_string(),
                        ClassRef::from(cls.ident.id.clone()),
                    ),
                    member.span(),
                    None,
                    None,
                ));
            }
            ClassMember::AbstractMethod(abs) => {
                let dec = method_hir_to_mir::abstract_method_hir_to_mir(
                    &class_inst,
                    &class_generics,
                    abs
                )?;

                methods.push(dec);
            }
            ClassMember::Field(field) => {
                let field = match field {
                    ClassField {
                        final_tok: Some(_),
                        vis,
                        binding,
                        ..
                    } => Field::new_final(
                        to_mir::vis_hir_to_mir(vis),
                        binding.id.as_ref(),
                        binding.type_dec.ty.as_class_inst(),
                    ),
                    ClassField {
                        final_tok: None,
                        vis,
                        binding,
                        ..
                    } => Field::new(
                        to_mir::vis_hir_to_mir(vis),
                        binding.id.as_ref(),
                        binding.type_dec.ty.as_class_inst(),
                    ),
                };
                fields.push(field);
            }
            ClassMember::Constructor(cons) => {
                todo!("transform constructor {cons:#?}")
            }
            ClassMember::Class(sub_class) => {
                todo!("transform sub-class {sub_class:#?}")
            }
        }
        Ok(())
    })?;

    let class = Class::new(
        vis,
        class_kind,
        id,
        class_generics,
        super_class,
        mixins,
        fields,
        methods,
        constructors,
        sub_classes,
    );

    let item = ClassItem::new(span, class, method_defs);
    Ok(item)
}