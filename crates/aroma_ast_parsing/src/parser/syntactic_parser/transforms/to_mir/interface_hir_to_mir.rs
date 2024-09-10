//! this mostly just converts to a normal class

use crate::parser::items::{InterfaceMember, ItemInterface};
use crate::parser::transforms::to_mir;
use crate::parser::transforms::to_mir::method_hir_to_mir;
use crate::parser::{Punctuated, SyntaxError};
use aroma_ast::items::ClassItem;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::Spanned;
use aroma_types::class::{AsClassRef, Class, ClassInst, ClassKind, ClassRef};
use aroma_types::generic::{GenericDeclaration, GenericParameterBound};
use aroma_types::hierarchy::intrinsics::OBJECT_CLASS;

/// converts interfaces hir to an item class
pub fn interface_hir_to_mir(
    namespace: Option<&Id>,
    interface: ItemInterface,
) -> Result<ClassItem, SyntaxError> {
    let id = match namespace {
        None => interface.ident.id.clone(),
        Some(namespace) => namespace.resolve(&interface.ident.id),
    };
    let span = interface.span();
    let vis = to_mir::vis_hir_to_mir(interface.vis);
    let class_kind = ClassKind::Interface;
    let class_generics = interface
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
    let mixins = interface
        .extends
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

    interface
        .members
        .members
        .into_iter()
        .try_for_each(|member| -> Result<(), SyntaxError> {
            match member {
                InterfaceMember::StaticMethod(method) => {
                    let (dec, def) = method_hir_to_mir::method_hir_to_mir(
                        &class_inst,
                        &fields,
                        &class_generics,
                        method,
                    )?;
                    methods.push(dec);
                    method_defs.push(def);
                }

                InterfaceMember::AbstractMethod(abs) => {
                    let dec = method_hir_to_mir::interface_method_hir_to_mir(
                        &class_inst,
                        &class_generics,
                        abs,
                    )?;

                    methods.push(dec);
                }
                InterfaceMember::StaticField(field) => {
                    todo!("static fields");
                    // fields.push(field);
                }
                InterfaceMember::Class(_) => {
                    todo!("inner classes for interfaces")
                }
            }
            Ok(())
        })?;

    let class = Class::new(
        vis,
        class_kind,
        id,
        class_generics,
        None,
        mixins,
        fields,
        methods,
        [],
        sub_classes,
    );

    let item = ClassItem::new(span, class, method_defs);
    Ok(item)
}
