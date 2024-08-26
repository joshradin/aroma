use crate::parser::items::{ClassField, ClassMember, ItemFn, Visibility};
use crate::parser::syntactic_parser::hir::translation_unit::TranslationUnit as ParsedTranslationUnit;
use crate::parser::SyntaxError;
use crate::parser::{items as parser_items, ErrorKind};
use aroma_ast::mir::items::{Item, ItemClass};
use aroma_ast::mir::translation_unit::TranslationUnit;
use aroma_ast::spanned::Spanned;
use aroma_types::class::{AsClassRef, Class, ClassInst, ClassKind, ClassRef};
use aroma_types::field::Field;
use aroma_types::generic::GenericDeclaration;
use aroma_types::hierarchy::intrinsics::OBJECT_CLASS;
use aroma_types::method::{MethodDeclaration, Parameter};
use aroma_types::vis::Vis;
use log::debug;
use std::collections::HashMap;

/// Runs the initial conversion to mir, removing most tokens while keeping spans
pub fn to_mir(translation_unit: ParsedTranslationUnit) -> Result<TranslationUnit, SyntaxError> {
    let span = translation_unit.span();
    let imports = translation_unit
        .imports
        .iter()
        .map(|import| import.id.clone())
        .collect();
    let items = translation_unit
        .items
        .into_iter()
        .map(|item| -> Result<Item, SyntaxError> {
            match item {
                parser_items::Item::Class(cls) => {
                    let class = class_hir_to_mir(cls)?;
                    debug!("parsed class {class:#?}");
                    Ok(Item::Class(class))
                }
                parser_items::Item::Func(func) => {
                    todo!()
                }
            }
        })
        .collect::<Result<Vec<_>, SyntaxError>>()?;

    let mir = TranslationUnit::new(
        span,
        translation_unit.namespace_declaration.map(|i| i.id),
        imports,
        items,
    );
    Ok(mir)
}

fn class_hir_to_mir(cls: parser_items::ItemClass) -> Result<ItemClass, SyntaxError> {
    let span = cls.span();
    let vis = vis_hir_to_mir(cls.vis);
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
    let mut fields = vec![];
    let mut methods = vec![];
    let mut method_defs = vec![];
    let mut sub_classes = vec![];
    let mut constructors = vec![];
    cls.members.members.into_iter().try_for_each(|member| {
        match member {
            ClassMember::Method(method) => {
                let span = method.span();
                debug!("creating method from {method:?}");
                let ItemFn {
                    vis,
                    static_tok,
                    fn_tok,
                    ident,
                    generics,
                    fn_parameters,
                    fn_return,
                    fn_throws,
                    body,
                } = method;

                let vis = vis_hir_to_mir(vis);
                let name = ident.as_ref().to_string();

                let mut func_generics: Vec<GenericDeclaration>  = vec![];
                let mut full_generics = {
                    let mut base_generics = class_generics.clone();
                    if let Some(generics) = generics {
                        func_generics = generics.bounds.items().into_iter().map(|i| i.into()).collect();
                        base_generics.extend(func_generics.clone());
                    }
                    base_generics
                };

                let return_type = fn_return
                    .as_ref()
                    .map(|i| i.returns.as_type_signature())
                    .unwrap_or_default();
                let parameters = fn_parameters
                    .parameters
                    .items()
                    .into_iter()
                    .map(|binding| Parameter {
                        name: name.clone(),
                        signature: binding.type_dec.ty.as_type_signature(),
                    })
                    .collect::<Vec<_>>();
                let throws = fn_throws
                    .as_ref()
                    .map(|throws| {
                        throws
                            .types
                            .items()
                            .into_iter()
                            .map(|t| t.as_class_inst())
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();

                let method_dec = MethodDeclaration::new(
                    vis,
                    &name,
                    func_generics,
                    return_type.clone(),
                    parameters.clone(),
                    throws.clone(),
                );
                debug!("created method_dec = {:#?}", method_dec);
            }
            ClassMember::AbstractMethod(ref abs) if matches!(class_kind, ClassKind::Concrete) => {
                return Err(SyntaxError::new(
                    ErrorKind::AbstractMethodInConcreteClass(
                        abs.id.as_ref().to_string(),
                        ClassRef::from(cls.ident.as_ref()),
                    ),
                    member.span(),
                    None,
                    None,
                ));
            }
            ClassMember::AbstractMethod(abs) => {}
            ClassMember::Field(field) => {
                let field = match field {
                    ClassField {
                        final_tok: Some(_),
                        vis,
                        binding,
                        ..
                    } => Field::new_final(
                        vis_hir_to_mir(vis),
                        binding.id.as_ref(),
                        binding.type_dec.ty.as_class_inst(),
                    ),
                    ClassField {
                        final_tok: None,
                        vis,
                        binding,
                        ..
                    } => Field::new(
                        vis_hir_to_mir(vis),
                        binding.id.as_ref(),
                        binding.type_dec.ty.as_class_inst(),
                    ),
                };
                fields.push(field);
            }
            ClassMember::Constructor(_) => {}
            ClassMember::Class(_) => {}
        }
        Ok(())
    })?;

    let class = Class::new(
        vis,
        class_kind,
        cls.ident,
        class_generics,
        super_class,
        mixins,
        fields,
        methods,
        constructors,
        sub_classes,
    );

    let item = ItemClass::new(span, class, method_defs);
    Ok(item)
}

pub fn vis_hir_to_mir(option: Option<Visibility>) -> Vis {
    option.as_ref().map(|v| Vis::from(v)).unwrap_or_default()
}
