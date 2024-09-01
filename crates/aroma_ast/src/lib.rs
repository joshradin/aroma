#![doc = include_str!("../README.md")]

use crate::items::{Item, ClassItem};
use crate::method::MethodDef;
use crate::translation_unit::TranslationUnit;
use aroma_tokens::id::Id;
use aroma_types::class::{Class, ClassInst, ClassRef};
use aroma_types::field::Field;
use aroma_types::functions::{FunctionDeclaration, Parameter};
use aroma_types::generic::{GenericDeclaration, GenericParameterBound, GenericParameterBounds};
use aroma_visitor_gen::visitor;

pub mod block;
pub mod expr;
pub mod items;
pub mod method;
pub mod references;
pub mod statements;
pub mod translation_unit;
pub mod typed;

visitor! {
    /// Ast visitor
    pub trait AstVisitor {
        visit fn translation_unit(v, tu: &TranslationUnit) -> Result<()> {
            tu.items.iter()
                .try_for_each(|e| {
                v.visit_item(e)
            })?;
            Ok(())
        }

        visit fn item(v, item: &Item) -> Result<()> {
            match item {
                Item::Class(cls) => {
                    v.visit_class(cls)
                }
                Item::Delegate(_) => {todo!()}
            }
        }

        visit fn class(v, class: &ClassItem) -> Result<()> {
            Ok(())
        }
    }
}

visitor! {
    /// Ast mut visitor
    pub trait AstVisitorMut {
        visit fn translation_unit(v, tu: &mut TranslationUnit) -> Result<()> {
            tu.items.iter_mut()
                .try_for_each(|e| {
                v.visit_item(e)
            })?;
            Ok(())
        }

        visit fn item(v, item: &mut Item) -> Result<()> {
            match item {
                Item::Class(cls) => {
                    v.visit_class_item(cls)
                }
                Item::Delegate(_) => {todo!()}
            }
        }

        visit fn class_item(v, class: &mut ClassItem) -> Result<()> {
            v.visit_class(&mut class.class)?;
            class.method_definitions.values_mut().try_for_each(|m| v.visit_method_definition(m))?;
            Ok(())
        }

        visit fn class(v, class: &mut Class) -> Result<()> {
            if let Some(super_class) = class.super_class_mut() {
                v.visit_class_inst(
                    super_class
                )?;
            };
            class.mixins_mut().iter_mut()
                .try_for_each(|mixin| {
                v.visit_class_inst(mixin)
            })?;

            class.fields_mut().iter_mut()
                .try_for_each(|field| {
                v.visit_field(field)
            })?;

            class.methods_mut().iter_mut()
                .try_for_each(|method| {
                v.visit_function_declaration(method)
            })?;
            Ok(())
        }

        visit fn generic_declaration(v, generic: &mut GenericDeclaration) -> Result<()> {
            v.visit_class_inst(generic.bound_mut())?;
            Ok(())
        }

        visit fn field(v, field: &mut Field) -> Result<()> {
            v.visit_class_inst(field.class_mut())?;
            Ok(())
        }

        visit fn function_declaration(v, func_dec: &mut FunctionDeclaration) -> Result<()> {
            func_dec.generic_declarations_mut().iter_mut().try_for_each(|gd| v.visit_generic_declaration(gd))?;
            func_dec.parameters_mut().into_iter().try_for_each(|p| v.visit_parameter(p))?;
            if let Some(return_type) = func_dec.return_type_mut() {
                v.visit_class_inst(return_type)?;
            }
            func_dec.throws_mut().iter_mut().try_for_each(|t| v.visit_class_inst(t))?;
            Ok(())
        }

        visit fn parameter(v, parameter: &mut Parameter) -> Result<()> {
            v.visit_class_inst(&mut parameter.class)?;
            Ok(())
        }

        visit fn method_definition(v, method: &mut MethodDef) -> Result<()> {
            Ok(())
        }


        visit fn id(v, id: &mut Id) -> Result<()> {
            Ok(())
        }

        visit fn class_inst(v, id: &mut ClassInst) -> Result<()> {
            v.visit_class_ref(id.class_ref_mut())?;
            v.visit_generic_parameter_bounds(id.generics_mut())?;
            Ok(())
        }

        visit fn class_ref(v, id: &mut ClassRef) -> Result<()> {
            v.visit_id(id.as_mut())?;
            Ok(())
        }

        visit fn generic_parameter_bounds(v, ids: &mut GenericParameterBounds) -> Result<()> {
            ids.iter_mut()
                .try_for_each(|id| v.visit_generic_parameter_bound(id))?;
            Ok(())
        }

        visit fn generic_parameter_bound(v, id: &mut GenericParameterBound) -> Result<()> {
            v.visit_class_inst(id.bound_class_instance_mut())?;
            Ok(())
        }
    }
}
