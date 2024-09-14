use crate::parser::hir::items::ClassConstructor;
use crate::parser::hir::Punctuated;
use crate::parser::transforms::to_mir::method_hir_to_mir::method_hir_to_mir_def;
use crate::parser::transforms::to_mir::vis_hir_to_mir;
use crate::parser::SyntaxError;
use aroma_ast::method::MethodDef;
use aroma_tokens::spanned::Spanned;
use aroma_types::class::ClassInst;
use aroma_types::constructor::Constructor;
use aroma_types::field::Field;
use aroma_types::functions::FunctionId;
use aroma_types::generic::GenericDeclaration;

pub fn constructor_hir_to_mir(
    parent: &ClassInst,
    fields: &[Field],
    class_generics: &[GenericDeclaration],
    constructor: ClassConstructor,
) -> Result<(Constructor, MethodDef), SyntaxError> {
    let span = constructor.span();
    let mut func_generics: Vec<GenericDeclaration> = vec![];
    let full_generics = {
        let mut base_generics = Vec::from(class_generics);
        if let Some(generics) = constructor.generics {
            func_generics = generics
                .bounds
                .items()
                .into_iter()
                .map(|i| i.into())
                .collect();
            base_generics.extend(func_generics.clone());
        }
        base_generics
    };
    let cons = Constructor::new(
        FunctionId::default(),
        vis_hir_to_mir(constructor.vis),
        full_generics,
        [],
        [],
        parent.clone().into(),
    );

    let cons_dec = cons.signature().declare(cons.vis(), "<init>");
    let method_def = method_hir_to_mir_def(
        span,
        parent,
        fields,
        constructor.parameters,
        &cons_dec,
        constructor.body,
    )?;

    Ok((cons, method_def))
}
