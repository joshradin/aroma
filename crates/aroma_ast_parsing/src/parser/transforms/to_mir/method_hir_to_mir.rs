use crate::parser::hir::binding::FnParameters;
use crate::parser::hir::expr::Expr;
use crate::parser::hir::items::{FnReturn, FnThrows, GenericDeclarations, ItemInterfaceFn, Visibility};
use crate::parser::hir::singletons::VarId;
use crate::parser::hir::statement::{ReturnStatement, Statement as ParsedStatement, Statement};
use crate::parser::hir::items::ItemFn;
use crate::parser::hir::items::{FnBody, ItemAbstractFn};
use crate::parser::transforms::to_mir;
use crate::parser::transforms::to_mir::expr_hir_to_mir::expr_hir_to_mir;
use crate::parser::hir::{Punctuated};
use crate::type_resolution::Bindings;
use aroma_ast::block::Block;
use aroma_ast::method::MethodDef;
use aroma_ast::statements::{DeclareStmt, ReturnStmt, Stmt, StmtKind};
use aroma_ast::typed::Typed;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::{Span, Spanned};
use aroma_types::class::ClassInst;
use aroma_types::field::Field;
use aroma_types::functions::{FunctionDeclaration, Parameter};
use aroma_types::generic::GenericDeclaration;
use aroma_types::type_signature::TypeSignature;
use std::collections::HashSet;
use tracing::{debug, trace};
use crate::parser::SyntaxError;

pub fn method_hir_to_mir(
    parent_inst: &ClassInst,
    fields: &[Field],
    class_generics: &[GenericDeclaration],
    method: ItemFn,
) -> Result<(FunctionDeclaration, MethodDef), SyntaxError> {
    let span = method.span();
    trace!("creating method from {method:#?}");
    let ItemFn {
        annotations: _,
        vis,
        static_tok,
        fn_tok: _,
        ident,
        generics,
        fn_parameters,
        fn_return,
        fn_throws,
        body,
    } = method;

    let vis = to_mir::vis_hir_to_mir(vis);
    let name = ident.as_ref().to_string();

    let mut func_generics: Vec<GenericDeclaration> = vec![];
    let full_generics = {
        let mut base_generics = Vec::from(class_generics);
        if let Some(generics) = generics {
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

    let return_type = fn_return.as_ref().map(|i| i.returns.as_class_inst());
    let parameters = fn_parameters
        .parameters
        .items()
        .into_iter()
        .map(|binding| Parameter {
            name: binding.id.as_ref().to_string(),
            class: binding.type_dec.ty.as_class_inst(),
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

    let delegate = match &static_tok {
        None => Some(parent_inst.clone()),
        Some(_) => None,
    };

    let method_dec = FunctionDeclaration::new(
        vis,
        &name,
        if static_tok.is_none() {
            full_generics
        } else {
            func_generics
        },
        delegate,
        parameters.clone(),
        return_type.clone(),
        throws.clone(),
    );
    trace!("created method_dec = {:#?}", method_dec);
    let method_def = method_hir_to_mir_def(
        span,
        if static_tok.is_some() {
            None
        } else {
            Some(parent_inst)
        },
        fields,
        &method_dec,
        body,
    )?;

    Ok((method_dec, method_def))
}

pub fn abstract_method_hir_to_mir(
    parent_inst: &ClassInst,
    class_generics: &[GenericDeclaration],
    method: ItemAbstractFn,
) -> Result<FunctionDeclaration, SyntaxError> {
    trace!("creating method from {method:#?}");
    let ItemAbstractFn {
        vis,
        abstract_tok: _,
        fn_tok: _,
        ident,
        generics,
        fn_parameters,
        fn_return,
        fn_throws,
        end: _,
    } = method;

    non_concrete_method_to_hair(
        parent_inst,
        class_generics,
        vis,
        ident,
        generics,
        fn_parameters,
        fn_return,
        fn_throws,
    )
}

pub fn interface_method_hir_to_mir(
    parent_inst: &ClassInst,
    class_generics: &[GenericDeclaration],
    method: ItemInterfaceFn,
) -> Result<FunctionDeclaration, SyntaxError> {
    trace!("creating method from {method:#?}");
    let ItemInterfaceFn {
        vis,
        fn_tok: _,
        ident,
        generics,
        fn_parameters,
        fn_return,
        fn_throws,
        end: _,
    } = method;

    non_concrete_method_to_hair(
        parent_inst,
        class_generics,
        vis.map(Visibility::Public),
        ident,
        generics,
        fn_parameters,
        fn_return,
        fn_throws,
    )
}

fn non_concrete_method_to_hair(
    parent_inst: &ClassInst,
    class_generics: &[GenericDeclaration],
    vis: Option<Visibility>,
    ident: VarId,
    generics: Option<GenericDeclarations>,
    fn_parameters: FnParameters,
    fn_return: Option<FnReturn>,
    fn_throws: Option<FnThrows>,
) -> Result<FunctionDeclaration, SyntaxError> {
    let vis = to_mir::vis_hir_to_mir(vis);
    let name = ident.as_ref().to_string();

    let mut func_generics: Vec<GenericDeclaration> = vec![];
    let full_generics = {
        let mut base_generics = Vec::from(class_generics);
        if let Some(generics) = generics {
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

    let return_type = fn_return.as_ref().map(|i| i.returns.as_class_inst());
    let parameters = fn_parameters
        .parameters
        .items()
        .into_iter()
        .map(|binding| Parameter {
            name: binding.id.as_ref().to_string(),
            class: binding.type_dec.ty.as_class_inst(),
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

    let method_dec = FunctionDeclaration::new(
        vis,
        &name,
        full_generics,
        parent_inst.clone(),
        parameters.clone(),
        return_type.clone(),
        throws.clone(),
    );
    trace!("created method_dec = {:#?}", method_dec);

    Ok(method_dec)
}

pub fn method_hir_to_mir_def<'a>(
    span: Span,
    parent: impl Into<Option<&'a ClassInst>>,
    fields: &[Field],
    dec: &FunctionDeclaration,
    body: FnBody,
) -> Result<MethodDef, SyntaxError> {
    let body = transform_fn_body(dec, fields, body)?;
    Ok(MethodDef::new(span.clone(), parent.into(), dec, body))
}

fn transform_fn_body(
    dec: &FunctionDeclaration,
    fields: &[Field],
    fn_body: FnBody,
) -> Result<Block, SyntaxError> {
    let span = fn_body.span();
    let mut statements = vec![];
    let mut declared_variables = Bindings::new();

    declared_variables.new_scope(Id::new_call_site(["this"]).unwrap());
    fields.iter().for_each(|field| {
        declared_variables.insert(
            Id::new_call_site(["this", field.name()]).expect("could not create id"),
            TypeSignature::from(field.class().clone()),
        )
    });

    declared_variables.new_scope(None);

    dec.parameters().iter().for_each(
        |Parameter {
             name,
             class: signature,
         }| {
            declared_variables.insert(
                Id::new_call_site([name]).expect("could not create id"),
                signature.clone().into(),
            )
        },
    );

    trace!("creating method with initial bindings: {declared_variables:#?}");
    for statement in fn_body.body.list.statements {
        transform_statement(statement, &mut statements, &mut declared_variables)?;
        trace!("statements: {statements:#?}");
    }
    declared_variables.pop_scope();
    Ok(Block::new(span, statements))
}

fn transform_statement(
    statement: ParsedStatement,
    statements: &mut Vec<Stmt>,
    bindings: &mut Bindings,
) -> Result<(), SyntaxError> {
    trace!("transforming statement {statement:?}");
    let span = statement.span();
    match statement {
        ParsedStatement::Let(stmt_let) => {
            let id = stmt_let.binding.id;
            let ty = stmt_let.binding.type_dec.map(|t| t.ty.as_type_signature());

            let set = stmt_let.expr;
            let expr = expr_hir_to_mir(set, bindings)?;

            bindings.insert(
                id.id.clone(),
                ty.clone()
                    .unwrap_or_else(|| expr.get_type().available().unwrap_or(TypeSignature::Never)),
            );

            statements.push(Stmt::new(
                span,
                StmtKind::Declare(DeclareStmt::new(id, true, ty, expr)),
            ));
        }
        ParsedStatement::Const(stmt_const) => {
            let id = stmt_const.binding.id;

            let ty = stmt_const
                .binding
                .type_dec
                .map(|t| t.ty.as_type_signature());
            let set = stmt_const.expr;
            let expr = expr_hir_to_mir(set, bindings)?;
            bindings.insert(
                id.id.clone(),
                ty.clone()
                    .unwrap_or_else(|| expr.get_type().available().unwrap_or(TypeSignature::Never)),
            );

            statements.push(Stmt::new(
                span,
                StmtKind::Declare(DeclareStmt::new(id, false, ty, expr)),
            ));
        }
        ParsedStatement::Expr(_) => {}
        ParsedStatement::Block(b) => {
            bindings.new_scope(None);

            bindings.pop_scope();
        }
        ParsedStatement::If(_) => {}
        ParsedStatement::While(_) => {}
        ParsedStatement::Loop(_) => {}
        ParsedStatement::Return(stmt_return) => {
            let ReturnStatement { expr, .. } = stmt_return;
            let stmt = match expr {
                None => ReturnStmt::new(None),
                Some(expr) => {
                    let expr = expr_hir_to_mir(expr, bindings)?;
                    ReturnStmt::new(expr)
                }
            };
            statements.push(Stmt::new(span, StmtKind::Return(stmt)))
        }
        ParsedStatement::Break(_) => {}
        ParsedStatement::Continue(_) => {}
        ParsedStatement::Match(_) => {}
        ParsedStatement::TryCatch(_) => {}
        ParsedStatement::Assign(_) => {}
    }
    trace!("declared variables: {bindings:#?}");
    Ok(())
}
