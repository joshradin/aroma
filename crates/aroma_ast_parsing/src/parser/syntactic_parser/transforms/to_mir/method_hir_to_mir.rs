use crate::parser::expr::Expr;
use crate::parser::items::FnBody;
use crate::parser::statement::{Statement as ParsedStatement, ReturnStatement, Statement};
use crate::parser::transforms::to_mir::expr_hir_to_mir::expr_hir_to_mir;
use crate::parser::SyntaxError;
use crate::type_resolution::Bindings;
use aroma_ast::id::Id;
use aroma_ast::mir::block::Block;
use aroma_ast::mir::method::MethodDef;
use aroma_ast::mir::statements::{Stmt, DeclareStmt, StmtKind, ReturnStmt};
use aroma_ast::mir::typed::Typed;
use aroma_ast::spanned::{Span, Spanned};
use aroma_types::class::ClassInst;
use aroma_types::field::Field;
use aroma_types::functions::{FunctionDeclaration, Parameter};
use aroma_types::type_signature::TypeSignature;
use log::debug;
use std::collections::HashSet;

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
    fields.iter()
        .for_each(|field| {
            declared_variables.insert(
                Id::new_call_site([
                    "this",
                    field.name()
                ]).expect("could not create id"),
                TypeSignature::from(field.kind().clone())
            )
        });

    declared_variables.new_scope(None);

    dec.parameters()
        .iter()
        .for_each(|Parameter { name, signature }| {
            declared_variables.insert(
                Id::new_call_site([name]).expect("could not create id"),
                signature.clone(),
            )
        });

    debug!("creating method with initial bindings: {declared_variables:#?}");
    for statement in fn_body.body.list.statements {
        transform_statement(statement, &mut statements, &mut declared_variables)?;
        debug!("statements: {statements:#?}");
    }
    declared_variables.pop_scope();
    Ok(Block::new(span, statements))
}

fn transform_statement(
    statement: ParsedStatement,
    statements: &mut Vec<Stmt>,
    bindings: &mut Bindings,
) -> Result<(), SyntaxError> {
    debug!("transforming statement {statement:?}");
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
    debug!("declared variables: {bindings:#?}");
    Ok(())
}
