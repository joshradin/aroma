use crate::parser::{expr as parsed_exprs, ErrorKind, Punctuated, SyntaxError};
use crate::type_resolution::Bindings;
use aroma_ast::expr::{CallMethodExpr, Expr, ExprThis, FieldExpr, GlobalExpr, VarExpr};
use aroma_ast::references::{FieldRef, NameType};
use aroma_ast::typed::TypedMut;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::Spanned;
use aroma_types::class::ClassInst;
use tracing::debug;
use parsed_exprs::Expr as ParsedExpr;

pub fn expr_hir_to_mir(
    expr: ParsedExpr,
    declared_variables: &Bindings,
) -> Result<Expr, SyntaxError> {
    debug!("transforming {expr:#?}");
    match expr {
        ParsedExpr::Path(ref path) => {
            if declared_variables.contains(&path) {
                get_var(&expr, declared_variables, &path)
            } else {
                Ok(Expr::Global(GlobalExpr::new(path.clone())))
            }
        }
        ParsedExpr::List(_) => {
            todo!()
        }
        ParsedExpr::Constant(_) => {
            todo!()
        }
        ParsedExpr::Binary(_) => {
            todo!()
        }
        ParsedExpr::Field(_) => {
            todo!()
        }
        ParsedExpr::Call(call) => {
            let parameters = match call.parameters {
                Some(p) => p.into_items().into_iter().try_fold(
                    Vec::new(),
                    |mut accum, item| -> Result<_, SyntaxError> {
                        let expr = expr_hir_to_mir(item, declared_variables)?;
                        accum.push(expr);
                        Ok(accum)
                    },
                )?,
                None => {
                    vec![]
                }
            };
            let callee = expr_hir_to_mir(*call.callee, declared_variables)?;
            Ok(Expr::CallMethod(CallMethodExpr::new(callee, parameters)))
        }
        ParsedExpr::Index(_) => {
            todo!()
        }
        ParsedExpr::Unary(_) => {
            todo!()
        }
        ParsedExpr::Closure(_) => {
            todo!()
        }
        ParsedExpr::Nested(_) => {
            todo!()
        }
        ParsedExpr::Ternary(_) => {
            todo!()
        }

        ParsedExpr::This(_) => {
            todo!()
        }
        ParsedExpr::Super(_) => {
            todo!()
        }
        ParsedExpr::Delegate(_) => {
            todo!()
        }
    }
}

fn get_var(
    expr: &ParsedExpr,
    declared_variables: &Bindings,
    path: &Id,
) -> Result<Expr, SyntaxError> {
    let (id, ty) = declared_variables.get(&path).ok_or_else(|| {
        SyntaxError::new(
            ErrorKind::UndeclaredVariable(path.clone()),
            expr.span(),
            None,
            None,
        )
    })?;
    debug!("resolved local variable to {id:?}");
    let parts = id.iter().collect::<Vec<_>>();
    match &parts[..] {
        ["this"] => {
            let this_ty = declared_variables
                .get(&Id::new_call_site(["this"]).unwrap())
                .map(|t| t.1.clone());
            Ok(Expr::This(ExprThis::new(this_ty)))
        }
        ["this", field] => {
            let this_ty = declared_variables
                .get(&Id::new_call_site(["this"]).unwrap())
                .map(|t| t.1.clone());

            debug!("got this field: {field}");
            Ok(Expr::Field(FieldExpr::new(
                Expr::This(ExprThis::new(this_ty)),
                field.to_string(),
            )))
        }
        [other] => {
            let mut var_expr = VarExpr::new(other.to_string());
            var_expr.set_type(ty.clone());
            Ok(Expr::Var(var_expr))
        }
        _ => unreachable!("var id restriction should prevent multi-ids at this point"),
    }
}
