use rustc::ty::adjustment::{Adjust, AutoBorrow, AutoBorrowMutability};
use syntax::ast::{Crate, Expr, ExprKind, Mutability, UnOp};
use syntax::ptr::P;

use c2rust_ast_builder::mk;
use crate::ast_manip::MutVisitNodes;
use crate::command::{CommandState, Registry};
use crate::driver::Phase;
use crate::transform::Transform;
use crate::RefactorCtxt;

/// Transformation that makes all autorefs and autoderefs explicit.
struct CanonicalizeRefs;

impl Transform for CanonicalizeRefs {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        MutVisitNodes::visit(krate, |expr: &mut P<Expr>| {
            let hir_expr = cx.hir_map().expect_expr(expr.id);
            let parent = cx.hir_map().get_parent_did(expr.id);
            let tables = cx.ty_ctxt().typeck_tables_of(parent);
            for adjustment in tables.expr_adjustments(hir_expr) {
                match adjustment.kind {
                    Adjust::Deref(_) => {
                        *expr = mk().unary_expr(UnOp::Deref, *expr);
                    }
                    Adjust::Borrow(AutoBorrow::Ref(_, ref mutability)) => {
                        let mutability = match mutability {
                            AutoBorrowMutability::Mutable{..} => Mutability::Mutable,
                            AutoBorrowMutability::Immutable => Mutability::Immutable,
                        };
                        *expr = mk().set_mutbl(mutability).addr_of_expr(*expr);
                    }
                    _ => {},
                }
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// Transformation that removes unnecessary refs and derefs.
struct RemoveUnnecessaryRefs;

impl Transform for RemoveUnnecessaryRefs {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, _cx: &RefactorCtxt) {
        MutVisitNodes::visit(krate, |expr: &mut P<Expr>| {
            match expr.node {
                ExprKind::MethodCall(path, args) => {
                    let (receiver, rest) = args.split_first().unwrap();
                    let receiver = remove_all_derefs(remove_ref(remove_reborrow(receiver.clone())));
                    let rest = rest.iter().map(|arg| remove_reborrow(arg.clone()));
                    let mut args = Vec::with_capacity(args.len() + 1);
                    args.push(receiver);
                    args.extend(rest);
                    expr.node = ExprKind::MethodCall(path, args);
                }
                ExprKind::Call(callee, args) => {
                    let args = args.iter().map(|arg| remove_reborrow(arg.clone())).collect();
                    expr.node = ExprKind::Call(callee, args);
                }
                _ => {}
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn remove_ref(expr: P<Expr>) -> P<Expr> {
    expr.map(|expr| match expr.node {
        ExprKind::AddrOf(_, expr) => expr.into_inner(),
        _ => expr,
    })
}

fn remove_all_derefs(expr: P<Expr>) -> P<Expr> {
    expr.map(|expr| match expr.node {
        ExprKind::Unary(UnOp::Deref, expr) => remove_all_derefs(expr).into_inner(),
        _ => expr,
    })
}

fn remove_reborrow(expr: P<Expr>) -> P<Expr> {
    if let ExprKind::AddrOf(_, ref subexpr) = expr.node {
        if let ExprKind::Unary(UnOp::Deref, ref subexpr) = subexpr.node {
            return remove_reborrow(subexpr.clone());
        }
    }
    expr
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("canonicalize_refs", |_args| mk(CanonicalizeRefs));

    reg.register("remove_unnecessary_refs", |_args| mk(RemoveUnnecessaryRefs));
}
