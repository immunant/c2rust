use rustc_middle::ty::adjustment::{Adjust, AutoBorrow, AutoBorrowMutability};
use rustc_ast::{Crate, Expr, ExprKind, Mutability, UnOp};
use rustc_ast::ptr::P;

use crate::ast_builder::mk;
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
            let hir_id = cx.hir_map().node_to_hir_id(expr.id);
            let hir_expr = cx.hir_map().expect_expr(hir_id);
            let parent = cx.hir_map().get_parent_item(hir_id);
            let tables = cx.ty_ctxt().typeck(parent);
            for adjustment in tables.expr_adjustments(hir_expr) {
                match adjustment.kind {
                    Adjust::Deref(_) => {
                        *expr = mk().unary_expr(UnOp::Deref, expr.clone());
                    }
                    Adjust::Borrow(AutoBorrow::Ref(_, ref mutability)) => {
                        let mutability = match mutability {
                            AutoBorrowMutability::Mut { .. } => Mutability::Mut,
                            AutoBorrowMutability::Not => Mutability::Not,
                        };
                        *expr = mk().set_mutbl(mutability).addr_of_expr(expr.clone());
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
            match &mut expr.kind {
                ExprKind::MethodCall(_path, args, _span) => {
                    let (receiver, rest) = args.split_first_mut().unwrap();
                    remove_reborrow(receiver);
                    remove_ref(receiver);
                    remove_all_derefs(receiver);
                    for arg in rest {
                        remove_reborrow(arg);
                    }
                }
                ExprKind::Call(_callee, args) => {
                    for arg in args.iter_mut() {
                        remove_reborrow(arg);
                    }
                }
                _ => {}
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn remove_ref(expr: &mut P<Expr>) {
    match &expr.kind {
        ExprKind::AddrOf(_, _, inner) => *expr = inner.clone(),
        _ => {}
    }
}

fn remove_all_derefs(expr: &mut P<Expr>) {
    match &expr.kind {
        ExprKind::Unary(UnOp::Deref, inner) => {
            *expr = inner.clone();
            remove_all_derefs(expr);
        }
        _ => {}
    }
}

fn remove_reborrow(expr: &mut P<Expr>) {
    if let ExprKind::AddrOf(_, _, ref subexpr) = expr.kind {
        if let ExprKind::Unary(UnOp::Deref, ref subexpr) = subexpr.kind {
            *expr = subexpr.clone();
            remove_reborrow(expr);
        }
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("canonicalize_refs", |_args| mk(CanonicalizeRefs));

    reg.register("remove_unnecessary_refs", |_args| mk(RemoveUnnecessaryRefs));
}
