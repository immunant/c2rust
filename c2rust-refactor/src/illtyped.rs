use rustc::hir;
use rustc::hir::def::Def;
use rustc::ty::{self, TyCtxt, ParamEnv};
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::mut_visit::{self, MutVisitor};
use syntax::ptr::P;

use crate::ast_manip::Fold;
use crate::RefactorCtxt;


fn types_approx_equal<'tcx>(tcx: TyCtxt<'_, 'tcx, 'tcx>,
                            ty1: ty::Ty<'tcx>,
                            ty2: ty::Ty<'tcx>) -> bool {
    // Normalizing and erasing regions fixes a few cases where `illtyped` would otherwise falsely
    // report a type error.  Specifically:
    //
    //  - On a method call `x.f()`, the autoref adjustment on `x` uses a specific region, while the
    //    signature returned by `opt_callee_fn_sig` has regions erased (because `opt_callee_info`
    //    itself calls `normalize_erasing_regions`).  Erasing regions fixes this issue, though at
    //    the cost of ignoring most borrow checker errors.
    //  - On array expressions, often one side has the length expression fully evaluated, while the
    //    other does not.  Normalizing makes sure both sides are evaluated, so no error is reported
    //    (assuming the lengths do, in fact, match).
    let ty1 = tcx.normalize_erasing_regions(ParamEnv::empty(), ty1);
    let ty2 = tcx.normalize_erasing_regions(ParamEnv::empty(), ty2);
    ty1 == ty2
}

pub trait IlltypedFolder<'tcx> {
    /// Called on each expr `e` whose `actual` type doesn't match the `expected` type propagated
    /// down from its parent.  Implementations should attempt to correct `e` to an expr that has
    /// type `expected`.
    #[allow(unused)]
    fn fix_expr(&mut self,
                e: &mut P<Expr>,
                actual: ty::Ty<'tcx>,
                expected: ty::Ty<'tcx>) {
    }

    /// Called on each expr `e` that is the subject of an invalid cast: `e` has type `actual`,
    /// which cannot be cast to `target`.  Implementations should attempt to correct `e` to an expr
    /// that has a type castable to `target`.
    ///
    /// The default implementation dispatches to `fix_expr`, since fixing `e` to have type exactly
    /// `target` will certainly make the cast succeed.
    fn fix_expr_cast(&mut self,
                     e: &mut P<Expr>,
                     actual: ty::Ty<'tcx>,
                     target: ty::Ty<'tcx>) {
        self.fix_expr(e, actual, target)
    }

    /// Called on each expr `e` that contains a subexpr whose actual type doesn't match the
    /// expected type propagated down from `e`.
    fn fix_expr_parent(&mut self, e: &mut P<Expr>) {
    }
}

impl<'a, 'tcx, F: IlltypedFolder<'tcx>> IlltypedFolder<'tcx> for &'a mut F {
    fn fix_expr(&mut self,
                e: &mut P<Expr>,
                actual: ty::Ty<'tcx>,
                expected: ty::Ty<'tcx>) {
        <F as IlltypedFolder>::fix_expr(self, e, actual, expected)
    }

    fn fix_expr_cast(&mut self,
                     e: &mut P<Expr>,
                     actual: ty::Ty<'tcx>,
                     target: ty::Ty<'tcx>) {
        <F as IlltypedFolder>::fix_expr_cast(self, e, actual, target)
    }

    fn fix_expr_parent(&mut self, e: &mut P<Expr>) {
        <F as IlltypedFolder>::fix_expr_parent(self, e)
    }
}


struct FoldIlltyped<'a, 'tcx, F> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    inner: F,
}

impl<'a, 'tcx, F: IlltypedFolder<'tcx>> FoldIlltyped<'a, 'tcx, F> {
    /// Attempt to ensure that `expr` has the type `expected_ty`. Return true if
    /// retyping was needed.
    fn ensure(
        &mut self,
        expr: &mut P<Expr>,
        expected_ty: ty::Ty<'tcx>,
    ) -> bool {
        if let Some(actual_ty) = self.cx.opt_adjusted_node_type(expr.id) {
            if !types_approx_equal(self.cx.ty_ctxt(), actual_ty, expected_ty) {
                self.inner.fix_expr(expr, actual_ty, expected_ty);
                return true;
            }
        }
        false
    }

    /// Attempt to ensure that `expr` has the type `expected_ty`, inserting
    /// casts if needed. Return true if retyping was needed.
    fn ensure_cast(
        &mut self,
        sub_e: &mut P<Expr>,
        target_ty: ty::Ty<'tcx>,
    ) -> bool {
        if let Some(actual_ty) = self.cx.opt_adjusted_node_type(sub_e.id) {
            self.inner.fix_expr_cast(sub_e, actual_ty, target_ty);
            return true;
        }
        false
    }
}

impl<'a, 'tcx, F: IlltypedFolder<'tcx>> MutVisitor for FoldIlltyped<'a, 'tcx, F> {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        let mut illtyped = false;

        mut_visit::noop_visit_expr(e, self);

        let ty = match self.cx.opt_node_type(e.id) {
            Some(x) => x,
            None => return,
        };
        if let ty::TyKind::Error = ty.sty {
            return;
        }

        // We need the whole `Expr` to do this lookup, so it can't happen inside the match.
        let opt_fn_sig = self.cx.opt_callee_fn_sig(&e);

        let tcx = self.cx.ty_ctxt();

        match &mut e.node {
            ExprKind::Box(content) => {
                illtyped |= self.ensure(content, ty.boxed_ty());
            }
            ExprKind::ObsoleteInPlace(..) => {} // NYI
            ExprKind::Array(elems) => {
                let expected_elem_ty = ty.builtin_index().unwrap();
                for e in elems {
                    illtyped |= self.ensure(e, expected_elem_ty);
                }
            }
            ExprKind::Repeat(elem, count) => {
                let expected_elem_ty = ty.builtin_index().unwrap();
                illtyped |= self.ensure(elem, expected_elem_ty);
            }
            ExprKind::Tup(elems) => {
                let elem_tys = expect!([ty.sty] ty::TyKind::Tuple(elem_tys) => elem_tys);
                for (elem, elem_ty) in elems.iter_mut().zip(elem_tys) {
                    illtyped |= self.ensure(elem, elem_ty);
                }
            }
            ExprKind::Call(callee, args) => {
                if let Some(fn_sig) = opt_fn_sig {
                    for (i, arg) in args.iter_mut().enumerate() {
                        if let Some(&ty) = fn_sig.inputs().get(i) {
                            illtyped |= self.ensure(arg, ty);
                        }
                    }
                }
            }
            ExprKind::MethodCall(seg, args) => {
                if let Some(fn_sig) = opt_fn_sig {
                    for (i, arg) in args.iter_mut().enumerate() {
                        if let Some(&ty) = fn_sig.inputs().get(i) {
                            illtyped |= self.ensure(arg, ty);
                        }
                    }
                }
            }
            ExprKind::Binary(binop, lhs, rhs) => {
                use syntax::ast::BinOpKind::*;
                // TODO: check for overloads
                match binop.node {
                    Add | Sub | Mul | Div | Rem |
                    BitXor | BitAnd | BitOr => {
                        illtyped |= self.ensure(lhs, ty);
                        illtyped |= self.ensure(rhs, ty);
                    }
                    Eq | Lt | Le | Ne | Ge | Gt => {
                        if let Some(lhs_ty) = self.cx.opt_node_type(lhs.id) {
                            illtyped |= self.ensure(rhs, lhs_ty);
                        } else if let Some(rhs_ty) = self.cx.opt_node_type(rhs.id) {
                            illtyped |= self.ensure(lhs, rhs_ty);
                        }
                    }
                    Shl | Shr => {
                        illtyped |= self.ensure(lhs, ty);
                    }
                    And | Or => {
                        illtyped |= self.ensure(lhs, ty);
                        illtyped |= self.ensure(rhs, ty);
                    }
                }
            }
            ExprKind::Unary(binop, ohs) => {
                // TODO: need cases for deref, neg/not, and a check for overloads
            }
            ExprKind::Lit(l) => {} // TODO
            ExprKind::Cast(sub_e, target) => {
                // Check if the cast is erroneous.  We do this by looking up the subexpression
                // (yes, the subexpression) in the `cast_kinds` table - if there's nothing
                // there, it's not a valid cast.
                let parent = self.cx.hir_map().get_parent_did(sub_e.id);
                let tables = self.cx.ty_ctxt().typeck_tables_of(parent);
                let hir_id = self.cx.hir_map().node_to_hir_id(sub_e.id);
                if tables.cast_kinds().get(hir_id).is_none() {
                    illtyped |= self.ensure_cast(sub_e, ty);
                }
            }
            ExprKind::AddrOf(m, ohs) => {} // TODO
            ExprKind::If(cond, tr, fl) => {
                // TODO: do something clever with tr + fl
                illtyped |= self.ensure(cond, tcx.mk_bool());
            }
            ExprKind::IfLet(pats, expr, tr, fl) => {
                if let Some(pat_ty) = self.cx.opt_node_type(pats[0].id) {
                    illtyped |= self.ensure(expr, pat_ty);
                }
                // TODO: do something clever with tr + fl
                // TODO: handle discrepancies between different pattern tys
            }
            ExprKind::While(cond, body, opt_label) => {
                illtyped |= self.ensure(cond, tcx.mk_bool());
            }
            ExprKind::WhileLet(pats, expr, body, opt_label) => {
                if let Some(pat_ty) = self.cx.opt_node_type(pats[0].id) {
                    illtyped |= self.ensure(expr, pat_ty);
                }
            }
            ExprKind::Match(expr, arms) => {
                if let Some(pat_ty) = arms.get(0).and_then(
                    |arm| self.cx.opt_node_type(arm.pats[0].id)) {
                    illtyped |= self.ensure(expr, pat_ty);
                }
                // TODO: self.ensure arm bodies match ty
            }
            ExprKind::Block(blk, opt_label) => {
                // TODO: self.ensure last expr matches ty
            }
            ExprKind::Assign(el, er) => {
                let lhs_ty = self.cx.node_type(el.id);
                illtyped |= self.ensure(er, lhs_ty);
            }
            ExprKind::AssignOp(op, el, er) => {
                // TODO: need cases for arith/bitwise, shift, &&/||, and a check for overloads
                let lhs_ty = self.cx.node_type(el.id);
                illtyped |= self.ensure(er, lhs_ty);
            }
            ExprKind::Index(el, er) => {
                // TODO: check for overloads
                illtyped |= self.ensure(er, tcx.mk_mach_uint(UintTy::Usize));
            }
            ExprKind::Range(e1, e2, lim) => {
                // TODO: e1 & e2 should have the same type if both present
            }
            ExprKind::Struct(path, fields, maybe_expr) => {
                handle_struct(self.cx, e.id, ty, fields, maybe_expr, |e, ty| illtyped |= self.ensure(e, ty));
            }

        _ => {}
        };

        if illtyped {
            self.inner.fix_expr_parent(e)
        }
    }

    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let items = mut_visit::noop_flat_map_item(i, self);
        for i in items.iter_mut() {
            match &mut i.node {
                ItemKind::Static(ty, mutbl, expr) => {
                    let did = self.cx.node_def_id(i.id);
                    let expected_ty = self.cx.ty_ctxt().type_of(did);
                    info!("STATIC: expected ty {:?}, expr {:?}", expected_ty, expr);

                    let tcx = self.cx.ty_ctxt();
                    let node_id = tcx.hir().as_local_node_id(did).unwrap();
                    match tcx.hir().get(node_id) {
                        hir::Node::Item(item) => {
                            match item.node {
                                hir::ItemKind::Static(ref t, ..) => info!("  - ty hir = {:?}", t),
                                _ => {},
                            }
                        },
                        _ => {},
                    }

                    self.ensure(expr, expected_ty);
                }
                ItemKind::Const(ty, expr) => {
                    let did = self.cx.node_def_id(i.id);
                    let expected_ty = self.cx.ty_ctxt().type_of(did);
                    self.ensure(expr, expected_ty);
                }
                _ => {}
            }
        }

        items
    }
}

fn handle_struct<'tcx, F>(cx: &RefactorCtxt<'_, 'tcx>,
                          expr_id: NodeId,
                          ty: ty::Ty<'tcx>,
                          fields: &mut Vec<Field>,
                          maybe_expr: &mut Option<P<Expr>>,
                          mut ensure: F)
        where F: FnMut(&mut P<Expr>, ty::Ty<'tcx>) {
    let (adt_def, substs) = match ty.sty {
        ty::TyKind::Adt(a, s) => (a, s),
        _ => return,
    };

    // Get the variant def using the resolution of the path.
    let variant_hir_def = match_or!([resolve_struct_path(cx, expr_id)] Some(x) => x;
                                    return);
    let vdef = adt_def.variant_of_def(variant_hir_def);

    mut_visit::visit_vec(fields, |f| {
        let idx = match_or!([cx.ty_ctxt().find_field_index(f.ident, vdef)] Some(x) => x; return);
        let fdef = &vdef.fields[idx];
        let field_ty = fdef.ty(cx.ty_ctxt(), substs);
        ensure(&mut f.expr, field_ty);
    });
    mut_visit::visit_opt(maybe_expr, |e| ensure(e, ty));
}

fn resolve_struct_path(cx: &RefactorCtxt, id: NodeId) -> Option<Def> {
    let node = match_or!([cx.hir_map().find(id)] Some(x) => x; return None);
    let expr = match_or!([node] hir::Node::Expr(e) => e; return None);
    let qpath: &hir::QPath = match_or!([expr.node] hir::ExprKind::Struct(ref q, ..) => q; return None);
    let path = match_or!([qpath] hir::QPath::Resolved(_, ref path) => path; return None);
    Some(path.def)
}


pub fn fold_illtyped<'tcx, F, T>(cx: &RefactorCtxt<'_, 'tcx>, x: T, f: F)
        where F: IlltypedFolder<'tcx>, T: MutVisit {
    let mut f2 = FoldIlltyped { cx, inner: f };
    x.visit(&mut f2)
}
