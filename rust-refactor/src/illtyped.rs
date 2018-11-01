use std::cell::{Cell, RefCell};
use rustc::hir;
use rustc::hir::def::Def;
use rustc::ty;
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::move_map::MoveMap;

use api::*;
use driver;


pub trait IlltypedFolder<'tcx> {
    /// Called on each expr `e` whose `actual` type doesn't match the `expected` type propagated
    /// down from its parent.  Implementations should attempt to correct `e` to an expr that has
    /// type `expected`.
    #[allow(unused)]
    fn fix_expr(&mut self,
                e: P<Expr>,
                actual: ty::Ty<'tcx>,
                expected: ty::Ty<'tcx>) -> P<Expr> {
        e
    }

    /// Called on each expr `e` that is the subject of an invalid cast: `e` has type `actual`,
    /// which cannot be cast to `target`.  Implementations should attempt to correct `e` to an expr
    /// that has a type castable to `target`.
    ///
    /// The default implementation dispatches to `fix_expr`, since fixing `e` to have type exactly
    /// `target` will certainly make the cast succeed.
    fn fix_expr_cast(&mut self,
                     e: P<Expr>,
                     actual: ty::Ty<'tcx>,
                     target: ty::Ty<'tcx>) -> P<Expr> {
        self.fix_expr(e, actual, target)
    }

    /// Called on each expr `e` that contains a subexpr whose actual type doesn't match the
    /// expected type propagated down from `e`.
    fn fix_expr_parent(&mut self, e: P<Expr>) -> P<Expr> {
        e
    }
}

impl<'a, 'tcx, F: IlltypedFolder<'tcx>> IlltypedFolder<'tcx> for &'a mut F {
    fn fix_expr(&mut self,
                e: P<Expr>,
                actual: ty::Ty<'tcx>,
                expected: ty::Ty<'tcx>) -> P<Expr> {
        <F as IlltypedFolder>::fix_expr(self, e, actual, expected)
    }

    fn fix_expr_cast(&mut self,
                     e: P<Expr>,
                     actual: ty::Ty<'tcx>,
                     target: ty::Ty<'tcx>) -> P<Expr> {
        <F as IlltypedFolder>::fix_expr_cast(self, e, actual, target)
    }

    fn fix_expr_parent(&mut self, e: P<Expr>) -> P<Expr> {
        <F as IlltypedFolder>::fix_expr_parent(self, e)
    }
}


struct FoldIlltyped<'a, 'tcx, F> {
    cx: &'a driver::Ctxt<'a, 'tcx>,
    inner: F,
}

impl<'a, 'tcx, F: IlltypedFolder<'tcx>> FoldIlltyped<'a, 'tcx, F> {
    fn ensure(&mut self,
              sub_e: P<Expr>,
              expected_ty: ty::Ty<'tcx>,
              illtyped: Option<&Cell<bool>>) -> P<Expr> {
        if let Some(actual_ty) = self.cx.opt_node_type(sub_e.id) {
            if actual_ty != expected_ty {
                illtyped.map(|i| i.set(true));
                return self.inner.fix_expr(sub_e, actual_ty, expected_ty);
            }
        }
        sub_e
    }

    fn ensure_cast(&mut self,
                   sub_e: P<Expr>,
                   target_ty: ty::Ty<'tcx>,
                   illtyped: Option<&Cell<bool>>) -> P<Expr> {
        if let Some(actual_ty) = self.cx.opt_node_type(sub_e.id) {
            illtyped.map(|i| i.set(true));
            return self.inner.fix_expr_cast(sub_e, actual_ty, target_ty);
        }
        sub_e
    }
}

impl<'a, 'tcx, F: IlltypedFolder<'tcx>> Folder for FoldIlltyped<'a, 'tcx, F> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        let cx = self.cx;
        let this = RefCell::new(self);

        let illtyped = Cell::new(false);
        let ensure = |sub_e, expected_ty| {
            this.borrow_mut().ensure(sub_e, expected_ty, Some(&illtyped))
        };
        let ensure_cast = |sub_e, target_ty| {
            this.borrow_mut().ensure_cast(sub_e, target_ty, Some(&illtyped))
        };

        let e = e.map(|e| {
            let e = fold::noop_fold_expr(e, &mut **this.borrow_mut());


            let ty = match cx.opt_node_type(e.id) {
                Some(x) => x,
                None => return e,
            };

            // We need the whole `Expr` to do this lookup, so it can't happen inside the match.
            let opt_fn_sig = cx.opt_callee_fn_sig(&e);

            let tcx = cx.ty_ctxt();

            let node = match e.node {
                ExprKind::Box(content) => {
                    ExprKind::Box(ensure(content, ty.boxed_ty()))
                }
                n @ ExprKind::ObsoleteInPlace(..) => n, // NYI
                ExprKind::Array(elems) => {
                    let expected_elem_ty = ty.builtin_index().unwrap();
                    ExprKind::Array(elems.move_map(|e| ensure(e, expected_elem_ty)))
                }
                ExprKind::Repeat(elem, count) => {
                    let expected_elem_ty = ty.builtin_index().unwrap();
                    ExprKind::Repeat(ensure(elem, expected_elem_ty), count)
                }
                ExprKind::Tup(elems) => {
                    let elem_tys = expect!([ty.sty] ty::TyKind::Tuple(elem_tys) => elem_tys);
                    ExprKind::Tup(elems.into_iter().zip(elem_tys)
                                  .map(|(elem, elem_ty)| ensure(elem, elem_ty))
                                  .collect())
                }
                ExprKind::Call(callee, args) => {
                    if let Some(fn_sig) = opt_fn_sig {
                        let mut retyped_args = Vec::with_capacity(args.len());
                        for (i, arg) in args.into_iter().enumerate() {
                            if let Some(&ty) = fn_sig.inputs().get(i) {
                                retyped_args.push(ensure(arg, ty));
                            } else {
                                retyped_args.push(arg);
                            }
                        }
                        ExprKind::Call(callee, retyped_args)
                    } else {
                        ExprKind::Call(callee, args)
                    }
                }
                ExprKind::MethodCall(seg, args) => {
                    if let Some(fn_sig) = opt_fn_sig {
                        let mut retyped_args = Vec::with_capacity(args.len());
                        for (i, arg) in args.into_iter().enumerate() {
                            if let Some(&ty) = fn_sig.inputs().get(i) {
                                retyped_args.push(ensure(arg, ty));
                            } else {
                                retyped_args.push(arg);
                            }
                        }
                        ExprKind::MethodCall(seg, retyped_args)
                    } else {
                        ExprKind::MethodCall(seg, args)
                    }
                },
                ExprKind::Binary(binop, lhs, rhs) => {
                    // TODO: need cases for arith + bitwise, shift, comparison, &&/||, and a check
                    // for overloads
                    ExprKind::Binary(binop, lhs, rhs)
                }
                ExprKind::Unary(binop, ohs) => {
                    // TODO: need cases for deref, neg/not, and a check for overloads
                    ExprKind::Unary(binop, ohs)
                }
                ExprKind::Lit(l) => ExprKind::Lit(l),   // TODO
                ExprKind::Cast(sub_e, target) => {
                    // Check if the cast is erroneous.  We do this by looking up the subexpression
                    // (yes, the subexpression) in the `cast_kinds` table - if there's nothing
                    // there, it's not a valid cast.
                    let parent = cx.hir_map().get_parent_did(sub_e.id);
                    let tables = cx.ty_ctxt().typeck_tables_of(parent);
                    let hir_id = cx.hir_map().node_to_hir_id(sub_e.id);
                    if tables.cast_kinds().get(hir_id).is_none() {
                        ExprKind::Cast(ensure_cast(sub_e, ty), target)
                    } else {
                        ExprKind::Cast(sub_e, target)
                    }
                }
                ExprKind::Type(sub_e, ascribed) => {
                    ExprKind::Type(sub_e, ascribed)
                }
                ExprKind::AddrOf(m, ohs) => ExprKind::AddrOf(m, ohs),   // TODO
                ExprKind::If(cond, tr, fl) => {
                    // TODO: do something clever with tr + fl
                    ExprKind::If(ensure(cond, tcx.mk_bool()), tr, fl)
                }
                ExprKind::IfLet(pats, expr, tr, fl) => {
                    let expr = if let Some(pat_ty) = cx.opt_node_type(pats[0].id) {
                        ensure(expr, pat_ty)
                    } else {
                        expr
                    };
                    // TODO: do something clever with tr + fl
                    // TODO: handle discrepancies between different pattern tys
                    ExprKind::IfLet(pats, expr, tr, fl)
                }
                ExprKind::While(cond, body, opt_label) => {
                    ExprKind::While(ensure(cond, tcx.mk_bool()), body, opt_label)
                }
                ExprKind::WhileLet(pats, expr, body, opt_label) => {
                    let expr = if let Some(pat_ty) = cx.opt_node_type(pats[0].id) {
                        ensure(expr, pat_ty)
                    } else {
                        expr
                    };
                    ExprKind::WhileLet(pats, expr, body, opt_label)
                }
                ExprKind::ForLoop(pat, iter, body, opt_label) => {
                    ExprKind::ForLoop(pat, iter, body, opt_label)
                }
                ExprKind::Loop(body, opt_label) => {
                    ExprKind::Loop(body, opt_label)
                }
                ExprKind::Match(expr, arms) => {
                    let expr = if let Some(pat_ty) = arms.get(0).and_then(
                            |arm| cx.opt_node_type(arm.pats[0].id)) {
                        ensure(expr, pat_ty)
                    } else {
                        expr
                    };
                    // TODO: ensure arm bodies match ty
                    ExprKind::Match(expr, arms)
                }
                ExprKind::Closure(capture_clause, asyncness, movability, decl, body, span) => {
                    ExprKind::Closure(capture_clause, asyncness, movability, decl, body, span)
                }
                ExprKind::Block(blk, opt_label) => {
                    // TODO: ensure last expr matches ty
                    ExprKind::Block(blk, opt_label)
                }
                ExprKind::Async(capture_clause, node_id, body) => {
                    ExprKind::Async(capture_clause, node_id, body)
                }
                ExprKind::Assign(el, er) => {
                    let lhs_ty = cx.node_type(el.id);
                    ExprKind::Assign(el, ensure(er, lhs_ty))
                }
                ExprKind::AssignOp(op, el, er) => {
                    // TODO: need cases for arith/bitwise, shift, &&/||, and a check for overloads
                    let lhs_ty = cx.node_type(el.id);
                    ExprKind::AssignOp(op, el, ensure(er, lhs_ty))
                }
                ExprKind::Field(el, ident) => {
                    ExprKind::Field(el, ident)
                }
                ExprKind::Index(el, er) => {
                    // TODO: normal case
                    // TODO: check for overloads
                    ExprKind::Index(el, er)
                }
                ExprKind::Range(e1, e2, lim) => {
                    // TODO: e1 & e2 should have the same type if both present
                    ExprKind::Range(e1, e2, lim)
                }
                ExprKind::Path(qself, path) => {
                    ExprKind::Path(qself, path)
                }
                ExprKind::Break(opt_label, opt_expr) => {
                    ExprKind::Break(opt_label, opt_expr)
                }
                ExprKind::Continue(opt_label) => {
                    ExprKind::Continue(opt_label)
                }
                ExprKind::Ret(e) => ExprKind::Ret(e),
                ExprKind::InlineAsm(asm) => ExprKind::InlineAsm(asm),
                ExprKind::Mac(mac) => ExprKind::Mac(mac),
                ExprKind::Struct(path, fields, maybe_expr) => {
                    let (fields, maybe_expr) = handle_struct(
                        cx, e.id, ty, fields, maybe_expr, |e, ty| ensure(e, ty));
                    ExprKind::Struct(path, fields, maybe_expr)
                },
                ExprKind::Paren(ex) => ExprKind::Paren(ex),
                ExprKind::Yield(ex) => ExprKind::Yield(ex),
                ExprKind::Try(ex) => ExprKind::Try(ex),
                ExprKind::TryBlock(body) => ExprKind::TryBlock(body),
            };

            Expr { node, ..e }
        });

        if illtyped.get() {
            this.borrow_mut().inner.fix_expr_parent(e)
        } else {
            e
        }
    }

    fn fold_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        fold::noop_fold_item(i, self).move_map(|i| i.map(|mut i| {
            i.node = match i.node {
                ItemKind::Static(ty, mutbl, expr) => {
                    let did = self.cx.node_def_id(i.id);
                    let expected_ty = self.cx.ty_ctxt().type_of(did);
                    info!("STATIC: expected ty {:?}, expr {:?}", expected_ty, expr);

                    let tcx = self.cx.ty_ctxt();
                    let node_id = tcx.hir.as_local_node_id(did).unwrap();
                    match tcx.hir.get(node_id) {
                        hir::Node::Item(item) => {
                            match item.node {
                                hir::ItemKind::Static(ref t, ..) => info!("  - ty hir = {:?}", t),
                                _ => {},
                            }
                        },
                        _ => {},
                    }

                    let expr = self.ensure(expr, expected_ty, None);
                    ItemKind::Static(ty, mutbl, expr)
                },
                ItemKind::Const(ty, expr) => {
                    let did = self.cx.node_def_id(i.id);
                    let expected_ty = self.cx.ty_ctxt().type_of(did);
                    let expr = self.ensure(expr, expected_ty, None);
                    ItemKind::Const(ty, expr)
                },
                n => n,
            };
            i
        }))
    }
}

fn handle_struct<'tcx, F>(cx: &driver::Ctxt<'_, 'tcx>,
                          expr_id: NodeId,
                          ty: ty::Ty<'tcx>,
                          fields: Vec<Field>,
                          maybe_expr: Option<P<Expr>>,
                          mut ensure: F) -> (Vec<Field>, Option<P<Expr>>)
        where F: FnMut(P<Expr>, ty::Ty<'tcx>) -> P<Expr> {
    let (adt_def, substs) = match ty.sty {
        ty::TyKind::Adt(a, s) => (a, s),
        _ => return (fields, maybe_expr),
    };

    // Get the variant def using the resolution of the path.
    let variant_hir_def = match_or!([resolve_struct_path(cx, expr_id)] Some(x) => x;
                                    return (fields, maybe_expr));
    let vdef = adt_def.variant_of_def(variant_hir_def);

    let fields = fields.move_map(|f| {
        let idx = match_or!([cx.ty_ctxt().find_field_index(f.ident, vdef)] Some(x) => x; return f);
        let fdef = &vdef.fields[idx];
        let field_ty = fdef.ty(cx.ty_ctxt(), substs);
        Field { expr: ensure(f.expr, field_ty), ..f }
    });
    let maybe_expr = maybe_expr.map(|e| ensure(e, ty));
    (fields, maybe_expr)
}

fn resolve_struct_path(cx: &driver::Ctxt, id: NodeId) -> Option<Def> {
    let node = match_or!([cx.hir_map().find(id)] Some(x) => x; return None);
    let expr = match_or!([node] hir::Node::Expr(e) => e; return None);
    let qpath = match_or!([expr.node] hir::ExprKind::Struct(ref q, ..) => q; return None);
    let path = match_or!([qpath] hir::QPath::Resolved(_, ref path) => path; return None);
    Some(path.def)
}


pub fn fold_illtyped<'tcx, F, T>(cx: &driver::Ctxt<'_, 'tcx>, x: T, f: F) -> <T as Fold>::Result
        where F: IlltypedFolder<'tcx>, T: Fold {
    let mut f2 = FoldIlltyped { cx, inner: f };
    x.fold(&mut f2)
}
