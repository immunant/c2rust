use rustc::hir;
use rustc::hir::def_id::DefId;
use syntax::ast::*;
use syntax::fold::{self, Folder};
use syntax::ptr::P;

use driver;
use fold::Fold;
use util::HirDefExt;




struct ResolvedPathFolder<'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'a, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, DefId) -> (Option<QSelf>, Path) {
    cx: &'a driver::Ctxt<'a, 'hir, 'gcx, 'tcx>,
    callback: F,
}

impl<'a, 'hir, 'gcx, 'tcx, F> ResolvedPathFolder<'a, 'hir, 'gcx, 'tcx, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, DefId) -> (Option<QSelf>, Path) {
    pub fn alter_pat_path(&mut self, p: P<Pat>, hir: &hir::Pat) -> P<Pat> {
        match hir.node {
            hir::PatKind::Struct(ref qpath, _, _) => p.map(|p| {
                unpack!([p.node] PatKind::Struct(path, fields, dotdot));
                let (new_qself, new_path) = self.handle_qpath(p.id, None, path, qpath);
                assert!(new_qself.is_none(),
                        "can't insert QSelf at this location (PatKind::Struct)");
                Pat {
                    node: PatKind::Struct(new_path, fields, dotdot),
                    .. p
                }
            }),

            hir::PatKind::TupleStruct(ref qpath, _, _) => p.map(|p| {
                unpack!([p.node] PatKind::TupleStruct(path, fields, dotdot_pos));
                let (new_qself, new_path) = self.handle_qpath(p.id, None, path, qpath);
                assert!(new_qself.is_none(),
                        "can't insert QSelf at this location (PatKind::TupleStruct)");
                Pat {
                    node: PatKind::TupleStruct(new_path, fields, dotdot_pos),
                    .. p
                }
            }),

            hir::PatKind::Path(ref qpath) => p.map(|p| {
                let (qself, path) =
                    match p.node {
                        PatKind::Ident(BindingMode::ByValue(Mutability::Immutable),
                                       ident, None) =>
                            (None, Path::from_ident(ident.span, ident.node)),
                        PatKind::Path(qself, path) => (qself, path),
                        _ => panic!("expected PatKind::Ident or PatKind::Path"),
                    };
                let (new_qself, new_path) = self.handle_qpath(p.id, qself, path, qpath);
                Pat {
                    node: PatKind::Path(new_qself, new_path),
                    .. p
                }
            }),

            _ => p
        }
    }

    pub fn alter_expr_path(&mut self, e: P<Expr>, hir: &hir::Expr) -> P<Expr> {
        match hir.node {
            hir::ExprPath(ref qpath) => e.map(|e| {
                unpack!([e.node] ExprKind::Path(qself, path));
                let (new_qself, new_path) = self.handle_qpath(e.id, qself, path, qpath);
                Expr {
                    node: ExprKind::Path(new_qself, new_path),
                    .. e
                }
            }),

            hir::ExprStruct(ref qpath, _, _) => e.map(|e| {
                // Bail out early if it's not really a path type in the original AST.
                match e.node {
                    // Technically still a struct expression, but the struct to use is referenced
                    // via lang item, not by name.
                    ExprKind::Range(_, _, _) => return e,
                    _ => {},
                }

                unpack!([e.node] ExprKind::Struct(path, fields, base));
                let (new_qself, new_path) = self.handle_qpath(e.id, None, path, qpath);
                assert!(new_qself.is_none(),
                        "can't insert QSelf at this location (ExprKind::Struct)");
                Expr {
                    node: ExprKind::Struct(new_path, fields, base),
                    .. e
                }
            }),

            _ => e
        }
    }

    pub fn alter_ty_path(&mut self, t: P<Ty>, hir: &hir::Ty) -> P<Ty> {
        match hir.node {
            hir::TyPath(ref qpath) => t.map(|t| {
                // Bail out early if it's not really a path type in the original AST.
                match t.node {
                    TyKind::ImplicitSelf => return t,
                    _ => {},
                }

                unpack!([t.node] TyKind::Path(qself, path));
                let (new_qself, new_path) = self.handle_qpath(t.id, qself, path, qpath);
                Ty {
                    node: TyKind::Path(new_qself, new_path),
                    .. t
                }
            }),

            _ => t
        }
    }

    fn handle_qpath(&mut self,
                    id: NodeId,
                    qself: Option<QSelf>,
                    path: Path,
                    hir_qpath: &hir::QPath) -> (Option<QSelf>, Path) {
        match *hir_qpath {
            hir::QPath::Resolved(_, ref hir_path) => {
                if let Some(def_id) = hir_path.def.opt_def_id() {
                    (self.callback)(id, qself, path, def_id)
                } else {
                    (qself, path)
                }
            },

            hir::QPath::TypeRelative(ref hir_ty, _) => {
                let mut path = path;
                let tail = path.segments.pop().unwrap();
                let (new_qself, mut new_path) = self.handle_relative_path(id, qself, path, hir_ty);
                new_path.segments.push(tail);
                (new_qself, new_path)
            },
        }
    }

    fn handle_relative_path(&mut self,
                            id: NodeId,
                            qself: Option<QSelf>,
                            path: Path,
                            hir_ty: &hir::Ty) -> (Option<QSelf>, Path) {
        match hir_ty.node {
            hir::TyPath(ref qpath) => {
                self.handle_qpath(id, qself, path, qpath)
            },

            _ => (qself, path),
        }
    }
}

impl<'a, 'hir, 'gcx, 'tcx, F> Folder for ResolvedPathFolder<'a, 'hir, 'gcx, 'tcx, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, DefId) -> (Option<QSelf>, Path) {
    // There are several places in the AST that a `Path` can appear:
    //  - PatKind::Ident (single-element paths only)
    //  - PatKind::Struct
    //  - PatKind::TupleStruct
    //  - PatKind::Path
    //  - ExprKind::Path
    //  - ExprKind::Struct
    //  - Mac_.path
    //  - TyKind::Path
    //  - ViewPath_ (all variants)
    //  - Attribute.path
    //  - TraitRef.path
    //  - Visibility::Restricted.path
    //
    // We currently support the PatKind, ExprKind, and TyKind cases.  The rest are NYI.

    fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> {
        let p = match self.cx.hir_map().find(p.id) {
            Some(node) => {
                let hir = expect!([node]
                                  hir::map::NodePat(pat) => pat,
                                  hir::map::NodeLocal(pat) => pat);

                self.alter_pat_path(p, hir)
            },
            None => p,
        };

        fold::noop_fold_pat(p, self)
    }

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        let e = match self.cx.hir_map().find(e.id) {
            Some(node) => {
                let hir = expect!([node]
                                  hir::map::NodeExpr(expr) => expr);

                self.alter_expr_path(e, hir)
            },
            None => e,
        };

        e.map(|e| fold::noop_fold_expr(e, self))
    }

    fn fold_ty(&mut self, t: P<Ty>) -> P<Ty> {
        let t = match self.cx.hir_map().find(t.id) {
            Some(node) => {
                let hir = expect!([node]
                                  hir::map::NodeTy(ty) => ty);

                self.alter_ty_path(t, hir)
            },
            None => t,
        };

        fold::noop_fold_ty(t, self)
    }
}

pub fn fold_resolved_paths<T, F>(target: T, cx: &driver::Ctxt, mut callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(Option<QSelf>, Path, DefId) -> (Option<QSelf>, Path) {
    let mut f = ResolvedPathFolder {
        cx: cx,
        callback: |_, q, p, d| callback(q, p, d),
    };
    target.fold(&mut f)
}

pub fn fold_resolved_paths_with_id<T, F>(target: T, cx: &driver::Ctxt, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(NodeId, Option<QSelf>, Path, DefId) -> (Option<QSelf>, Path) {
    let mut f = ResolvedPathFolder {
        cx: cx,
        callback: callback,
    };
    target.fold(&mut f)
}
