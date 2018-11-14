//! `fold_resolved_paths` function, for rewriting paths based on their resolved `DefId`.
use rustc::hir;
use rustc::hir::def::Def;
use syntax::ast::*;
use syntax::fold::{self, Folder};
use syntax::ptr::P;

use ast_manip::Fold;
use driver;


struct ResolvedPathFolder<'a, 'tcx: 'a, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    cx: &'a driver::Ctxt<'a, 'tcx>,
    callback: F,
}

impl<'a, 'tcx, F> ResolvedPathFolder<'a, 'tcx, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    // Some helper functions that get both the AST node and its HIR equivalent.

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
                            (None, Path::from_ident(ident)),
                        PatKind::Path(qself, path) => (qself, path),
                        _ => panic!("expected PatKind::Ident or PatKind::Path"),
                    };
                let (new_qself, new_path) = self.handle_qpath(p.id, qself, path, qpath);

                // If it's a single-element path like `None`, emit PatKind::Ident instead of
                // PatKind::Path.  The parser treats `None` as an Ident, so if we emit Paths
                // instead, we run into "new and reparsed ASTs don't match" during rewriting.
                let node = if new_qself.is_none() && new_path.segments.len() == 1 {
                    PatKind::Ident(BindingMode::ByValue(Mutability::Immutable),
                                   new_path.segments[0].ident, None)
                } else {
                    PatKind::Path(new_qself, new_path)
                };

                Pat { node, .. p }
            }),

            _ => p
        }
    }

    pub fn alter_expr_path(&mut self, e: P<Expr>, hir: &hir::Expr) -> P<Expr> {
        match hir.node {
            hir::ExprKind::Path(ref qpath) => e.map(|e| {
                unpack!([e.node] ExprKind::Path(qself, path));
                let (new_qself, new_path) = self.handle_qpath(e.id, qself, path, qpath);
                Expr {
                    node: ExprKind::Path(new_qself, new_path),
                    .. e
                }
            }),

            hir::ExprKind::Struct(ref qpath, _, _) => e.map(|e| {
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
            hir::TyKind::Path(ref qpath) => t.map(|t| {
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


    /// Common implementation of path rewriting.  If the resolved `Def` of the path is available,
    /// rewrites the path using `self.callback`.  Otherwise the path is left unchanged.
    fn handle_qpath(&mut self,
                    id: NodeId,
                    qself: Option<QSelf>,
                    path: Path,
                    hir_qpath: &hir::QPath) -> (Option<QSelf>, Path) {
        match *hir_qpath {
            hir::QPath::Resolved(_, ref hir_path) => {
                (self.callback)(id, qself, path, &hir_path.def)
            },

            hir::QPath::TypeRelative(ref hir_ty, _) => {
                // If the path is type-relative, then no `DefId` is available for the whole path.
                // However, we might still be able to do something with the base `Ty`.  Pop off the
                // last segment, which is the name of the associated item, and recursively try to
                // process the `Ty`.
                let mut path = path;
                let tail = path.segments.pop().unwrap();
                let (new_qself, mut new_path) = self.handle_relative_path(id, qself, path, hir_ty);
                new_path.segments.push(tail);
                (new_qself, new_path)
            },
        }
    }

    /// Handle the base of a type-relative path.
    fn handle_relative_path(&mut self,
                            id: NodeId,
                            qself: Option<QSelf>,
                            path: Path,
                            hir_ty: &hir::Ty) -> (Option<QSelf>, Path) {
        match hir_ty.node {
            hir::TyKind::Path(ref qpath) => {
                self.handle_qpath(id, qself, path, qpath)
            },

            _ => (qself, path),
        }
    }
}

impl<'a, 'tcx, F> Folder for ResolvedPathFolder<'a, 'tcx, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
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
                                  hir::Node::Pat(pat) => pat,
                                  hir::Node::Binding(pat) => pat);

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
                                  hir::Node::Expr(expr) => expr);

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
                                  hir::Node::Ty(ty) => ty);

                self.alter_ty_path(t, hir)
            },
            None => t,
        };

        fold::noop_fold_ty(t, self)
    }
}

/// Rewrite paths, with access to their resolved `Def`s in the callback.
pub fn fold_resolved_paths<T, F>(target: T, cx: &driver::Ctxt, mut callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    let mut f = ResolvedPathFolder {
        cx: cx,
        callback: |_, q, p, d| callback(q, p, d),
    };
    target.fold(&mut f)
}

/// Like `fold_resolved_paths`, but also passes the `NodeId` of the AST node containing the path.
/// (Paths don't have `NodeId`s of their own.)
pub fn fold_resolved_paths_with_id<T, F>(target: T, cx: &driver::Ctxt, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(NodeId, Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    let mut f = ResolvedPathFolder {
        cx: cx,
        callback: callback,
    };
    target.fold(&mut f)
}
