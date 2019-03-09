//! `fold_resolved_paths` function, for rewriting paths based on their resolved `DefId`.
use rustc::hir;
use rustc::hir::def::Def;
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::mut_visit::{self, MutVisitor};
use syntax::ptr::P;
use syntax::util::move_map::MoveMap;

use crate::ast_manip::util::split_uses;
use crate::ast_manip::MutVisit;
use crate::RefactorCtxt;



struct ResolvedPathFolder<'a, 'tcx: 'a, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    callback: F,
}

impl<'a, 'tcx, F> ResolvedPathFolder<'a, 'tcx, F>
        where F: FnMut(NodeId, Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    // Some helper functions that get both the AST node and its HIR equivalent.

    pub fn alter_pat_path(&mut self, p: &mut P<Pat>, hir: &hir::Pat) {
        match hir.node {
            hir::PatKind::Struct(ref qpath, _, _) => {
                unpack!([p.node] PatKind::Struct(path, fields, dotdot));
                let (new_qself, new_path) = self.handle_qpath(p.id, None, path, qpath);
                assert!(new_qself.is_none(),
                        "can't insert QSelf at this location (PatKind::Struct)");
                p.node = PatKind::Struct(new_path, fields, dotdot);
            }

            hir::PatKind::TupleStruct(ref qpath, _, _) => {
                unpack!([p.node] PatKind::TupleStruct(path, fields, dotdot_pos));
                let (new_qself, new_path) = self.handle_qpath(p.id, None, path, qpath);
                assert!(new_qself.is_none(),
                        "can't insert QSelf at this location (PatKind::TupleStruct)");
                p.node = PatKind::TupleStruct(new_path, fields, dotdot_pos);
            }

            hir::PatKind::Path(ref qpath) => {
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
                if new_qself.is_none() && new_path.segments.len() == 1 {
                    p.node = PatKind::Ident(
                        BindingMode::ByValue(Mutability::Immutable),
                        new_path.segments[0].ident,
                        None,
                    );
                } else {
                    p.node = PatKind::Path(new_qself, new_path);
                };
            }

            _ => {}
        }
    }

    pub fn alter_expr_path(&mut self, e: &mut P<Expr>, hir: &hir::Expr) {
        match hir.node {
            hir::ExprKind::Path(ref qpath) => {
                unpack!([e.node] ExprKind::Path(qself, path));
                let (new_qself, new_path) = self.handle_qpath(e.id, qself, path, qpath);
                e.node = ExprKind::Path(new_qself, new_path);
            }

            hir::ExprKind::Struct(ref qpath, _, _) => {
                // Bail out early if it's not really a path type in the original AST.
                match e.node {
                    // Technically still a struct expression, but the struct to use is referenced
                    // via lang item, not by name.
                    ExprKind::Range(_, _, _) => return,
                    _ => {},
                }

                unpack!([e.node] ExprKind::Struct(path, fields, base));
                let (new_qself, new_path) = self.handle_qpath(e.id, None, path, qpath);
                assert!(new_qself.is_none(),
                        "can't insert QSelf at this location (ExprKind::Struct)");
                e.node = ExprKind::Struct(new_path, fields, base);
            }

            _ => {}
        }
    }

    pub fn alter_ty_path(&mut self, t: &mut P<Ty>, hir: &hir::Ty) {
        match hir.node {
            hir::TyKind::Path(ref qpath) => {
                // Bail out early if it's not really a path type in the original AST.
                match t.node {
                    TyKind::ImplicitSelf => return,
                    _ => {},
                }

                unpack!([t.node] TyKind::Path(qself, path));
                let (new_qself, new_path) = self.handle_qpath(t.id, qself, path, qpath);
                t.node = TyKind::Path(new_qself, new_path);
            }

            _ => {}
        }
    }

    pub fn alter_use_path(&mut self, mut item: P<Item>, hir: &hir::Item) -> P<Item> {
        // We are ignoring the extra namespaces in the Simple case. If we
        // need to handle these we can look up HIR nodes with the other
        // NodeIds in Simple().
        let id = item.id;
        unpack!([&mut item.node] ItemKind::Use(tree));
        if let hir::ItemKind::Use(ref hir_path, _) = hir.node {
            debug!("{:?}", hir_path);
            let (_, new_path) = (self.callback)(id, None, tree.prefix.clone(), &hir_path.def);
            tree.prefix = new_path;
        }

        item
    }

    /// Common implementation of path rewriting.  If the resolved `Def` of the path is available,
    /// rewrites the path using `self.callback`.  Otherwise the path is left unchanged.
    fn handle_qpath(
        &mut self,
        id: NodeId,
        qself: Option<QSelf>,
        path: Path,
        hir_qpath: &hir::QPath,
    ) -> (Option<QSelf>, Path) {
        match *hir_qpath {
            hir::QPath::Resolved(_, ref hir_path) => {
                (self.callback)(id, qself, path, &hir_path.def)
            }

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
            }
        }
    }

    /// Handle the base of a type-relative path.
    fn handle_relative_path(
        &mut self,
        id: NodeId,
        qself: Option<QSelf>,
        path: Path,
        hir_ty: &hir::Ty,
    ) -> (Option<QSelf>, Path) {
        match hir_ty.node {
            hir::TyKind::Path(ref qpath) => self.handle_qpath(id, qself, path, qpath),

            _ => (qself, path),
        }
    }
}

impl<'a, 'tcx, F> MutVisitor for ResolvedPathFolder<'a, 'tcx, F>
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
    //  - UseTree.prefix
    //
    // We currently support the PatKind, ExprKind, and TyKind cases.  The rest are NYI.

    fn visit_pat(&mut self, p: &mut P<Pat>) {
        if let Some(node) = self.cx.hir_map().find(p.id) {
            let hir = expect!([node]
                              hir::Node::Pat(pat) => pat,
                              hir::Node::Binding(pat) => pat);

            self.alter_pat_path(p, hir);
        }

        mut_visit::noop_visit_pat(p, self)
    }

    fn visit_expr(&mut self, e: &mut P<Expr>) {
        if let Some(node) = self.cx.hir_map().find(e.id) {
            let hir = expect!([node]
                              hir::Node::Expr(expr) => expr);

            self.alter_expr_path(e, hir);
        }

        mut_visit::noop_visit_expr(e, self)
    }

    fn visit_ty(&mut self, t: &mut P<Ty>) {
        if let Some(node) = self.cx.hir_map().find(t.id) {
            let hir = expect!([node]
                              hir::Node::Ty(ty) => ty);

            self.alter_ty_path(t, hir);
        }

        mut_visit::noop_visit_ty(t, self)
    }

    fn fold_item(&mut self, item: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let v = match item.node {
            ItemKind::Use(..) => {
                // We split nested uses into simple uses to make path rewriting
                // of use statements simpler.
                split_uses(item).move_map(|item| {
                    if let Some(node) = self.cx.hir_map().find(item.id) {
                        let hir = expect!([node] hir::Node::Item(i) => i);
                        self.alter_use_path(item, hir)
                    } else {
                        debug!("Couldn't find HIR node for {:?}", item);
                        item
                    }
                })
            }
            _ => smallvec![item],
        };

        v.move_flat_map(|item| fold::noop_fold_item(item, self))
    }
}

/// Rewrite paths, with access to their resolved `Def`s in the callback.
pub fn fold_resolved_paths<T, F>(target: T, cx: &RefactorCtxt, mut callback: F)
        where T: MutVisit,
              F: FnMut(Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    let mut f = ResolvedPathFolder {
        cx: cx,
        callback: |_, q, p, d| callback(q, p, d),
    };
    target.visit(&mut f)
}

/// Like `fold_resolved_paths`, but also passes the `NodeId` of the AST node containing the path.
/// (Paths don't have `NodeId`s of their own.)
pub fn fold_resolved_paths_with_id<T, F>(target: T, cx: &RefactorCtxt, callback: F)
        where T: MutVisit,
              F: FnMut(NodeId, Option<QSelf>, Path, &Def) -> (Option<QSelf>, Path) {
    let mut f = ResolvedPathFolder {
        cx: cx,
        callback: callback,
    };
    target.visit(&mut f)
}
