//! `fold_resolved_paths` function, for rewriting paths based on their resolved `DefId`.
use log::debug;
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_data_structures::map_in_place::MapInPlace;
use rustc_hir as hir;
use rustc_hir::def::Res;
use smallvec::smallvec;
use smallvec::SmallVec;

use crate::ast_manip::util::split_uses;
use crate::ast_manip::MutVisit;
use crate::RefactorCtxt;
use crate::{expect, unpack};

struct ResolvedPathFolder<'a, 'tcx: 'a, F>
where
    F: FnMut(NodeId, Option<QSelf>, Path, &[Res]) -> (Option<QSelf>, Path),
{
    cx: &'a RefactorCtxt<'a, 'tcx>,
    callback: F,
}

impl<'a, 'tcx, F> ResolvedPathFolder<'a, 'tcx, F>
where
    F: FnMut(NodeId, Option<QSelf>, Path, &[Res]) -> (Option<QSelf>, Path),
{
    // Some helper functions that get both the AST node and its HIR equivalent.

    pub fn alter_pat_path(&mut self, p: &mut P<Pat>, hir: &hir::Pat) {
        let id = p.id;
        match hir.kind {
            hir::PatKind::Struct(ref qpath, _, _) => {
                unpack!([&mut p.kind] PatKind::Struct(_qself, path, _fields, _dotdot));
                // TODO: can we pass _qself to handle_qpath???
                let (new_qself, new_path) = self.handle_qpath(id, None, path.clone(), qpath);
                assert!(
                    new_qself.is_none(),
                    "can't insert QSelf at this location (PatKind::Struct)"
                );
                *path = new_path;
            }

            hir::PatKind::TupleStruct(ref qpath, _, _) => {
                unpack!([&mut p.kind] PatKind::TupleStruct(_qself, path, _fields));
                // TODO: can we pass _qself to handle_qpath???
                let (new_qself, new_path) = self.handle_qpath(id, None, path.clone(), qpath);
                assert!(
                    new_qself.is_none(),
                    "can't insert QSelf at this location (PatKind::TupleStruct)"
                );
                *path = new_path;
            }

            hir::PatKind::Path(ref qpath) => {
                let (qself, path) = match &mut p.kind {
                    PatKind::Ident(BindingMode::ByValue(Mutability::Not), ident, None) => {
                        (None, Path::from_ident(*ident))
                    }
                    PatKind::Path(qself, path) => (qself.clone(), path.clone()),
                    _ => panic!("expected PatKind::Ident or PatKind::Path"),
                };
                let (new_qself, new_path) = self.handle_qpath(id, qself, path, qpath);

                // If it's a single-element path like `None`, emit PatKind::Ident instead of
                // PatKind::Path.  The parser treats `None` as an Ident, so if we emit Paths
                // instead, we run into "new and reparsed ASTs don't match" during rewriting.
                if new_qself.is_none() && new_path.segments.len() == 1 {
                    p.kind = PatKind::Ident(
                        BindingMode::ByValue(Mutability::Not),
                        new_path.segments[0].ident,
                        None,
                    );
                } else {
                    p.kind = PatKind::Path(new_qself, new_path);
                };
            }

            _ => {}
        }
    }

    pub fn alter_expr_path(&mut self, e: &mut P<Expr>, hir: &hir::Expr) {
        let id = e.id;
        match hir.kind {
            hir::ExprKind::Path(ref qpath) => {
                unpack!([&mut e.kind] ExprKind::Path(qself, path));
                let (new_qself, new_path) =
                    self.handle_qpath(id, qself.clone(), path.clone(), qpath);
                e.kind = ExprKind::Path(new_qself, new_path);
            }

            hir::ExprKind::Struct(ref qpath, _, _) => {
                // Bail out early if it's not really a path type in the original AST.
                match e.kind {
                    // Technically still a struct expression, but the struct to use is referenced
                    // via lang item, not by name.
                    ExprKind::Range(_, _, _) => return,
                    _ => {}
                }

                let path = expect!([&mut e.kind] ExprKind::Struct(se) => &mut se.path);
                // TODO: can we pass _qself to handle_qpath???
                let (new_qself, new_path) = self.handle_qpath(id, None, path.clone(), qpath);
                assert!(
                    new_qself.is_none(),
                    "can't insert QSelf at this location (ExprKind::Struct)"
                );
                *path = new_path;
            }

            _ => {}
        }
    }

    pub fn alter_ty_path(&mut self, t: &mut P<Ty>, hir: &hir::Ty) {
        let id = t.id;
        match hir.kind {
            hir::TyKind::Path(ref qpath) => {
                // Bail out early if it's not really a path type in the original AST.
                match t.kind {
                    TyKind::ImplicitSelf => return,
                    _ => {}
                }

                unpack!([&mut t.kind] TyKind::Path(qself, path));
                let (new_qself, new_path) =
                    self.handle_qpath(id, qself.clone(), path.clone(), qpath);
                *qself = new_qself;
                *path = new_path;
            }

            _ => {}
        }
    }

    pub fn alter_use_path(&mut self, item: &mut P<Item>, nodes: &[hir::Node]) {
        let id = item.id;
        unpack!([&mut item.kind] ItemKind::Use(tree));
        let resolutions: Vec<_> = nodes
            .iter()
            .map(|node| {
                let hir = expect!([node] hir::Node::Item(i) => i);
                if let hir::ItemKind::Use(ref hir_path, _) = hir.kind {
                    debug!("{:?}", hir_path);
                    Some(hir_path.res)
                } else {
                    None
                }
            })
            .flatten()
            .collect();
        let (_, new_path) = (self.callback)(id, None, tree.prefix.clone(), &resolutions);
        tree.prefix = new_path;
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
                (self.callback)(id, qself, path, &[hir_path.res])
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

            hir::QPath::LangItem(..) => unimplemented!(),
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
        match hir_ty.kind {
            hir::TyKind::Path(ref qpath) => self.handle_qpath(id, qself, path, qpath),

            _ => (qself, path),
        }
    }
}

impl<'a, 'tcx, F> MutVisitor for ResolvedPathFolder<'a, 'tcx, F>
where
    F: FnMut(NodeId, Option<QSelf>, Path, &[Res]) -> (Option<QSelf>, Path),
{
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
            let hir = expect!([node] hir::Node::Pat(pat) => pat);

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

    fn flat_map_item(&mut self, item: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let mut v = match item.kind {
            ItemKind::Use(..) => {
                // We split nested uses into simple uses to make path rewriting
                // of use statements simpler.
                let mut uses = split_uses(item);
                for item in uses.iter_mut() {
                    let use_tree = expect!([&item.kind] ItemKind::Use(u) => u);
                    let mut hir_nodes = vec![self.cx.hir_map().find(item.id)];
                    if let UseTreeKind::Simple(_, i, j) = &use_tree.kind {
                        hir_nodes.push(self.cx.hir_map().find(*i));
                        hir_nodes.push(self.cx.hir_map().find(*j));
                    }
                    let hir_nodes: Vec<_> = hir_nodes.into_iter().flatten().collect();
                    self.alter_use_path(item, &hir_nodes);
                }
                uses
            }
            _ => smallvec![item],
        };

        v.flat_map_in_place(|item| mut_visit::noop_flat_map_item(item, self));
        v
    }
}

/// Rewrite paths, with access to their resolved `Def`s in the callback.
pub fn fold_resolved_paths<T, F>(target: &mut T, cx: &RefactorCtxt, mut callback: F)
where
    T: MutVisit,
    F: FnMut(Option<QSelf>, Path, &[Res]) -> (Option<QSelf>, Path),
{
    let mut f = ResolvedPathFolder {
        cx,
        callback: |_, q, p, d| callback(q, p, d),
    };
    target.visit(&mut f)
}

/// Like `fold_resolved_paths`, but also passes the `NodeId` of the AST node containing the path.
/// (Paths don't have `NodeId`s of their own.)
pub fn fold_resolved_paths_with_id<T, F>(target: &mut T, cx: &RefactorCtxt, callback: F)
where
    T: MutVisit,
    F: FnMut(NodeId, Option<QSelf>, Path, &[Res]) -> (Option<QSelf>, Path),
{
    let mut f = ResolvedPathFolder { cx, callback };
    target.visit(&mut f)
}
