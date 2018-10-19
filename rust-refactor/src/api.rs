//! A variety of helpers for writing transformations.  Meant to be glob-imported by transform
//! implementation modules.
use rustc::hir;
use rustc::hir::def_id::DefId;
use rustc::hir::Node;
use rustc::ty::Ty;
use syntax::ast;    // Can't glob-import because `Ty` already refers to `rustc::ty::Ty`.
use syntax::ast::{NodeId, DUMMY_NODE_ID};
use syntax::ast::{Expr, ExprKind};
use syntax::ast::{Path, QSelf};

// Reexports of various helpers
pub use ast_manip::*;
pub use ast_manip::fn_edit::{fold_fns, fold_fns_multi};
pub use ast_manip::lr_expr::fold_expr_with_context;
pub use ast_manip::make_ast::mk;
pub use driver::{parse_expr, parse_pat, parse_ty, parse_stmts, parse_items};
pub use matcher::{MatchCtxt, Bindings, BindingType, Subst};
pub use matcher::{fold_match, fold_match_with};
pub use path_edit::{self, fold_resolved_paths, fold_resolved_paths_with_id};

use matcher::Pattern;
use command::CommandState;
use driver;
use reflect;
use util::HirDefExt;


/// Replace all instances of expression `pat` with expression `repl`.
pub fn replace_expr<T: Fold>(st: &CommandState,
                             cx: &driver::Ctxt,
                             ast: T,
                             pat: &str,
                             repl: &str) -> <T as Fold>::Result {
    let pat = parse_expr(cx.session(), pat);
    let repl = parse_expr(cx.session(), repl);
    fold_match(st, cx, pat, ast, |_, bnd| repl.clone().subst(st, cx, &bnd))
}

/// Replace all instances of the statement sequence `pat` with `repl`.
pub fn replace_stmts<T: Fold>(st: &CommandState,
                              cx: &driver::Ctxt,
                              ast: T,
                              pat: &str,
                              repl: &str) -> <T as Fold>::Result {
    let pat = parse_stmts(cx.session(), pat);
    let repl = parse_stmts(cx.session(), repl);
    fold_match(st, cx, pat, ast, |_, bnd| repl.clone().subst(st, cx, &bnd))
}


/// Find the first place where `pattern` matches under initial context `init_mcx`, and return the
/// resulting `Bindings`.
pub fn find_first_with<P, T>(init_mcx: MatchCtxt,
                             pattern: P,
                             target: T) -> Option<Bindings>
        where P: Pattern, T: Fold {
    let mut result = None;
    fold_match_with(init_mcx, pattern, target, |p, bnd| {
        if result.is_none() {
            result = Some(bnd);
        }
        p
    });
    result
}

/// Find the first place where `pattern` matches, and return the resulting `Bindings`.
pub fn find_first<P, T>(st: &CommandState,
                        cx: &driver::Ctxt,
                        pattern: P,
                        target: T) -> Option<Bindings>
        where P: Pattern, T: Fold {
    find_first_with(MatchCtxt::new(st, cx), pattern, target)
}


/// `driver::Ctxt` extension trait.
pub trait DriverCtxtExt<'tcx> {
    /// Get the `ty::Ty` computed for a node.
    fn node_type(&self, id: NodeId) -> Ty<'tcx>;
    /// Get the `ty::Ty` computed for a node, taking into account any adjustments that were applied.
    fn adjusted_node_type(&self, id: NodeId) -> Ty<'tcx>;

    fn def_type(&self, id: DefId) -> Ty<'tcx>;
    /// Build a `Path` referring to a particular def.  This method returns an absolute path when
    /// possible.
    fn def_path(&self, id: DefId) -> Path;
    fn def_qpath(&self, id: DefId) -> (Option<QSelf>, Path);

    /// Obtain the `DefId` of a definition node, such as a `fn` item.
    fn node_def_id(&self, id: NodeId) -> DefId;

    /// Get the target `DefId` of a path expr.
    fn resolve_expr(&self, e: &Expr) -> DefId;
    fn try_resolve_expr(&self, e: &Expr) -> Option<DefId>;

    fn def_to_hir_id(&self, def: &hir::def::Def) -> Option<hir::HirId>;
    fn try_resolve_expr_to_hid(&self, e: &Expr) -> Option<hir::HirId>;

    /// Get the target `DefId` of a path ty.
    fn resolve_ty(&self, e: &ast::Ty) -> DefId;
    fn try_resolve_ty(&self, e: &ast::Ty) -> Option<DefId>;

    /// Get the `DefId` of the function or method being called by a `Call` or `MethodCall` expr.
    fn callee(&self, e: &Expr) -> DefId;
    fn opt_callee(&self, e: &Expr) -> Option<DefId>;
}

impl<'a, 'tcx> DriverCtxtExt<'tcx> for driver::Ctxt<'a, 'tcx> {
    fn node_type(&self, id: NodeId) -> Ty<'tcx> {
        let parent = self.hir_map().get_parent_did(id);
        let tables = self.ty_ctxt().typeck_tables_of(parent);
        let hir_id = self.hir_map().node_to_hir_id(id);
        tables.node_id_to_type(hir_id)
    }

    fn adjusted_node_type(&self, id: NodeId) -> Ty<'tcx> {
        let parent = self.hir_map().get_parent_did(id);
        let tables = self.ty_ctxt().typeck_tables_of(parent);
        let hir_id = self.hir_map().node_to_hir_id(id);
        if let Some(adj) = tables.adjustments().get(hir_id).and_then(|adjs| adjs.last()) {
            adj.target
        } else {
            tables.node_id_to_type(hir_id)
        }
    }

    fn def_type(&self, id: DefId) -> Ty<'tcx> {
        self.ty_ctxt().type_of(id)
    }

    fn def_path(&self, id: DefId) -> Path {
        reflect::reflect_def_path(self.ty_ctxt(), id).1
    }

    fn def_qpath(&self, id: DefId) -> (Option<QSelf>, Path) {
        reflect::reflect_def_path(self.ty_ctxt(), id)
    }

    fn node_def_id(&self, id: NodeId) -> DefId {
        match self.hir_map().find(id) {
            Some(Node::NodeBinding(_)) => self.node_def_id(self.hir_map().get_parent_node(id)),
            Some(Node::NodeItem(item)) => self.hir_map().local_def_id(item.id),
            _ => self.hir_map().local_def_id(id),
        }
    }

    fn def_to_hir_id(&self, def: &hir::def::Def) -> Option<hir::HirId> {
        use rustc::hir::def::Def;
        match def {
            Def::Mod(did) |
            Def::Struct(did) |
            Def::Union(did) |
            Def::Enum(did) |
            Def::Variant(did) |
            Def::Trait(did) |
            Def::Existential(did) |
            Def::TyAlias(did) |
            Def::TyForeign(did) |
            Def::AssociatedTy(did) |
            Def::TyParam(did) |
            Def::Fn(did) |
            Def::Const(did) |
            Def::Static(did, _) |
            Def::StructCtor(did, _) |
            Def::VariantCtor(did, _) |
            Def::Method(did) |
            Def::AssociatedConst(did) |
            Def::Macro(did, _) |
            Def::GlobalAsm(did) |
            Def::TraitAlias(did) =>
                if did.is_local() {
                    Some(self.hir_map().local_def_id_to_hir_id(did.to_local()))
                } else {
                    None
                },

            // Local variables stopped having DefIds at some point and switched to NodeId
            Def::Local(node) |
            Def::Upvar(node, _, _) |
            Def::Label(node) => Some(self.hir_map().node_to_hir_id(*node)),

            Def::PrimTy(_) |
            Def::SelfTy(_, _) |
            Def::Err => None
        }
    }

    fn try_resolve_expr_to_hid(&self, e: &Expr) -> Option<hir::HirId> {
        let node = match_or!([self.hir_map().find(e.id)] Some(x) => x;
                             return None);
        let e = match_or!([node] hir::map::NodeExpr(e) => e;
                          return None);
        let qpath = match_or!([e.node] hir::ExprPath(ref q) => q;
                              return None);
        let path = match_or!([*qpath] hir::QPath::Resolved(_, ref path) => path;
                             return None);
        self.def_to_hir_id(&path.def)
    }

    fn try_resolve_expr(&self, e: &Expr) -> Option<DefId> {
        let node = match_or!([self.hir_map().find(e.id)] Some(x) => x;
                             return None);
        let e = match_or!([node] hir::map::NodeExpr(e) => e;
                          return None);
        let qpath = match_or!([e.node] hir::ExprPath(ref q) => q;
                              return None);
        let path = match_or!([*qpath] hir::QPath::Resolved(_, ref path) => path;
                             return None);
        path.def.opt_def_id()
    }

    fn resolve_expr(&self, e: &Expr) -> DefId {
        self.try_resolve_expr(e)
            .unwrap_or_else(|| panic!("expr does not resolve to a def: {:?}", e))
    }

    fn try_resolve_ty(&self, t: &ast::Ty) -> Option<DefId> {
        let node = match_or!([self.hir_map().find(t.id)] Some(x) => x;
                             return None);
        let t = match_or!([node] hir::map::NodeTy(t) => t;
                          return None);
        let qpath = match_or!([t.node] hir::TyPath(ref q) => q;
                              return None);
        let path = match_or!([*qpath] hir::QPath::Resolved(_, ref path) => path;
                             return None);
        path.def.opt_def_id()
    }

    fn resolve_ty(&self, t: &ast::Ty) -> DefId {
        self.try_resolve_ty(t)
            .unwrap_or_else(|| panic!("ty does not resolve to a def: {:?}", t))
    }

    fn opt_callee(&self, e: &Expr) -> Option<DefId> {
        if e.id == DUMMY_NODE_ID {
            return None;
        }
        let parent = self.hir_map().get_parent(e.id);
        let parent_body = match_or!([self.hir_map().maybe_body_owned_by(parent)]
                                    Some(x) => x; return None);
        let tables = self.ty_ctxt().body_tables(parent_body);

        match e.node {
            ExprKind::Call(ref func, _) => {
                // Only type-dependent methods (as in `T::f()`) show up in `type_dependent_defs`.
                // Regular functions (`f()`) aren't present there, but they are resolvable by
                // `try_resolve_expr`.
                if let Some(def_id) = self.try_resolve_expr(func) {
                    return Some(def_id);
                } else {
                    let func_hir_id = self.hir_map().node_to_hir_id(func.id);
                    tables.type_dependent_defs().get(func_hir_id).and_then(|d| d.opt_def_id())
                }
            },
            ExprKind::MethodCall(..) => {
                let hir_id = self.hir_map().node_to_hir_id(e.id);
                tables.type_dependent_defs().get(hir_id).and_then(|d| d.opt_def_id())
            },
            _ => None,
        }
    }

    fn callee(&self, e: &Expr) -> DefId {
        self.opt_callee(e).expect("callee: expr is not a call")
    }
}
