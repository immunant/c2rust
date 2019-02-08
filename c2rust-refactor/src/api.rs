//! A variety of helpers for writing transformations.  Meant to be glob-imported by transform
//! implementation modules.
use rustc::hir;
use rustc::hir::def::Def;
use rustc::hir::def_id::DefId;
use rustc::hir::Node;
use rustc::ty::{Ty, TyKind, FnSig, PolyFnSig, ParamEnv};
use rustc::ty::subst::Substs;
use syntax::ast;    // Can't glob-import because `Ty` already refers to `rustc::ty::Ty`.
use syntax::ast::{NodeId, DUMMY_NODE_ID};
use syntax::ast::{Expr, ExprKind};
use syntax::ast::{Path, QSelf};

// Reexports of various helpers
pub use crate::ast_manip::*;
pub use crate::ast_manip::fn_edit::{fold_fns, fold_fns_multi};
pub use crate::ast_manip::lr_expr::fold_expr_with_context;
pub use c2rust_ast_builder::mk;
pub use crate::driver::{parse_expr, parse_pat, parse_ty, parse_stmts, parse_items};
pub use crate::driver::{parse_free_expr, parse_free_pat, parse_free_ty, parse_free_stmts, parse_free_items};
pub use crate::matcher::{MatchCtxt, Bindings, BindingType, Subst};
pub use crate::matcher::{fold_match, fold_match_with};
pub use crate::path_edit::{self, fold_resolved_paths, fold_resolved_paths_with_id};

use crate::matcher::Pattern;
use crate::command::CommandState;
use crate::driver;
use crate::reflect;


/// Replace all instances of expression `pat` with expression `repl`.
pub fn replace_expr<T: Fold>(st: &CommandState,
                             cx: &driver::Ctxt,
                             ast: T,
                             pat: &str,
                             repl: &str) -> <T as Fold>::Result {
    let mut mcx = MatchCtxt::new(st, cx);
    let (pat, pat_bt) = parse_free_expr(cx.session(), pat);
    mcx.merge_binding_types(pat_bt);
    let (repl, repl_bt) = parse_free_expr(cx.session(), repl);
    mcx.merge_binding_types(repl_bt);
    fold_match_with(mcx, pat, ast, |_, mcx| repl.clone().subst(st, cx, &mcx.bindings))
}

/// Replace all instances of the statement sequence `pat` with `repl`.
pub fn replace_stmts<T: Fold>(st: &CommandState,
                              cx: &driver::Ctxt,
                              ast: T,
                              pat: &str,
                              repl: &str) -> <T as Fold>::Result {
    let mut mcx = MatchCtxt::new(st, cx);
    let (pat, pat_bt) = parse_free_stmts(cx.session(), pat);
    mcx.merge_binding_types(pat_bt);
    let (repl, repl_bt) = parse_free_stmts(cx.session(), repl);
    mcx.merge_binding_types(repl_bt);
    fold_match_with(mcx, pat, ast, |_, mcx| repl.clone().subst(st, cx, &mcx.bindings))
}


/// Find the first place where `pattern` matches under initial context `init_mcx`, and return the
/// resulting `Bindings`.
pub fn find_first_with<P, T>(init_mcx: MatchCtxt,
                             pattern: P,
                             target: T) -> Option<Bindings>
        where P: Pattern, T: Fold {
    let mut result = None;
    fold_match_with(init_mcx, pattern, target, |p, mcx| {
        if result.is_none() {
            result = Some(mcx.bindings);
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
    fn opt_node_type(&self, id: NodeId) -> Option<Ty<'tcx>>;
    /// Get the `ty::Ty` computed for a node, taking into account any adjustments that were applied.
    fn adjusted_node_type(&self, id: NodeId) -> Ty<'tcx>;
    fn opt_adjusted_node_type(&self, id: NodeId) -> Option<Ty<'tcx>>;

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

    fn opt_callee_info(&self, e: &Expr) -> Option<CalleeInfo<'tcx>>;
    fn opt_callee_fn_sig(&self, e: &Expr) -> Option<FnSig<'tcx>>;
}

impl<'a, 'tcx> DriverCtxtExt<'tcx> for driver::Ctxt<'a, 'tcx> {
    fn node_type(&self, id: NodeId) -> Ty<'tcx> {
        let parent = self.hir_map().get_parent_did(id);
        let tables = self.ty_ctxt().typeck_tables_of(parent);
        let hir_id = self.hir_map().node_to_hir_id(id);
        tables.node_id_to_type(hir_id)
    }

    fn opt_node_type(&self, id: NodeId) -> Option<Ty<'tcx>> {
        let parent_node = self.hir_map().get_parent(id);
        let parent = self.hir_map().opt_local_def_id(parent_node)?;
        if !self.ty_ctxt().has_typeck_tables(parent) {
            return None;
        }
        let tables = self.ty_ctxt().typeck_tables_of(parent);
        let hir_id = self.hir_map().node_to_hir_id(id);
        tables.node_id_to_type_opt(hir_id)
    }

    fn adjusted_node_type(&self, id: NodeId) -> Ty<'tcx> {
        self.opt_adjusted_node_type(id)
            .unwrap_or_else(|| panic!("adjusted node type unavailable for {:?}", id))
    }

    fn opt_adjusted_node_type(&self, id: NodeId) -> Option<Ty<'tcx>> {
        let parent_node = self.hir_map().get_parent(id);
        let parent = self.hir_map().opt_local_def_id(parent_node)?;
        if !self.ty_ctxt().has_typeck_tables(parent) {
            return None;
        }
        let tables = self.ty_ctxt().typeck_tables_of(parent);
        let hir_id = self.hir_map().node_to_hir_id(id);
        if let Some(adj) = tables.adjustments().get(hir_id).and_then(|adjs| adjs.last()) {
            Some(adj.target)
        } else {
            tables.node_id_to_type_opt(hir_id)
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
            Some(Node::Binding(_)) => self.node_def_id(self.hir_map().get_parent_node(id)),
            Some(Node::Item(item)) => self.hir_map().local_def_id(item.id),
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
            Def::ForeignTy(did) |
            Def::AssociatedTy(did) |
            Def::AssociatedExistential(did) |
            Def::TyParam(did) |
            Def::Fn(did) |
            Def::Const(did) |
            Def::Static(did, _) |
            Def::StructCtor(did, _) |
            Def::VariantCtor(did, _) |
            Def::SelfCtor(did) |
            Def::Method(did) |
            Def::AssociatedConst(did) |
            Def::Macro(did, _) |
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
            Def::ToolMod |
            Def::NonMacroAttr(_) |
            Def::Err => None
        }
    }

    fn try_resolve_expr_to_hid(&self, e: &Expr) -> Option<hir::HirId> {
        if let Some(def) = try_resolve_expr_hir(self, e) {
            return self.def_to_hir_id(&def);
        }

        if self.has_ty_ctxt() {
            if let Some(def) = try_resolve_node_type_dep(self, e.id) {
                return self.def_to_hir_id(&def);
            }
        }

        None
    }

    fn try_resolve_expr(&self, e: &Expr) -> Option<DefId> {
        if let Some(def) = try_resolve_expr_hir(self, e) {
            return def.opt_def_id();
        }

        if self.has_ty_ctxt() {
            // Only try the type_dependent_defs fallback on Path exprs.  Other expr kinds,
            // particularly MethodCall, can show up in type_dependent_defs, and we don't want to
            // wrongly treat those as path-like.
            if let ExprKind::Path(..) = e.node {
                if let Some(def) = try_resolve_node_type_dep(self, e.id) {
                    return def.opt_def_id();
                }
            }
        }

        None
    }

    fn resolve_expr(&self, e: &Expr) -> DefId {
        self.try_resolve_expr(e)
            .unwrap_or_else(|| panic!("expr does not resolve to a def: {:?}", e))
    }

    fn try_resolve_ty(&self, t: &ast::Ty) -> Option<DefId> {
        if let Some(def) = try_resolve_ty_hir(self, t) {
            return def.opt_def_id();
        }

        if self.has_ty_ctxt() {
            if let ast::TyKind::Path(..) = t.node {
                if let Some(def) = try_resolve_node_type_dep(self, t.id) {
                    return def.opt_def_id();
                }
            }
        }

        None
    }

    fn resolve_ty(&self, t: &ast::Ty) -> DefId {
        self.try_resolve_ty(t)
            .unwrap_or_else(|| panic!("ty does not resolve to a def: {:?}", t))
    }

    fn opt_callee(&self, e: &Expr) -> Option<DefId> {
        self.opt_callee_info(e).and_then(|info| info.def_id)
    }

    fn callee(&self, e: &Expr) -> DefId {
        self.opt_callee(e).expect("callee: expr is not a call")
    }


    fn opt_callee_info(&self, e: &Expr) -> Option<CalleeInfo<'tcx>> {
        if e.id == DUMMY_NODE_ID {
            return None;
        }
        let tcx = self.ty_ctxt();
        let hir_map = self.hir_map();

        let parent = hir_map.get_parent(e.id);
        let parent_body = match_or!([hir_map.maybe_body_owned_by(parent)]
                                    Some(x) => x; return None);
        let tables = tcx.body_tables(parent_body);

        let mut def_id = None;
        let poly_sig;
        let mut substs = None;

        // Note this method gets used inside `fold_illtyped_exprs`, which means the tcx may be in a
        // more-or-less bad state due type errors.  We try really hard here to return `None`
        // instead of panicking when weird stuff happens.

        match e.node {
            ExprKind::Call(ref func, _) => {
                let call_hir_id = hir_map.node_to_hir_id(e.id);
                let func_hir_id = hir_map.node_to_hir_id(func.id);

                // (1) Overloaded calls (FnOnce, etc).  These are special in two ways.  First, all
                // the information about the callee is attached to the Call expr itself, not the
                // func.  And second, it uses the special "rust-call" ABI where arguments are
                // gathered up and passed in a single tuple.
                //
                // We detect this case by the presence of a type-dependent def on the Call.
                if let Some(func_def) = tables.type_dependent_defs().get(call_hir_id) {
                    if !matches!([func_def] Def::Fn(..), Def::Method(..)) {
                        warn!("overloaded call dispatches to non-fnlike def {:?}", func_def);
                        return None;
                    }
                    let func_def_id = func_def.def_id();
                    def_id = Some(func_def_id);
                    poly_sig = tcx.fn_sig(func_def_id);
                    substs = tables.node_substs_opt(call_hir_id);
                    // TODO: adjust for rust-call ABI

                } else {
                    let func_hir = expect!([hir_map.find(func.id)] Some(hir::Node::Expr(e)) => e);

                    // (2) Function pointers.  We have to check for this first because it's
                    // possible that `func` might be a normal or type-dependent path to a
                    // fnptr-typed static or const item.
                    //
                    // We use the adjusted type here in case an `&fn()` got auto-derefed in order
                    // to make the call.
                    if let Some(&TyKind::FnPtr(sig)) = tables.expr_ty_adjusted_opt(func_hir)
                            .map(|ty| &ty.sty) {
                        poly_sig = sig;
                        // No substs.  fn ptrs can't be generic over anything but late-bound
                        // regions, and late-bound regions don't show up in the substs.

                    // (3) Type-dependent function (`S::f()`).  Unlike the next case, these don't
                    // get fully resolved until typeck, so the results are recorded differently.
                    } else if let Some(func_def) = tables.type_dependent_defs().get(func_hir_id) {
                        if !matches!([func_def] Def::Fn(..), Def::Method(..)) {
                            warn!("type-dep call dispatches to non-fnlike def {:?}", func_def);
                            return None;
                        }
                        let func_def_id = func_def.def_id();
                        def_id = Some(func_def_id);
                        poly_sig = tcx.fn_sig(func_def_id);
                        substs = tables.node_substs_opt(func_hir_id);

                    // (4) Ordinary function call (`f()`).
                    } else if let Some(func_def_id) = self.try_resolve_expr(func) {
                        def_id = Some(func_def_id);
                        poly_sig = tcx.fn_sig(func_def_id);
                        substs = tables.node_substs_opt(func_hir_id);

                    } else {
                        // Failed to resolve.  Probably a really bad type error somewhere.
                        warn!("failed to resolve call expr {:?}", e);
                        return None;
                    }
                }
            },

            ExprKind::MethodCall(..) => {
                // These cases are much simpler - just get the method definition from
                // type_dependent_defs.
                let hir_id = hir_map.node_to_hir_id(e.id);
                if let Some(func_def) = tables.type_dependent_defs().get(hir_id) {
                    if !matches!([func_def] Def::Fn(..), Def::Method(..)) {
                        warn!("type-dep call dispatches to non-fnlike def {:?}", func_def);
                        return None;
                    }
                    let func_def_id = func_def.def_id();
                    def_id = Some(func_def_id);
                    poly_sig = tcx.fn_sig(func_def_id);
                    substs = tables.node_substs_opt(hir_id);
                } else {
                    return None;
                }
            },

            _ => return None,
        }

        let unsubst_fn_sig = tcx.erase_late_bound_regions(&poly_sig);
        let fn_sig = if let Some(substs) = substs {
            tcx.subst_and_normalize_erasing_regions(substs, ParamEnv::empty(), &unsubst_fn_sig)
        } else {
            tcx.normalize_erasing_regions(ParamEnv::empty(), unsubst_fn_sig)
        };

        Some(CalleeInfo { fn_sig, poly_sig, def_id, substs })
    }

    fn opt_callee_fn_sig(&self, e: &Expr) -> Option<FnSig<'tcx>> {
        self.opt_callee_info(e).map(|info| info.fn_sig)
    }
}

#[derive(Clone, Debug)]
pub struct CalleeInfo<'tcx> {
    /// The final signature used at the call site, after substituting in type and region arguments.
    pub fn_sig: FnSig<'tcx>,

    /// The un-substituted signature of the callee.
    pub poly_sig: PolyFnSig<'tcx>,

    /// The DefId of the function or method being called.  If the callee is a fn pointer, this is
    /// `None`.
    pub def_id: Option<DefId>,

    /// The type and region arguments that were substituted in at the call site.
    pub substs: Option<&'tcx Substs<'tcx>>,
}


fn try_resolve_expr_hir(cx: &driver::Ctxt, e: &Expr) -> Option<Def> {
    let node = match_or!([cx.hir_map().find(e.id)] Some(x) => x;
                         return None);
    let e = match_or!([node] hir::Node::Expr(e) => e;
                      return None);
    let qpath = match_or!([e.node] hir::ExprKind::Path(ref q) => q;
                          return None);
    let path = match_or!([*qpath] hir::QPath::Resolved(_, ref path) => path;
                         return None);
    Some(path.def)
}

fn try_resolve_ty_hir(cx: &driver::Ctxt, t: &ast::Ty) -> Option<Def> {
    let node = match_or!([cx.hir_map().find(t.id)] Some(x) => x;
                         return None);
    let t = match_or!([node] hir::Node::Ty(t) => t;
                      return None);
    let qpath = match_or!([t.node] hir::TyKind::Path(ref q) => q;
                          return None);
    let path = match_or!([*qpath] hir::QPath::Resolved(_, ref path) => path;
                         return None);
    Some(path.def)
}

/// Try to resolve a node as a reference to a type-dependent definition, like `Vec::new` (a.k.a.
/// `<Vec>::new`) or `<Vec as IntoIterator>::into_iter`.
///
/// Note that this method doesn't look up the node itself, so it can return results even for
/// non-path nodes (unlike `try_resolve_expr/ty_hir`).
fn try_resolve_node_type_dep(cx: &driver::Ctxt, id: NodeId) -> Option<Def> {
    let hir_map = cx.hir_map();
    let tcx = cx.ty_ctxt();

    let parent = hir_map.get_parent(id);
    let parent_body = match_or!([hir_map.maybe_body_owned_by(parent)]
                                Some(x) => x; return None);
    let tables = tcx.body_tables(parent_body);

    let hir_id = hir_map.node_to_hir_id(id);
    let tdd = tables.type_dependent_defs();
    let def = match_or!([tdd.get(hir_id)] Some(x) => x; return None);
    Some(*def)
}
