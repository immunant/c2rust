use rustc::hir;
use rustc::hir::def_id::DefId;
use rustc::session::Session;
use rustc::ty::Ty;
use rustc::ty::item_path::{ItemPathBuffer, RootMode};
use syntax::ast::{NodeId, DUMMY_NODE_ID};
use syntax::ast::{Expr, ExprKind};
use syntax::ast::{Path, PathSegment, Ident};
use syntax::codemap::DUMMY_SP;
use syntax::symbol::keywords;

pub use matcher::MatchCtxt;
pub use matcher::{fold_match, fold_match_with};
pub use driver::{parse_expr, parse_pat, parse_ty, parse_stmts, parse_items};
pub use subst::Subst;
pub use bindings::Type as BindingType;
pub use seq_edit::{fold_blocks, fold_modules};
pub use make_ast::mk;
pub use fold_node::fold_nodes;
pub use visit_node::visit_nodes;
pub use path_edit::{self, fold_resolved_paths};
pub use fn_edit::fold_fns;

use bindings::Bindings;
use driver;
use fold::Fold;
use matcher::Pattern;
use util::HirDefExt;
use util::IntoSymbol;

pub fn replace_expr<T: Fold>(sess: &Session,
                             ast: T,
                             pat: &str,
                             repl: &str) -> <T as Fold>::Result {
    let pat = parse_expr(sess, pat);
    let repl = parse_expr(sess, repl);
    fold_match(pat, ast, |_, bnd| repl.clone().subst(&bnd))
}

pub fn replace_stmts<T: Fold>(sess: &Session,
                              ast: T,
                              pat: &str,
                              repl: &str) -> <T as Fold>::Result {
    let pat = parse_stmts(sess, pat);
    let repl = parse_stmts(sess, repl);
    fold_match(pat, ast, |_, bnd| repl.clone().subst(&bnd))
}


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

pub fn find_first<P, T>(pattern: P,
                        target: T) -> Option<Bindings>
        where P: Pattern, T: Fold {
    find_first_with(MatchCtxt::new(), pattern, target)
}


pub trait DriverCtxtExt<'gcx> {
    fn node_type(&self, id: NodeId) -> Ty<'gcx>;
    fn adjusted_node_type(&self, id: NodeId) -> Ty<'gcx>;
    fn def_type(&self, id: DefId) -> Ty<'gcx>;
    fn def_path(&self, id: DefId) -> Path;

    fn node_def_id(&self, id: NodeId) -> DefId;
    fn try_resolve_expr(&self, e: &Expr) -> Option<DefId>;
    fn resolve_expr(&self, e: &Expr) -> DefId;
    fn opt_callee(&self, e: &Expr) -> Option<DefId>;
    fn callee(&self, e: &Expr) -> DefId;
}

impl<'a, 'hir, 'gcx, 'tcx> DriverCtxtExt<'gcx> for driver::Ctxt<'a, 'hir, 'gcx, 'tcx> {
    /// Get the `ty::Ty` computed for a node.
    fn node_type(&self, id: NodeId) -> Ty<'gcx> {
        let parent = self.hir_map().get_parent(id);
        let parent_body = self.hir_map().body_owned_by(parent);
        let tables = self.ty_ctxt().body_tables(parent_body);
        tables.node_id_to_type(id)
    }

    /// Get the `ty::Ty` computed for a node, taking into account any adjustments that were applied.
    fn adjusted_node_type(&self, id: NodeId) -> Ty<'gcx> {
        let parent = self.hir_map().get_parent(id);
        let parent_body = self.hir_map().body_owned_by(parent);
        let tables = self.ty_ctxt().body_tables(parent_body);
        if let Some(adj) = tables.adjustments.get(&id).and_then(|adjs| adjs.last()) {
            adj.target
        } else {
            tables.node_id_to_type(id)
        }
    }

    fn def_type(&self, id: DefId) -> Ty<'gcx> {
        self.ty_ctxt().type_of(id)
    }

    /// Construct a `Path` AST suitable for referring to a definition.
    fn def_path(&self, id: DefId) -> Path {
        let root = PathSegment {
            identifier: keywords::CrateRoot.ident(),
            span: DUMMY_SP,
            parameters: None,
        };
        let mut buf = ItemPathVec(RootMode::Local, vec![root]);
        self.ty_ctxt().push_item_path(&mut buf, id);

        let mut segs = buf.1;
        // If `id` refers to an `extern` item, there will be an entry in the path for the `extern`
        // block (`ItemKind::ForeignMod`), with an empty ident.  Filter it out.
        segs.retain(|seg| seg.identifier.name.as_str() != "");

        Path {
            span: DUMMY_SP,
            segments: segs,
        }
    }

    /// Obtain the `DefId` of a definition node, such as a `fn` item.
    fn node_def_id(&self, id: NodeId) -> DefId {
        match self.hir_map().opt_local_def_id(id) {
            Some(x) => x,
            None => panic!("not a definition node"),
        }
    }

    /// Obtain the `DefId` referenced by a path `Expr`.
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

    /// Obtain the `DefId` referenced by a path `Expr`.
    fn resolve_expr(&self, e: &Expr) -> DefId {
        self.try_resolve_expr(e)
            .unwrap_or_else(|| panic!("expr does not resolve to a def: {:?}", e))
    }

    /// Obtain the `DefId` of the function being called, if `e` is a function or method call.
    fn opt_callee(&self, e: &Expr) -> Option<DefId> {
        if e.id == DUMMY_NODE_ID {
            return None;
        }
        let parent = self.hir_map().get_parent(e.id);
        let parent_body = self.hir_map().body_owned_by(parent);
        let tables = self.ty_ctxt().body_tables(parent_body);

        match e.node {
            ExprKind::Call(ref func, _) => {
                // Only type-dependent methods (as in `T::f()`) show up in `type_dependent_defs`.
                // Regular functions (`f()`) aren't present there, but they are resolvable by
                // `try_resolve_expr`.
                if let Some(def_id) = self.try_resolve_expr(func) {
                    return Some(def_id);
                } else {
                    tables.type_dependent_defs.get(&func.id).and_then(|d| d.opt_def_id())
                }
            },
            ExprKind::MethodCall(..) =>
                tables.type_dependent_defs.get(&e.id).and_then(|d| d.opt_def_id()),
            _ => None,
        }
    }

    fn callee(&self, e: &Expr) -> DefId {
        self.opt_callee(e).expect("callee: expr is not a call")
    }
}


struct ItemPathVec(RootMode, Vec<PathSegment>);

impl ItemPathBuffer for ItemPathVec {
    fn root_mode(&self) -> &RootMode {
        &self.0
    }

    fn push(&mut self, text: &str) {
        self.1.push(PathSegment {
            identifier: Ident::with_empty_ctxt(text.into_symbol()),
            span: DUMMY_SP,
            parameters: None,
        });
    }
}
