//! Helper function for visiting only one type of AST node.
use rustc_ast::visit::{self, Visitor};
use rustc_ast::*;
use rustc_hir::def_id::DefId;
use rustc_hir::Node;
use rustc_span::sym;
use std::collections::HashSet;

use crate::ast_manip::Visit;
use crate::RefactorCtxt;

/// Trait for AST node types that can be visited.
pub trait VisitNode {
    fn visit_nodes<T, F>(target: &T, callback: F)
    where
        T: Visit,
        F: FnMut(&Self);
    fn visit_nodes_post<T, F>(target: &T, callback: F)
    where
        T: Visit,
        F: FnMut(&Self);
}

macro_rules! gen_visit_node_impl {
    (
        node = $Node:ty;
        visitor = $NodeVisitor:ident;
        visitor_post = $NodeVisitorPost:ident;

        fn $visit_thing:ident ( &mut $slf:ident,
                                $arg:ident : &'ast $ArgTy:ty
                                $( , $args:ident : $ArgTys:ty )* );

        walk = $walk:expr;
    ) => {
        struct $NodeVisitor<F>
                where F: FnMut(&$ArgTy) {
            callback: F,
        }

        impl<'ast, F> Visitor<'ast> for $NodeVisitor<F>
                where F: FnMut(&$ArgTy) {
            fn $visit_thing(&mut $slf, $arg: &'ast $ArgTy, $($args: $ArgTys,)*) {
                ($slf.callback)($arg);
                $walk;
            }

            fn visit_mac_call(&mut self, mac: &'ast MacCall) {
                visit::walk_mac(self, mac);
            }
        }

        struct $NodeVisitorPost<F>
                where F: FnMut(&$ArgTy) {
            callback: F,
        }

        impl<'ast, F> Visitor<'ast> for $NodeVisitorPost<F>
                where F: FnMut(&$ArgTy) {
            fn $visit_thing(&mut $slf, $arg: &'ast $ArgTy, $($args: $ArgTys,)*) {
                $walk;
                ($slf.callback)($arg);
            }

            fn visit_mac_call(&mut self, mac: &'ast MacCall) {
                visit::walk_mac(self, mac);
            }
        }

        impl VisitNode for $Node {
            fn visit_nodes<T, F>(target: &T, callback: F)
                    where T: Visit,
                          F: FnMut(&Self) {
                let mut f = $NodeVisitor { callback: callback };
                target.visit(&mut f)
            }

            fn visit_nodes_post<T, F>(target: &T, callback: F)
                    where T: Visit,
                          F: FnMut(&Self) {
                let mut f = $NodeVisitorPost { callback: callback };
                target.visit(&mut f)
            }
        }
    };
}

// If you want to use `visit_nodes` with more node types, add more `gen_visit_node_impl!`
// invocations below.

// `gen_visit_node_impl!` works much like `gen_fold_node_impl!` in fold_node.rs.
gen_visit_node_impl! {
    node = Expr;
    visitor = ExprNodeVisitor;
    visitor_post = ExprNodeVisitorPost;
    fn visit_expr(&mut self, e: &'ast Expr);
    walk = visit::walk_expr(self, e);
}

gen_visit_node_impl! {
    node = Item;
    visitor = ItemNodeVisitor;
    visitor_post = ItemNodeVisitorPost;
    fn visit_item(&mut self, i: &'ast Item);
    walk = visit::walk_item(self, i);
}

gen_visit_node_impl! {
    node = Path;
    visitor = PathNodeVisitor;
    visitor_post = PathNodeVisitorPost;
    fn visit_path(&mut self, p: &'ast Path, _id: NodeId);
    walk = visit::walk_path(self, p);
}

gen_visit_node_impl! {
    node = Block;
    visitor = BlockNodeVisitor;
    visitor_post = BlockNodeVisitorPost;
    fn visit_block(&mut self, b: &'ast Block);
    walk = visit::walk_block(self, b);
}

gen_visit_node_impl! {
    node = Local;
    visitor = LocalNodeVisitor;
    visitor_post = LocalNodeVisitorPost;
    fn visit_local(&mut self, l: &'ast Local);
    walk = visit::walk_local(self, l);
}

gen_visit_node_impl! {
    node = ForeignItem;
    visitor = ForeignItemNodeVisitor;
    visitor_post = ForeignItemNodeVisitorPost;
    fn visit_foreign_item(&mut self, i: &'ast ForeignItem);
    walk = visit::walk_foreign_item(self, i);
}

gen_visit_node_impl! {
    node = Stmt;
    visitor = StmtNodeVisitor;
    visitor_post = StmtNodeVisitorPost;
    fn visit_stmt(&mut self, s: &'ast Stmt);
    walk = visit::walk_stmt(self, s);
}

/// Visit nodes of the callback's argument type within `target`.  This function performs a preorder
/// traversal.
pub fn visit_nodes<N, T, F>(target: &T, callback: F)
where
    N: VisitNode,
    T: Visit,
    F: FnMut(&N),
{
    N::visit_nodes(target, callback)
}

/// Visit nodes of the callback's argument type within `target`.  This function performs a
/// postorder traversal.
pub fn visit_nodes_post<N, T, F>(target: &T, callback: F)
where
    N: VisitNode,
    T: Visit,
    F: FnMut(&N),
{
    N::visit_nodes(target, callback)
}

/// Visit non-local foreign functions.
///
/// "Non-local" here means that the function isn't defined elsewhere in the
/// crate. This is primarily intended for transforming calls to extern
/// functions, e.g. the libc functions. Functions must be defined in an `extern
/// "C"` block and be marked `#[no_mangle]` in order to ensure that we only
/// visit foreign functions.
pub fn visit_foreign_fns<T, F>(target: &T, cx: &RefactorCtxt, mut callback: F)
where
    T: Visit,
    F: FnMut(&ForeignItem, DefId),
{
    // Collect all locally defined `no_mangle` function names.
    let mut local_no_mangle_names = HashSet::new();
    visit_nodes(target, |item: &Item| {
        if let ItemKind::Fn(_) = item.kind {
            if crate::util::contains_name(&item.attrs, sym::no_mangle) {
                local_no_mangle_names.insert(item.ident.name);
            }
        }
    });

    visit_nodes(target, |fi: &ForeignItem| {
        if !crate::util::contains_name(&fi.attrs, sym::no_mangle) {
            return;
        }
        let ForeignItemKind::Fn(_) = fi.kind else {
            return;
        };

        let def_id = cx.node_def_id(fi.id);

        // Ignore functions that are defined locally, either directly or as
        // indirect `extern "C"` imports, since those have to be custom
        // functions. We only want to translate calls to foreign libc functions.
        if local_no_mangle_names.contains(&fi.ident.name) {
            return;
        }
        let Some(Node::ForeignItem(_)) = cx.hir_map().get_if_local(def_id) else {
            return;
        };

        callback(fi, def_id);
    });
}
