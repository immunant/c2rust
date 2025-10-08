//! Helper function for visiting only one type of AST node.
use rustc_ast::visit::{self, Visitor};
use rustc_ast::*;

use crate::ast_manip::Visit;

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
