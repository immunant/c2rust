//! Helper function for visiting only one type of AST node.
use syntax::ast::*;
use syntax::visit::{self, Visitor};

use visit::Visit;


/// Trait for AST node types that can be visited.
pub trait VisitNode {
    fn visit_nodes<T, F>(target: &T, callback: F)
        where T: Visit,
              F: FnMut(&Self);
    fn visit_nodes_post<T, F>(target: &T, callback: F)
        where T: Visit,
              F: FnMut(&Self);
}

macro_rules! gen_visit_node_impl {
    (
        node = $Node:ty;
        visitor = $NodeVisitor:ident;
        visitor_post = $NodeVisitorPost:ident;

        fn $visit_thing:ident ( &mut $slf:ident,
                                $arg:ident : &'ast $ArgTy:ty
                                $( , $args:ident : $ArgTys:ty )* ) {
            $finish:expr
        }
    ) => {
        struct $NodeVisitor<F>
                where F: FnMut(&$ArgTy) {
            callback: F,
        }

        impl<'ast, F> Visitor<'ast> for $NodeVisitor<F>
                where F: FnMut(&$ArgTy) {
            fn $visit_thing(&mut $slf, $arg: &'ast $ArgTy, $($args: $ArgTys,)*) {
                ($slf.callback)($arg);
                $finish;
            }
        }

        struct $NodeVisitorPost<F>
                where F: FnMut(&$ArgTy) {
            callback: F,
        }

        impl<'ast, F> Visitor<'ast> for $NodeVisitorPost<F>
                where F: FnMut(&$ArgTy) {
            fn $visit_thing(&mut $slf, $arg: &'ast $ArgTy, $($args: $ArgTys,)*) {
                $finish;
                ($slf.callback)($arg);
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

// `gen_visit_node_impl!` works much like `gen_fold_node_impl!` in fold_node.rs, except that `walk`
// is written directly as the function body instead.
// (TODO: No reason not to make these consistent, by switching this one to `walk = ...` syntax.)
gen_visit_node_impl! {
    node = Expr;
    visitor = ExprNodeVisitor;
    visitor_post = ExprNodeVisitorPost;
    fn visit_expr(&mut self, e: &'ast Expr) {
        visit::walk_expr(self, e)
    }
}

gen_visit_node_impl! {
    node = Item;
    visitor = ItemNodeVisitor;
    visitor_post = ItemNodeVisitorPost;
    fn visit_item(&mut self, i: &'ast Item) {
        visit::walk_item(self, i)
    }
}

gen_visit_node_impl! {
    node = Path;
    visitor = PathNodeVisitor;
    visitor_post = PathNodeVisitorPost;
    fn visit_path(&mut self, p: &'ast Path, _id: NodeId) {
        visit::walk_path(self, p)
    }
}

gen_visit_node_impl! {
    node = Block;
    visitor = BlockNodeVisitor;
    visitor_post = BlockNodeVisitorPost;
    fn visit_block(&mut self, b: &'ast Block) {
        visit::walk_block(self, b)
    }
}

gen_visit_node_impl! {
    node = Local;
    visitor = LocalNodeVisitor;
    visitor_post = LocalNodeVisitorPost;
    fn visit_local(&mut self, l: &'ast Local) {
        visit::walk_local(self, l)
    }
}

gen_visit_node_impl! {
    node = ForeignItem;
    visitor = ForeignItemNodeVisitor;
    visitor_post = ForeignItemNodeVisitorPost;
    fn visit_foreign_item(&mut self, i: &'ast ForeignItem) {
        visit::walk_foreign_item(self, i)
    }
}

/// Visit nodes of the callback's argument type within `target`.  This function performs a preorder
/// traversal.
pub fn visit_nodes<N, T, F>(target: &T, callback: F)
        where N: VisitNode,
              T: Visit,
              F: FnMut(&N) {
    N::visit_nodes(target, callback)
}

/// Visit nodes of the callback's argument type within `target`.  This function performs a
/// postorder traversal.
pub fn visit_nodes_post<N, T, F>(target: &T, callback: F)
        where N: VisitNode,
              T: Visit,
              F: FnMut(&N) {
    N::visit_nodes(target, callback)
}
