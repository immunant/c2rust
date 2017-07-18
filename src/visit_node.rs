use syntax::ast::*;
use syntax::visit::{self, Visitor};

use visit::Visit;


pub trait VisitNode {
    fn visit_nodes<T, F>(target: &T, callback: F)
        where T: Visit,
              F: FnMut(&Self);
}

macro_rules! gen_visit_node_impl {
    (
        node = $Node:ty;
        visitor = $NodeVisitor:ident;

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
                $finish
            }
        }

        impl VisitNode for $Node {
            fn visit_nodes<T, F>(target: &T, callback: F)
                    where T: Visit,
                          F: FnMut(&Self) {
                let mut f = $NodeVisitor { callback: callback };
                target.visit(&mut f)
            }
        }
    };
}

gen_visit_node_impl! {
    node = Expr;
    visitor = ExprNodeVisitor;
    fn visit_expr(&mut self, e: &'ast Expr) {
        visit::walk_expr(self, e)
    }
}

gen_visit_node_impl! {
    node = Item;
    visitor = ItemNodeVisitor;
    fn visit_item(&mut self, i: &'ast Item) {
        visit::walk_item(self, i)
    }
}

gen_visit_node_impl! {
    node = Path;
    visitor = PathNodeVisitor;
    fn visit_path(&mut self, p: &'ast Path, _id: NodeId) {
        visit::walk_path(self, p)
    }
}

gen_visit_node_impl! {
    node = Block;
    visitor = BlockNodeVisitor;
    fn visit_block(&mut self, b: &'ast Block) {
        visit::walk_block(self, b)
    }
}

gen_visit_node_impl! {
    node = Local;
    visitor = LocalNodeVisitor;
    fn visit_local(&mut self, l: &'ast Local) {
        visit::walk_local(self, l)
    }
}

gen_visit_node_impl! {
    node = ForeignItem;
    visitor = ForeignItemNodeVisitor;
    fn visit_foreign_item(&mut self, i: &'ast ForeignItem) {
        visit::walk_foreign_item(self, i)
    }
}

pub fn visit_nodes<N, T, F>(target: &T, callback: F)
        where N: VisitNode,
              T: Visit,
              F: FnMut(&N) {
    N::visit_nodes(target, callback)
}
