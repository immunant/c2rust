//! Helper function for performing a fold that transforms only one type of AST node.
use syntax::ast::*;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;
use syntax::util::move_map::MoveMap;

use fold::Fold;


/// Trait for AST node types that can be rewritten with a fold.
pub trait FoldNode: Fold + Sized {
    fn fold_nodes<T, F>(target: T, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(Self) -> <Self as Fold>::Result;
}

macro_rules! gen_fold_node_impl {
    (
        node = $Node:ty;
        folder = $NodeFolder:ident;

        fn $fold_thing:ident ( &mut $slf:ident , $arg:ident : $ArgTy:ty ) -> $RetTy:ty;
        walk = $walk:expr;
        map = $map:expr;
    ) => {
        struct $NodeFolder<F>
                where F: FnMut($ArgTy) -> $RetTy {
            callback: F,
        }

        impl<F> Folder for $NodeFolder<F>
                where F: FnMut($ArgTy) -> $RetTy {
            fn $fold_thing(&mut $slf, $arg: $ArgTy) -> $RetTy {
                let $arg = $walk;
                $map
            }
        }

        impl FoldNode for $Node {
            fn fold_nodes<T, F>(target: T, callback: F) -> <T as Fold>::Result
                    where T: Fold,
                    F: FnMut(Self) -> <Self as Fold>::Result {
                let mut f = $NodeFolder { callback: callback };
                target.fold(&mut f)
            }
        }
    };
}

// If you want to use `fold_nodes` with more node types, add more `gen_fold_node_impl!` invocations
// below.
gen_fold_node_impl! {
    // The node type.
    node = P<Expr>;
    // A name to use for the `Folder` that rewrites this node type.
    folder = ExprNodeFolder;
    // The signature of the `Folder` method for this node type.
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr>;
    // An expression that invokes the default `Folder` behavior for this node type.  Can refer to
    // the node being folded using the argument name from the signature above.
    walk = e.map(|e| fold::noop_fold_expr(e, self));
    // An expression that computes the application of `self.callback` to the result of `walk`.
    // This will be more complicated if folding this node type returns a sequence of nodes (see
    // below for examples).
    map = (self.callback)(e);
}

gen_fold_node_impl! {
    node = P<Ty>;
    folder = TyNodeFolder;
    fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty>;
    walk = fold::noop_fold_ty(ty, self);
    map = (self.callback)(ty);
}

gen_fold_node_impl! {
    node = P<Item>;
    folder = ItemNodeFolder;
    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>>;
    walk = fold::noop_fold_item(i, self);
    map = i.move_flat_map(|i| (self.callback)(i));
}

gen_fold_node_impl! {
    node = ImplItem;
    folder = ImplItemNodeFolder;
    fn fold_impl_item(&mut self, i: ImplItem) -> SmallVector<ImplItem>;
    walk = fold::noop_fold_impl_item(i, self);
    map = i.move_flat_map(|i| (self.callback)(i));
}

gen_fold_node_impl! {
    node = Path;
    folder = PathNodeFolder;
    fn fold_path(&mut self, p: Path) -> Path;
    walk = fold::noop_fold_path(p, self);
    map = (self.callback)(p);
}

gen_fold_node_impl! {
    node = P<Block>;
    folder = BlockNodeFolder;
    fn fold_block(&mut self, b: P<Block>) -> P<Block>;
    walk = fold::noop_fold_block(b, self);
    map = (self.callback)(b);
}

gen_fold_node_impl! {
    node = P<Local>;
    folder = LocalNodeFolder;
    fn fold_local(&mut self, l: P<Local>) -> P<Local>;
    walk = fold::noop_fold_local(l, self);
    map = (self.callback)(l);
}

gen_fold_node_impl! {
    node = ForeignMod;
    folder = ForeignModNodeFolder;
    fn fold_foreign_mod(&mut self, nm: ForeignMod) -> ForeignMod;
    walk = fold::noop_fold_foreign_mod(nm, self);
    map = (self.callback)(nm);
}

/// Fold over nodes of the callback's argument type within `target`.  This function performs a
/// postorder traversal.
pub fn fold_nodes<N, T, F>(target: T, callback: F) -> <T as Fold>::Result
        where N: FoldNode,
              T: Fold,
              F: FnMut(N) -> <N as Fold>::Result {
    N::fold_nodes(target, callback)
}
