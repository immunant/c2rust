use syntax::ast::*;
use syntax::codemap::Span;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::parse::token::{Token, Nonterminal};
use syntax::tokenstream::{TokenTree, TokenStream};
use syntax::util::small_vector::SmallVector;

use fold::Fold;


pub trait FoldNode: Fold + Sized {
    fn fold_nodes<T, F>(target: T, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(Self) -> <Self as Fold>::Result;
}

macro_rules! gen_fold_node_impl {
    (
        node = $Node:ty;
        folder = $NodeFolder:ident;

        fn $fold_thing:ident ( &mut $slf:ident , $arg:ident : $ArgTy:ty ) -> $RetTy:ty {
            $finish:expr
        }
    ) => {
        struct $NodeFolder<F>
                where F: FnMut($ArgTy) -> $RetTy {
            callback: F,
        }

        impl<F> Folder for $NodeFolder<F>
                where F: FnMut($ArgTy) -> $RetTy {
            fn $fold_thing(&mut $slf, $arg: $ArgTy) -> $RetTy {
                let $arg = ($slf.callback)($arg);
                $finish
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

gen_fold_node_impl! {
    node = P<Expr>;
    folder = ExprNodeFolder;
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        e.map(|e| fold::noop_fold_expr(e, self))
    }
}

gen_fold_node_impl! {
    node = P<Item>;
    folder = ItemNodeFolder;
    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
        // Note that `i` has been rebound as a SmallVector<P<Item>> by this point.
        SmallVector::many(i.into_iter().flat_map(|i| fold::noop_fold_item(i, self)))
    }
}

gen_fold_node_impl! {
    node = Path;
    folder = PathNodeFolder;
    fn fold_path(&mut self, p: Path) -> Path {
        fold::noop_fold_path(p, self)
    }
}

pub fn fold_nodes<N, T, F>(target: T, callback: F) -> <T as Fold>::Result
        where N: FoldNode,
              T: Fold,
              F: FnMut(N) -> <N as Fold>::Result {
    N::fold_nodes(target, callback)
}
