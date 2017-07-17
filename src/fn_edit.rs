use syntax::ast::*;
use syntax::codemap::Span;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use fold::Fold;
use get_node_id::GetNodeId;
use get_span::GetSpan;


pub struct FnLike {
    pub id: NodeId,
    pub span: Span,
    pub decl: P<FnDecl>,
    pub block: Option<P<Block>>,
}

impl GetNodeId for FnLike {
    fn get_node_id(&self) -> NodeId {
        self.id
    }
}

impl GetSpan for FnLike {
    fn get_span(&self) -> Span {
        self.span
    }
}


struct FnFolder<F>
        where F: FnMut(FnLike) -> FnLike {
    callback: F,
}

impl<F> Folder for FnFolder<F>
        where F: FnMut(FnLike) -> FnLike {
    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
        match i.node {
            ItemKind::Fn(..) => {},
            _ => return fold::noop_fold_item(i, self),
        }

        SmallVector::one(i.map(|i| {
            unpack!([i.node] ItemKind::Fn(decl, unsafety, constness, abi, generics, block));

            let fl = FnLike {
                id: i.id,
                span: i.span,
                decl: decl,
                block: Some(block),
            };
            let fl = (self.callback)(fl);

            let block = fl.block.expect("can't remove Block from ItemKind::Fn");
            Item {
                id: fl.id,
                span: fl.span,
                node: ItemKind::Fn(fl.decl, unsafety, constness, abi, generics, block),
                .. i
            }
        }))
    }

    fn fold_impl_item(&mut self, i: ImplItem) -> SmallVector<ImplItem> {
        match i.node {
            ImplItemKind::Method(..) => {},
            _ => return fold::noop_fold_impl_item(i, self),
        }

        SmallVector::one({
            unpack!([i.node] ImplItemKind::Method(sig, block));

            let fl = FnLike {
                id: i.id,
                span: i.span,
                decl: sig.decl,
                block: Some(block),
            };
            let fl = (self.callback)(fl);

            let sig = MethodSig {
                decl: fl.decl,
                .. sig
            };
            let block = fl.block.expect("can't remove Block from ImplItemKind::Method");
            ImplItem {
                id: fl.id,
                span: fl.span,
                node: ImplItemKind::Method(sig, block),
                .. i
            }
        })
    }

    fn fold_trait_item(&mut self, i: TraitItem) -> SmallVector<TraitItem> {
        match i.node {
            TraitItemKind::Method(..) => {},
            _ => return fold::noop_fold_trait_item(i, self),
        }

        SmallVector::one({
            unpack!([i.node] TraitItemKind::Method(sig, block));

            let fl = FnLike {
                id: i.id,
                span: i.span,
                decl: sig.decl,
                block: block,
            };
            let fl = (self.callback)(fl);


            let sig = MethodSig {
                decl: fl.decl,
                .. sig
            };
            TraitItem {
                id: fl.id,
                span: fl.span,
                node: TraitItemKind::Method(sig, fl.block),
                .. i
            }
        })
    }
}

/// Fold over all item-like function definitions, including `ItemKind::Fn`, `ImplItemKind::Method`,
/// and `TraitItemKind::Method`.
pub fn fold_fns<T, F>(target: T, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(FnLike) -> FnLike {
    let mut f = FnFolder {
        callback: callback,
    };
    target.fold(&mut f)
}
