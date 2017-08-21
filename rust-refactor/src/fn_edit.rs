use syntax::ast::*;
use syntax::codemap::Span;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;
use syntax::visit::{self, Visitor};

use fold::Fold;
use get_node_id::GetNodeId;
use get_span::GetSpan;
use visit::Visit;


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FnKind {
    Normal,
    ImplMethod,
    TraitMethod,
    Foreign,
}

pub struct FnLike {
    pub kind: FnKind,
    pub id: NodeId,
    pub ident: Ident,
    pub span: Span,
    pub decl: P<FnDecl>,
    pub block: Option<P<Block>>,
    pub attrs: Vec<Attribute>,
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

        let i = i.map(|i| {
            unpack!([i.node] ItemKind::Fn(decl, unsafety, constness, abi, generics, block));

            let fl = FnLike {
                kind: FnKind::Normal,
                id: i.id,
                ident: i.ident,
                span: i.span,
                decl: decl,
                block: Some(block),
                attrs: i.attrs,
            };
            let fl = (self.callback)(fl);

            let block = fl.block.expect("can't remove Block from ItemKind::Fn");
            Item {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: ItemKind::Fn(fl.decl, unsafety, constness, abi, generics, block),
                attrs: fl.attrs,
                .. i
            }
        });
        fold::noop_fold_item(i, self)
    }

    fn fold_impl_item(&mut self, i: ImplItem) -> SmallVector<ImplItem> {
        match i.node {
            ImplItemKind::Method(..) => {},
            _ => return fold::noop_fold_impl_item(i, self),
        }

        let i = {
            unpack!([i.node] ImplItemKind::Method(sig, block));

            let fl = FnLike {
                kind: FnKind::ImplMethod,
                id: i.id,
                ident: i.ident,
                span: i.span,
                decl: sig.decl,
                block: Some(block),
                attrs: i.attrs,
            };
            let fl = (self.callback)(fl);

            let sig = MethodSig {
                decl: fl.decl,
                .. sig
            };
            let block = fl.block.expect("can't remove Block from ImplItemKind::Method");
            ImplItem {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: ImplItemKind::Method(sig, block),
                attrs: fl.attrs,
                .. i
            }
        };
        fold::noop_fold_impl_item(i, self)
    }

    fn fold_trait_item(&mut self, i: TraitItem) -> SmallVector<TraitItem> {
        match i.node {
            TraitItemKind::Method(..) => {},
            _ => return fold::noop_fold_trait_item(i, self),
        }

        let i = {
            unpack!([i.node] TraitItemKind::Method(sig, block));

            let fl = FnLike {
                kind: FnKind::TraitMethod,
                id: i.id,
                ident: i.ident,
                span: i.span,
                decl: sig.decl,
                block: block,
                attrs: i.attrs,
            };
            let fl = (self.callback)(fl);


            let sig = MethodSig {
                decl: fl.decl,
                .. sig
            };
            TraitItem {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: TraitItemKind::Method(sig, fl.block),
                attrs: fl.attrs,
                .. i
            }
        };
        fold::noop_fold_trait_item(i, self)
    }

    fn fold_foreign_item(&mut self, i: ForeignItem) -> ForeignItem {
        match i.node {
            ForeignItemKind::Fn(..) => {},
            _ => return fold::noop_fold_foreign_item(i, self),
        }

        let i = {
            unpack!([i.node] ForeignItemKind::Fn(decl, generics));

            let fl = FnLike {
                kind: FnKind::Foreign,
                id: i.id,
                ident: i.ident,
                span: i.span,
                decl: decl,
                block: None,
                attrs: i.attrs,
            };
            let fl = (self.callback)(fl);

            ForeignItem {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: ForeignItemKind::Fn(fl.decl, generics),
                attrs: fl.attrs,
                .. i
            }
        };
        fold::noop_fold_foreign_item(i, self)
    }
}

/// Fold over all item-like function definitions, including `ItemKind::Fn`, `ImplItemKind::Method`,
/// `TraitItemKind::Method`, and `ForeignItemKind::Fn`.
pub fn fold_fns<T, F>(target: T, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(FnLike) -> FnLike {
    let mut f = FnFolder {
        callback: callback,
    };
    target.fold(&mut f)
}


struct FnVisitor<F>
        where F: FnMut(FnLike) {
    callback: F,
}

impl<'ast, F> Visitor<'ast> for FnVisitor<F>
        where F: FnMut(FnLike) {
    fn visit_item(&mut self, i: &'ast Item) {
        visit::walk_item(self, i);
        match i.node {
            ItemKind::Fn(..) => {},
            _ => return,
        }

        let (decl, block) = expect!([i.node]
                                    ItemKind::Fn(ref decl, _, _, _, _, ref block) =>
                                        (decl.clone(), block.clone()));

        (self.callback)(FnLike {
            kind: FnKind::Normal,
            id: i.id,
            ident: i.ident.clone(),
            span: i.span,
            decl: decl,
            block: Some(block),
            attrs: i.attrs.clone(),
        });
    }

    fn visit_impl_item(&mut self, i: &'ast ImplItem) {
        visit::walk_impl_item(self, i);
        match i.node {
            ImplItemKind::Method(..) => {},
            _ => return,
        }

        let (decl, block) = expect!([i.node]
                                    ImplItemKind::Method(ref sig, ref block) =>
                                        (sig.decl.clone(), block.clone()));

        (self.callback)(FnLike {
            kind: FnKind::ImplMethod,
            id: i.id,
            ident: i.ident.clone(),
            span: i.span,
            decl: decl,
            block: Some(block),
            attrs: i.attrs.clone(),
        });
    }

    fn visit_trait_item(&mut self, i: &'ast TraitItem) {
        visit::walk_trait_item(self, i);
        match i.node {
            TraitItemKind::Method(..) => {},
            _ => return,
        }

        let (decl, block) = expect!([i.node]
                                    TraitItemKind::Method(ref sig, ref block) =>
                                        (sig.decl.clone(), block.clone()));

        (self.callback)(FnLike {
            kind: FnKind::TraitMethod,
            id: i.id,
            ident: i.ident.clone(),
            span: i.span,
            decl: decl,
            block: block,
            attrs: i.attrs.clone(),
        });
    }

    fn visit_foreign_item(&mut self, i: &'ast ForeignItem) {
        visit::walk_foreign_item(self, i);
        match i.node {
            ForeignItemKind::Fn(..) => {},
            _ => return,
        }

        let decl = expect!([i.node]
                           ForeignItemKind::Fn(ref decl, _) => decl.clone());

        (self.callback)(FnLike {
            kind: FnKind::Foreign,
            id: i.id,
            ident: i.ident.clone(),
            span: i.span,
            decl: decl,
            block: None,
            attrs: i.attrs.clone(),
        });
    }
}

/// Visit all item-like function definitions, including `ItemKind::Fn`, `ImplItemKind::Method`,
/// `TraitItemKind::Method`, and `ForeignItemKind::Fn`.
pub fn visit_fns<T, F>(target: &T, callback: F)
        where T: Visit,
              F: FnMut(FnLike) {
    let mut f = FnVisitor {
        callback: callback,
    };
    target.visit(&mut f)
}
