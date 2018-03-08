//! Helpers for rewriting all `fn` itemlikes, regardless of item kind.
use syntax::ast::*;
use syntax::codemap::Span;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::move_map::MoveMap;
use syntax::util::small_vector::SmallVector;
use syntax::visit::{self, Visitor};

use ast_manip::{Fold, Visit, GetNodeId, GetSpan};


/// Enum indicating which kind of itemlike a `fn` is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FnKind {
    Normal,
    ImplMethod,
    TraitMethod,
    Foreign,
}

/// Generic representation of a `fn`.
#[derive(Clone, Debug)]
pub struct FnLike {
    pub kind: FnKind,
    pub id: NodeId,
    pub ident: Ident,
    pub span: Span,
    pub decl: P<FnDecl>,
    pub block: Option<P<Block>>,
    pub attrs: Vec<Attribute>,
    // TODO: This should probably include `generics`, and maybe some kind of "parent generics" for
    // impl and trait items.
    // TODO: Also unsafety, constness, and abi (these should be much easier to add)
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


/// Folder for rewriting `fn`s using a `FnLike` callback.
struct FnFolder<F>
        where F: FnMut(FnLike) -> SmallVector<FnLike> {
    callback: F,
}

impl<F> FnFolder<F>
        where F: FnMut(FnLike) -> SmallVector<FnLike> {
    // `Folder::fold_foreign_item` can only return a single `ForeignItem`, so we need a custom
    // sequence-returning version.  This is called below in `fold_foreign_mod`.
    fn fold_foreign_item_multi(&mut self, i: ForeignItem) -> SmallVector<ForeignItem> {
        match i.node {
            ForeignItemKind::Fn(..) => {},
            _ => return SmallVector::one(fold::noop_fold_foreign_item(i, self)),
        }

        unpack!([i.node] ForeignItemKind::Fn(decl, generics));
        let vis = i.vis;

        let fl = FnLike {
            kind: FnKind::Foreign,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl: decl,
            block: None,
            attrs: i.attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter().map(|fl| {
            ForeignItem {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: ForeignItemKind::Fn(fl.decl, generics.clone()),
                attrs: fl.attrs,
                vis: vis.clone(),
            }
        }).map(|i| fold::noop_fold_foreign_item(i, self)).collect()
    }
}

impl<F> Folder for FnFolder<F>
        where F: FnMut(FnLike) -> SmallVector<FnLike> {
    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
        match i.node {
            ItemKind::Fn(..) => {},
            _ => return fold::noop_fold_item(i, self),
        }

        let i = i.into_inner();
        unpack!([i.node] ItemKind::Fn(decl, unsafety, constness, abi, generics, block));
        let vis = i.vis;

        let fl = FnLike {
            kind: FnKind::Normal,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl: decl,
            block: Some(block),
            attrs: i.attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter().map(|fl| {
            let block = fl.block.expect("can't remove Block from ItemKind::Fn");
            P(Item {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: ItemKind::Fn(fl.decl, unsafety, constness, abi, generics.clone(), block),
                attrs: fl.attrs,
                vis: vis.clone(),
                // Don't keep the old tokens.  The callback could have made arbitrary changes to
                // the signature and body of the function.
                tokens: None,
            })
        }).flat_map(|i| fold::noop_fold_item(i, self)).collect()
    }

    fn fold_impl_item(&mut self, i: ImplItem) -> SmallVector<ImplItem> {
        match i.node {
            ImplItemKind::Method(..) => {},
            _ => return fold::noop_fold_impl_item(i, self),
        }

        unpack!([i.node] ImplItemKind::Method(sig, block));
        let vis = i.vis;
        let defaultness = i.defaultness;
        let generics = i.generics;
        let MethodSig { unsafety, constness, abi, decl } = sig;

        let fl = FnLike {
            kind: FnKind::ImplMethod,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl: decl,
            block: Some(block),
            attrs: i.attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter().map(|fl| {
            let sig = MethodSig {
                unsafety: unsafety,
                constness: constness,
                abi: abi,
                decl: fl.decl,
            };
            let block = fl.block.expect("can't remove Block from ImplItemKind::Method");
            ImplItem {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: ImplItemKind::Method(sig, block),
                attrs: fl.attrs,
                generics: generics.clone(),
                vis: vis.clone(),
                defaultness: defaultness,
                tokens: None,
            }
        }).flat_map(|i| fold::noop_fold_impl_item(i, self)).collect()
    }

    fn fold_trait_item(&mut self, i: TraitItem) -> SmallVector<TraitItem> {
        match i.node {
            TraitItemKind::Method(..) => {},
            _ => return fold::noop_fold_trait_item(i, self),
        }

        unpack!([i.node] TraitItemKind::Method(sig, block));
        let MethodSig { unsafety, constness, abi, decl } = sig;
        let generics = i.generics;

        let fl = FnLike {
            kind: FnKind::TraitMethod,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl: decl,
            block: block,
            attrs: i.attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter().map(|fl| {
            let sig = MethodSig {
                unsafety: unsafety,
                constness: constness,
                abi: abi,
                decl: fl.decl,
            };
            TraitItem {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                node: TraitItemKind::Method(sig, fl.block),
                attrs: fl.attrs,
                generics: generics.clone(),
                tokens: None,
            }
        }).flat_map(|i| fold::noop_fold_trait_item(i, self)).collect()
    }

    fn fold_foreign_mod(&mut self, mut nm: ForeignMod) -> ForeignMod {
        nm.items = nm.items.move_flat_map(|i| self.fold_foreign_item_multi(i));
        fold::noop_fold_foreign_mod(nm, self)
    }
}

/// Fold over all item-like function definitions, including `ItemKind::Fn`, `ImplItemKind::Method`,
/// `TraitItemKind::Method`, and `ForeignItemKind::Fn`.
pub fn fold_fns<T, F>(target: T, mut callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(FnLike) -> FnLike {
    fold_fns_multi(target, |fl| SmallVector::one(callback(fl)))
}

/// Similar to `fold_fns`, but allows transforming each `FnLike` into a sequence of zero or more
/// `FnLike`s.
pub fn fold_fns_multi<T, F>(target: T, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(FnLike) -> SmallVector<FnLike> {
    let mut f = FnFolder {
        callback: callback,
    };
    target.fold(&mut f)
}


/// Visitor for visiting `fn`s using a `FnLike` callback.
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
