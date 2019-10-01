//! Helpers for rewriting all `fn` itemlikes, regardless of item kind.
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::mut_visit::{self, MutVisitor};
use syntax::ptr::P;
use syntax::util::map_in_place::MapInPlace;
use syntax::visit::{self, Visitor};
use syntax_pos::Span;

use crate::ast_manip::{AstName, GetNodeId, GetSpan, MutVisit, Visit};

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

impl AstName for FnKind {
    fn ast_name(&self) -> String {
        match self {
            FnKind::Normal => "Normal",
            FnKind::ImplMethod => "ImplMethod",
            FnKind::TraitMethod => "TraitMethod",
            FnKind::Foreign => "Foreign",
        }.into()
    }
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

/// MutVisitor for rewriting `fn`s using a `FnLike` callback.
struct FnFolder<F>
where
    F: FnMut(FnLike) -> SmallVec<[FnLike; 1]>,
{
    callback: F,
}

impl<F> MutVisitor for FnFolder<F>
where
    F: FnMut(FnLike) -> SmallVec<[FnLike; 1]>,
{
    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        match i.kind {
            ItemKind::Fn(..) => {}
            _ => return mut_visit::noop_flat_map_item(i, self),
        }

        let i = i.into_inner();
        unpack!([i.kind] ItemKind::Fn(decl, header, generics, block));
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

        fls.into_iter()
            .map(|fl| {
                let block = fl.block.expect("can't remove Block from ItemKind::Fn");
                P(Item {
                    id: fl.id,
                    ident: fl.ident,
                    span: fl.span,
                    kind: ItemKind::Fn(fl.decl, header.clone(), generics.clone(), block),
                    attrs: fl.attrs,
                    vis: vis.clone(),
                    // Don't keep the old tokens.  The callback could have made arbitrary changes to
                    // the signature and body of the function.
                    tokens: None,
                })
            })
            .flat_map(|i| mut_visit::noop_flat_map_item(i, self))
            .collect()
    }

    fn flat_map_impl_item(&mut self, i: ImplItem) -> SmallVec<[ImplItem; 1]> {
        match i.kind {
            ImplItemKind::Method(..) => {}
            _ => return mut_visit::noop_flat_map_impl_item(i, self),
        }

        unpack!([i.kind] ImplItemKind::Method(sig, block));
        let vis = i.vis;
        let defaultness = i.defaultness;
        let generics = i.generics;
        let MethodSig { header, decl } = sig;

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

        fls.into_iter()
            .map(|fl| {
                let sig = MethodSig {
                    header: header.clone(),
                    decl: fl.decl,
                };
                let block = fl
                    .block
                    .expect("can't remove Block from ImplItemKind::Method");
                ImplItem {
                    id: fl.id,
                    ident: fl.ident,
                    span: fl.span,
                    kind: ImplItemKind::Method(sig, block),
                    attrs: fl.attrs,
                    generics: generics.clone(),
                    vis: vis.clone(),
                    defaultness: defaultness,
                    tokens: None,
                }
            })
            .flat_map(|i| mut_visit::noop_flat_map_impl_item(i, self))
            .collect()
    }

    fn flat_map_trait_item(&mut self, i: TraitItem) -> SmallVec<[TraitItem; 1]> {
        match i.kind {
            TraitItemKind::Method(..) => {}
            _ => return mut_visit::noop_flat_map_trait_item(i, self),
        }

        unpack!([i.kind] TraitItemKind::Method(sig, block));
        let MethodSig { header, decl } = sig;
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

        fls.into_iter()
            .map(|fl| {
                let sig = MethodSig {
                    header: header.clone(),
                    decl: fl.decl,
                };
                TraitItem {
                    id: fl.id,
                    ident: fl.ident,
                    span: fl.span,
                    kind: TraitItemKind::Method(sig, fl.block),
                    attrs: fl.attrs,
                    generics: generics.clone(),
                    tokens: None,
                }
            })
            .flat_map(|i| mut_visit::noop_flat_map_trait_item(i, self))
            .collect()
    }

    fn visit_foreign_mod(&mut self, nm: &mut ForeignMod) {
        nm.items
            .flat_map_in_place(|i| self.flat_map_foreign_item(i));
    }

    fn flat_map_foreign_item(&mut self, i: ForeignItem) -> SmallVec<[ForeignItem; 1]> {
        match i.kind {
            ForeignItemKind::Fn(..) => {}
            _ => return mut_visit::noop_flat_map_foreign_item(i, self),
        }

        unpack!([i.kind] ForeignItemKind::Fn(decl, generics));
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

        fls.into_iter()
            .map(|fl| ForeignItem {
                id: fl.id,
                ident: fl.ident,
                span: fl.span,
                kind: ForeignItemKind::Fn(fl.decl, generics.clone()),
                attrs: fl.attrs,
                vis: vis.clone(),
            })
            .flat_map(|i| mut_visit::noop_flat_map_foreign_item(i, self))
            .collect()
    }
}

/// Fold over all item-like function definitions, including `ItemKind::Fn`, `ImplItemKind::Method`,
/// `TraitItemKind::Method`, and `ForeignItemKind::Fn`.
pub fn mut_visit_fns<T, F>(target: &mut T, mut callback: F)
where
    T: MutVisit,
    F: FnMut(&mut FnLike),
{
    flat_map_fns(target, |mut fl| {
        callback(&mut fl);
        smallvec![fl]
    })
}

/// Similar to `mut_visit_fns`, but allows transforming each `FnLike` into a sequence of zero or more
/// `FnLike`s.
pub fn flat_map_fns<T, F>(target: &mut T, callback: F)
where
    T: MutVisit,
    F: FnMut(FnLike) -> SmallVec<[FnLike; 1]>,
{
    let mut f = FnFolder { callback: callback };
    target.visit(&mut f)
}

/// Visitor for visiting `fn`s using a `FnLike` callback.
struct FnVisitor<F>
where
    F: FnMut(FnLike),
{
    callback: F,
}

impl<'ast, F> Visitor<'ast> for FnVisitor<F>
where
    F: FnMut(FnLike),
{
    fn visit_item(&mut self, i: &'ast Item) {
        visit::walk_item(self, i);
        match i.kind {
            ItemKind::Fn(..) => {}
            _ => return,
        }

        let (decl, block) = expect!([i.kind]
                                    ItemKind::Fn(ref decl, _, _, ref block) =>
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
        match i.kind {
            ImplItemKind::Method(..) => {}
            _ => return,
        }

        let (decl, block) = expect!([i.kind]
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
        match i.kind {
            TraitItemKind::Method(..) => {}
            _ => return,
        }

        let (decl, block) = expect!([i.kind]
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
        match i.kind {
            ForeignItemKind::Fn(..) => {}
            _ => return,
        }

        let decl = expect!([i.kind]
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
where
    T: Visit,
    F: FnMut(FnLike),
{
    let mut f = FnVisitor { callback: callback };
    target.visit(&mut f)
}
