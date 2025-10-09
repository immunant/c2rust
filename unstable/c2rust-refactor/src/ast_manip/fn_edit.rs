//! Helpers for rewriting all `fn` itemlikes, regardless of item kind.
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::visit::{self, AssocCtxt, Visitor};
use rustc_ast::*;
use rustc_data_structures::map_in_place::MapInPlace;
use rustc_span::symbol::Ident;
use rustc_span::Span;
use smallvec::smallvec;
use smallvec::SmallVec;

use crate::ast_manip::{AstName, GetNodeId, GetSpan, MutVisit, Visit};
use crate::expect;

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
    pub body: Option<P<Block>>,
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
        }
        .into()
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
        let (defaultness, generics, sig, body) = expect!([i.kind]
            ItemKind::Fn(box Fn { defaultness, generics, sig, body })
            => (defaultness, generics, sig, body));
        let vis = i.vis;

        let fl = FnLike {
            kind: FnKind::Normal,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl: sig.decl.clone(),
            body,
            attrs: i.attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter()
            .map(|fl| {
                P(Item {
                    id: fl.id,
                    ident: fl.ident,
                    span: fl.span,
                    kind: ItemKind::Fn(Box::new(Fn {
                        defaultness,
                        sig: FnSig {
                            decl: fl.decl,
                            header: sig.header,
                            span: sig.span,
                        },
                        generics: generics.clone(),
                        body: fl.body,
                    })),
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

    fn flat_map_impl_item(&mut self, i: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
        match i.kind {
            AssocItemKind::Fn(..) => {}
            _ => return mut_visit::noop_flat_map_assoc_item(i, self),
        }

        let AssocItem {
            attrs,
            id,
            span,
            vis,
            ident,
            kind,
            tokens: _,
        } = i.into_inner();
        let (defaultness, generics, sig, body) = expect!([kind]
            AssocItemKind::Fn(box Fn { defaultness, generics, sig, body })
            => (defaultness, generics, sig, body));
        let FnSig {
            header,
            decl,
            span: sig_span,
        } = sig;

        let fl = FnLike {
            kind: FnKind::ImplMethod,
            id,
            ident,
            span,
            decl,
            body,
            attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter()
            .map(|fl| {
                let sig = FnSig {
                    header,
                    decl: fl.decl,
                    span: sig_span,
                };
                P(AssocItem {
                    id: fl.id,
                    ident: fl.ident,
                    span: fl.span,
                    kind: AssocItemKind::Fn(Box::new(Fn {
                        defaultness,
                        generics: generics.clone(),
                        sig,
                        body: fl.body,
                    })),
                    attrs: fl.attrs,
                    vis: vis.clone(),
                    tokens: None,
                })
            })
            .flat_map(|i| mut_visit::noop_flat_map_assoc_item(i, self))
            .collect()
    }

    fn flat_map_trait_item(&mut self, i: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
        match i.kind {
            AssocItemKind::Fn(..) => {}
            _ => return mut_visit::noop_flat_map_assoc_item(i, self),
        }

        let AssocItem {
            attrs,
            id,
            span,
            vis,
            ident,
            kind,
            tokens: _,
        } = i.into_inner();
        let (defaultness, generics, sig, body) = expect!([kind]
            AssocItemKind::Fn(box Fn { defaultness, generics, sig, body })
            => (defaultness, generics, sig, body));
        let FnSig {
            header,
            decl,
            span: sig_span,
        } = sig;

        let fl = FnLike {
            kind: FnKind::TraitMethod,
            id,
            ident,
            span,
            decl,
            body,
            attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter()
            .map(|fl| {
                let sig = FnSig {
                    header,
                    decl: fl.decl,
                    span: sig_span,
                };
                P(AssocItem {
                    id: fl.id,
                    ident: fl.ident,
                    span: fl.span,
                    vis: vis.clone(),
                    kind: AssocItemKind::Fn(Box::new(Fn {
                        defaultness,
                        generics: generics.clone(),
                        sig,
                        body: fl.body,
                    })),
                    attrs: fl.attrs,
                    tokens: None,
                })
            })
            .flat_map(|i| mut_visit::noop_flat_map_assoc_item(i, self))
            .collect()
    }

    fn visit_foreign_mod(&mut self, nm: &mut ForeignMod) {
        nm.items
            .flat_map_in_place(|i| self.flat_map_foreign_item(i));
    }

    fn flat_map_foreign_item(&mut self, i: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
        match i.kind {
            ForeignItemKind::Fn(..) => {}
            _ => return mut_visit::noop_flat_map_foreign_item(i, self),
        }

        let ForeignItem {
            attrs,
            id,
            span,
            vis,
            ident,
            kind,
            tokens: _,
        } = i.into_inner();
        let (defaultness, generics, sig, body) = expect!([kind]
            ForeignItemKind::Fn(box Fn { defaultness, generics, sig, body })
            => (defaultness, generics, sig, body));
        let FnSig {
            header,
            decl,
            span: sig_span,
        } = sig;

        // TODO: do we need vis and tokens in here too?
        let fl = FnLike {
            kind: FnKind::Foreign,
            id,
            ident,
            span,
            decl,
            body,
            attrs,
        };
        let fls = (self.callback)(fl);

        fls.into_iter()
            .map(|fl| {
                P(ForeignItem {
                    id: fl.id,
                    ident: fl.ident,
                    span: fl.span,
                    kind: ForeignItemKind::Fn(Box::new(Fn {
                        defaultness,
                        generics: generics.clone(),
                        sig: FnSig {
                            header,
                            decl: fl.decl,
                            span: sig_span,
                        },
                        body: fl.body,
                    })),
                    attrs: fl.attrs,
                    vis: vis.clone(),
                    tokens: None,
                })
            })
            .flat_map(|i| mut_visit::noop_flat_map_foreign_item(i, self))
            .collect()
    }
}

/// Fold over all item-like function definitions, including `ItemKind::Fn`,
/// `AssocItemKind::Fn`, and `ForeignItemKind::Fn`.
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
    let mut f = FnFolder { callback };
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

        let (sig, body) = expect!([i.kind]
                                  ItemKind::Fn(box Fn { ref sig, ref body, .. }) =>
                                    (sig.clone(), body.clone()));

        (self.callback)(FnLike {
            kind: FnKind::Normal,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl: sig.decl,
            body,
            attrs: i.attrs.clone(),
        });
    }

    fn visit_assoc_item(&mut self, i: &'ast AssocItem, ctxt: AssocCtxt) {
        visit::walk_assoc_item(self, i, ctxt);
        match i.kind {
            AssocItemKind::Fn(..) => {}
            _ => return,
        }

        let (decl, body) = expect!([i.kind]
                                   AssocItemKind::Fn(box Fn { ref sig, ref body, .. }) =>
                                     (sig.decl.clone(), body.clone()));

        (self.callback)(FnLike {
            kind: FnKind::ImplMethod,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl,
            body,
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
                           ForeignItemKind::Fn(box Fn { ref sig, .. }) => sig.decl.clone());

        (self.callback)(FnLike {
            kind: FnKind::Foreign,
            id: i.id,
            ident: i.ident,
            span: i.span,
            decl,
            body: None,
            attrs: i.attrs.clone(),
        });
    }
}

/// Visit all item-like function definitions, including `ItemKind::Fn`,
/// `AssocItemKind::Fn`, and `ForeignItemKind::Fn`.
pub fn visit_fns<T, F>(target: &T, callback: F)
where
    T: Visit,
    F: FnMut(FnLike),
{
    let mut f = FnVisitor { callback };
    target.visit(&mut f)
}
