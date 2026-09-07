//! `MutVisit` trait for AST types that can be modified.
#[cfg(test)]
mod tests;
pub mod walk;
use self::walk::*;
use rustc_ast::ptr::P;
use rustc_ast::visit::BoundKind;
use rustc_ast::*;
use rustc_span::Ident;
use rustc_span::Span;

use smallvec::{smallvec, SmallVec};

use crate::util::Lone;
use c2rust_macros::gen_visitor_impls;

trait MapInPlace<T> {
    fn flat_map_in_place<F>(&mut self, f: F)
    where
        F: FnMut(T) -> SmallVec<[T; 1]>;
}

impl<T> MapInPlace<T> for SmallVec<[T; 1]> {
    fn flat_map_in_place<F>(&mut self, f: F)
    where
        F: FnMut(T) -> SmallVec<[T; 1]>,
    {
        *self = std::mem::take(self).into_iter().flat_map(f).collect();
    }
}

/// A trait for AST nodes that can accept a `MutVisitor`.
pub trait MutVisit: Sized {
    fn visit<F: MutVisitor>(&mut self, _: &mut F) {
        unimplemented!("visit is not implemented for {}", stringify!(Self));
    }

    fn flat_map<F: MutVisitor>(mut self, f: &mut F) -> SmallVec<[Self; 1]> {
        self.visit(f);
        smallvec![self]
    }

    fn visit_item_kinds<F: FnMut(&mut ItemKind)>(&mut self, callback: &mut F) {
        self.visit(&mut ItemKindFolder { callback });
    }
}

/// Trait to walk children of AST nodes with a MutVisitor
pub trait WalkAst: Sized {
    fn walk<T: MutVisitor>(&mut self, visitor: &mut T);
}

/// Trait for AST node types that can be rewritten with a mutable visit.
pub trait MutVisitNodes: MutVisit + Sized {
    fn visit<T, F>(target: &mut T, callback: F)
    where
        T: MutVisit,
        F: FnMut(&mut Self);
}

/// Trait for AST node types that can be rewritten with a flat_map.
pub trait FlatMapNodes: MutVisit + Sized {
    fn visit<T, F>(target: &mut T, callback: F)
    where
        T: MutVisit,
        F: FnMut(Self) -> SmallVec<[Self; 1]>;

    fn flat_map<T, F>(target: T, callback: F) -> SmallVec<[T; 1]>
    where
        T: MutVisit,
        F: FnMut(Self) -> SmallVec<[Self; 1]>;
}

impl<T> MutVisit for Vec<T>
where
    T: MutVisit,
{
    fn visit<F: MutVisitor>(&mut self, f: &mut F) {
        for elem in self {
            elem.visit(f);
        }
    }

    fn visit_item_kinds<F: FnMut(&mut ItemKind)>(&mut self, callback: &mut F) {
        for elem in self {
            elem.visit_item_kinds(callback);
        }
    }
}

impl<T> MutVisit for Option<T>
where
    T: MutVisit,
{
    fn visit<F: MutVisitor>(&mut self, f: &mut F) {
        if let Some(elem) = self {
            elem.visit(f)
        }
    }

    fn visit_item_kinds<F: FnMut(&mut ItemKind)>(&mut self, callback: &mut F) {
        if let Some(elem) = self {
            elem.visit_item_kinds(callback);
        }
    }
}

impl MutVisit for GenericBound {
    fn visit<F: MutVisitor>(&mut self, visitor: &mut F) {
        visitor.visit_param_bound(self, BoundKind::Bound);
    }
}

impl WalkAst for GenericBound {
    fn walk<F: MutVisitor>(&mut self, visitor: &mut F) {
        walk_param_bound(visitor, self);
    }
}

impl MutVisitNodes for GenericBound {
    fn visit<T: MutVisit, F: FnMut(&mut Self)>(target: &mut T, callback: F) {
        struct Folder<F>(F);
        impl<F: FnMut(&mut GenericBound)> MutVisitor for Folder<F> {
            fn visit_param_bound(&mut self, bound: &mut GenericBound, _ctxt: BoundKind) {
                walk_param_bound(self, bound);
                (self.0)(bound);
            }
        }
        target.visit(&mut Folder(callback));
    }
}

impl MutVisit for ItemKind {
    fn visit<F: MutVisitor>(&mut self, visitor: &mut F) {
        WalkAst::walk(self, visitor);
    }

    fn visit_item_kinds<F: FnMut(&mut ItemKind)>(&mut self, callback: &mut F) {
        WalkAst::walk(self, &mut ItemKindFolder { callback });
        callback(self);
    }
}

impl WalkAst for ItemKind {
    fn walk<F: MutVisitor>(&mut self, visitor: &mut F) {
        // A standalone kind has no owning item's identity or visibility. As in
        // the old visit_item_kind API, walk its children without visiting a
        // fabricated Item. Normal Item traversal supplies the real metadata.
        walk_item_kind(
            self,
            rustc_span::DUMMY_SP,
            DUMMY_NODE_ID,
            &mut Ident::empty(),
            &mut Visibility {
                kind: VisibilityKind::Inherited,
                span: rustc_span::DUMMY_SP,
                tokens: None,
            },
            (),
            visitor,
        );
    }
}

struct ItemKindFolder<'a, F> {
    callback: &'a mut F,
}

impl<F: FnMut(&mut ItemKind)> MutVisitor for ItemKindFolder<'_, F> {
    fn visit_item(&mut self, item: &mut P<Item>) {
        walk_item(self, item);
        (self.callback)(&mut item.kind);
    }
}

impl MutVisitNodes for ItemKind {
    fn visit<T: MutVisit, F: FnMut(&mut Self)>(target: &mut T, mut callback: F) {
        target.visit_item_kinds(&mut callback);
    }
}

gen_visitor_impls! {
pub trait MutVisitor: Sized {
    // Methods in this trait have one of three forms:
    //
    //   fn visit_t(&mut self, t: &mut T);                      // common
    //   fn flat_map_t(&mut self, t: T) -> SmallVec<[T; 1]>;    // rare
    //   fn filter_map_t(&mut self, t: T) -> Option<T>;         // rarest
    //
    // Any additions to this trait should happen in form of a call to a public
    // `noop_*` function that only calls out to the visitor again, not other
    // `noop_*` functions. This is a necessary API workaround to the problem of
    // not being able to call out to the super default method in an overridden
    // default method.
    //
    // When writing these methods, it is better to use destructuring like this:
    //
    //   fn visit_abc(&mut self, ABC { a, b, c: _ }: &mut ABC) {
    //       visit_a(a);
    //       visit_b(b);
    //   }
    //
    // than to use field access like this:
    //
    //   fn visit_abc(&mut self, abc: &mut ABC) {
    //       visit_a(&mut abc.a);
    //       visit_b(&mut abc.b);
    //       // ignore abc.c
    //   }
    //
    // As well as being more concise, the former is explicit about which fields
    // are skipped. Furthermore, if a new field is added, the destructuring
    // version will cause a compile error, which is good. In comparison, the
    // field access version will continue working and it would be easy to
    // forget to add handling for it.

    fn visit_crate(&mut self, c: &mut Crate) {
        walk_crate(self, c)
    }

    fn visit_meta_list_item(&mut self, list_item: &mut MetaItemInner) {
        walk_meta_list_item(self, list_item);
    }

    fn visit_meta_item(&mut self, meta_item: &mut MetaItem) {
        walk_meta_item(self, meta_item);
    }

    fn visit_use_tree(&mut self, use_tree: &mut UseTree) {
        walk_use_tree(self, use_tree);
    }

    fn flat_map_foreign_item(&mut self, ni: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
        walk_flat_map_foreign_item(self, ni)
    }

    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        walk_flat_map_item(self, i)
    }

    fn visit_fn_header(&mut self, header: &mut FnHeader) {
        walk_fn_header(self, header);
    }

    fn flat_map_field_def(&mut self, fd: FieldDef) -> SmallVec<[FieldDef; 1]> {
        walk_flat_map_field_def(self, fd)
    }

    fn visit_fn_decl(&mut self, d: &mut P<FnDecl>) {
        walk_fn_decl(self, d);
    }

    fn visit_coroutine_kind(&mut self, a: &mut CoroutineKind) {
        walk_coroutine_kind(self, a);
    }

    fn visit_closure_binder(&mut self, b: &mut ClosureBinder) {
        walk_closure_binder(self, b);
    }

    fn visit_block(&mut self, b: &mut P<Block>) {
        walk_block(self, b);
    }

    fn flat_map_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]> {
        walk_flat_map_stmt(self, s)
    }

    fn flat_map_arm(&mut self, arm: Arm) -> SmallVec<[Arm; 1]> {
        walk_flat_map_arm(self, arm)
    }

    fn visit_pat(&mut self, p: &mut P<Pat>) {
        walk_pat(self, p);
    }

    fn visit_anon_const(&mut self, c: &mut AnonConst) {
        walk_anon_const(self, c);
    }

    fn visit_expr(&mut self, e: &mut P<Expr>) {
        walk_expr(self, e);
    }

    // fn filter_map_expr(&mut self, e: P<Expr>) -> Option<P<Expr>> {
    //     noop_filter_map_expr(e, self)
    // }

    fn visit_generic_arg(&mut self, arg: &mut GenericArg) {
        walk_generic_arg(self, arg);
    }

    fn visit_ty(&mut self, t: &mut P<Ty>) {
        walk_ty(self, t);
    }

    // noop_visit_lifetime is private, so we can't walk lifetimes
    // fn visit_lifetime(&mut self, l: &mut Lifetime) {
    //     walk_lifetime(self, l);
    // }

    fn visit_assoc_item_constraint(&mut self, t: &mut AssocItemConstraint) {
        walk_assoc_item_constraint(self, t);
    }

    fn visit_foreign_mod(&mut self, nm: &mut ForeignMod) {
        walk_foreign_mod(self, nm);
    }

    fn flat_map_variant(&mut self, v: Variant) -> SmallVec<[Variant; 1]>  {
        walk_flat_map_variant(self, v)
    }

    fn visit_ident(&mut self, i: &mut Ident) {
        walk_ident(self, i);
    }

    fn visit_path(&mut self, p: &mut Path) {
        walk_path(self, p);
    }

    fn visit_qself(&mut self, qs: &mut Option<P<QSelf>>) {
        walk_qself(self, qs);
    }

    fn visit_generic_args(&mut self, p: &mut GenericArgs) {
        walk_generic_args(self, p);
    }

    fn visit_angle_bracketed_parameter_data(&mut self, p: &mut AngleBracketedArgs) {
        walk_angle_bracketed_parameter_data(self, p);
    }

    fn visit_parenthesized_parameter_data(&mut self, p: &mut ParenthesizedArgs) {
        walk_parenthesized_parameter_data(self, p);
    }

    fn visit_local(&mut self, l: &mut P<Local>) {
        walk_local(self, l);
    }

    // fn visit_mac_call(&mut self, _mac: &mut MacCall) {
    //     panic!("visit_mac disabled by default");
    //     // N.B., see note about macros above. If you really want a visitor that
    //     // works on macros, use this definition in your trait impl:
    //     //   mut_visit::walk_mac_call(self, _mac);
    // }

    fn visit_macro_def(&mut self, def: &mut MacroDef) {
        walk_macro_def(self, def);
    }

    fn visit_label(&mut self, label: &mut Label) {
        walk_label(self, label);
    }

    fn visit_attribute(&mut self, at: &mut Attribute) {
        walk_attribute(self, at);
    }

    fn flat_map_param(&mut self, param: Param) -> SmallVec<[Param; 1]> {
        walk_flat_map_param(self, param)
    }

    fn visit_generics(&mut self, generics: &mut Generics) {
        walk_generics(self, generics);
    }

    fn visit_trait_ref(&mut self, tr: &mut TraitRef) {
        walk_trait_ref(self, tr);
    }

    fn visit_poly_trait_ref(&mut self, p: &mut PolyTraitRef) {
        walk_poly_trait_ref(self, p);
    }

    fn visit_variant_data(&mut self, vdata: &mut VariantData) {
        walk_variant_data(self, vdata);
    }

    fn flat_map_generic_param(&mut self, param: GenericParam) -> SmallVec<[GenericParam; 1]> {
        walk_flat_map_generic_param(self, param)
    }

    fn visit_mt(&mut self, mt: &mut MutTy) {
        walk_mt(self, mt);
    }

    fn flat_map_expr_field(&mut self, f: ExprField) -> SmallVec<[ExprField; 1]> {
        walk_flat_map_expr_field(self, f)
    }

    fn visit_where_clause(&mut self, where_clause: &mut WhereClause) {
        walk_where_clause(self, where_clause);
    }

    fn visit_where_predicate(&mut self, where_predicate: &mut WherePredicate) {
        walk_where_predicate(self, where_predicate);
    }

    fn visit_vis(&mut self, vis: &mut Visibility) {
        walk_vis(self, vis);
    }

    fn visit_id(&mut self, _id: &mut NodeId) {
        // Do nothing.
    }

    fn visit_span(&mut self, _sp: &mut Span) {
        // Do nothing.
    }

    fn flat_map_pat_field(&mut self, pf: PatField) -> SmallVec<[PatField; 1]> {
        walk_flat_map_pat_field(self, pf)
    }
}
}
