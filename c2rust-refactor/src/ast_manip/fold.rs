//! `MutVisit` trait for AST types that can be modified.
use rustc_ast::mut_visit::*;
use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_data_structures::map_in_place::MapInPlace;
use rustc_span::source_map::Span;
use rustc_span::symbol::Ident;

use smallvec::{smallvec, SmallVec};

use crate::util::Lone;
use c2rust_macros::gen_visitor_impls;

/// A trait for AST nodes that can accept a `MutVisitor`.
pub trait MutVisit: Sized {
    fn visit<F: MutVisitor>(&mut self, _: &mut F) {
        unimplemented!("visit is not implemented for {}", stringify!(Self));
    }

    fn flat_map<F: MutVisitor>(mut self, f: &mut F) -> SmallVec<[Self; 1]> {
        self.visit(f);
        smallvec![self]
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
        noop_visit_crate(c, self)
    }

    fn visit_meta_list_item(&mut self, list_item: &mut NestedMetaItem) {
        noop_visit_meta_list_item(list_item, self);
    }

    fn visit_meta_item(&mut self, meta_item: &mut MetaItem) {
        noop_visit_meta_item(meta_item, self);
    }

    fn visit_use_tree(&mut self, use_tree: &mut UseTree) {
        noop_visit_use_tree(use_tree, self);
    }

    fn flat_map_foreign_item(&mut self, ni: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
        noop_flat_map_foreign_item(ni, self)
    }

    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        noop_flat_map_item(i, self)
    }

    fn visit_fn_header(&mut self, header: &mut FnHeader) {
        noop_visit_fn_header(header, self);
    }

    fn flat_map_field_def(&mut self, fd: FieldDef) -> SmallVec<[FieldDef; 1]> {
        noop_flat_map_field_def(fd, self)
    }

    fn visit_item_kind(&mut self, i: &mut ItemKind) {
        noop_visit_item_kind(i, self);
    }

    fn visit_fn_decl(&mut self, d: &mut P<FnDecl>) {
        noop_visit_fn_decl(d, self);
    }

    fn visit_asyncness(&mut self, a: &mut Async) {
        noop_visit_asyncness(a, self);
    }

    fn visit_closure_binder(&mut self, b: &mut ClosureBinder) {
        noop_visit_closure_binder(b, self);
    }

    fn visit_block(&mut self, b: &mut P<Block>) {
        noop_visit_block(b, self);
    }

    fn flat_map_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]> {
        noop_flat_map_stmt(s, self)
    }

    fn flat_map_arm(&mut self, arm: Arm) -> SmallVec<[Arm; 1]> {
        noop_flat_map_arm(arm, self)
    }

    fn visit_pat(&mut self, p: &mut P<Pat>) {
        noop_visit_pat(p, self);
    }

    fn visit_anon_const(&mut self, c: &mut AnonConst) {
        noop_visit_anon_const(c, self);
    }

    fn visit_expr(&mut self, e: &mut P<Expr>) {
        noop_visit_expr(e, self);
    }

    // fn filter_map_expr(&mut self, e: P<Expr>) -> Option<P<Expr>> {
    //     noop_filter_map_expr(e, self)
    // }

    fn visit_generic_arg(&mut self, arg: &mut GenericArg) {
        noop_visit_generic_arg(arg, self);
    }

    fn visit_ty(&mut self, t: &mut P<Ty>) {
        noop_visit_ty(t, self);
    }

    // noop_visit_lifetime is private, so we can't walk lifetimes
    // fn visit_lifetime(&mut self, l: &mut Lifetime) {
    //     noop_visit_lifetime(l, self);
    // }

    fn visit_constraint(&mut self, t: &mut AssocConstraint) {
        noop_visit_constraint(t, self);
    }

    fn visit_foreign_mod(&mut self, nm: &mut ForeignMod) {
        noop_visit_foreign_mod(nm, self);
    }

    fn flat_map_variant(&mut self, v: Variant) -> SmallVec<[Variant; 1]>  {
        noop_flat_map_variant(v, self)
    }

    fn visit_ident(&mut self, i: &mut Ident) {
        noop_visit_ident(i, self);
    }

    fn visit_path(&mut self, p: &mut Path) {
        noop_visit_path(p, self);
    }

    fn visit_qself(&mut self, qs: &mut Option<QSelf>) {
        noop_visit_qself(qs, self);
    }

    fn visit_generic_args(&mut self, p: &mut GenericArgs) {
        noop_visit_generic_args(p, self);
    }

    fn visit_angle_bracketed_parameter_data(&mut self, p: &mut AngleBracketedArgs) {
        noop_visit_angle_bracketed_parameter_data(p, self);
    }

    fn visit_parenthesized_parameter_data(&mut self, p: &mut ParenthesizedArgs) {
        noop_visit_parenthesized_parameter_data(p, self);
    }

    fn visit_local(&mut self, l: &mut P<Local>) {
        noop_visit_local(l, self);
    }

    // fn visit_mac_call(&mut self, _mac: &mut MacCall) {
    //     panic!("visit_mac disabled by default");
    //     // N.B., see note about macros above. If you really want a visitor that
    //     // works on macros, use this definition in your trait impl:
    //     //   mut_visit::noop_visit_mac(_mac, self);
    // }

    fn visit_macro_def(&mut self, def: &mut MacroDef) {
        noop_visit_macro_def(def, self);
    }

    fn visit_label(&mut self, label: &mut Label) {
        noop_visit_label(label, self);
    }

    fn visit_attribute(&mut self, at: &mut Attribute) {
        noop_visit_attribute(at, self);
    }

    fn flat_map_param(&mut self, param: Param) -> SmallVec<[Param; 1]> {
        noop_flat_map_param(param, self)
    }

    fn visit_generics(&mut self, generics: &mut Generics) {
        noop_visit_generics(generics, self);
    }

    fn visit_trait_ref(&mut self, tr: &mut TraitRef) {
        noop_visit_trait_ref(tr, self);
    }

    fn visit_poly_trait_ref(&mut self, p: &mut PolyTraitRef) {
        noop_visit_poly_trait_ref(p, self);
    }

    fn visit_variant_data(&mut self, vdata: &mut VariantData) {
        noop_visit_variant_data(vdata, self);
    }

    fn flat_map_generic_param(&mut self, param: GenericParam) -> SmallVec<[GenericParam; 1]> {
        noop_flat_map_generic_param(param, self)
    }

    fn visit_param_bound(&mut self, tpb: &mut GenericBound) {
        noop_visit_param_bound(tpb, self);
    }

    fn visit_mt(&mut self, mt: &mut MutTy) {
        noop_visit_mt(mt, self);
    }

    fn flat_map_expr_field(&mut self, f: ExprField) -> SmallVec<[ExprField; 1]> {
        noop_flat_map_expr_field(f, self)
    }

    fn visit_where_clause(&mut self, where_clause: &mut WhereClause) {
        noop_visit_where_clause(where_clause, self);
    }

    fn visit_where_predicate(&mut self, where_predicate: &mut WherePredicate) {
        noop_visit_where_predicate(where_predicate, self);
    }

    fn visit_vis(&mut self, vis: &mut Visibility) {
        noop_visit_vis(vis, self);
    }

    fn visit_id(&mut self, _id: &mut NodeId) {
        // Do nothing.
    }

    fn visit_span(&mut self, _sp: &mut Span) {
        // Do nothing.
    }

    fn flat_map_pat_field(&mut self, pf: PatField) -> SmallVec<[PatField; 1]> {
        noop_flat_map_pat_field(pf, self)
    }
}
}
