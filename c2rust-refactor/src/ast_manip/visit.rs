//! `Visit` trait for AST types that can be visited.
use rustc_ast::visit::Visitor;
use rustc_ast::*;

/// A trait for AST nodes that can accept a `Visitor`.
pub trait Visit {
    fn visit<'ast, V: Visitor<'ast>>(&'ast self, v: &mut V);
}

// There's no `visit_crate` method in `Visitor`, for some reason.
impl Visit for Crate {
    fn visit<'ast, V: Visitor<'ast>>(&'ast self, v: &mut V) {
        rustc_ast::visit::walk_crate(v, self);
    }
}

// This macro takes as input the definition of `rustc_ast::visit::Visitor` as it appears the libsyntax
// docs, and emits a `Visit` impl for each method it finds.
macro_rules! gen_visit_impls {
    (
        pub trait Visitor<'ast>: Sized {
            $(
                fn $visit_fn:ident (&mut self, $arg:ident : &'ast $ArgTy:ty) { ... }
            )*
        }
    ) => {
        $(
            impl Visit for $ArgTy {
                fn visit<'ast, V: Visitor<'ast>>(&'ast self, v: &mut V) {
                    v.$visit_fn(self)
                }
            }
        )*
    };
}

gen_visit_impls! {
    // Copy-pasted from the rustc_ast::visit::Visitor docs.  Some methods take multiple arguments, so
    // they are commented out in this copy.
    pub trait Visitor<'ast>: Sized {
        //fn visit_name(&mut self, _span: Span, _name: Name) { ... }
        //fn visit_ident(&mut self, span: Span, ident: Ident) { ... }
        //fn visit_mod(&mut self, m: &'ast Mod, _s: Span, _n: NodeId) { ... }
        fn visit_foreign_item(&mut self, i: &'ast ForeignItem) { ... }
        fn visit_inline_asm(&mut self, ga: &'ast InlineAsm) { ... }
        fn visit_item(&mut self, i: &'ast Item) { ... }
        fn visit_local(&mut self, l: &'ast Local) { ... }
        fn visit_block(&mut self, b: &'ast Block) { ... }
        fn visit_stmt(&mut self, s: &'ast Stmt) { ... }
        fn visit_arm(&mut self, a: &'ast Arm) { ... }
        fn visit_pat(&mut self, p: &'ast Pat) { ... }
        fn visit_anon_const(&mut self, c: &'ast AnonConst) { ... }
        fn visit_expr(&mut self, ex: &'ast Expr) { ... }
        // This is a second visit method for `Expr`
        //fn visit_expr_post(&mut self, _ex: &'ast Expr) { ... }
        fn visit_ty(&mut self, t: &'ast Ty) { ... }
        fn visit_generic_param(&mut self, param: &'ast GenericParam) { ... }
        fn visit_generics(&mut self, g: &'ast Generics) { ... }
        fn visit_where_predicate(&mut self, p: &'ast WherePredicate) { ... }
        //fn visit_fn(
        //    &mut self,
        //    fk: FnKind<'ast>,
        //    s: Span,
        //    _: NodeId
        //) { ... }
        //fn visit_assoc_item(&mut self, ti: &'ast AssocItem, ctxt: AssocCtxt) { ... }
        fn visit_trait_ref(&mut self, t: &'ast TraitRef) { ... }
        //fn visit_param_bound(&mut self, bounds: &'ast GenericBound, ctxt: BoundKind) { ... }
        //fn visit_poly_trait_ref(
        //    &mut self,
        //    t: &'ast PolyTraitRef,
        //    m: &'ast TraitBoundModifier
        //) { ... }
        //fn visit_variant_data(
        //    &mut self,
        //    s: &'ast VariantData,
        //    _: Ident,
        //    _: &'ast Generics,
        //    _: NodeId,
        //    _: Span
        //) { ... }
        fn visit_field_def(&mut self, s: &'ast FieldDef) { ... }
        //fn visit_enum_def(
        //    &mut self,
        //    enum_definition: &'ast EnumDef,
        //    generics: &'ast Generics,
        //    item_id: NodeId,
        //    _: Span
        //) { ... }
        //fn visit_variant(
        //    &mut self,
        //    v: &'ast Variant,
        //    g: &'ast Generics,
        //    item_id: NodeId
        //) { ... }
        fn visit_label(&mut self, label: &'ast Label) { ... }
        //fn visit_lifetime(&mut self, lifetime: &'ast Lifetime, ctxt: LifetimeCtxt) { ... }
        fn visit_mac_call(&mut self, mac: &'ast MacCall) { ... }
        //fn visit_mac_def(&mut self, _mac: &'ast MacroDef, _id: NodeId) { ... }
        //fn visit_path(&mut self, path: &'ast Path, _id: NodeId) { ... }
        //fn visit_use_tree(&mut self, use_tree: &'ast UseTree, id: NodeId, _nested: bool) { ... }
        //fn visit_path_segment(
        //    &mut self,
        //    path_span: Span,
        //    path_segment: &'ast PathSegment
        //) { ... }
        //fn visit_generic_args(
        //    &mut self,
        //    path_span: Span,
        //    generic_args: &'ast GenericArgs
        //) { ... }
        fn visit_generic_arg(&mut self, generic_arg: &'ast GenericArg) { ... }
        fn visit_attribute(&mut self, _attr: &'ast Attribute) { ... }
        //fn visit_tt(&mut self, tt: TokenTree) { ... }
        //fn visit_tts(&mut self, tts: TokenStream) { ... }
        //fn visit_token(&mut self, _t: Token) { ... }
        fn visit_vis(&mut self, vis: &'ast Visibility) { ... }
        fn visit_fn_ret_ty(&mut self, ret_ty: &'ast FnRetTy) { ... }
    }
}
