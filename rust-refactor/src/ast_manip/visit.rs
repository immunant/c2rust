//! `Visit` trait for AST types that can be visited.
use syntax;
use syntax::ast::*;
use syntax::visit::Visitor;



/// A trait for AST nodes that can accept a `Visitor`.
pub trait Visit {
    fn visit<'ast, V: Visitor<'ast>>(&'ast self, v: &mut V);
}

// There's no `visit_crate` method in `Visitor`, for some reason.
impl Visit for Crate {
    fn visit<'ast, V: Visitor<'ast>>(&'ast self, v: &mut V) {
        syntax::visit::walk_crate(v, self);
    }
}


// This macro takes as input the definition of `syntax::visit::Visitor` as it appears the libsyntax
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
    // Copy-pasted from the syntax::visit::Visitor docs.  Some methods take multiple arguments, so
    // they are commented out in this copy.
    pub trait Visitor<'ast>: Sized {
        //fn visit_name(&mut self, _span: Span, _name: Name) { ... }
        //fn visit_ident(&mut self, span: Span, ident: Ident) { ... }
        //fn visit_mod(&mut self, m: &'ast Mod, _s: Span, _n: NodeId) { ... }
        fn visit_foreign_item(&mut self, i: &'ast ForeignItem) { ... }
        fn visit_global_asm(&mut self, ga: &'ast GlobalAsm) { ... }
        fn visit_item(&mut self, i: &'ast Item) { ... }
        fn visit_local(&mut self, l: &'ast Local) { ... }
        fn visit_block(&mut self, b: &'ast Block) { ... }
        fn visit_stmt(&mut self, s: &'ast Stmt) { ... }
        fn visit_arm(&mut self, a: &'ast Arm) { ... }
        fn visit_pat(&mut self, p: &'ast Pat) { ... }
        fn visit_expr(&mut self, ex: &'ast Expr) { ... }
        // This is a second visit method for `Expr`
        //fn visit_expr_post(&mut self, _ex: &'ast Expr) { ... }
        fn visit_ty(&mut self, t: &'ast Ty) { ... }
        fn visit_generics(&mut self, g: &'ast Generics) { ... }
        fn visit_where_predicate(&mut self, p: &'ast WherePredicate) { ... }
        //fn visit_fn(
        //    &mut self, 
        //    fk: FnKind<'ast>, 
        //    fd: &'ast FnDecl, 
        //    s: Span, 
        //    _: NodeId
        //) { ... }
        fn visit_trait_item(&mut self, ti: &'ast TraitItem) { ... }
        fn visit_impl_item(&mut self, ii: &'ast ImplItem) { ... }
        fn visit_trait_ref(&mut self, t: &'ast TraitRef) { ... }
        fn visit_ty_param_bound(&mut self, bounds: &'ast TyParamBound) { ... }
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
        fn visit_struct_field(&mut self, s: &'ast StructField) { ... }
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
        fn visit_lifetime(&mut self, lifetime: &'ast Lifetime) { ... }
        fn visit_mac(&mut self, _mac: &'ast Mac) { ... }
        //fn visit_path(&mut self, path: &'ast Path, _id: NodeId) { ... }
        //fn visit_path_list_item(
        //    &mut self, 
        //    prefix: &'ast Path, 
        //    item: &'ast PathListItem
        //) { ... }
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
        fn visit_assoc_type_binding(&mut self, type_binding: &'ast TypeBinding) { ... }
        fn visit_attribute(&mut self, _attr: &'ast Attribute) { ... }
        fn visit_vis(&mut self, vis: &'ast Visibility) { ... }
        fn visit_fn_ret_ty(&mut self, ret_ty: &'ast FunctionRetTy) { ... }
    }
}
