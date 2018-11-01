//! `Fold` trait for AST types that can be folded over.
use syntax::ast::*;
use syntax::fold::Folder;
use syntax::ptr::P;
use syntax::parse::token::{Token, Nonterminal};
use syntax::source_map::Span;
use syntax::tokenstream::{TokenTree, TokenStream};
use smallvec::SmallVec;



/// A trait for AST nodes that can accept a `Folder`.
pub trait Fold {
    /// The result of a fold over `Self`.  Typically this is either `Self` or `SmallVector<Self>`.
    type Result;

    fn fold<F: Folder>(self, f: &mut F) -> Self::Result;
}

// This macro takes as input the definition of `syntax::fold::Folder` as it appears the libsyntax
// docs, and emits a `Fold` impl for each method it finds.
macro_rules! gen_folder_impls {
    (
        pub trait Folder: Sized {
            $(
                fn $fold_fn:ident (&mut self, $arg:ident : $ArgTy:ty) -> $ResultTy:ty { ... }
            )*
        }
    ) => {
        $(
            impl Fold for $ArgTy {
                type Result = $ResultTy;
                fn fold<F: Folder>(self, f: &mut F) -> Self::Result {
                    f.$fold_fn(self)
                }
            }
        )*
    };
}

impl<T: Fold> Fold for Vec<T> {
    type Result = Vec<<T as Fold>::Result>;
    fn fold<F: Folder>(self, f: &mut F) -> Self::Result {
        let mut results = Vec::with_capacity(self.len());
        for x in self {
            results.push(x.fold(f));
        }
        results
    }
}

impl<T: Fold> Fold for Option<T> {
    type Result = Option<<T as Fold>::Result>;
    fn fold<F: Folder>(self, f: &mut F) -> Self::Result {
        self.map(|x| x.fold(f))
    }
}

gen_folder_impls! {
    // Copy-pasted from the syntax::fold::Folder docs.  Omit functions that take Vec<T> or
    // Option<T>, so we can write the generic impls above without conflicts.  Additional changes
    // are noted below.
    pub trait Folder: Sized {
        fn fold_crate(&mut self, c: Crate) -> Crate { ... }
        //fn fold_meta_items(&mut self, meta_items: Vec<MetaItem>) -> Vec<MetaItem> { ... }
        fn fold_meta_list_item(
            &mut self, 
            list_item: NestedMetaItem
        ) -> NestedMetaItem { ... }
        fn fold_meta_item(&mut self, meta_item: MetaItem) -> MetaItem { ... }
        fn fold_use_tree(&mut self, use_tree: UseTree) -> UseTree { ... }
        fn fold_foreign_item(&mut self, ni: ForeignItem) -> SmallVec<[ForeignItem; 1]> { ... }
        //fn fold_foreign_item_simple(&mut self, ni: ForeignItem) -> ForeignItem { ... }
        fn fold_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> { ... }
        //fn fold_item_simple(&mut self, i: Item) -> Item { ... }
        fn fold_struct_field(&mut self, sf: StructField) -> StructField { ... }
        fn fold_item_kind(&mut self, i: ItemKind) -> ItemKind { ... }
        fn fold_trait_item(&mut self, i: TraitItem) -> SmallVec<[TraitItem; 1]> { ... }
        fn fold_impl_item(&mut self, i: ImplItem) -> SmallVec<[ImplItem; 1]> { ... }
        fn fold_fn_decl(&mut self, d: P<FnDecl>) -> P<FnDecl> { ... }
        fn fold_asyncness(&mut self, a: IsAsync) -> IsAsync { ... }
        fn fold_block(&mut self, b: P<Block>) -> P<Block> { ... }
        fn fold_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]> { ... }
        fn fold_arm(&mut self, a: Arm) -> Arm { ... }
        fn fold_guard(&mut self, g: Guard) -> Guard { ... }
        fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> { ... }
        fn fold_anon_const(&mut self, c: AnonConst) -> AnonConst { ... }
        fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> { ... }
        fn fold_range_end(&mut self, re: RangeEnd) -> RangeEnd { ... }
        // Skip this method.  We already have an impl for P<Expr>, from fold_expr above
        //fn fold_opt_expr(&mut self, e: P<Expr>) -> Option<P<Expr>> { ... }
        //fn fold_exprs(&mut self, es: Vec<P<Expr>>) -> Vec<P<Expr>> { ... }
        fn fold_generic_arg(&mut self, arg: GenericArg) -> GenericArg { ... }
        fn fold_ty(&mut self, t: P<Ty>) -> P<Ty> { ... }
        fn fold_lifetime(&mut self, l: Lifetime) -> Lifetime { ... }
        fn fold_ty_binding(&mut self, t: TypeBinding) -> TypeBinding { ... }
        fn fold_mod(&mut self, m: Mod) -> Mod { ... }
        fn fold_foreign_mod(&mut self, nm: ForeignMod) -> ForeignMod { ... }
        fn fold_global_asm(&mut self, ga: P<GlobalAsm>) -> P<GlobalAsm> { ... }
        fn fold_variant(&mut self, v: Variant) -> Variant { ... }
        fn fold_ident(&mut self, i: Ident) -> Ident { ... }
        fn fold_usize(&mut self, i: usize) -> usize { ... }
        fn fold_path(&mut self, p: Path) -> Path { ... }
        //fn fold_qpath(&mut self, qs: Option<QSelf>, p: Path) -> (Option<QSelf>, Path) { ... } 
        fn fold_generic_args(&mut self, p: GenericArgs) -> GenericArgs { ... }
        fn fold_angle_bracketed_parameter_data(
            &mut self, 
            p: AngleBracketedArgs
        ) -> AngleBracketedArgs { ... }
        fn fold_parenthesized_parameter_data(
            &mut self, 
            p: ParenthesisedArgs
        ) -> ParenthesisedArgs { ... }
        fn fold_local(&mut self, l: P<Local>) -> P<Local> { ... }
        fn fold_mac(&mut self, _mac: Mac) -> Mac { ... }
        fn fold_macro_def(&mut self, def: MacroDef) -> MacroDef { ... }
        fn fold_label(&mut self, label: Label) -> Label { ... }
        fn fold_attribute(&mut self, at: Attribute) -> Option<Attribute> { ... }
        fn fold_arg(&mut self, a: Arg) -> Arg { ... }
        fn fold_generics(&mut self, generics: Generics) -> Generics { ... }
        fn fold_trait_ref(&mut self, p: TraitRef) -> TraitRef { ... }
        fn fold_poly_trait_ref(&mut self, p: PolyTraitRef) -> PolyTraitRef { ... }
        fn fold_variant_data(&mut self, vdata: VariantData) -> VariantData { ... }
        fn fold_generic_param(&mut self, param: GenericParam) -> GenericParam { ... }
        //fn fold_generic_params(&mut self, params: Vec<GenericParam>) -> Vec<GenericParam> { ... }
        fn fold_tt(&mut self, tt: TokenTree) -> TokenTree { ... }
        fn fold_tts(&mut self, tts: TokenStream) -> TokenStream { ... }
        fn fold_token(&mut self, t: Token) -> Token { ... }
        fn fold_interpolated(&mut self, nt: Nonterminal) -> Nonterminal { ... }
        //fn fold_opt_bounds(
        //    &mut self, 
        //    b: Option<GenericBounds>
        //) -> Option<GenericBounds> { ... }
        //fn fold_bounds(&mut self, b: GenericBounds) -> GenericBounds { ... }
        fn fold_param_bound(&mut self, tpb: GenericBound) -> GenericBound { ... }
        fn fold_mt(&mut self, mt: MutTy) -> MutTy { ... }
        fn fold_field(&mut self, field: Field) -> Field { ... }
        fn fold_where_clause(&mut self, where_clause: WhereClause) -> WhereClause { ... }
        fn fold_where_predicate(
            &mut self, 
            where_predicate: WherePredicate
        ) -> WherePredicate { ... }
        fn fold_vis(&mut self, vis: Visibility) -> Visibility { ... }
        fn new_id(&mut self, i: NodeId) -> NodeId { ... }
        fn new_span(&mut self, sp: Span) -> Span { ... }
    }
}


