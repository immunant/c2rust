//! Defaults made private in rustc's mutable AST visitor.
//!
//! Copied from rustc_ast/src/mut_visit.rs at Rust commit
//! d117b7f211835282b3b177dc64245fff0327c04c (nightly-2025-01-01).
//! See COPYRIGHT and LICENSE-MIT in this directory for upstream attribution.
//! Keep the upstream callback order and VISIT_TOKENS behavior. The only
//! storage adaptation is visit_tts: TokenStream's field is private, so rebuild
//! its trees through the public API without changing spans or spacing.

pub use rustc_ast::mut_visit::*;
use rustc_ast::ptr::P;
use rustc_ast::tokenstream::*;
use rustc_ast::visit::BoundKind;
use rustc_ast::*;
use rustc_data_structures::flat_map_in_place::FlatMapInPlace;
use rustc_data_structures::sync::Lrc;
use rustc_span::Ident;
use std::ops::DerefMut;
use thin_vec::ThinVec;

pub fn visit_attr_args<T: MutVisitor>(vis: &mut T, args: &mut AttrArgs) {
    match args {
        AttrArgs::Empty => {}
        AttrArgs::Delimited(args) => visit_delim_args(vis, args),
        AttrArgs::Eq { eq_span, expr } => {
            vis.visit_expr(expr);
            vis.visit_span(eq_span);
        }
    }
}

pub fn visit_attr_tt<T: MutVisitor>(vis: &mut T, tt: &mut AttrTokenTree) {
    match tt {
        AttrTokenTree::Token(token, _spacing) => {
            visit_token(vis, token);
        }
        AttrTokenTree::Delimited(dspan, _spacing, _delim, tts) => {
            visit_attr_tts(vis, tts);
            visit_delim_span(vis, dspan);
        }
        AttrTokenTree::AttrsTarget(AttrsTarget { attrs, tokens }) => {
            visit_attrs(vis, attrs);
            visit_lazy_tts_opt_mut(vis, Some(tokens));
        }
    }
}

pub fn visit_attr_tts<T: MutVisitor>(vis: &mut T, AttrTokenStream(tts): &mut AttrTokenStream) {
    if T::VISIT_TOKENS && !tts.is_empty() {
        let tts = Lrc::make_mut(tts);
        visit_vec(tts, |tree| visit_attr_tt(vis, tree));
    }
}

pub fn visit_attrs<T: MutVisitor>(vis: &mut T, attrs: &mut AttrVec) {
    for attr in attrs.iter_mut() {
        vis.visit_attribute(attr);
    }
}

pub fn visit_bounds<T: MutVisitor>(vis: &mut T, bounds: &mut GenericBounds, ctxt: BoundKind) {
    visit_vec(bounds, |bound| vis.visit_param_bound(bound, ctxt));
}

pub fn visit_constness<T: MutVisitor>(vis: &mut T, constness: &mut Const) {
    match constness {
        Const::Yes(span) => vis.visit_span(span),
        Const::No => {}
    }
}

pub fn visit_delim_args<T: MutVisitor>(vis: &mut T, args: &mut DelimArgs) {
    let DelimArgs {
        dspan,
        delim: _,
        tokens,
    } = args;
    visit_tts(vis, tokens);
    visit_delim_span(vis, dspan);
}

pub fn visit_lazy_tts<T: MutVisitor>(vis: &mut T, lazy_tts: &mut Option<LazyAttrTokenStream>) {
    visit_lazy_tts_opt_mut(vis, lazy_tts.as_mut());
}

pub fn visit_lazy_tts_opt_mut<T: MutVisitor>(
    vis: &mut T,
    lazy_tts: Option<&mut LazyAttrTokenStream>,
) {
    if T::VISIT_TOKENS {
        if let Some(lazy_tts) = lazy_tts {
            let mut tts = lazy_tts.to_attr_token_stream();
            visit_attr_tts(vis, &mut tts);
            *lazy_tts = LazyAttrTokenStream::new(tts);
        }
    }
}

pub fn visit_opt<T, F>(opt: &mut Option<T>, mut visit_elem: F)
where
    F: FnMut(&mut T),
{
    if let Some(elem) = opt {
        visit_elem(elem);
    }
}

pub fn visit_safety<T: MutVisitor>(vis: &mut T, safety: &mut Safety) {
    match safety {
        Safety::Unsafe(span) => vis.visit_span(span),
        Safety::Safe(span) => vis.visit_span(span),
        Safety::Default => {}
    }
}

pub fn visit_thin_vec<T, F>(elems: &mut ThinVec<T>, mut visit_elem: F)
where
    F: FnMut(&mut T),
{
    for elem in elems {
        visit_elem(elem);
    }
}

pub fn visit_tt<T: MutVisitor>(vis: &mut T, tt: &mut TokenTree) {
    match tt {
        TokenTree::Token(token, _spacing) => {
            visit_token(vis, token);
        }
        TokenTree::Delimited(dspan, _spacing, _delim, tts) => {
            visit_tts(vis, tts);
            visit_delim_span(vis, dspan);
        }
    }
}

pub fn visit_tts<T: MutVisitor>(vis: &mut T, tts: &mut TokenStream) {
    if T::VISIT_TOKENS && !tts.is_empty() {
        let mut trees = tts.iter().cloned().collect::<Vec<_>>();
        visit_vec(&mut trees, |tree| visit_tt(vis, tree));
        *tts = TokenStream::new(trees);
    }
}

pub fn visit_vec<T, F>(elems: &mut Vec<T>, mut visit_elem: F)
where
    F: FnMut(&mut T),
{
    for elem in elems {
        visit_elem(elem);
    }
}

pub fn walk_angle_bracketed_parameter_data<T: MutVisitor>(
    vis: &mut T,
    data: &mut AngleBracketedArgs,
) {
    let AngleBracketedArgs { args, span } = data;
    visit_thin_vec(args, |arg| match arg {
        AngleBracketedArg::Arg(arg) => vis.visit_generic_arg(arg),
        AngleBracketedArg::Constraint(constraint) => vis.visit_assoc_item_constraint(constraint),
    });
    vis.visit_span(span);
}

pub fn walk_anon_const<T: MutVisitor>(vis: &mut T, AnonConst { id, value }: &mut AnonConst) {
    vis.visit_id(id);
    vis.visit_expr(value);
}

pub fn walk_assoc_item_constraint<T: MutVisitor>(
    vis: &mut T,
    AssocItemConstraint {
        id,
        ident,
        gen_args,
        kind,
        span,
    }: &mut AssocItemConstraint,
) {
    vis.visit_id(id);
    vis.visit_ident(ident);
    if let Some(gen_args) = gen_args {
        vis.visit_generic_args(gen_args);
    }
    match kind {
        AssocItemConstraintKind::Equality { term } => match term {
            Term::Ty(ty) => vis.visit_ty(ty),
            Term::Const(c) => vis.visit_anon_const(c),
        },
        AssocItemConstraintKind::Bound { bounds } => visit_bounds(vis, bounds, BoundKind::Bound),
    }
    vis.visit_span(span);
}

pub fn walk_attribute<T: MutVisitor>(vis: &mut T, attr: &mut Attribute) {
    let Attribute {
        kind,
        id: _,
        style: _,
        span,
    } = attr;
    match kind {
        AttrKind::Normal(normal) => {
            let NormalAttr {
                item:
                    AttrItem {
                        unsafety: _,
                        path,
                        args,
                        tokens,
                    },
                tokens: attr_tokens,
            } = &mut **normal;
            vis.visit_path(path);
            visit_attr_args(vis, args);
            visit_lazy_tts(vis, tokens);
            visit_lazy_tts(vis, attr_tokens);
        }
        AttrKind::DocComment(_kind, _sym) => {}
    }
    vis.visit_span(span);
}

pub fn walk_closure_binder<T: MutVisitor>(vis: &mut T, binder: &mut ClosureBinder) {
    match binder {
        ClosureBinder::NotPresent => {}
        ClosureBinder::For {
            span: _,
            generic_params,
        } => {
            generic_params.flat_map_in_place(|param| vis.flat_map_generic_param(param));
        }
    }
}

pub fn walk_coroutine_kind<T: MutVisitor>(vis: &mut T, coroutine_kind: &mut CoroutineKind) {
    match coroutine_kind {
        CoroutineKind::Async {
            span,
            closure_id,
            return_impl_trait_id,
        }
        | CoroutineKind::Gen {
            span,
            closure_id,
            return_impl_trait_id,
        }
        | CoroutineKind::AsyncGen {
            span,
            closure_id,
            return_impl_trait_id,
        } => {
            vis.visit_id(closure_id);
            vis.visit_id(return_impl_trait_id);
            vis.visit_span(span);
        }
    }
}

pub fn walk_fn_decl<T: MutVisitor>(vis: &mut T, decl: &mut P<FnDecl>) {
    let FnDecl { inputs, output } = decl.deref_mut();
    inputs.flat_map_in_place(|param| vis.flat_map_param(param));
    vis.visit_fn_ret_ty(output);
}

pub fn walk_fn_header<T: MutVisitor>(vis: &mut T, header: &mut FnHeader) {
    let FnHeader {
        safety,
        coroutine_kind,
        constness,
        ext: _,
    } = header;
    visit_constness(vis, constness);
    coroutine_kind
        .as_mut()
        .map(|coroutine_kind| vis.visit_coroutine_kind(coroutine_kind));
    visit_safety(vis, safety);
}

pub fn walk_foreign_mod<T: MutVisitor>(vis: &mut T, foreign_mod: &mut ForeignMod) {
    let ForeignMod {
        extern_span: _,
        safety,
        abi: _,
        items,
    } = foreign_mod;
    visit_safety(vis, safety);
    items.flat_map_in_place(|item| vis.flat_map_foreign_item(item));
}

pub fn walk_generic_arg<T: MutVisitor>(vis: &mut T, arg: &mut GenericArg) {
    match arg {
        GenericArg::Lifetime(lt) => vis.visit_lifetime(lt),
        GenericArg::Type(ty) => vis.visit_ty(ty),
        GenericArg::Const(ct) => vis.visit_anon_const(ct),
    }
}

pub fn walk_generic_args<T: MutVisitor>(vis: &mut T, generic_args: &mut GenericArgs) {
    match generic_args {
        GenericArgs::AngleBracketed(data) => vis.visit_angle_bracketed_parameter_data(data),
        GenericArgs::Parenthesized(data) => vis.visit_parenthesized_parameter_data(data),
        GenericArgs::ParenthesizedElided(span) => vis.visit_span(span),
    }
}

pub fn walk_generics<T: MutVisitor>(vis: &mut T, generics: &mut Generics) {
    let Generics {
        params,
        where_clause,
        span,
    } = generics;
    params.flat_map_in_place(|param| vis.flat_map_generic_param(param));
    vis.visit_where_clause(where_clause);
    vis.visit_span(span);
}

pub fn walk_ident<T: MutVisitor>(vis: &mut T, Ident { name: _, span }: &mut Ident) {
    vis.visit_span(span);
}

pub fn walk_label<T: MutVisitor>(vis: &mut T, Label { ident }: &mut Label) {
    vis.visit_ident(ident);
}

pub fn walk_lifetime<T: MutVisitor>(vis: &mut T, Lifetime { id, ident }: &mut Lifetime) {
    vis.visit_id(id);
    vis.visit_ident(ident);
}

pub fn walk_local<T: MutVisitor>(vis: &mut T, local: &mut P<Local>) {
    let Local {
        id,
        pat,
        ty,
        kind,
        span,
        colon_sp,
        attrs,
        tokens,
    } = local.deref_mut();
    vis.visit_id(id);
    visit_attrs(vis, attrs);
    vis.visit_pat(pat);
    visit_opt(ty, |ty| vis.visit_ty(ty));
    match kind {
        LocalKind::Decl => {}
        LocalKind::Init(init) => {
            vis.visit_expr(init);
        }
        LocalKind::InitElse(init, els) => {
            vis.visit_expr(init);
            vis.visit_block(els);
        }
    }
    visit_lazy_tts(vis, tokens);
    visit_opt(colon_sp, |sp| vis.visit_span(sp));
    vis.visit_span(span);
}

pub fn walk_mac<T: MutVisitor>(vis: &mut T, mac: &mut MacCall) {
    let MacCall { path, args } = mac;
    vis.visit_path(path);
    visit_delim_args(vis, args);
}

pub fn walk_macro_def<T: MutVisitor>(vis: &mut T, macro_def: &mut MacroDef) {
    let MacroDef {
        body,
        macro_rules: _,
    } = macro_def;
    visit_delim_args(vis, body);
}

pub fn walk_meta_item<T: MutVisitor>(vis: &mut T, mi: &mut MetaItem) {
    let MetaItem {
        unsafety: _,
        path: _,
        kind,
        span,
    } = mi;
    match kind {
        MetaItemKind::Word => {}
        MetaItemKind::List(mis) => visit_thin_vec(mis, |mi| vis.visit_meta_list_item(mi)),
        MetaItemKind::NameValue(_s) => {}
    }
    vis.visit_span(span);
}

pub fn walk_meta_list_item<T: MutVisitor>(vis: &mut T, li: &mut MetaItemInner) {
    match li {
        MetaItemInner::MetaItem(mi) => vis.visit_meta_item(mi),
        MetaItemInner::Lit(_lit) => {}
    }
}

pub fn walk_mt<T: MutVisitor>(vis: &mut T, MutTy { ty, mutbl: _ }: &mut MutTy) {
    vis.visit_ty(ty);
}

pub fn walk_param_bound<T: MutVisitor>(vis: &mut T, pb: &mut GenericBound) {
    match pb {
        GenericBound::Trait(trait_ref) => vis.visit_poly_trait_ref(trait_ref),
        GenericBound::Outlives(lifetime) => walk_lifetime(vis, lifetime),
        GenericBound::Use(args, span) => {
            for arg in args {
                vis.visit_precise_capturing_arg(arg);
            }
            vis.visit_span(span);
        }
    }
}

pub fn walk_parenthesized_parameter_data<T: MutVisitor>(vis: &mut T, args: &mut ParenthesizedArgs) {
    let ParenthesizedArgs {
        inputs,
        output,
        span,
        inputs_span,
    } = args;
    visit_thin_vec(inputs, |input| vis.visit_ty(input));
    vis.visit_fn_ret_ty(output);
    vis.visit_span(span);
    vis.visit_span(inputs_span);
}

pub fn walk_path<T: MutVisitor>(
    vis: &mut T,
    Path {
        segments,
        span,
        tokens,
    }: &mut Path,
) {
    for segment in segments {
        vis.visit_path_segment(segment);
    }
    visit_lazy_tts(vis, tokens);
    vis.visit_span(span);
}

pub fn walk_poly_trait_ref<T: MutVisitor>(vis: &mut T, p: &mut PolyTraitRef) {
    let PolyTraitRef {
        bound_generic_params,
        modifiers: _,
        trait_ref,
        span,
    } = p;
    bound_generic_params.flat_map_in_place(|param| vis.flat_map_generic_param(param));
    vis.visit_trait_ref(trait_ref);
    vis.visit_span(span);
}

pub fn walk_qself<T: MutVisitor>(vis: &mut T, qself: &mut Option<P<QSelf>>) {
    visit_opt(qself, |qself| {
        let QSelf {
            ty,
            path_span,
            position: _,
        } = &mut **qself;
        vis.visit_ty(ty);
        vis.visit_span(path_span);
    })
}

pub fn walk_trait_ref<T: MutVisitor>(vis: &mut T, TraitRef { path, ref_id }: &mut TraitRef) {
    vis.visit_id(ref_id);
    vis.visit_path(path);
}

pub fn walk_use_tree<T: MutVisitor>(vis: &mut T, use_tree: &mut UseTree) {
    let UseTree { prefix, kind, span } = use_tree;
    vis.visit_path(prefix);
    match kind {
        UseTreeKind::Simple(rename) => visit_opt(rename, |rename| vis.visit_ident(rename)),
        UseTreeKind::Nested { items, span } => {
            for (tree, id) in items {
                vis.visit_id(id);
                vis.visit_use_tree(tree);
            }
            vis.visit_span(span);
        }
        UseTreeKind::Glob => {}
    }
    vis.visit_span(span);
}

pub fn walk_variant_data<T: MutVisitor>(vis: &mut T, vdata: &mut VariantData) {
    match vdata {
        VariantData::Struct {
            fields,
            recovered: _,
        } => {
            fields.flat_map_in_place(|field| vis.flat_map_field_def(field));
        }
        VariantData::Tuple(fields, id) => {
            vis.visit_id(id);
            fields.flat_map_in_place(|field| vis.flat_map_field_def(field));
        }
        VariantData::Unit(id) => vis.visit_id(id),
    }
}

pub fn walk_vis<T: MutVisitor>(vis: &mut T, visibility: &mut Visibility) {
    let Visibility { kind, span, tokens } = visibility;
    match kind {
        VisibilityKind::Public | VisibilityKind::Inherited => {}
        VisibilityKind::Restricted {
            path,
            id,
            shorthand: _,
        } => {
            vis.visit_id(id);
            vis.visit_path(path);
        }
    }
    visit_lazy_tts(vis, tokens);
    vis.visit_span(span);
}

pub fn walk_where_clause<T: MutVisitor>(vis: &mut T, wc: &mut WhereClause) {
    let WhereClause {
        has_where_token: _,
        predicates,
        span,
    } = wc;
    visit_thin_vec(predicates, |predicate| vis.visit_where_predicate(predicate));
    vis.visit_span(span);
}
