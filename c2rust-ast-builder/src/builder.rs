//! Helpers for building AST nodes.  Normally used by calling `mk().some_node(args...)`.
use std::str;

use proc_macro2::{Span, TokenStream, TokenTree};
use std::default::Default;
use std::iter::FromIterator;
use syn::{__private::ToTokens, punctuated::Punctuated, *};

/// a MetaItem that has already been turned into tokens in preparation for being added as an attribute
pub struct PreparedMetaItem {
    pub path: Path,
    pub tokens: TokenStream,
}

pub mod properties {
    pub trait ToToken {
        type Token;
        fn to_token(&self) -> Option<Self::Token>;
    }

    #[derive(Debug, Clone)]
    pub enum Mutability {
        Mutable,
        Immutable,
    }
    impl ToToken for Mutability {
        type Token = syn::token::Mut;
        fn to_token(&self) -> Option<Self::Token> {
            match self {
                Mutability::Mutable => Some(Default::default()),
                Mutability::Immutable => None,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Unsafety {
        Normal,
        Unsafe,
    }
    impl ToToken for Unsafety {
        type Token = syn::token::Unsafe;
        fn to_token(&self) -> Option<Self::Token> {
            match self {
                Unsafety::Normal => None,
                Unsafety::Unsafe => Some(Default::default()),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Constness {
        Const,
        NotConst,
    }
    impl ToToken for Constness {
        type Token = syn::token::Const;
        fn to_token(&self) -> Option<Self::Token> {
            match self {
                Constness::NotConst => None,
                Constness::Const => Some(Default::default()),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Movability {
        Movable,
        Immovable,
    }
    impl ToToken for Movability {
        type Token = syn::token::Static;
        fn to_token(&self) -> Option<Self::Token> {
            match self {
                Movability::Immovable => Some(Default::default()),
                Movability::Movable => None,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum IsAsync {
        Async,
        NotAsync,
    }
    impl ToToken for IsAsync {
        type Token = syn::token::Async;
        fn to_token(&self) -> Option<Self::Token> {
            match self {
                IsAsync::NotAsync => None,
                IsAsync::Async => Some(Default::default()),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Defaultness {
        Final,
        Default,
    }
    impl ToToken for Defaultness {
        type Token = syn::token::Default;
        fn to_token(&self) -> Option<Self::Token> {
            match self {
                Defaultness::Final => None,
                Defaultness::Default => Some(Default::default()),
            }
        }
    }
}

use self::properties::*;

pub type FnDecl = (Ident, Vec<FnArg>, Option<Variadic>, ReturnType);
pub type BareFnTyParts = (Vec<BareFnArg>, Option<Variadic>, ReturnType);

pub enum CaptureBy {
    Value,
    Ref,
}

#[derive(Debug, Clone)]
pub enum Extern {
    None,
    Implicit,
    Explicit(String),
}

//const Async : IsAsync = Some(Default::default());

pub enum SelfKind {
    Value(Mutability),
    Region(Lifetime, Mutability),
}

fn use_tree_with_prefix(prefix: Path, leaf: UseTree) -> UseTree {
    let mut out = leaf;
    for seg in prefix.segments.into_iter().rev() {
        out = UseTree::Path(UsePath {
            ident: seg.ident,
            colon2_token: Default::default(),
            tree: Box::new(out),
        });
    }
    out
}

fn punct<T, P: Default>(x: Vec<T>) -> Punctuated<T, P> {
    Punctuated::from_iter(x.into_iter())
}

fn punct_box<T, P: Default>(x: Vec<Box<T>>) -> Punctuated<T, P> {
    Punctuated::from_iter(x.into_iter().map(|x| *x))
}

pub trait Make<T> {
    fn make(self, mk: &Builder) -> T;
}

impl<T> Make<T> for T {
    fn make(self, _mk: &Builder) -> T {
        self
    }
}

impl<'a, T: Clone> Make<T> for &'a T {
    fn make(self, _mk: &Builder) -> T {
        self.clone()
    }
}

impl Make<Ident> for &str {
    fn make(self, mk: &Builder) -> Ident {
        Ident::new(self, mk.span)
    }
}

impl Make<Ident> for String {
    fn make(self, mk: &Builder) -> Ident {
        Ident::new(&*self, mk.span)
    }
}

impl Make<Ident> for &String {
    fn make(self, mk: &Builder) -> Ident {
        Ident::new(&*self, mk.span)
    }
}

impl<L: Make<Ident>> Make<Label> for L {
    fn make(self, mk: &Builder) -> Label {
        Label {
            name: Lifetime {
                apostrophe: mk.span,
                ident: self.make(mk),
            },
            colon_token: token::Colon(mk.span),
        }
    }
}

impl<'a> Make<Path> for &'a str {
    fn make(self, mk: &Builder) -> Path {
        let v = vec![self];
        Make::<Path>::make(v, mk)
    }
}

impl<'a> Make<Visibility> for &'a str {
    fn make(self, mk_: &Builder) -> Visibility {
        let kind = match self {
            "pub" => Visibility::Public(VisPublic {
                pub_token: token::Pub(mk_.span),
            }),
            "priv" | "" | "inherit" => Visibility::Inherited,
            "crate" => Visibility::Crate(VisCrate {
                crate_token: token::Crate(mk_.span),
            }),
            "pub(crate)" => Visibility::Restricted(VisRestricted {
                pub_token: token::Pub(mk_.span),
                paren_token: token::Paren(mk_.span),
                in_token: None,
                path: Box::new(mk().path("crate")),
            }),
            "pub(super)" => Visibility::Restricted(VisRestricted {
                pub_token: token::Pub(mk_.span),
                paren_token: token::Paren(mk_.span),
                in_token: None,
                path: Box::new(mk().path("super")),
            }),
            _ => panic!("unrecognized string for Visibility: {:?}", self),
        };
        kind
    }
}

impl<'a> Make<Abi> for &'a str {
    fn make(self, mk: &Builder) -> Abi {
        Abi {
            extern_token: token::Extern(mk.span),
            name: Some(LitStr::new(self, mk.span)),
        }
        // TODO: validate string: format!("unrecognized string for Abi: {:?}", self))
    }
}

impl<'a> Make<Extern> for &'a str {
    fn make(self, _mk: &Builder) -> Extern {
        Extern::Explicit(self.to_owned())
    }
}

impl<'a> Make<Extern> for Abi {
    fn make(self, _mk: &Builder) -> Extern {
        Extern::Explicit(self.name.to_token_stream().to_string())
    }
}

impl<'a> Make<Mutability> for &'a str {
    fn make(self, _mk: &Builder) -> Mutability {
        match self {
            "" | "imm" | "immut" | "immutable" => Mutability::Immutable,
            "mut" | "mutable" => Mutability::Mutable,
            _ => panic!("unrecognized string for Mutability: {:?}", self),
        }
    }
}

impl<'a> Make<Unsafety> for &'a str {
    fn make(self, _mk: &Builder) -> Unsafety {
        match self {
            "" | "safe" | "normal" => Unsafety::Normal,
            "unsafe" => Unsafety::Unsafe,
            _ => panic!("unrecognized string for Unsafety: {:?}", self),
        }
    }
}

impl<'a> Make<Constness> for &'a str {
    fn make(self, _mk: &Builder) -> Constness {
        match self {
            "" | "normal" | "not-const" => Constness::NotConst,
            "const" => Constness::Const,
            _ => panic!("unrecognized string for Constness: {:?}", self),
        }
    }
}

impl<'a> Make<UnOp> for &'a str {
    fn make(self, _mk: &Builder) -> UnOp {
        match self {
            "deref" | "*" => UnOp::Deref(Default::default()),
            "not" | "!" => UnOp::Not(Default::default()),
            "neg" | "-" => UnOp::Neg(Default::default()),
            _ => panic!("unrecognized string for UnOp: {:?}", self),
        }
    }
}

impl<I: Make<Ident>> Make<Lifetime> for I {
    fn make(self, mk: &Builder) -> Lifetime {
        Lifetime {
            apostrophe: mk.span,
            ident: self.make(mk),
        }
    }
}

impl<I: Make<Ident>> Make<PathSegment> for I {
    fn make(self, mk: &Builder) -> PathSegment {
        PathSegment {
            ident: self.make(mk),
            arguments: PathArguments::None,
        }
    }
}

impl<S: Make<PathSegment>> Make<Path> for Vec<S> {
    fn make(self, mk: &Builder) -> Path {
        let mut segments = Punctuated::new();
        for s in self {
            let segment = s.make(mk);
            let has_params = !segment.arguments.is_empty();
            segments.push(segment);
            // separate params from their segment with ::
            if has_params {
                segments.push_punct(Default::default());
            }
        }
        Path {
            leading_colon: None,
            segments,
        }
    }
}

impl Make<TokenStream> for Vec<TokenTree> {
    fn make(self, _mk: &Builder) -> TokenStream {
        self.into_iter().collect::<TokenStream>().into()
    }
}

impl Make<PathArguments> for AngleBracketedGenericArguments {
    fn make(self, _mk: &Builder) -> PathArguments {
        PathArguments::AngleBracketed(self)
    }
}

impl Make<PathArguments> for ParenthesizedGenericArguments {
    fn make(self, _mk: &Builder) -> PathArguments {
        PathArguments::Parenthesized(self)
    }
}

impl Make<GenericArgument> for Box<Type> {
    fn make(self, _mk: &Builder) -> GenericArgument {
        GenericArgument::Type(*self)
    }
}

impl Make<GenericArgument> for Lifetime {
    fn make(self, _mk: &Builder) -> GenericArgument {
        GenericArgument::Lifetime(self)
    }
}

impl Make<NestedMeta> for Meta {
    fn make(self, _mk: &Builder) -> NestedMeta {
        NestedMeta::Meta(self)
    }
}

impl Make<NestedMeta> for Lit {
    fn make(self, _mk: &Builder) -> NestedMeta {
        NestedMeta::Lit(self)
    }
}

impl Make<Lit> for String {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Str(LitStr::new(&*self, mk.span))
    }
}

impl Make<Lit> for &String {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Str(LitStr::new(&*self, mk.span))
    }
}

impl Make<Lit> for &str {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Str(LitStr::new(self, mk.span))
    }
}

impl Make<Lit> for Vec<u8> {
    fn make(self, mk: &Builder) -> Lit {
        Lit::ByteStr(LitByteStr::new(&self, mk.span))
    }
}

impl Make<Lit> for u8 {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Byte(LitByte::new(self, mk.span))
    }
}

impl Make<Lit> for char {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Char(LitChar::new(self, mk.span))
    }
}

impl Make<Lit> for u128 {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Int(LitInt::new(&self.to_string(), mk.span))
    }
}

impl Make<Signature> for Box<FnDecl> {
    fn make(self, mk: &Builder) -> Signature {
        let (name, inputs, variadic, output) = *self;
        Signature {
            unsafety: mk.unsafety.to_token(),
            asyncness: IsAsync::NotAsync.to_token(),
            constness: mk.constness.to_token(),
            fn_token: token::Fn(mk.span),
            paren_token: Default::default(),
            generics: mk.generics.clone(),
            abi: mk.get_abi_opt(),
            ident: name,
            inputs: punct(inputs),
            variadic,
            output,
        }
    }
}

pub trait LitStringable {
    fn lit_string(self, _: &Builder) -> String;
}

impl<T> LitStringable for T
where
    T: Make<Type>,
{
    fn lit_string(self, b: &Builder) -> String {
        let ty: Type = self.make(b);
        ty.to_token_stream().to_string()
    }
}

impl LitStringable for &str {
    fn lit_string(self, _: &Builder) -> String {
        self.to_string()
    }
}
#[derive(Clone, Debug)]
pub struct Builder {
    // The builder holds a set of "modifiers", such as visibility and mutability.  Functions for
    // building AST nodes don't take arguments of these types, but instead use any applicable
    // modifiers from the builder to set the node's visibility, mutability, etc.
    vis: Visibility,
    mutbl: Mutability,
    generics: Generics,
    unsafety: Unsafety,
    constness: Constness,
    ext: Extern,
    attrs: Vec<Attribute>,
    span: Span,
}

#[allow(dead_code)]
impl Builder {
    pub fn new() -> Builder {
        Builder {
            vis: Visibility::Inherited,
            mutbl: Mutability::Immutable,
            generics: Generics::default(),
            unsafety: Unsafety::Normal,
            constness: Constness::NotConst,
            ext: Extern::None,
            attrs: Vec::new(),
            span: Span::call_site(),
        }
    }

    // Modifier updates.

    pub fn vis<V: Make<Visibility>>(self, vis: V) -> Self {
        let vis = vis.make(&self);
        Builder { vis: vis, ..self }
    }

    pub fn pub_(self) -> Self {
        let pub_token = token::Pub(self.span);
        self.vis(Visibility::Public(VisPublic { pub_token }))
    }

    pub fn set_mutbl<M: Make<Mutability>>(self, mutbl: M) -> Self {
        let mutbl = mutbl.make(&self);
        Builder {
            mutbl: mutbl,
            ..self
        }
    }

    pub fn mutbl(self) -> Self {
        self.set_mutbl(Mutability::Mutable)
    }

    pub fn unsafety<U: Make<Unsafety>>(self, unsafety: U) -> Self {
        let unsafety = unsafety.make(&self);
        Builder {
            unsafety: unsafety,
            ..self
        }
    }

    pub fn unsafe_(self) -> Self {
        self.unsafety(Unsafety::Unsafe)
    }

    pub fn constness<C: Make<Constness>>(self, constness: C) -> Self {
        let constness = constness.make(&self);
        Builder {
            constness: constness,
            ..self
        }
    }

    pub fn const_(self) -> Self {
        self.constness(Constness::Const)
    }

    pub fn extern_<A: Make<Extern>>(self, ext: A) -> Self {
        let ext = ext.make(&self);
        Builder { ext: ext, ..self }
    }

    pub fn span<S: Make<Span>>(self, span: S) -> Self {
        let span = span.make(&self);
        Builder { span: span, ..self }
    }

    pub fn generic_over<P: Make<GenericParam>>(mut self, param: P) -> Self {
        let param = param.make(&self);
        self.generics.params.push(param);
        self
    }

    pub fn prepare_meta_namevalue(&self, mnv: MetaNameValue) -> PreparedMetaItem {
        let mut tokens = TokenStream::new();
        mnv.eq_token.to_tokens(&mut tokens);
        mnv.lit.to_tokens(&mut tokens);

        PreparedMetaItem {
            path: mnv.path,
            tokens,
        }
    }

    pub fn prepare_meta_list(&self, list: MetaList) -> PreparedMetaItem {
        let mut tokens = TokenStream::new();
        let comma_token = token::Comma(self.span);
        let mut it = list.nested.into_iter();
        tokens.extend(
            Some(proc_macro2::TokenTree::Punct(proc_macro2::Punct::new(
                '(',
                proc_macro2::Spacing::Alone,
            )))
            .into_iter(),
        );
        if let Some(value) = it.next() {
            value.to_tokens(&mut tokens);
        }
        for value in it {
            comma_token.to_tokens(&mut tokens);
            value.to_tokens(&mut tokens);
        }
        tokens.extend(
            Some(proc_macro2::TokenTree::Punct(proc_macro2::Punct::new(
                ')',
                proc_macro2::Spacing::Alone,
            )))
            .into_iter(),
        );
        PreparedMetaItem {
            path: list.path,
            tokens,
        }
    }

    pub fn prepare_meta_path<I>(&self, path: I) -> PreparedMetaItem
    where
        I: Make<Path>,
    {
        let path = path.make(&self);
        PreparedMetaItem {
            path,
            tokens: TokenStream::new(),
        }
    }

    pub fn prepare_meta<K>(&self, kind: K) -> PreparedMetaItem
    where
        K: Make<Meta>,
    {
        let kind: Meta = kind.make(&self);
        match kind {
            Meta::List(ml) => self.prepare_meta_list(ml),
            Meta::NameValue(mnv) => self.prepare_meta_namevalue(mnv),
            Meta::Path(path) => self.prepare_meta_path(path),
        }
    }

    pub fn prepare_nested_meta_item<I, K>(&self, path: I, kind: K) -> PreparedMetaItem
    where
        I: Make<Path>,
        K: Make<Meta>,
    {
        let path = path.make(&self);
        let kind = kind.make(&self);
        PreparedMetaItem {
            path: path,
            tokens: kind.to_token_stream(),
        }
    }

    pub fn prepared_attr(self, prepared: PreparedMetaItem) -> Self {
        let attr = self
            .clone()
            .attribute(AttrStyle::Outer, prepared.path, prepared.tokens);
        let mut attrs = self.attrs;
        attrs.push(attr);
        Builder {
            attrs: attrs,
            ..self
        }
    }

    pub fn str_attr<K, V>(self, key: K, value: V) -> Self
    where
        K: Make<Path>,
        V: Make<Lit>,
    {
        let key = key.make(&self);
        let value = value.make(&self);

        let mnv = MetaNameValue {
            path: key,
            eq_token: token::Eq(self.span),
            lit: value,
        };
        let prepared = self.prepare_meta_namevalue(mnv);
        self.prepared_attr(prepared)
    }

    pub fn single_attr<K>(self, key: K) -> Self
    where
        K: Make<PathSegment>,
    {
        let prepared = self.prepare_meta_path(vec![key]);
        self.prepared_attr(prepared)
    }

    pub fn call_attr<K, V>(self, func: K, arguments: Vec<V>) -> Self
    where
        K: Make<PathSegment>,
        V: Make<Ident>,
    {
        let func: Path = vec![func].make(&self);
        let arguments: Vec<_> = arguments
            .into_iter()
            .map(|x| NestedMeta::Meta(Meta::Path(vec![x.make(&self)].make(&self))))
            .collect();

        let metalist = MetaList {
            path: func,
            paren_token: token::Paren(self.span),
            nested: punct(arguments),
        };
        let prepared = self.prepare_meta_list(metalist);
        self.prepared_attr(prepared)
    }

    // Path segments with parameters

    pub fn path_segment_with_args<I, P>(self, identifier: I, args: P) -> PathSegment
    where
        I: Make<Ident>,
        P: Make<PathArguments>,
    {
        let identifier = identifier.make(&self);
        let args = args.make(&self);
        PathSegment {
            ident: identifier,
            arguments: args,
        }
    }

    pub fn parenthesized_args<Ts>(self, tys: Ts) -> ParenthesizedGenericArguments
    where
        Ts: Make<Vec<Box<Type>>>,
    {
        let tys = tys.make(&self);
        ParenthesizedGenericArguments {
            paren_token: token::Paren(self.span),
            inputs: punct_box(tys),
            output: ReturnType::Default,
        }
    }

    pub fn angle_bracketed_args<A>(self, args: Vec<A>) -> AngleBracketedGenericArguments
    where
        A: Make<GenericArgument>,
    {
        let args = args.into_iter().map(|arg| arg.make(&self)).collect();
        AngleBracketedGenericArguments {
            colon2_token: Some(token::Colon2(self.span)), // Always include a colon2 for turbofish
            lt_token: token::Lt(self.span),
            args: args,
            gt_token: token::Gt(self.span),
        }
    }

    pub fn generic_arg<A>(self, arg: A) -> GenericArgument
    where
        A: Make<GenericArgument>,
    {
        arg.make(&self)
    }

    // Simple nodes

    pub fn ident<I>(self, name: I) -> Ident
    where
        I: Make<Ident>,
    {
        name.make(&self)
    }

    pub fn path_segment<S>(self, seg: S) -> PathSegment
    where
        S: Make<PathSegment>,
    {
        seg.make(&self)
    }

    pub fn path<Pa>(self, path: Pa) -> Path
    where
        Pa: Make<Path>,
    {
        path.make(&self)
    }

    pub fn use_tree<Pa, K>(self, prefix: Pa, kind: K) -> UseTree
    where
        Pa: Make<Path>,
        K: Make<UseTree>,
    {
        let path: Path = prefix.make(&self);
        let mut tree = kind.make(&self);
        for seg in path.segments {
            tree = UseTree::Path(UsePath {
                ident: seg.ident,
                colon2_token: token::Colon2(self.span),
                tree: Box::new(tree),
            });
        }
        tree
    }

    pub fn abs_path<Pa>(self, path: Pa) -> Path
    where
        Pa: Make<Path>,
    {
        let mut path = path.make(&self);
        path.leading_colon = Some(token::Colon2(self.span));
        path
    }

    // Exprs
    // These are sorted in the same order as the corresponding ExprKind variants, with additional
    // variant-specific details following each variant.

    pub fn array_expr<A>(self, args: Vec<A>) -> Box<Expr>
    where
        A: Make<Box<Expr>>,
    {
        let args = args.into_iter().map(|a| *a.make(&self)).collect();
        Box::new(Expr::Array(ExprArray {
            attrs: self.attrs.into(),
            bracket_token: token::Bracket(self.span),
            elems: args,
        }))
    }

    pub fn call_expr<F, A>(self, func: F, args: Vec<A>) -> Box<Expr>
    where
        F: Make<Box<Expr>>,
        A: Make<Box<Expr>>,
    {
        let func = func.make(&self);
        let args = args.into_iter().map(|a| *a.make(&self)).collect();
        Box::new(parenthesize_if_necessary(Expr::Call(ExprCall {
            attrs: self.attrs.into(),
            paren_token: token::Paren(self.span),
            func,
            args,
        })))
    }

    pub fn method_call_expr<E, S, A>(self, expr: E, seg: S, args: Vec<A>) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
        S: Make<PathSegment>,
        A: Make<Box<Expr>>,
    {
        let expr = expr.make(&self);
        let seg = seg.make(&self);

        let mut arg_vals = Vec::with_capacity(args.len());
        for arg in args {
            arg_vals.push(arg.make(&self));
        }

        // Convert ::<> if present in seg
        fn generic_arg_to_method_generic_arg(a: GenericArgument) -> GenericMethodArgument {
            match a {
                GenericArgument::Type(t) => GenericMethodArgument::Type(t),
                GenericArgument::Const(c) => GenericMethodArgument::Const(c),
                _ => panic!(
                    "non-type-or-const generic argument found in method arguments: {:?}",
                    a
                ),
            }
        }
        let turbofish = match seg.arguments {
            PathArguments::None => None,
            PathArguments::AngleBracketed(ab) => Some(MethodTurbofish {
                colon2_token: ab.colon2_token.unwrap_or_default(),
                lt_token: ab.lt_token,
                args: ab
                    .args
                    .into_iter()
                    .map(generic_arg_to_method_generic_arg)
                    .collect(),
                gt_token: ab.gt_token,
            }),
            PathArguments::Parenthesized(_) => {
                panic!("Found parenthesized arguments on path segment for method call")
            }
        };

        Box::new(parenthesize_if_necessary(Expr::MethodCall(
            ExprMethodCall {
                attrs: self.attrs.into(),
                dot_token: token::Dot(self.span),
                paren_token: token::Paren(self.span),
                turbofish,
                receiver: expr,
                method: seg.ident,
                args: punct_box(arg_vals),
            },
        )))
    }

    pub fn tuple_expr<E>(self, exprs: Vec<E>) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
    {
        let exprs: Vec<Box<Expr>> = exprs.into_iter().map(|x| x.make(&self)).collect();
        Box::new(Expr::Tuple(ExprTuple {
            attrs: self.attrs.into(),
            paren_token: token::Paren(self.span),
            elems: punct_box(exprs),
        }))
    }

    pub fn binary_expr<O, E>(self, op: O, lhs: E, rhs: E) -> Box<Expr>
    where
        O: Make<BinOp>,
        E: Make<Box<Expr>>,
    {
        let op = op.make(&self);
        // FIXME: set span for op
        let mut lhs = lhs.make(&self);
        let rhs = rhs.make(&self);

        match op {
            BinOp::Lt(_) | BinOp::Shl(_) if has_rightmost_cast(&*lhs) => lhs = mk().paren_expr(lhs),
            _ => {}
        }

        Box::new(parenthesize_if_necessary(Expr::Binary(ExprBinary {
            attrs: self.attrs.into(),
            left: lhs,
            op,
            right: rhs,
        })))
    }

    pub fn unary_expr<O, E>(self, op: O, a: E) -> Box<Expr>
    where
        O: Make<UnOp>,
        E: Make<Box<Expr>>,
    {
        let op = op.make(&self);
        // FIXME: set span for op
        let a = a.make(&self);
        Box::new(parenthesize_if_necessary(Expr::Unary(ExprUnary {
            attrs: self.attrs.into(),
            op,
            expr: a,
        })))
    }

    pub fn lit_expr<L>(self, lit: L) -> Box<Expr>
    where
        L: Make<Lit>,
    {
        let mut lit = lit.make(&self);
        lit.set_span(self.span);
        Box::new(Expr::Lit(ExprLit {
            attrs: self.attrs.into(),
            lit,
        }))
    }

    pub fn cast_expr<E, T>(self, e: E, t: T) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
        T: Make<Box<Type>>,
    {
        let e = e.make(&self);
        let t = t.make(&self);

        Box::new(parenthesize_if_necessary(Expr::Cast(ExprCast {
            attrs: self.attrs.into(),
            as_token: token::As(self.span),
            expr: e,
            ty: t,
        })))
    }

    pub fn type_expr<E, T>(self, e: E, t: T) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
        T: Make<Box<Type>>,
    {
        let e = e.make(&self);
        let t = t.make(&self);
        Box::new(Expr::Type(ExprType {
            attrs: self.attrs.into(),
            colon_token: token::Colon(self.span),
            expr: e,
            ty: t,
        }))
    }

    pub fn unsafe_block_expr<B>(self, unsafe_blk: B) -> Box<Expr>
    where
        B: Make<ExprUnsafe>,
    {
        let unsafe_blk = unsafe_blk.make(&self);
        Box::new(Expr::Unsafe(unsafe_blk))
    }

    pub fn block_expr<B>(self, blk: B) -> Box<Expr>
    where
        B: Make<Box<Block>>,
    {
        let blk = blk.make(&self);
        Box::new(Expr::Block(ExprBlock {
            attrs: self.attrs.into(),
            block: *blk,
            label: None,
        }))
    }

    pub fn labelled_block_expr<B, L>(self, blk: B, lbl: L) -> Box<Expr>
    where
        B: Make<Box<Block>>,
        L: Make<Label>,
    {
        let blk = blk.make(&self);
        let lbl = lbl.make(&self);
        Box::new(Expr::Block(ExprBlock {
            attrs: self.attrs.into(),
            block: *blk,
            label: Some(lbl),
        }))
    }

    pub fn assign_expr<E1, E2>(self, lhs: E1, rhs: E2) -> Box<Expr>
    where
        E1: Make<Box<Expr>>,
        E2: Make<Box<Expr>>,
    {
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        Box::new(Expr::Assign(ExprAssign {
            attrs: self.attrs.into(),
            eq_token: token::Eq(self.span),
            left: lhs,
            right: rhs,
        }))
    }

    pub fn assign_op_expr<O, E1, E2>(self, op: O, lhs: E1, rhs: E2) -> Box<Expr>
    where
        O: Make<BinOp>,
        E1: Make<Box<Expr>>,
        E2: Make<Box<Expr>>,
    {
        let op = op.make(&self);
        // FIXME: set span for op
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        Box::new(Expr::AssignOp(ExprAssignOp {
            attrs: self.attrs.into(),
            op,
            left: lhs,
            right: rhs,
        }))
    }

    pub fn index_expr<E1, E2>(self, lhs: E1, rhs: E2) -> Box<Expr>
    where
        E1: Make<Box<Expr>>,
        E2: Make<Box<Expr>>,
    {
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        Box::new(parenthesize_if_necessary(Expr::Index(ExprIndex {
            attrs: self.attrs.into(),
            bracket_token: token::Bracket(self.span),
            expr: lhs,
            index: rhs,
        })))
    }

    pub fn abs_path_expr<Pa>(self, path: Pa) -> Box<Expr>
    where
        Pa: Make<Path>,
    {
        let path = mk().abs_path(path);
        self.path_expr(path)
    }

    pub fn path_expr<Pa>(self, path: Pa) -> Box<Expr>
    where
        Pa: Make<Path>,
    {
        self.qpath_expr(None, path)
    }

    pub fn qpath_expr<Pa>(self, qself: Option<QSelf>, path: Pa) -> Box<Expr>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Box::new(Expr::Path(ExprPath {
            attrs: self.attrs.into(),
            qself,
            path,
        }))
    }

    /// An array literal constructed from one repeated element.
    /// `[expr; n]`
    pub fn repeat_expr<E, N>(self, expr: E, n: N) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
        N: Make<Box<Expr>>,
    {
        let expr = expr.make(&self);
        let n = n.make(&self);
        Box::new(Expr::Repeat(ExprRepeat {
            attrs: self.attrs.into(),
            bracket_token: token::Bracket(self.span),
            semi_token: token::Semi(self.span),
            expr,
            len: n,
        }))
    }

    pub fn paren_expr<E>(self, e: E) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
    {
        let e = e.make(&self);
        Box::new(Expr::Paren(ExprParen {
            attrs: self.attrs.into(),
            paren_token: token::Paren(self.span),
            expr: e,
        }))
    }

    // Special case of path_expr
    pub fn ident_expr<I>(self, name: I) -> Box<Expr>
    where
        I: Make<Ident>,
    {
        self.path_expr(vec![name])
    }

    pub fn addr_of_expr<E>(self, e: E) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
    {
        let e = e.make(&self);
        Box::new(Expr::Reference(ExprReference {
            attrs: self.attrs.into(),
            and_token: token::And(self.span),
            raw: Default::default(),
            mutability: self.mutbl.to_token(),
            expr: e,
        }))
    }

    pub fn mac_expr<M>(self, mac: M) -> Box<Expr>
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        Box::new(Expr::Macro(ExprMacro {
            attrs: self.attrs.into(),
            mac,
        }))
    }

    pub fn struct_expr<Pa>(self, path: Pa, fields: Vec<FieldValue>) -> Box<Expr>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Box::new(Expr::Struct(ExprStruct {
            attrs: self.attrs.into(),
            brace_token: token::Brace(self.span),
            dot2_token: None,
            path,
            fields: punct(fields),
            rest: None,
        }))
    }

    // struct_expr, but with optional base expression
    pub fn struct_expr_base<Pa, E>(
        self,
        path: Pa,
        fields: Vec<FieldValue>,
        base: Option<E>,
    ) -> Box<Expr>
    where
        Pa: Make<Path>,
        E: Make<Box<Expr>>,
    {
        let path = path.make(&self);
        let base = base.map(|e| e.make(&self));
        Box::new(Expr::Struct(ExprStruct {
            attrs: self.attrs.into(),
            brace_token: token::Brace(self.span),
            dot2_token: Some(token::Dot2(self.span)),
            path,
            fields: punct(fields),
            rest: base,
        }))
    }

    pub fn field_expr<E, F>(self, val: E, field: F) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
        F: Make<Ident>,
    {
        let val = val.make(&self);
        let field = field.make(&self);
        Box::new(parenthesize_if_necessary(Expr::Field(ExprField {
            attrs: self.attrs.into(),
            dot_token: token::Dot(self.span),
            base: val,
            member: Member::Named(field),
        })))
    }

    pub fn anon_field_expr<E>(self, val: E, field: u32) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
    {
        let val = val.make(&self);
        let field = field.make(&self);
        Box::new(parenthesize_if_necessary(Expr::Field(ExprField {
            attrs: self.attrs.into(),
            dot_token: token::Dot(self.span),
            base: val,
            member: Member::Unnamed(Index {
                index: field,
                span: self.span,
            }),
        })))
    }

    pub fn field<I, E>(self, ident: I, expr: E) -> FieldValue
    where
        I: Make<Ident>,
        E: Make<Box<Expr>>,
    {
        let ident = ident.make(&self);
        let expr = expr.make(&self);
        FieldValue {
            member: Member::Named(ident),
            expr: *expr,
            colon_token: Some(token::Colon(self.span)),
            attrs: self.attrs.into(),
        }
    }

    pub fn match_expr<E>(self, cond: E, arms: Vec<Arm>) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
    {
        let cond = cond.make(&self);
        let arms = arms.into_iter().map(|arm| arm.make(&self)).collect();
        Box::new(Expr::Match(ExprMatch {
            attrs: self.attrs.into(),
            match_token: token::Match(self.span),
            brace_token: token::Brace(self.span),
            expr: cond,
            arms,
        }))
    }

    pub fn arm<Pa, E>(self, pat: Pa, guard: Option<E>, body: E) -> Arm
    where
        E: Make<Box<Expr>>,
        Pa: Make<Box<Pat>>,
    {
        let pat = *pat.make(&self);
        let guard = guard.map(|g| (token::If(self.span), g.make(&self)));
        let body = body.make(&self);
        Arm {
            attrs: self.attrs,
            pat,
            guard,
            body,
            fat_arrow_token: token::FatArrow(self.span),
            comma: Some(token::Comma(self.span)),
        }
    }

    // Literals

    pub fn int_lit<T>(self, i: u128, ty: T) -> Lit
    where
        T: LitStringable,
    {
        Lit::Int(LitInt::new(
            &format!("{}{}", i, ty.lit_string(&self)),
            self.span,
        ))
    }

    pub fn int_unsuffixed_lit(self, i: u128) -> Lit {
        Lit::Int(LitInt::new(&format!("{}", i), self.span))
    }

    pub fn float_lit<T>(self, s: &str, ty: T) -> Lit
    where
        T: LitStringable,
    {
        Lit::Float(LitFloat::new(
            &format!("{}{}", s, ty.lit_string(&self)),
            self.span,
        ))
    }

    pub fn float_unsuffixed_lit(self, s: &str) -> Lit {
        Lit::Float(LitFloat::new(s, self.span))
    }

    pub fn bool_lit(self, b: bool) -> Lit {
        Lit::Bool(LitBool {
            value: b,
            span: self.span,
        })
    }

    pub fn ifte_expr<C, T, E>(self, cond: C, then_case: T, else_case: Option<E>) -> Box<Expr>
    where
        C: Make<Box<Expr>>,
        T: Make<Box<Block>>,
        E: Make<Box<Expr>>,
    {
        let cond = cond.make(&self);
        let then_case = *then_case.make(&self);
        let else_case = else_case.map(|x| {
            let e = x.make(&self);

            // The else branch in libsyntax must be one of these three cases,
            // otherwise we have to manually add the block around the else expression
            (
                token::Else(self.span),
                match &*e {
                    Expr::If(..)
                    | Expr::Block(ExprBlock {
                        attrs: _,
                        label: None,
                        block: _,
                    }) => e,
                    _ => mk().block_expr(mk().block(vec![mk().expr_stmt(e)])),
                },
            )
        });

        Box::new(Expr::If(ExprIf {
            attrs: self.attrs.into(),
            if_token: token::If(self.span),
            cond,
            then_branch: then_case,
            else_branch: else_case,
        }))
    }

    pub fn while_expr<C, B, I>(self, cond: C, body: B, label: Option<I>) -> Box<Expr>
    where
        C: Make<Box<Expr>>,
        B: Make<Box<Block>>,
        I: Make<Ident>,
    {
        let cond = cond.make(&self);
        let body = *body.make(&self);
        let label = label.map(|l| Label {
            name: Lifetime {
                ident: l.make(&self),
                apostrophe: self.span,
            },
            colon_token: token::Colon(self.span),
        });

        Box::new(Expr::While(ExprWhile {
            attrs: self.attrs.into(),
            while_token: token::While(self.span),
            cond,
            body,
            label,
        }))
    }

    pub fn loop_expr<B, I>(self, body: B, label: Option<I>) -> Box<Expr>
    where
        B: Make<Box<Block>>,
        I: Make<Ident>,
    {
        let body = *body.make(&self);
        let label = label.map(|l| Label {
            name: Lifetime {
                ident: l.make(&self),
                apostrophe: self.span,
            },
            colon_token: token::Colon(self.span),
        });

        Box::new(Expr::Loop(ExprLoop {
            attrs: self.attrs.into(),
            loop_token: token::Loop(self.span),
            body,
            label,
        }))
    }

    pub fn for_expr<Pa, E, B, I>(self, pat: Pa, expr: E, body: B, label: Option<I>) -> Box<Expr>
    where
        Pa: Make<Box<Pat>>,
        E: Make<Box<Expr>>,
        B: Make<Box<Block>>,
        I: Make<Ident>,
    {
        let pat = *pat.make(&self);
        let expr = expr.make(&self);
        let body = *body.make(&self);
        let label = label.map(|l| Label {
            name: Lifetime {
                ident: l.make(&self),
                apostrophe: self.span,
            },
            colon_token: token::Colon(self.span),
        });

        Box::new(Expr::ForLoop(ExprForLoop {
            attrs: self.attrs.into(),
            for_token: token::For(self.span),
            in_token: token::In(self.span),
            pat,
            expr,
            body,
            label,
        }))
    }

    // Patterns

    pub fn ident_pat<I>(self, name: I) -> Box<Pat>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(Pat::Ident(PatIdent {
            attrs: self.attrs.into(),
            mutability: self.mutbl.to_token(),
            by_ref: None,
            ident: name,
            subpat: None,
        }))
    }

    pub fn tuple_pat<Pa>(self, pats: Vec<Pa>) -> Box<Pat>
    where
        Pa: Make<Box<Pat>>,
    {
        let pats: Vec<Box<Pat>> = pats.into_iter().map(|x| x.make(&self)).collect();
        Box::new(Pat::Tuple(PatTuple {
            attrs: self.attrs.into(),
            paren_token: token::Paren(self.span),
            elems: punct_box(pats),
        }))
    }

    pub fn qpath_pat<Pa>(self, qself: Option<QSelf>, path: Pa) -> Box<Pat>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Box::new(Pat::Path(PatPath {
            attrs: self.attrs.into(),
            qself,
            path,
        }))
    }

    pub fn wild_pat(self) -> Box<Pat> {
        Box::new(Pat::Wild(PatWild {
            attrs: self.attrs.into(),
            underscore_token: token::Underscore(self.span),
        }))
    }

    pub fn lit_pat<L>(self, lit: L) -> Box<Pat>
    where
        L: Make<Box<Expr>>,
    {
        let lit = lit.make(&self);
        Box::new(Pat::Lit(PatLit {
            attrs: self.attrs.into(),
            expr: lit,
        }))
    }

    pub fn mac_pat<M>(self, mac: M) -> Box<Pat>
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        Box::new(Pat::Macro(PatMacro {
            attrs: self.attrs,
            mac,
        }))
    }

    pub fn ident_ref_pat<I>(self, name: I) -> Box<Pat>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(Pat::Ident(PatIdent {
            attrs: self.attrs.into(),
            by_ref: Some(token::Ref(self.span)),
            mutability: self.mutbl.to_token(),
            ident: name,
            subpat: None,
        }))
    }

    pub fn or_pat<Pa>(self, pats: Vec<Pa>) -> Box<Pat>
    where
        Pa: Make<Box<Pat>>,
    {
        let pats: Vec<Box<Pat>> = pats.into_iter().map(|p| p.make(&self)).collect();
        Box::new(Pat::Or(PatOr {
            attrs: self.attrs.into(),
            leading_vert: None, // Untested
            cases: punct_box(pats),
        }))
    }

    // Types

    pub fn barefn_ty<T>(self, decl: T) -> Box<Type>
    where
        T: Make<Box<BareFnTyParts>>,
    {
        let decl = decl.make(&self);
        let (inputs, variadic, output) = *decl;
        let abi = self.get_abi_opt();

        let barefn = TypeBareFn {
            fn_token: token::Fn(self.span),
            paren_token: token::Paren(self.span),
            unsafety: self.unsafety.to_token(),
            abi,
            inputs: punct(inputs),
            output,
            variadic,
            lifetimes: None,
        };

        Box::new(Type::BareFn(barefn))
    }

    pub fn array_ty<T, E>(self, ty: T, len: E) -> Box<Type>
    where
        T: Make<Box<Type>>,
        E: Make<Box<Expr>>,
    {
        let ty = ty.make(&self);
        let len = *len.make(&self);
        Box::new(Type::Array(TypeArray {
            bracket_token: token::Bracket(self.span),
            semi_token: token::Semi(self.span),
            elem: ty,
            len,
        }))
    }

    pub fn slice_ty<T>(self, ty: T) -> Box<Type>
    where
        T: Make<Box<Type>>,
    {
        let ty = ty.make(&self);
        Box::new(Type::Slice(TypeSlice {
            elem: ty,
            bracket_token: token::Bracket(self.span),
        }))
    }

    pub fn ptr_ty<T>(self, ty: T) -> Box<Type>
    where
        T: Make<Box<Type>>,
    {
        let ty = ty.make(&self);
        let const_token = if self.mutbl.to_token().is_none() {
            Some(token::Const(self.span))
        } else {
            None
        };
        Box::new(Type::Ptr(TypePtr {
            elem: ty,
            mutability: self.mutbl.to_token(),
            const_token,
            star_token: token::Star(self.span),
        }))
    }

    pub fn ref_ty<T>(self, ty: T) -> Box<Type>
    where
        T: Make<Box<Type>>,
    {
        let ty = ty.make(&self);
        Box::new(Type::Reference(TypeReference {
            lifetime: None,
            elem: ty,
            mutability: self.mutbl.to_token(),
            and_token: token::And(self.span),
        }))
    }

    pub fn ref_lt_ty<L, T>(self, lt: L, ty: T) -> Box<Type>
    where
        L: Make<Lifetime>,
        T: Make<Box<Type>>,
    {
        let lt = lt.make(&self);
        let ty = ty.make(&self);
        Box::new(Type::Reference(TypeReference {
            and_token: token::And(self.span),
            lifetime: Some(lt),
            mutability: self.mutbl.to_token(),
            elem: ty,
        }))
    }

    pub fn never_ty(self) -> Box<Type> {
        Box::new(Type::Never(TypeNever {
            bang_token: token::Bang(self.span),
        }))
    }

    pub fn tuple_ty<T>(self, elem_tys: Vec<T>) -> Box<Type>
    where
        T: Make<Box<Type>>,
    {
        let elem_tys = punct(elem_tys.into_iter().map(|ty| *ty.make(&self)).collect());
        Box::new(Type::Tuple(TypeTuple {
            paren_token: token::Paren(self.span),
            elems: elem_tys,
        }))
    }

    pub fn path_ty<Pa>(self, path: Pa) -> Box<Type>
    where
        Pa: Make<Path>,
    {
        self.qpath_ty(None, path)
    }

    pub fn qpath_ty<Pa>(self, qself: Option<QSelf>, path: Pa) -> Box<Type>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Box::new(Type::Path(TypePath { qself, path }))
    }

    pub fn ident_ty<I>(self, name: I) -> Box<Type>
    where
        I: Make<Ident>,
    {
        self.path_ty(vec![name])
    }

    pub fn infer_ty(self) -> Box<Type> {
        Box::new(Type::Infer(TypeInfer {
            underscore_token: token::Underscore(self.span),
        }))
    }

    pub fn mac_ty<M>(self, mac: M) -> Box<Type>
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        Box::new(Type::Macro(TypeMacro { mac }))
    }

    pub fn cvar_args_ty(self) -> Box<Type> {
        let dot = TokenTree::Punct(proc_macro2::Punct::new('.', proc_macro2::Spacing::Joint));
        let dots = vec![dot.clone(), dot.clone(), dot];
        Box::new(Type::Verbatim(TokenStream::from_iter(dots.into_iter())))
    }

    // Stmts

    pub fn local_stmt<L>(self, local: L) -> Stmt
    where
        L: Make<Box<Local>>,
    {
        let local = *local.make(&self);
        Stmt::Local(local)
    }

    pub fn expr_stmt<E>(self, expr: E) -> Stmt
    where
        E: Make<Box<Expr>>,
    {
        let expr = *expr.make(&self);
        Stmt::Expr(expr)
    }

    pub fn semi_stmt<E>(self, expr: E) -> Stmt
    where
        E: Make<Box<Expr>>,
    {
        let expr = *expr.make(&self);
        Stmt::Semi(expr, token::Semi(self.span))
    }

    pub fn item_stmt<I>(self, item: I) -> Stmt
    where
        I: Make<Box<Item>>,
    {
        let item = *item.make(&self);
        Stmt::Item(item)
    }

    pub fn mac_stmt<M>(self, mac: M) -> Stmt
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        self.semi_stmt(mk().mac_expr(mac))
    }

    // Items

    pub fn static_item<I, T, E>(self, name: I, ty: T, init: E) -> Box<Item>
    where
        I: Make<Ident>,
        T: Make<Box<Type>>,
        E: Make<Box<Expr>>,
    {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.make(&self);
        Box::new(Item::Static(ItemStatic {
            attrs: self.attrs,
            vis: self.vis,
            mutability: self.mutbl.to_token(),
            ident: name,
            static_token: token::Static(self.span),
            colon_token: token::Colon(self.span),
            eq_token: token::Eq(self.span),
            semi_token: token::Semi(self.span),
            expr: init,
            ty,
        }))
    }

    pub fn const_item<I, T, E>(self, name: I, ty: T, init: E) -> Box<Item>
    where
        I: Make<Ident>,
        T: Make<Box<Type>>,
        E: Make<Box<Expr>>,
    {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.make(&self);
        Box::new(Item::Const(ItemConst {
            attrs: self.attrs,
            vis: self.vis,
            const_token: token::Const(self.span),
            colon_token: token::Colon(self.span),
            eq_token: token::Eq(self.span),
            semi_token: token::Semi(self.span),
            ident: name,
            ty,
            expr: init,
        }))
    }

    pub fn fn_item<S, B>(self, sig: S, block: B) -> Box<Item>
    where
        S: Make<Signature>,
        B: Make<Box<Block>>,
    {
        let sig = sig.make(&self);
        let block = block.make(&self);
        Box::new(Item::Fn(ItemFn {
            attrs: self.attrs,
            vis: self.vis,
            sig,
            block,
        }))
    }

    pub fn variadic_arg(self, variadic_attrs: Vec<Attribute>) -> Variadic {
        Variadic {
            dots: token::Dot3(self.span),
            attrs: variadic_attrs,
        }
    }

    pub fn fn_decl<I>(
        self,
        name: I,
        inputs: Vec<FnArg>,
        variadic: Option<Variadic>,
        output: ReturnType,
    ) -> Box<FnDecl>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new((name, inputs, variadic, output))
    }

    pub fn struct_item<I>(self, name: I, fields: Vec<Field>, is_tuple: bool) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        let fields = if is_tuple {
            Fields::Unnamed(FieldsUnnamed {
                paren_token: token::Paren(self.span),
                unnamed: fields.into_iter().collect(),
            })
        } else {
            Fields::Named(FieldsNamed {
                brace_token: token::Brace(self.span),
                named: fields.into_iter().collect(),
            })
        };
        Box::new(Item::Struct(ItemStruct {
            attrs: self.attrs,
            vis: self.vis,
            struct_token: token::Struct(self.span),
            semi_token: None,
            ident: name,
            generics: self.generics,
            fields,
        }))
    }

    pub fn union_item<I>(self, name: I, fields: Vec<Field>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        let fields = FieldsNamed {
            brace_token: token::Brace(self.span),
            named: punct(fields),
        };
        Box::new(Item::Union(ItemUnion {
            attrs: self.attrs,
            vis: self.vis,
            ident: name,
            fields,
            union_token: token::Union(self.span),
            generics: self.generics,
        }))
    }

    pub fn enum_item<I>(self, name: I, fields: Vec<Variant>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(Item::Enum(ItemEnum {
            attrs: self.attrs,
            vis: self.vis,
            ident: name,
            enum_token: token::Enum(self.span),
            brace_token: token::Brace(self.span),
            variants: punct(fields),
            generics: self.generics,
        }))
    }

    pub fn type_item<I, T>(self, name: I, ty: T) -> Box<Item>
    where
        I: Make<Ident>,
        T: Make<Box<Type>>,
    {
        let ty = ty.make(&self);
        let name = name.make(&self);
        Box::new(Item::Type(ItemType {
            attrs: self.attrs,
            vis: self.vis,
            ident: name,
            generics: self.generics,
            type_token: token::Type(self.span),
            eq_token: token::Eq(self.span),
            semi_token: token::Semi(self.span),
            ty,
        }))
    }

    pub fn mod_item<I>(self, name: I, items: Option<Vec<Item>>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let items = items.map(|i| (token::Brace(self.span), i));
        let name = name.make(&self);
        Box::new(Item::Mod(ItemMod {
            attrs: self.attrs,
            vis: self.vis,
            ident: name,
            mod_token: token::Mod(self.span),
            semi: None,
            content: items,
        }))
    }

    pub fn mod_<I>(self, items: Vec<I>) -> Vec<Item>
    where
        I: Make<Box<Item>>,
    {
        items.into_iter().map(|i| *i.make(&self)).collect()
    }

    pub fn mac_item<M>(self, mac: M) -> Box<Item>
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        Box::new(Item::Macro(ItemMacro {
            attrs: self.attrs,
            semi_token: Some(token::Semi(self.span)), // Untested
            ident: None,
            mac,
        }))
    }

    pub fn variant<I>(self, name: I, fields: Fields) -> Variant
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Variant {
            ident: name,
            attrs: self.attrs,
            fields,
            discriminant: None,
        }
    }

    pub fn unit_variant<I, E>(self, name: I, disc: Option<E>) -> Variant
    where
        I: Make<Ident>,
        E: Make<Box<Expr>>,
    {
        let name = name.make(&self);
        Variant {
            ident: name,
            fields: Fields::Unit,
            discriminant: disc.map(|e| (token::Eq(self.span), *e.make(&self))),
            attrs: self.attrs,
        }
    }

    pub fn impl_item<T>(self, ty: T, items: Vec<ImplItem>) -> Box<Item>
    where
        T: Make<Box<Type>>,
    {
        let ty = ty.make(&self);
        Box::new(Item::Impl(ItemImpl {
            attrs: self.attrs,
            unsafety: self.unsafety.to_token(),
            defaultness: Defaultness::Final.to_token(),
            generics: self.generics,
            trait_: None, // not a trait implementation, no ! on said trait name
            self_ty: ty,
            impl_token: token::Impl(self.span),
            brace_token: token::Brace(self.span),
            items,
        }))
    }

    pub fn extern_crate_item<I>(self, name: I, rename: Option<I>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        let rename = rename.map(|n| (token::As(self.span), n.make(&self)));
        Box::new(Item::ExternCrate(ItemExternCrate {
            attrs: self.attrs,
            vis: self.vis,
            crate_token: token::Crate(self.span),
            extern_token: token::Extern(self.span),
            semi_token: token::Semi(self.span),
            ident: name,
            rename,
        }))
    }

    pub fn use_item<U>(self, tree: U) -> Box<Item>
    where
        U: Make<UseTree>,
    {
        let tree = tree.make(&self);
        Box::new(Item::Use(ItemUse {
            attrs: self.attrs,
            vis: self.vis,
            use_token: token::Use(self.span),
            leading_colon: None,
            semi_token: token::Semi(self.span),
            tree,
        }))
    }

    // `use <path>;` item
    pub fn use_simple_item<Pa, I>(self, path: Pa, rename: Option<I>) -> Box<Item>
    where
        Pa: Make<Path>,
        I: Make<Ident>,
    {
        let path = path.make(&self);
        let rename = rename.map(|n| n.make(&self));

        fn split_path(mut p: Path) -> (Path, Option<Ident>) {
            if let Some(punct) = p.segments.pop() {
                (p, Some(punct.into_value().ident))
            } else {
                (p, None)
            }
        }
        let leading_colon = path.leading_colon;
        let (prefix, ident) = split_path(path);
        let ident = ident.expect("use_simple_item called with path `::`");
        let tree = if let Some(rename) = rename {
            use_tree_with_prefix(
                prefix,
                UseTree::Rename(UseRename {
                    ident,
                    as_token: token::As(self.span),
                    rename: rename,
                }),
            )
        } else {
            use_tree_with_prefix(prefix, UseTree::Name(UseName { ident }))
        };
        Box::new(Item::Use(ItemUse {
            attrs: self.attrs,
            vis: self.vis,
            use_token: token::Use(self.span),
            leading_colon,
            semi_token: token::Semi(self.span),
            tree,
        }))
    }

    pub fn use_multiple_item<Pa, I, It>(self, path: Pa, inner: It) -> Box<Item>
    where
        Pa: Make<Path>,
        I: Make<Ident>,
        It: Iterator<Item = I>,
    {
        let path = path.make(&self);
        let inner_trees = inner
            .map(|i| {
                UseTree::Name(UseName {
                    ident: i.make(&self),
                })
            })
            .collect();
        let leading_colon = path.leading_colon;
        let tree = use_tree_with_prefix(
            path,
            UseTree::Group(UseGroup {
                brace_token: token::Brace(self.span),
                items: inner_trees,
            }),
        );
        Box::new(Item::Use(ItemUse {
            attrs: self.attrs,
            vis: self.vis,
            use_token: token::Use(self.span),
            leading_colon,
            semi_token: token::Semi(self.span),
            tree,
        }))
    }

    pub fn use_glob_item<Pa>(self, path: Pa) -> Box<Item>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        let leading_colon = path.leading_colon;
        let tree = use_tree_with_prefix(
            path,
            UseTree::Glob(UseGlob {
                star_token: token::Star(self.span),
            }),
        );
        Box::new(Item::Use(ItemUse {
            attrs: self.attrs,
            vis: self.vis,
            use_token: token::Use(self.span),
            leading_colon,
            semi_token: token::Semi(self.span),
            tree,
        }))
    }

    pub fn foreign_items(self, items: Vec<ForeignItem>) -> Box<Item> {
        let abi = self.get_abi();

        Box::new(Item::ForeignMod(ItemForeignMod {
            attrs: self.attrs,
            brace_token: token::Brace(self.span),
            items,
            abi,
        }))
    }

    pub fn get_abi(&self) -> Abi {
        Abi {
            extern_token: token::Extern(self.span),
            name: match self.ext {
                Extern::None | Extern::Implicit => None,
                Extern::Explicit(ref s) => Some(LitStr::new(s, self.span)),
            },
        }
    }

    pub fn get_abi_opt(&self) -> Option<Abi> {
        let name: Option<LitStr> = match self.ext {
            Extern::None => return None,
            Extern::Implicit => None,
            Extern::Explicit(ref s) => Some(LitStr::new(s, self.span)),
        };
        Some(Abi {
            extern_token: token::Extern(self.span),
            name,
        })
    }

    // Impl Items

    pub fn mac_impl_item<M>(self, mac: M) -> ImplItem
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        ImplItem::Macro(ImplItemMacro {
            attrs: self.attrs,
            semi_token: None,
            mac,
        })
    }

    // Trait Items

    pub fn mac_trait_item<M>(self, mac: M) -> TraitItem
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        TraitItem::Macro(TraitItemMacro {
            attrs: self.attrs,
            semi_token: None,
            mac,
        })
    }

    // Foreign Items

    pub fn fn_foreign_item<D>(self, decl: D) -> ForeignItem
    where
        D: Make<Box<FnDecl>>,
    {
        let decl = decl.make(&self);
        let sig = Signature {
            constness: None,
            asyncness: None,
            generics: self.generics.clone(),
            ..decl.make(&self)
        };
        ForeignItem::Fn(ForeignItemFn {
            attrs: self.attrs,
            vis: self.vis,
            sig,
            semi_token: token::Semi(self.span),
        })
    }

    pub fn static_foreign_item<I, T>(self, name: I, ty: T) -> ForeignItem
    where
        I: Make<Ident>,
        T: Make<Box<Type>>,
    {
        let name = name.make(&self);
        let ty = ty.make(&self);
        ForeignItem::Static(ForeignItemStatic {
            attrs: self.attrs,
            vis: self.vis,
            mutability: self.mutbl.to_token(),
            ident: name,
            ty,
            static_token: token::Static(self.span),
            colon_token: token::Colon(self.span),
            semi_token: token::Semi(self.span),
        })
    }

    pub fn ty_foreign_item<I>(self, name: I) -> ForeignItem
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        ForeignItem::Type(ForeignItemType {
            attrs: self.attrs,
            vis: self.vis,
            ident: name,
            type_token: token::Type(self.span),
            semi_token: token::Semi(self.span),
        })
    }

    pub fn mac_foreign_item<M>(self, mac: M) -> ForeignItem
    where
        M: Make<Macro>,
    {
        let mac = mac.make(&self);
        ForeignItem::Macro(ForeignItemMacro {
            attrs: self.attrs,
            mac,
            semi_token: None,
        })
    }

    // struct fields

    pub fn struct_field<I, T>(self, ident: I, ty: T) -> Field
    where
        I: Make<Ident>,
        T: Make<Box<Type>>,
    {
        let ident = ident.make(&self);
        let ty = *ty.make(&self);
        Field {
            ident: Some(ident),
            vis: self.vis,
            attrs: self.attrs,
            ty,
            colon_token: Some(token::Colon(self.span)),
        }
    }

    pub fn enum_field<T>(self, ty: T) -> Field
    where
        T: Make<Box<Type>>,
    {
        let ty = *ty.make(&self);
        Field {
            ident: None,
            vis: self.vis,
            ty,
            attrs: self.attrs,
            colon_token: None,
        }
    }

    // Misc nodes

    pub fn unsafe_block<S>(self, stmts: Vec<S>) -> ExprUnsafe
    where
        S: Make<Stmt>,
    {
        let stmts = stmts.into_iter().map(|s| s.make(&self)).collect();
        let blk = Block {
            stmts: stmts,
            brace_token: token::Brace(self.span),
        };
        ExprUnsafe {
            attrs: self.attrs.into(),
            unsafe_token: token::Unsafe(self.span),
            block: blk,
        }
    }

    pub fn block<S>(self, stmts: Vec<S>) -> Box<Block>
    where
        S: Make<Stmt>,
    {
        let stmts = stmts.into_iter().map(|s| s.make(&self)).collect();
        Box::new(Block {
            stmts: stmts,
            brace_token: token::Brace(self.span),
        })
    }

    pub fn label<L>(self, lbl: L) -> Label
    where
        L: Make<Label>,
    {
        lbl.make(&self)
    }

    pub fn break_expr_value<L, E>(self, label: Option<L>, value: Option<E>) -> Box<Expr>
    where
        L: Make<Label>,
        E: Make<Box<Expr>>,
    {
        let label = label.map(|l| l.make(&self).name);
        let value = value.map(|v| v.make(&self));
        Box::new(Expr::Break(ExprBreak {
            attrs: self.attrs.into(),
            break_token: token::Break(self.span),
            label,
            expr: value,
        }))
    }

    pub fn bare_arg<T, I>(self, ty: T, name: Option<I>) -> BareFnArg
    where
        T: Make<Box<Type>>,
        I: Make<Box<Ident>>,
    {
        let ty = *ty.make(&self);
        let name = name.map(|n| (*n.make(&self), token::Colon(self.span)));
        BareFnArg {
            attrs: Vec::new(),
            name,
            ty,
        }
    }

    pub fn arg<T, Pt>(self, ty: T, pat: Pt) -> FnArg
    where
        T: Make<Box<Type>>,
        Pt: Make<Box<Pat>>,
    {
        let ty = ty.make(&self);
        let pat = pat.make(&self);
        FnArg::Typed(PatType {
            attrs: Vec::new(),
            ty: ty,
            pat: pat,
            colon_token: token::Colon(self.span),
        })
    }

    pub fn self_arg<S>(self, kind: S) -> FnArg
    where
        S: Make<SelfKind>,
    {
        let eself = kind.make(&self);
        let (reference, mutability) = match eself {
            SelfKind::Value(mutability) => (None, mutability),
            SelfKind::Region(lt, mutability) => {
                (Some((token::And(self.span), Some(lt))), mutability)
            }
        };
        let attrs = Vec::new();
        FnArg::Receiver(Receiver {
            attrs,
            reference,
            mutability: mutability.to_token(),
            self_token: token::SelfValue(self.span),
        })
    }

    pub fn ty_param<I>(self, ident: I) -> GenericParam
    where
        I: Make<Ident>,
    {
        let ident = ident.make(&self);
        GenericParam::Type(TypeParam {
            attrs: self.attrs.into(),
            ident: ident,
            bounds: punct(vec![]),
            colon_token: None,
            eq_token: None,
            default: None,
        })
    }

    pub fn ty<T>(self, kind: Type) -> Type {
        kind
    }

    pub fn lt_param<L>(self, lifetime: L) -> GenericParam
    where
        L: Make<Lifetime>,
    {
        let lifetime = lifetime.make(&self);
        GenericParam::Lifetime(LifetimeDef {
            attrs: self.attrs.into(),
            lifetime,
            colon_token: None,
            bounds: punct(vec![]),
        })
    }

    pub fn lifetime<L: Make<Lifetime>>(self, lt: L) -> Lifetime {
        lt.make(&self)
    }

    pub fn attribute<Pa, Ma>(self, style: AttrStyle, path: Pa, args: Ma) -> Attribute
    where
        Pa: Make<Path>,
        Ma: Make<TokenStream>,
    {
        let path = path.make(&self);
        let args = args.make(&self).into();
        Attribute {
            style,
            path,
            tokens: args,
            pound_token: token::Pound(self.span),
            bracket_token: token::Bracket(self.span),
        }
    }

    pub fn meta_item_attr(mut self, style: AttrStyle, meta_item: Meta) -> Self {
        let prepared = self.prepare_meta(meta_item);
        let attr = self
            .clone()
            .attribute(style, prepared.path, prepared.tokens);
        self.attrs.push(attr);
        self
    }

    pub fn meta_path<Pa>(self, path: Pa) -> Meta
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Meta::Path(path)
    }

    pub fn meta_list<I, N>(self, path: I, args: N) -> Meta
    where
        I: Make<Path>,
        N: Make<Vec<NestedMeta>>,
    {
        let path = path.make(&self);
        let args = args.make(&self);
        Meta::List(MetaList {
            path: path,
            paren_token: token::Paren(self.span),
            nested: punct(args),
        })
    }

    pub fn meta_namevalue<K, V>(self, key: K, value: V) -> Meta
    where
        K: Make<Path>,
        V: Make<Lit>,
    {
        let key = key.make(&self);
        let value = value.make(&self);

        Meta::NameValue(MetaNameValue {
            path: key,
            eq_token: token::Eq(self.span),
            lit: value,
        })
    }

    pub fn nested_meta_item<K>(self, kind: K) -> NestedMeta
    where
        K: Make<NestedMeta>,
    {
        kind.make(&self)
    }

    // Convert the current internal list of outer attributes
    // into a vector of inner attributes, e.g.:
    // `#[foo]` => `#![foo]`
    pub fn as_inner_attrs(self) -> Vec<Attribute> {
        self.attrs
            .into_iter()
            .map(|outer_attr| Attribute {
                style: AttrStyle::Inner(Default::default()),
                ..outer_attr
            })
            .collect::<Vec<Attribute>>()
    }

    pub fn into_attrs(self) -> Vec<Attribute> {
        self.attrs
    }

    pub fn empty_mac<Pa>(self, path: Pa, delim: MacroDelimiter) -> Macro
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Macro {
            path,
            tokens: TokenStream::new(),
            bang_token: token::Bang(self.span),
            delimiter: delim,
        }
    }

    pub fn mac<Pa, Ts>(self, func: Pa, arguments: Ts, delim: MacroDelimiter) -> Macro
    where
        Pa: Make<Path>,
        Ts: Make<TokenStream>,
    {
        let func: Path = func.make(&self);
        let tokens = arguments.make(&self);
        Macro {
            path: func,
            tokens,
            bang_token: token::Bang(self.span),
            delimiter: delim,
        }
    }

    /// Create a local variable
    pub fn local<V, T, E>(self, pat: V, ty: Option<T>, init: Option<E>) -> Local
    where
        V: Make<Box<Pat>>,
        T: Make<Box<Type>>,
        E: Make<Box<Expr>>,
    {
        let pat = pat.make(&self);
        let ty = ty.map(|x| x.make(&self));
        let init = init.map(|x| (Default::default(), x.make(&self)));
        let pat = if let Some(ty) = ty {
            Pat::Type(PatType {
                attrs: vec![],
                pat,
                colon_token: token::Colon(self.span),
                ty,
            })
        } else {
            *pat
        };
        Local {
            attrs: self.attrs.into(),
            let_token: token::Let(self.span),
            semi_token: token::Semi(self.span),
            pat,
            init,
        }
    }

    pub fn return_expr<E>(self, val: Option<E>) -> Box<Expr>
    where
        E: Make<Box<Expr>>,
    {
        let val = val.map(|x| x.make(&self));
        Box::new(Expr::Return(ExprReturn {
            attrs: self.attrs.into(),
            return_token: token::Return(self.span),
            expr: val,
        }))
    }

    pub fn continue_expr<I>(self, label: Option<I>) -> Box<Expr>
    where
        I: Make<Ident>,
    {
        let label = label.map(|l| Lifetime {
            ident: l.make(&self),
            apostrophe: self.span,
        });

        Box::new(Expr::Continue(ExprContinue {
            attrs: self.attrs.into(),
            continue_token: token::Continue(self.span),
            label,
        }))
    }

    pub fn break_expr<I>(self, label: Option<I>) -> Box<Expr>
    where
        I: Make<Ident>,
    {
        let label = label.map(|l| Lifetime {
            ident: l.make(&self),
            apostrophe: self.span,
        });

        Box::new(Expr::Break(ExprBreak {
            attrs: self.attrs.into(),
            break_token: token::Break(self.span),
            label,
            expr: None,
        }))
    }

    pub fn closure_expr<D, E>(
        self,
        capture: CaptureBy,
        mov: Movability,
        decl: D,
        body: E,
    ) -> Box<Expr>
    where
        D: Make<Box<FnDecl>>,
        E: Make<Box<Expr>>,
    {
        let decl = decl.make(&self);
        let body = body.make(&self);
        let (_name, inputs, _variadic, output) = *decl;
        let inputs = inputs
            .into_iter()
            .map(|e| match e {
                FnArg::Receiver(_s) => panic!("found 'self' in closure arguments"),
                FnArg::Typed(PatType { pat, .. }) => *pat,
            })
            .collect();
        let capture = match capture {
            CaptureBy::Ref => None,
            CaptureBy::Value => Some(Default::default()),
        };
        Box::new(Expr::Closure(ExprClosure {
            attrs: self.attrs.into(),
            or1_token: token::Or(self.span),
            or2_token: token::Or(self.span),
            capture,
            asyncness: IsAsync::NotAsync.to_token(),
            movability: mov.to_token(),
            body,
            inputs: inputs,
            output,
        }))
    }
}

pub fn mk() -> Builder {
    Builder::new()
}

/// Detect a cast that would create a syntax error when it was the left
/// argument to a less-than operator. This is a work-around for an upstream
/// libsyntax bug.
fn has_rightmost_cast(expr: &Expr) -> bool {
    match &expr {
        &Expr::Cast(..) => true,
        &Expr::Unary(ExprUnary {
            attrs: _,
            op: _,
            ref expr,
        }) => has_rightmost_cast(&**expr),
        &Expr::Binary(ExprBinary {
            attrs: _,
            left: _,
            op: _,
            ref right,
        }) => has_rightmost_cast(&**right),
        _ => false,
    }
}

fn expr_precedence(e: &Expr) -> u8 {
    match e {
        Expr::Path(_ep) => 18,
        Expr::MethodCall(_emc) => 17,
        Expr::Field(_ef) => 16,
        Expr::Call(_) | Expr::Index(_) => 15,
        Expr::Try(_et) => 14,
        Expr::Unary(_eu) => 13,
        Expr::Cast(_ec) => 12,
        Expr::Binary(eb) => 2 + binop_precedence(&eb.op),
        Expr::Assign(_) | Expr::AssignOp(_) => 1,
        Expr::Return(_) | Expr::Closure(_) => 0,
        _ => 255,
    }
}

/// See <https://doc.rust-lang.org/reference/expressions.html>
fn binop_precedence(b: &BinOp) -> u8 {
    match b {
        BinOp::Add(_) => 8,
        BinOp::Sub(_) => 8,
        BinOp::Mul(_) => 9,
        BinOp::Div(_) => 9,
        BinOp::Rem(_) => 9,
        BinOp::And(_) => 2,
        BinOp::Or(_) => 1,
        BinOp::BitXor(_) => 5,
        BinOp::BitAnd(_) => 6,
        BinOp::BitOr(_) => 4,
        BinOp::Shl(_) => 7,
        BinOp::Shr(_) => 7,
        BinOp::Eq(_) => 3,
        BinOp::Lt(_) => 3,
        BinOp::Le(_) => 3,
        BinOp::Ne(_) => 3,
        BinOp::Ge(_) => 3,
        BinOp::Gt(_) => 3,
        BinOp::AddEq(_) => 0,
        BinOp::SubEq(_) => 0,
        BinOp::MulEq(_) => 0,
        BinOp::DivEq(_) => 0,
        BinOp::RemEq(_) => 0,
        BinOp::BitXorEq(_) => 0,
        BinOp::BitAndEq(_) => 0,
        BinOp::BitOrEq(_) => 0,
        BinOp::ShlEq(_) => 0,
        BinOp::ShrEq(_) => 0,
    }
}

/// Wrap an expression in parentheses
fn parenthesize_mut(e: &mut Box<Expr>) {
    let mut temp = mk().tuple_expr(Vec::<Box<Expr>>::new());
    std::mem::swap(e, &mut temp);
    *e = Box::new(Expr::Paren(ExprParen {
        attrs: vec![],
        paren_token: Default::default(),
        expr: temp,
    }))
}

/// Wrap an expression's subexpressions in an explicit ExprParen if the
/// pretty-printed form of the expression would otherwise reparse differently
fn parenthesize_if_necessary(mut outer: Expr) -> Expr {
    // If outer operation has higher precedence, parenthesize inner operation
    let outer_precedence = expr_precedence(&outer);
    let parenthesize_if_gte = |inner: &mut Box<Expr>| {
        if expr_precedence(&*inner) <= outer_precedence {
            parenthesize_mut(inner);
        }
    };
    let parenthesize_if_gt = |inner: &mut Box<Expr>| {
        if expr_precedence(&*inner) < outer_precedence {
            parenthesize_mut(inner);
        }
    };
    match outer {
        Expr::Field(ref mut ef) => {
            if let Expr::Index(_) = *ef.base {
                /* we do not need to parenthesize the indexing in a[b].c */
            } else {
                /*if let Expr::Unary(_) = *ef.base {
                    parenthesize_mut(&mut ef.base);
                } else { */
                parenthesize_if_gt(&mut ef.base);
            }
        }
        Expr::MethodCall(ref mut emc) => {
            parenthesize_if_gt(&mut emc.receiver);
        }
        Expr::Call(ref mut ec) => {
            parenthesize_if_gt(&mut ec.func);
        }
        Expr::Cast(ref mut ec) => {
            if let Expr::If(_) = *ec.expr {
                parenthesize_mut(&mut ec.expr);
            } else {
                parenthesize_if_gt(&mut ec.expr);
            }
        }
        Expr::Unary(ref mut eu) => {
            parenthesize_if_gt(&mut eu.expr);
        }
        Expr::Binary(ref mut eb) => {
            parenthesize_if_gt(&mut eb.left);
            // Because binops associate right, parenthesize same-precedence RHS
            // (e.g. `5 - (6 - 7)`).
            parenthesize_if_gte(&mut eb.right);
        }
        Expr::Index(ref mut ei) => {
            parenthesize_if_gt(&mut ei.expr);
        }
        _ => (),
    };
    outer
}
