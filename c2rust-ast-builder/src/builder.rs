//! Helpers for building AST nodes.  Normally used by calling `mk().some_node(args...)`.

use std::str;

use itertools::intersperse;
use proc_macro2::{Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::default::Default;
use std::iter::FromIterator;
use syn::{__private::ToTokens, punctuated::Punctuated, *};

pub mod properties {
    use proc_macro2::Span;
    use syn::{StaticMutability, Token};

    pub trait ToToken {
        type Token;
        fn to_token(&self) -> Option<Self::Token>;
    }

    #[derive(Debug, Copy, Clone)]
    pub enum Mutability {
        Mutable,
        Immutable,
    }

    impl ToToken for Mutability {
        type Token = Token![mut];
        fn to_token(&self) -> Option<Self::Token> {
            match self {
                Mutability::Mutable => Some(Default::default()),
                Mutability::Immutable => None,
            }
        }
    }

    impl Mutability {
        pub fn to_static_mutability(&self, span: Span) -> StaticMutability {
            match self {
                Mutability::Mutable => StaticMutability::Mut(Token![mut](span)),
                Mutability::Immutable => StaticMutability::None,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Unsafety {
        Normal,
        Unsafe,
    }
    impl ToToken for Unsafety {
        type Token = Token![unsafe];
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
        type Token = Token![const];
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
        type Token = Token![static];
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
        type Token = Token![async];
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
        type Token = Token![default];
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
pub type BareFnTyParts = (Vec<BareFnArg>, Option<BareVariadic>, ReturnType);

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
    Punctuated::from_iter(x)
}

fn punct_box<T, P: Default>(x: Vec<Box<T>>) -> Punctuated<T, P> {
    Punctuated::from_iter(x.into_iter().map(|x| *x))
}

fn comma_separated<I, T>(items: I) -> TokenStream
where
    I: Iterator<Item = T>,
    T: ToTokens + Clone,
{
    let items = items.map(|items| items.to_token_stream());
    let comma = TokenTree::Punct(Punct::new(',', Spacing::Alone)).into_token_stream();
    intersperse(items, comma).collect()
}

pub trait Make<T> {
    fn make(self, mk: &Builder) -> T;
}

impl<T> Make<T> for T {
    fn make(self, _mk: &Builder) -> T {
        self
    }
}

impl Make<Ident> for &str {
    fn make(self, mk: &Builder) -> Ident {
        Ident::new(self, mk.span)
    }
}

impl Make<Ident> for String {
    fn make(self, mk: &Builder) -> Ident {
        Ident::new(&self, mk.span)
    }
}

impl Make<Ident> for &String {
    fn make(self, mk: &Builder) -> Ident {
        Ident::new(self, mk.span)
    }
}

impl<L: Make<Ident>> Make<Label> for L {
    fn make(self, mk: &Builder) -> Label {
        Label {
            name: Lifetime {
                apostrophe: mk.span,
                ident: self.make(mk),
            },
            colon_token: Token![:](mk.span),
        }
    }
}

impl Make<Path> for &str {
    fn make(self, mk: &Builder) -> Path {
        let v = vec![self];
        Make::<Path>::make(v, mk)
    }
}

impl Make<Abi> for &str {
    fn make(self, mk: &Builder) -> Abi {
        Abi {
            extern_token: Token![extern](mk.span),
            name: Some(LitStr::new(self, mk.span)),
        }
        // TODO: validate string: format!("unrecognized string for Abi: {:?}", self))
    }
}

impl Make<Extern> for &str {
    fn make(self, _mk: &Builder) -> Extern {
        Extern::Explicit(self.to_owned())
    }
}

impl Make<Extern> for Abi {
    fn make(self, _mk: &Builder) -> Extern {
        Extern::Explicit(self.name.to_token_stream().to_string())
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
            segments.push(s.make(mk));
        }
        Path {
            leading_colon: None,
            segments,
        }
    }
}

impl Make<TokenStream> for Vec<TokenTree> {
    fn make(self, _mk: &Builder) -> TokenStream {
        self.into_iter().collect::<TokenStream>()
    }
}

impl Make<TokenStream> for Vec<&str> {
    fn make(self, _mk: &Builder) -> TokenStream {
        comma_separated(self.iter().map(|&s| Ident::new(s, Span::call_site())))
    }
}

impl Make<TokenStream> for Vec<u64> {
    fn make(self, _mk: &Builder) -> TokenStream {
        comma_separated(self.iter().map(|&s| Literal::u64_unsuffixed(s)))
    }
}

impl Make<TokenStream> for Vec<Meta> {
    fn make(self, _mk: &Builder) -> TokenStream {
        comma_separated(self.iter())
    }
}

impl Make<PathArguments> for AngleBracketedGenericArguments {
    fn make(self, _mk: &Builder) -> PathArguments {
        PathArguments::AngleBracketed(self)
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

impl Make<Lit> for String {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Str(LitStr::new(&self, mk.span))
    }
}

impl Make<Lit> for &String {
    fn make(self, mk: &Builder) -> Lit {
        Lit::Str(LitStr::new(self, mk.span))
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
            fn_token: Token![fn](mk.span),
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

impl Make<String> for i32 {
    fn make(self, mk: &Builder) -> String {
        (self as i128).make(mk)
    }
}

impl Make<String> for i64 {
    fn make(self, mk: &Builder) -> String {
        (self as i128).make(mk)
    }
}

impl Make<String> for u64 {
    fn make(self, mk: &Builder) -> String {
        (self as u128).make(mk)
    }
}

impl Make<String> for i128 {
    fn make(self, _mk: &Builder) -> String {
        self.to_string()
    }
}

impl Make<String> for u128 {
    fn make(self, _mk: &Builder) -> String {
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

impl Default for Builder {
    fn default() -> Self {
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
}

impl Builder {
    pub fn new() -> Builder {
        Builder::default()
    }

    // Modifier updates.

    pub fn vis<V: Make<Visibility>>(self, vis: V) -> Self {
        let vis = vis.make(&self);
        Builder { vis, ..self }
    }

    pub fn pub_(self) -> Self {
        let pub_token = Token![pub](self.span);
        self.vis(Visibility::Public(pub_token))
    }

    pub fn set_mutbl<M: Make<Mutability>>(self, mutbl: M) -> Self {
        let mutbl = mutbl.make(&self);
        Builder { mutbl, ..self }
    }

    pub fn mutbl(self) -> Self {
        self.set_mutbl(Mutability::Mutable)
    }

    pub fn unsafety<U: Make<Unsafety>>(self, unsafety: U) -> Self {
        let unsafety = unsafety.make(&self);
        Builder { unsafety, ..self }
    }

    pub fn unsafe_(self) -> Self {
        self.unsafety(Unsafety::Unsafe)
    }

    pub fn constness<C: Make<Constness>>(self, constness: C) -> Self {
        let constness = constness.make(&self);
        Builder { constness, ..self }
    }

    pub fn const_(self) -> Self {
        self.constness(Constness::Const)
    }

    pub fn extern_<A: Make<Extern>>(self, ext: A) -> Self {
        let ext = ext.make(&self);
        Builder { ext, ..self }
    }

    pub fn span<S: Make<Span>>(self, span: S) -> Self {
        let span = span.make(&self);
        Builder { span, ..self }
    }

    pub fn generic_over(mut self, param: GenericParam) -> Self {
        self.generics.params.push(param);
        self
    }

    pub fn prepared_attr(self, meta: Meta) -> Self {
        let attr = self.clone().attribute(AttrStyle::Outer, meta);
        let mut attrs = self.attrs;
        attrs.push(attr);
        Builder { attrs, ..self }
    }

    pub fn str_attr<K, V>(self, key: K, value: V) -> Self
    where
        K: Make<Path>,
        V: Make<Lit>,
    {
        let meta = mk().meta_namevalue(key, value);
        self.prepared_attr(meta)
    }

    pub fn single_attr<K>(self, key: K) -> Self
    where
        K: Make<Path>,
    {
        let meta = mk().meta_path(key);
        self.prepared_attr(meta)
    }

    pub fn call_attr<K, V>(self, func: K, arguments: V) -> Self
    where
        K: Make<Path>,
        V: Make<TokenStream>,
    {
        let meta = mk().meta_list(func, arguments);
        self.prepared_attr(meta)
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

    pub fn angle_bracketed_args<A>(self, args: Vec<A>) -> AngleBracketedGenericArguments
    where
        A: Make<GenericArgument>,
    {
        let args = args.into_iter().map(|arg| arg.make(&self)).collect();
        AngleBracketedGenericArguments {
            colon2_token: Some(Token![::](self.span)), // Always include a colon2 for turbofish
            lt_token: Token![<](self.span),
            args,
            gt_token: Token![>](self.span),
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

    pub fn use_tree<Pa>(self, prefix: Pa, mut tree: UseTree) -> UseTree
    where
        Pa: Make<Path>,
    {
        let path: Path = prefix.make(&self);
        for seg in path.segments {
            tree = UseTree::Path(UsePath {
                ident: seg.ident,
                colon2_token: Token![::](self.span),
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
        path.leading_colon = Some(Token![::](self.span));
        path
    }

    // Exprs
    // These are sorted in the same order as the corresponding ExprKind variants, with additional
    // variant-specific details following each variant.

    pub fn array_expr(self, args: Vec<Box<Expr>>) -> Box<Expr> {
        let args = args.into_iter().map(|a| *a).collect();
        Box::new(Expr::Array(ExprArray {
            attrs: self.attrs,
            bracket_token: token::Bracket(self.span),
            elems: args,
        }))
    }

    pub fn call_expr(self, func: Box<Expr>, args: Vec<Box<Expr>>) -> Box<Expr> {
        let args = args.into_iter().map(|a| *a).collect();
        Box::new(parenthesize_if_necessary(Expr::Call(ExprCall {
            attrs: self.attrs,
            paren_token: token::Paren(self.span),
            func,
            args,
        })))
    }

    pub fn method_call_expr<S>(self, expr: Box<Expr>, seg: S, args: Vec<Box<Expr>>) -> Box<Expr>
    where
        S: Make<PathSegment>,
    {
        let seg = seg.make(&self);
        let args = args
            .into_iter()
            .map(|arg| *arg)
            .map(|arg| arg.make(&self))
            .collect();

        let turbofish = match seg.arguments {
            PathArguments::None => None,
            PathArguments::AngleBracketed(ab) => Some(ab),
            PathArguments::Parenthesized(_) => {
                panic!("Found parenthesized arguments on path segment for method call")
            }
        };

        Box::new(parenthesize_if_necessary(Expr::MethodCall(
            ExprMethodCall {
                attrs: self.attrs,
                dot_token: Token![.](self.span),
                paren_token: token::Paren(self.span),
                turbofish,
                receiver: expr,
                method: seg.ident,
                args,
            },
        )))
    }

    pub fn tuple_expr(self, exprs: Vec<Box<Expr>>) -> Box<Expr> {
        Box::new(Expr::Tuple(ExprTuple {
            attrs: self.attrs,
            paren_token: token::Paren(self.span),
            elems: punct_box(exprs),
        }))
    }

    pub fn binary_expr(self, op: BinOp, mut lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        match op {
            BinOp::Lt(_) | BinOp::Shl(_) if has_rightmost_cast(&lhs) => lhs = mk().paren_expr(lhs),
            _ => {}
        }

        Box::new(parenthesize_if_necessary(Expr::Binary(ExprBinary {
            attrs: self.attrs,
            left: lhs,
            op,
            right: rhs,
        })))
    }

    pub fn unary_expr<O>(self, op: O, a: Box<Expr>) -> Box<Expr>
    where
        O: Make<UnOp>,
    {
        let op = op.make(&self);
        // FIXME: set span for op
        Box::new(parenthesize_if_necessary(Expr::Unary(ExprUnary {
            attrs: self.attrs,
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
            attrs: self.attrs,
            lit,
        }))
    }

    pub fn cast_expr(self, e: Box<Expr>, t: Box<Type>) -> Box<Expr> {
        Box::new(parenthesize_if_necessary(Expr::Cast(ExprCast {
            attrs: self.attrs,
            as_token: Token![as](self.span),
            expr: e,
            ty: t,
        })))
    }

    pub fn unsafe_block_expr(self, unsafe_blk: ExprUnsafe) -> Box<Expr> {
        Box::new(Expr::Unsafe(unsafe_blk))
    }

    pub fn block_expr(self, block: Block) -> Box<Expr> {
        Box::new(Expr::Block(ExprBlock {
            attrs: self.attrs,
            block,
            label: None,
        }))
    }

    pub fn labelled_block_expr<L>(self, block: Block, lbl: L) -> Box<Expr>
    where
        L: Make<Label>,
    {
        let lbl = lbl.make(&self);
        Box::new(Expr::Block(ExprBlock {
            attrs: self.attrs,
            block,
            label: Some(lbl),
        }))
    }

    pub fn assign_expr(self, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Assign(ExprAssign {
            attrs: self.attrs,
            eq_token: Token![=](self.span),
            left: lhs,
            right: rhs,
        }))
    }

    pub fn assign_op_expr(self, op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Binary(ExprBinary {
            attrs: self.attrs,
            op,
            left: lhs,
            right: rhs,
        }))
    }

    pub fn index_expr(self, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        Box::new(parenthesize_if_necessary(Expr::Index(ExprIndex {
            attrs: self.attrs,
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
            attrs: self.attrs,
            qself,
            path,
        }))
    }

    /// An array literal constructed from one repeated element.
    /// `[expr; n]`
    pub fn repeat_expr(self, expr: Box<Expr>, n: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Repeat(ExprRepeat {
            attrs: self.attrs,
            bracket_token: token::Bracket(self.span),
            semi_token: Token![;](self.span),
            expr,
            len: n,
        }))
    }

    pub fn paren_expr(self, e: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Paren(ExprParen {
            attrs: self.attrs,
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

    pub fn addr_of_expr(self, e: Box<Expr>) -> Box<Expr> {
        Box::new(parenthesize_if_necessary(Expr::Reference(ExprReference {
            attrs: self.attrs,
            and_token: Token![&](self.span),
            mutability: self.mutbl.to_token(),
            expr: e,
        })))
    }

    pub fn mac_expr(self, mac: Macro) -> Box<Expr> {
        Box::new(Expr::Macro(ExprMacro {
            attrs: self.attrs,
            mac,
        }))
    }

    pub fn struct_expr<Pa>(self, path: Pa, fields: Vec<FieldValue>) -> Box<Expr>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Box::new(Expr::Struct(ExprStruct {
            attrs: self.attrs,
            qself: None,
            brace_token: token::Brace(self.span),
            dot2_token: None,
            path,
            fields: punct(fields),
            rest: None,
        }))
    }

    // struct_expr, but with optional base expression
    pub fn struct_expr_base<Pa>(
        self,
        path: Pa,
        fields: Vec<FieldValue>,
        base: Option<Box<Expr>>,
    ) -> Box<Expr>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Box::new(Expr::Struct(ExprStruct {
            attrs: self.attrs,
            qself: None,
            brace_token: token::Brace(self.span),
            dot2_token: Some(Token![..](self.span)),
            path,
            fields: punct(fields),
            rest: base,
        }))
    }

    pub fn field_expr<F>(self, val: Box<Expr>, field: F) -> Box<Expr>
    where
        F: Make<Ident>,
    {
        let field = field.make(&self);
        Box::new(parenthesize_if_necessary(Expr::Field(ExprField {
            attrs: self.attrs,
            dot_token: Token![.](self.span),
            base: val,
            member: Member::Named(field),
        })))
    }

    pub fn anon_field_expr(self, val: Box<Expr>, field: u32) -> Box<Expr> {
        Box::new(parenthesize_if_necessary(Expr::Field(ExprField {
            attrs: self.attrs,
            dot_token: Token![.](self.span),
            base: val,
            member: Member::Unnamed(Index {
                index: field,
                span: self.span,
            }),
        })))
    }

    pub fn field<I>(self, ident: I, expr: Box<Expr>) -> FieldValue
    where
        I: Make<Ident>,
    {
        let ident = ident.make(&self);
        FieldValue {
            member: Member::Named(ident),
            expr: *expr,
            colon_token: Some(Token![:](self.span)),
            attrs: self.attrs,
        }
    }

    pub fn match_expr(self, cond: Box<Expr>, arms: Vec<Arm>) -> Box<Expr> {
        let arms = arms.into_iter().collect();
        Box::new(Expr::Match(ExprMatch {
            attrs: self.attrs,
            match_token: Token![match](self.span),
            brace_token: token::Brace(self.span),
            expr: cond,
            arms,
        }))
    }

    pub fn arm(self, pat: Pat, guard: Option<Box<Expr>>, body: Box<Expr>) -> Arm {
        let guard = guard.map(|g| (Token![if](self.span), g));
        Arm {
            attrs: self.attrs,
            pat,
            guard,
            body,
            fat_arrow_token: Token![=>](self.span),
            comma: Some(Token![,](self.span)),
        }
    }

    // Literals

    pub fn int_lit(self, i: u128, ty: &str) -> Lit {
        Lit::Int(LitInt::new(&format!("{}{}", i, ty), self.span))
    }

    pub fn int_unsuffixed_lit<S>(self, s: S) -> Lit
    where
        S: Make<String>,
    {
        let s = s.make(&self);
        Lit::Int(LitInt::new(&s, self.span))
    }

    pub fn float_lit(self, s: &str, ty: &str) -> Lit {
        Lit::Float(LitFloat::new(&format!("{}{}", s, ty), self.span))
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

    pub fn str_lit(self, s: &str) -> Lit {
        Lit::Str(LitStr::new(s, self.span))
    }

    pub fn ifte_expr(
        self,
        cond: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Box<Expr>>,
    ) -> Box<Expr> {
        let else_branch = else_branch.map(|e| {
            // The else branch in libsyntax must be one of these three cases,
            // otherwise we have to manually add the block around the else expression
            (
                Token![else](self.span),
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
            attrs: self.attrs,
            if_token: Token![if](self.span),
            cond,
            then_branch,
            else_branch,
        }))
    }

    pub fn while_expr<I>(self, cond: Box<Expr>, body: Block, label: Option<I>) -> Box<Expr>
    where
        I: Make<Ident>,
    {
        let label = label.map(|l| Label {
            name: Lifetime {
                ident: l.make(&self),
                apostrophe: self.span,
            },
            colon_token: Token![:](self.span),
        });

        Box::new(Expr::While(ExprWhile {
            attrs: self.attrs,
            while_token: Token![while](self.span),
            cond,
            body,
            label,
        }))
    }

    pub fn loop_expr<I>(self, body: Block, label: Option<I>) -> Box<Expr>
    where
        I: Make<Ident>,
    {
        let label = label.map(|l| Label {
            name: Lifetime {
                ident: l.make(&self),
                apostrophe: self.span,
            },
            colon_token: Token![:](self.span),
        });

        Box::new(Expr::Loop(ExprLoop {
            attrs: self.attrs,
            loop_token: Token![loop](self.span),
            body,
            label,
        }))
    }

    pub fn for_expr<I>(self, pat: Pat, expr: Box<Expr>, body: Block, label: Option<I>) -> Box<Expr>
    where
        I: Make<Ident>,
    {
        let label = label.map(|l| Label {
            name: Lifetime {
                ident: l.make(&self),
                apostrophe: self.span,
            },
            colon_token: Token![:](self.span),
        });

        Box::new(Expr::ForLoop(ExprForLoop {
            attrs: self.attrs,
            for_token: Token![for](self.span),
            in_token: Token![in](self.span),
            pat: Box::new(pat),
            expr,
            body,
            label,
        }))
    }

    // Patterns

    pub fn ident_pat<I>(self, name: I) -> Pat
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Pat::Ident(PatIdent {
            attrs: self.attrs,
            mutability: self.mutbl.to_token(),
            by_ref: None,
            ident: name,
            subpat: None,
        })
    }

    pub fn tuple_pat(self, pats: Vec<Pat>) -> Pat {
        Pat::Tuple(PatTuple {
            attrs: self.attrs,
            paren_token: token::Paren(self.span),
            elems: punct(pats),
        })
    }

    pub fn qpath_pat<Pa>(self, qself: Option<QSelf>, path: Pa) -> Box<Pat>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Box::new(Pat::Path(PatPath {
            attrs: self.attrs,
            qself,
            path,
        }))
    }

    pub fn wild_pat(self) -> Pat {
        Pat::Wild(PatWild {
            attrs: self.attrs,
            underscore_token: Token![_](self.span),
        })
    }

    pub fn lit_pat(self, lit: Lit) -> Pat {
        Pat::Lit(PatLit {
            attrs: self.attrs,
            lit,
        })
    }

    pub fn path_pat(self, path: Path, qself: Option<QSelf>) -> Pat {
        Pat::Path(PatPath {
            attrs: self.attrs,
            qself,
            path,
        })
    }

    pub fn mac_pat(self, mac: Macro) -> Pat {
        Pat::Macro(PatMacro {
            attrs: self.attrs,
            mac,
        })
    }

    pub fn ident_ref_pat<I>(self, name: I) -> Pat
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Pat::Ident(PatIdent {
            attrs: self.attrs,
            by_ref: Some(Token![ref](self.span)),
            mutability: self.mutbl.to_token(),
            ident: name,
            subpat: None,
        })
    }

    pub fn or_pat(self, pats: Vec<Pat>) -> Pat {
        Pat::Or(PatOr {
            attrs: self.attrs,
            leading_vert: None, // Untested
            cases: punct(pats),
        })
    }

    // Types

    pub fn barefn_ty(self, decl: BareFnTyParts) -> Box<Type> {
        let (inputs, variadic, output) = decl;
        let abi = self.get_abi_opt();

        let barefn = TypeBareFn {
            fn_token: Token![fn](self.span),
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

    pub fn array_ty(self, ty: Box<Type>, len: Box<Expr>) -> Box<Type> {
        Box::new(Type::Array(TypeArray {
            bracket_token: token::Bracket(self.span),
            semi_token: Token![;](self.span),
            elem: ty,
            len: *len,
        }))
    }

    pub fn slice_ty(self, ty: Box<Type>) -> Box<Type> {
        Box::new(Type::Slice(TypeSlice {
            elem: ty,
            bracket_token: token::Bracket(self.span),
        }))
    }

    pub fn ptr_ty(self, ty: Box<Type>) -> Box<Type> {
        let const_token = if self.mutbl.to_token().is_none() {
            Some(Token![const](self.span))
        } else {
            None
        };
        Box::new(Type::Ptr(TypePtr {
            elem: ty,
            mutability: self.mutbl.to_token(),
            const_token,
            star_token: Token![*](self.span),
        }))
    }

    pub fn ref_ty(self, ty: Box<Type>) -> Box<Type> {
        Box::new(Type::Reference(TypeReference {
            lifetime: None,
            elem: ty,
            mutability: self.mutbl.to_token(),
            and_token: Token![&](self.span),
        }))
    }

    pub fn ref_lt_ty<L>(self, lt: L, ty: Box<Type>) -> Box<Type>
    where
        L: Make<Lifetime>,
    {
        let lt = lt.make(&self);
        Box::new(Type::Reference(TypeReference {
            and_token: Token![&](self.span),
            lifetime: Some(lt),
            mutability: self.mutbl.to_token(),
            elem: ty,
        }))
    }

    pub fn never_ty(self) -> Box<Type> {
        Box::new(Type::Never(TypeNever {
            bang_token: Token![!](self.span),
        }))
    }

    pub fn tuple_ty(self, elem_tys: Vec<Box<Type>>) -> Box<Type> {
        let elem_tys = elem_tys.into_iter().map(|ty| *ty).collect();
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
            underscore_token: Token![_](self.span),
        }))
    }

    pub fn mac_ty(self, mac: Macro) -> Box<Type> {
        Box::new(Type::Macro(TypeMacro { mac }))
    }

    // Stmts

    pub fn local_stmt(self, local: Box<Local>) -> Stmt {
        Stmt::Local(*local)
    }

    pub fn expr_stmt(self, expr: Box<Expr>) -> Stmt {
        Stmt::Expr(*expr, None)
    }

    pub fn semi_stmt(self, expr: Box<Expr>) -> Stmt {
        Stmt::Expr(*expr, Some(Token![;](self.span)))
    }

    pub fn item_stmt(self, item: Box<Item>) -> Stmt {
        Stmt::Item(*item)
    }

    pub fn mac_stmt(self, mac: Macro) -> Stmt {
        self.semi_stmt(mk().mac_expr(mac))
    }

    // Items

    pub fn static_item<I>(self, name: I, ty: Box<Type>, init: Box<Expr>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(Item::Static(ItemStatic {
            attrs: self.attrs,
            vis: self.vis,
            mutability: self.mutbl.to_static_mutability(self.span),
            ident: name,
            static_token: Token![static](self.span),
            colon_token: Token![:](self.span),
            eq_token: Token![=](self.span),
            semi_token: Token![;](self.span),
            expr: init,
            ty,
        }))
    }

    pub fn const_item<I>(self, name: I, ty: Box<Type>, init: Box<Expr>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(Item::Const(ItemConst {
            attrs: self.attrs,
            vis: self.vis,
            const_token: Token![const](self.span),
            generics: self.generics,
            colon_token: Token![:](self.span),
            eq_token: Token![=](self.span),
            semi_token: Token![;](self.span),
            ident: name,
            ty,
            expr: init,
        }))
    }

    pub fn fn_item<S>(self, sig: S, block: Block) -> Box<Item>
    where
        S: Make<Signature>,
    {
        let sig = sig.make(&self);
        Box::new(Item::Fn(ItemFn {
            attrs: self.attrs,
            vis: self.vis,
            sig,
            block: Box::new(block),
        }))
    }

    pub fn variadic_arg(self, name: Option<String>) -> Variadic {
        let pat = if let Some(name) = name {
            let pat = Box::new(self.clone().ident_pat(name));
            Some((pat, Token![:](self.span)))
        } else {
            None
        };

        Variadic {
            dots: Token![...](self.span),
            attrs: self.attrs,
            pat,
            comma: None,
        }
    }

    pub fn bare_variadic_arg(self) -> BareVariadic {
        BareVariadic {
            attrs: self.attrs,
            name: None,
            dots: Token![...](self.span),
            comma: None,
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
            struct_token: Token![struct](self.span),
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
            union_token: Token![union](self.span),
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
            enum_token: Token![enum](self.span),
            brace_token: token::Brace(self.span),
            variants: punct(fields),
            generics: self.generics,
        }))
    }

    pub fn type_item<I>(self, name: I, ty: Box<Type>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(Item::Type(ItemType {
            attrs: self.attrs,
            vis: self.vis,
            ident: name,
            generics: self.generics,
            type_token: Token![type](self.span),
            eq_token: Token![=](self.span),
            semi_token: Token![;](self.span),
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
            unsafety: self.unsafety.to_token(),
            ident: name,
            mod_token: Token![mod](self.span),
            semi: None,
            content: items,
        }))
    }

    pub fn mod_(self, items: Vec<Box<Item>>) -> Vec<Item> {
        items.into_iter().map(|i| *i).collect()
    }

    pub fn mac_item(self, mac: Macro) -> Box<Item> {
        Box::new(Item::Macro(ItemMacro {
            attrs: self.attrs,
            semi_token: Some(Token![;](self.span)), // Untested
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

    pub fn unit_variant<I>(self, name: I, disc: Option<Box<Expr>>) -> Variant
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Variant {
            ident: name,
            fields: Fields::Unit,
            discriminant: disc.map(|e| (Token![=](self.span), *e)),
            attrs: self.attrs,
        }
    }

    pub fn impl_item(self, ty: Box<Type>, items: Vec<ImplItem>) -> Box<Item> {
        Box::new(Item::Impl(ItemImpl {
            attrs: self.attrs,
            unsafety: self.unsafety.to_token(),
            defaultness: Defaultness::Final.to_token(),
            generics: self.generics,
            trait_: None, // not a trait implementation, no ! on said trait name
            self_ty: ty,
            impl_token: Token![impl](self.span),
            brace_token: token::Brace(self.span),
            items,
        }))
    }

    pub fn extern_crate_item<I>(self, name: I, rename: Option<I>) -> Box<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        let rename = rename.map(|n| (Token![as](self.span), n.make(&self)));
        Box::new(Item::ExternCrate(ItemExternCrate {
            attrs: self.attrs,
            vis: self.vis,
            crate_token: Token![crate](self.span),
            extern_token: Token![extern](self.span),
            semi_token: Token![;](self.span),
            ident: name,
            rename,
        }))
    }

    pub fn use_item(self, tree: UseTree) -> Box<Item> {
        Box::new(Item::Use(ItemUse {
            attrs: self.attrs,
            vis: self.vis,
            use_token: Token![use](self.span),
            leading_colon: None,
            semi_token: Token![;](self.span),
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
                    as_token: Token![as](self.span),
                    rename,
                }),
            )
        } else {
            use_tree_with_prefix(prefix, UseTree::Name(UseName { ident }))
        };
        Box::new(Item::Use(ItemUse {
            attrs: self.attrs,
            vis: self.vis,
            use_token: Token![use](self.span),
            leading_colon,
            semi_token: Token![;](self.span),
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
            use_token: Token![use](self.span),
            leading_colon,
            semi_token: Token![;](self.span),
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
                star_token: Token![*](self.span),
            }),
        );
        Box::new(Item::Use(ItemUse {
            attrs: self.attrs,
            vis: self.vis,
            use_token: Token![use](self.span),
            leading_colon,
            semi_token: Token![;](self.span),
            tree,
        }))
    }

    pub fn foreign_items(self, items: Vec<ForeignItem>) -> Box<Item> {
        let abi = self.get_abi();

        Box::new(Item::ForeignMod(ItemForeignMod {
            attrs: self.attrs,
            unsafety: None,
            brace_token: token::Brace(self.span),
            items,
            abi,
        }))
    }

    pub fn get_abi(&self) -> Abi {
        Abi {
            extern_token: Token![extern](self.span),
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
            extern_token: Token![extern](self.span),
            name,
        })
    }

    // Impl Items

    pub fn mac_impl_item(self, mac: Macro) -> ImplItem {
        ImplItem::Macro(ImplItemMacro {
            attrs: self.attrs,
            semi_token: None,
            mac,
        })
    }

    // Trait Items

    pub fn mac_trait_item(self, mac: Macro) -> TraitItem {
        TraitItem::Macro(TraitItemMacro {
            attrs: self.attrs,
            semi_token: None,
            mac,
        })
    }

    // Foreign Items

    /// [`ForeignItem`] is large (472 bytes), so [`Box`] it.
    pub fn fn_foreign_item(self, decl: Box<FnDecl>) -> Box<ForeignItem> {
        let sig = Signature {
            constness: None,
            asyncness: None,
            generics: self.generics.clone(),
            ..decl.make(&self)
        };
        Box::new(ForeignItem::Fn(ForeignItemFn {
            attrs: self.attrs,
            vis: self.vis,
            sig,
            semi_token: Token![;](self.span),
        }))
    }

    /// [`ForeignItem`] is large (472 bytes), so [`Box`] it.
    pub fn static_foreign_item<I>(self, name: I, ty: Box<Type>) -> Box<ForeignItem>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(ForeignItem::Static(ForeignItemStatic {
            attrs: self.attrs,
            vis: self.vis,
            mutability: self.mutbl.to_static_mutability(self.span),
            ident: name,
            ty,
            static_token: Token![static](self.span),
            colon_token: Token![:](self.span),
            semi_token: Token![;](self.span),
        }))
    }

    /// [`ForeignItem`] is large (472 bytes), so [`Box`] it.
    pub fn ty_foreign_item<I>(self, name: I) -> Box<ForeignItem>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Box::new(ForeignItem::Type(ForeignItemType {
            attrs: self.attrs,
            vis: self.vis,
            generics: self.generics,
            ident: name,
            type_token: Token![type](self.span),
            semi_token: Token![;](self.span),
        }))
    }

    pub fn mac_foreign_item(self, mac: Macro) -> ForeignItem {
        ForeignItem::Macro(ForeignItemMacro {
            attrs: self.attrs,
            mac,
            semi_token: None,
        })
    }

    // struct fields

    pub fn struct_field<I>(self, ident: I, ty: Box<Type>) -> Field
    where
        I: Make<Ident>,
    {
        let ident = ident.make(&self);
        Field {
            ident: Some(ident),
            vis: self.vis,
            mutability: FieldMutability::None,
            attrs: self.attrs,
            ty: *ty,
            colon_token: Some(Token![:](self.span)),
        }
    }

    pub fn enum_field(self, ty: Box<Type>) -> Field {
        Field {
            ident: None,
            vis: self.vis,
            mutability: FieldMutability::None,
            ty: *ty,
            attrs: self.attrs,
            colon_token: None,
        }
    }

    // Misc nodes

    pub fn unsafe_block(self, stmts: Vec<Stmt>) -> ExprUnsafe {
        let blk = Block {
            stmts,
            brace_token: token::Brace(self.span),
        };
        ExprUnsafe {
            attrs: self.attrs,
            unsafe_token: Token![unsafe](self.span),
            block: blk,
        }
    }

    pub fn block(self, stmts: Vec<Stmt>) -> Block {
        Block {
            stmts,
            brace_token: token::Brace(self.span),
        }
    }

    pub fn label<L>(self, lbl: L) -> Label
    where
        L: Make<Label>,
    {
        lbl.make(&self)
    }

    pub fn break_expr_value<L>(self, label: Option<L>, value: Option<Box<Expr>>) -> Box<Expr>
    where
        L: Make<Label>,
    {
        let label = label.map(|l| l.make(&self).name);
        Box::new(Expr::Break(ExprBreak {
            attrs: self.attrs,
            break_token: Token![break](self.span),
            label,
            expr: value,
        }))
    }

    pub fn bare_arg<I>(self, ty: Box<Type>, name: Option<I>) -> BareFnArg
    where
        I: Make<Box<Ident>>,
    {
        let name = name.map(|n| (*n.make(&self), Token![:](self.span)));
        BareFnArg {
            attrs: Vec::new(),
            name,
            ty: *ty,
        }
    }

    pub fn arg(self, ty: Box<Type>, pat: Pat) -> FnArg {
        FnArg::Typed(PatType {
            attrs: Vec::new(),
            ty,
            pat: Box::new(pat),
            colon_token: Token![:](self.span),
        })
    }

    pub fn self_arg(self, kind: SelfKind) -> FnArg {
        let (reference, mutability) = match kind {
            SelfKind::Value(mutability) => (None, mutability),
            SelfKind::Region(lt, mutability) => {
                (Some((Token![&](self.span), Some(lt))), mutability)
            }
        };
        let ty = mk().path_ty("Self");
        let attrs = Vec::new();
        FnArg::Receiver(Receiver {
            attrs,
            reference,
            mutability: mutability.to_token(),
            self_token: Token![self](self.span),
            colon_token: None,
            ty,
        })
    }

    pub fn ty_param<I>(self, ident: I) -> GenericParam
    where
        I: Make<Ident>,
    {
        let ident = ident.make(&self);
        GenericParam::Type(TypeParam {
            attrs: self.attrs,
            ident,
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
        GenericParam::Lifetime(LifetimeParam {
            attrs: self.attrs,
            lifetime,
            colon_token: None,
            bounds: punct(vec![]),
        })
    }

    pub fn lifetime<L: Make<Lifetime>>(self, lt: L) -> Lifetime {
        lt.make(&self)
    }

    pub fn attribute(self, style: AttrStyle, meta: Meta) -> Attribute {
        Attribute {
            style,
            pound_token: Token![#](self.span),
            bracket_token: token::Bracket(self.span),
            meta,
        }
    }

    /// makes a meta item with just a path
    /// # Examples
    ///
    /// mk().meta_path("C") // ->  `C`
    pub fn meta_path<Pa>(self, path: Pa) -> Meta
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Meta::Path(path)
    }

    /// makes a meta item with the given path and some arguments
    /// # Examples
    ///
    /// mk().meta_list("derive", vec!["Clone", "Copy"]) // ->  `derive(Clone, Copy)`
    pub fn meta_list<I, N>(self, path: I, args: N) -> Meta
    where
        I: Make<Path>,
        N: Make<TokenStream>,
    {
        let path = path.make(&self);
        let args = args.make(&self);
        Meta::List(MetaList {
            path,
            delimiter: MacroDelimiter::Paren(token::Paren(self.span)),
            tokens: args,
        })
    }

    /// makes a meta item with key value argument
    /// # Examples
    ///
    /// mk().meta_namevalue("target_os", "linux") // ->  `target_os = "linux"`
    pub fn meta_namevalue<K, V>(self, key: K, value: V) -> Meta
    where
        K: Make<Path>,
        V: Make<Lit>,
    {
        let key = key.make(&self);
        let lit = value.make(&self);
        let value = Expr::Lit(ExprLit {
            attrs: self.attrs,
            lit,
        });

        Meta::NameValue(MetaNameValue {
            path: key,
            eq_token: Token![=](self.span),
            value,
        })
    }

    pub fn empty_mac<Pa>(self, path: Pa, delim: MacroDelimiter) -> Macro
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        Macro {
            path,
            tokens: TokenStream::new(),
            bang_token: Token![!](self.span),
            delimiter: delim,
        }
    }

    pub fn mac<Ts>(self, func: Path, arguments: Ts, delim: MacroDelimiter) -> Macro
    where
        Ts: Make<TokenStream>,
    {
        let tokens = arguments.make(&self);
        Macro {
            path: func,
            tokens,
            bang_token: Token![!](self.span),
            delimiter: delim,
        }
    }

    /// Create a local variable
    pub fn local(self, pat: Pat, ty: Option<Box<Type>>, init: Option<Box<Expr>>) -> Local {
        let init = init.map(|x| LocalInit {
            eq_token: Token![=](self.span),
            expr: x,
            diverge: None,
        });

        let pat = if let Some(ty) = ty {
            Pat::Type(PatType {
                attrs: vec![],
                pat: Box::new(pat),
                colon_token: Token![:](self.span),
                ty,
            })
        } else {
            pat
        };
        Local {
            attrs: self.attrs,
            let_token: Token![let](self.span),
            semi_token: Token![;](self.span),
            pat,
            init,
        }
    }

    pub fn return_expr(self, val: Option<Box<Expr>>) -> Box<Expr> {
        Box::new(Expr::Return(ExprReturn {
            attrs: self.attrs,
            return_token: Token![return](self.span),
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
            attrs: self.attrs,
            continue_token: Token![continue](self.span),
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
            attrs: self.attrs,
            break_token: Token![break](self.span),
            label,
            expr: None,
        }))
    }

    pub fn closure_expr(
        self,
        capture: CaptureBy,
        mov: Movability,
        decl: FnDecl,
        body: Box<Expr>,
    ) -> Box<Expr> {
        let (_name, inputs, _variadic, output) = decl;
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
            attrs: self.attrs,
            lifetimes: None,
            constness: None,
            or1_token: Token![|](self.span),
            or2_token: Token![|](self.span),
            capture,
            asyncness: IsAsync::NotAsync.to_token(),
            movability: mov.to_token(),
            body,
            inputs,
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
    match expr {
        Expr::Cast(..) => true,
        Expr::Unary(ExprUnary {
            attrs: _,
            op: _,
            ref expr,
        }) => has_rightmost_cast(expr),
        Expr::Binary(ExprBinary {
            attrs: _,
            left: _,
            op: _,
            ref right,
        }) => has_rightmost_cast(right),
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
        Expr::Unary(_) | Expr::Reference(_) => 13,
        Expr::Cast(_ec) => 12,
        Expr::Binary(eb) => 2 + binop_precedence(&eb.op),
        Expr::Assign(_) => 1,
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
        BinOp::AddAssign(_) => 0,
        BinOp::SubAssign(_) => 0,
        BinOp::MulAssign(_) => 0,
        BinOp::DivAssign(_) => 0,
        BinOp::RemAssign(_) => 0,
        BinOp::BitXorAssign(_) => 0,
        BinOp::BitAndAssign(_) => 0,
        BinOp::BitOrAssign(_) => 0,
        BinOp::ShlAssign(_) => 0,
        BinOp::ShrAssign(_) => 0,
        _ => panic!("mising binop"),
    }
}

/// Wrap an expression in parentheses
fn parenthesize_mut(e: &mut Box<Expr>) {
    let mut temp = mk().tuple_expr(Vec::new());
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
        Expr::Reference(ref mut er) => {
            parenthesize_if_gt(&mut er.expr);
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
