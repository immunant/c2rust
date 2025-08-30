//! Helpers for building AST nodes.  Normally used by calling `mk().some_node(args...)`.
//!
//! This is a fork of c2rust-ast-builder, since that one switched to
//! the syn crate while this copy uses the old internal rustc AST.
use rustc_ast::attr::mk_attr_inner;
use rustc_ast::ptr::P;
use rustc_ast::token::{Token, TokenKind};
use rustc_ast::tokenstream::{DelimSpan, Spacing, TokenStream, TokenStreamBuilder, TokenTree};
use rustc_ast::*;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_middle::ty;
use rustc_span::source_map::{dummy_spanned, Spanned};
use rustc_span::symbol::Ident;
use rustc_span::{Span, DUMMY_SP};
use rustc_target::spec::abi::{self, Abi};
use std::rc::Rc;
use std::str;

use super::IntoSymbol;

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

impl<S: IntoSymbol> Make<Ident> for S {
    fn make(self, _mk: &Builder) -> Ident {
        Ident::with_dummy_span(self.into_symbol())
    }
}

impl<L: Make<Ident>> Make<Label> for L {
    fn make(self, mk: &Builder) -> Label {
        Label {
            ident: self.make(mk),
        }
    }
}

impl<'a> Make<Path> for &'a str {
    fn make(self, mk: &Builder) -> Path {
        vec![self].make(mk)
    }
}

impl<'a> Make<Visibility> for &'a str {
    fn make(self, _mk: &Builder) -> Visibility {
        let kind = match self {
            "pub" => VisibilityKind::Public,
            "priv" | "" | "inherit" => VisibilityKind::Inherited,
            "pub(crate)" => VisibilityKind::Restricted {
                path: P(mk().path("crate")),
                id: DUMMY_NODE_ID,
            },
            "pub(super)" => VisibilityKind::Restricted {
                path: P(mk().path("super")),
                id: DUMMY_NODE_ID,
            },
            _ => panic!("unrecognized string for Visibility: {:?}", self),
        };
        Visibility {
            kind,
            span: DUMMY_SP,
            tokens: None,
        }
    }
}

impl<'a> Make<Abi> for &'a str {
    fn make(self, _mk: &Builder) -> Abi {
        abi::lookup(self).expect(&format!("unrecognized string for Abi: {:?}", self))
    }
}

impl<'a> Make<Extern> for &'a str {
    fn make(self, mk: &Builder) -> Extern {
        Extern::from_abi(Some(mk.clone().str_lit(self)), DUMMY_SP)
    }
}

impl<'a> Make<Extern> for Abi {
    fn make(self, mk: &Builder) -> Extern {
        Extern::from_abi(Some(mk.clone().str_lit(self.name())), DUMMY_SP)
    }
}

impl<'a> Make<Mutability> for &'a str {
    fn make(self, _mk: &Builder) -> Mutability {
        match self {
            "" | "imm" | "immut" | "immutable" => Mutability::Not,
            "mut" | "mutable" => Mutability::Mut,
            _ => panic!("unrecognized string for Mutability: {:?}", self),
        }
    }
}

impl<'a> Make<Unsafe> for &'a str {
    fn make(self, _mk: &Builder) -> Unsafe {
        match self {
            "" | "safe" | "normal" => Unsafe::No,
            "unsafe" => Unsafe::Yes(DUMMY_SP),
            _ => panic!("unrecognized string for Unsafe: {:?}", self),
        }
    }
}

impl<'a> Make<Const> for &'a str {
    fn make(self, _mk: &Builder) -> Const {
        match self {
            "" | "normal" | "not-const" => Const::No,
            "const" => Const::Yes(DUMMY_SP),
            _ => panic!("unrecognized string for Const: {:?}", self),
        }
    }
}

impl<'a> Make<UnOp> for &'a str {
    fn make(self, _mk: &Builder) -> UnOp {
        match self {
            "deref" | "*" => UnOp::Deref,
            "not" | "!" => UnOp::Not,
            "neg" | "-" => UnOp::Neg,
            _ => panic!("unrecognized string for UnOp: {:?}", self),
        }
    }
}

impl<'a> Make<LitIntType> for &'a str {
    fn make(self, _mk: &Builder) -> LitIntType {
        match self {
            "is" | "isize" => LitIntType::Signed(IntTy::Isize),
            "i8" => LitIntType::Signed(IntTy::I8),
            "i16" => LitIntType::Signed(IntTy::I16),
            "i32" => LitIntType::Signed(IntTy::I32),
            "i64" => LitIntType::Signed(IntTy::I64),
            "i128" => LitIntType::Signed(IntTy::I128),

            "us" | "usize" => LitIntType::Unsigned(UintTy::Usize),
            "u8" => LitIntType::Unsigned(UintTy::U8),
            "u16" => LitIntType::Unsigned(UintTy::U16),
            "u32" => LitIntType::Unsigned(UintTy::U32),
            "u64" => LitIntType::Unsigned(UintTy::U64),
            "u128" => LitIntType::Unsigned(UintTy::U128),

            "" | "unsuffixed" => LitIntType::Unsuffixed,

            _ => panic!("unrecognized string for LitIntType: {:?}", self),
        }
    }
}

impl<I: Make<Ident>> Make<Lifetime> for I {
    fn make(self, mk: &Builder) -> Lifetime {
        Lifetime {
            id: DUMMY_NODE_ID,
            ident: self.make(mk),
        }
    }
}

impl<'a> Make<LitIntType> for IntTy {
    fn make(self, _mk: &Builder) -> LitIntType {
        LitIntType::Signed(self)
    }
}

impl<'a> Make<LitIntType> for UintTy {
    fn make(self, _mk: &Builder) -> LitIntType {
        LitIntType::Unsigned(self)
    }
}

impl Make<LitIntType> for ty::IntTy {
    fn make(self, _mk: &Builder) -> LitIntType {
        LitIntType::Signed(ty::ast_int_ty(self))
    }
}

impl Make<LitIntType> for ty::UintTy {
    fn make(self, _mk: &Builder) -> LitIntType {
        LitIntType::Unsigned(ty::ast_uint_ty(self))
    }
}

impl Make<FloatTy> for ty::FloatTy {
    fn make(self, _mk: &Builder) -> FloatTy {
        match self {
            ty::FloatTy::F32 => FloatTy::F32,
            ty::FloatTy::F64 => FloatTy::F64,
        }
    }
}

impl Make<Lit> for rustc_hir::Lit {
    fn make(self, _mk: &Builder) -> Lit {
        Lit {
            token: self.node.to_lit_token(),
            kind: self.node,
            span: self.span,
        }
    }
}

impl<I: Make<Ident>> Make<PathSegment> for I {
    fn make(self, mk: &Builder) -> PathSegment {
        PathSegment {
            id: DUMMY_NODE_ID,
            ident: self.make(mk),
            args: None,
        }
    }
}

impl<S: Make<PathSegment>> Make<Path> for Vec<S> {
    fn make(self, mk: &Builder) -> Path {
        Path {
            span: DUMMY_SP,
            segments: self.into_iter().map(|s| s.make(mk)).collect(),
            tokens: None,
        }
    }
}

//impl Make<TokenStream> for TokenStream {
//    fn make(self, _mk: &Builder) -> TokenStream {
//        self.into()
//    }
//}

impl Make<TokenStream> for Vec<TokenTree> {
    fn make(self, _mk: &Builder) -> TokenStream {
        self.into_iter().collect::<TokenStream>().into()
    }
}

impl Make<TokenTree> for Token {
    fn make(self, _mk: &Builder) -> TokenTree {
        TokenTree::Token(self, Spacing::Alone)
    }
}

impl Make<GenericArgs> for AngleBracketedArgs {
    fn make(self, _mk: &Builder) -> GenericArgs {
        AngleBracketed(self)
    }
}

impl Make<GenericArgs> for ParenthesizedArgs {
    fn make(self, _mk: &Builder) -> GenericArgs {
        Parenthesized(self)
    }
}

impl Make<GenericArg> for P<Ty> {
    fn make(self, _mk: &Builder) -> GenericArg {
        GenericArg::Type(self)
    }
}

impl Make<GenericArg> for Lifetime {
    fn make(self, _mk: &Builder) -> GenericArg {
        GenericArg::Lifetime(self)
    }
}

impl Make<NestedMetaItem> for MetaItem {
    fn make(self, _mk: &Builder) -> NestedMetaItem {
        NestedMetaItem::MetaItem(self)
    }
}

impl Make<NestedMetaItem> for Lit {
    fn make(self, _mk: &Builder) -> NestedMetaItem {
        NestedMetaItem::Literal(self)
    }
}

impl<L: Make<Lit>> Make<MetaItemKind> for L {
    fn make(self, mk: &Builder) -> MetaItemKind {
        MetaItemKind::NameValue(self.make(mk))
    }
}

impl<'a, S> Make<Lit> for S
where
    S: IntoSymbol,
{
    fn make(self, mk: &Builder) -> Lit {
        let s = self.into_symbol();
        Lit::from_lit_kind(LitKind::Str(s, StrStyle::Cooked), mk.span)
    }
}

impl Make<Lit> for Vec<u8> {
    fn make(self, mk: &Builder) -> Lit {
        Lit::from_lit_kind(LitKind::ByteStr(Rc::from(self)), mk.span)
    }
}

impl Make<Lit> for u8 {
    fn make(self, mk: &Builder) -> Lit {
        Lit::from_lit_kind(LitKind::Byte(self), mk.span)
    }
}

impl Make<Lit> for char {
    fn make(self, mk: &Builder) -> Lit {
        Lit::from_lit_kind(LitKind::Char(self), mk.span)
    }
}

impl Make<Lit> for u128 {
    fn make(self, mk: &Builder) -> Lit {
        Lit::from_lit_kind(LitKind::Int(self, LitIntType::Unsuffixed), mk.span)
    }
}

impl Make<FnSig> for P<FnDecl> {
    fn make(self, mk: &Builder) -> FnSig {
        FnSig {
            header: FnHeader {
                unsafety: mk.unsafety,
                asyncness: Async::No,
                constness: mk.constness,
                ext: mk.ext,
            },
            decl: self,
            span: DUMMY_SP,
        }
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
    unsafety: Unsafe,
    constness: Const,
    ext: Extern,
    inline: Inline,
    attrs: AttrVec,
    span: Span,
    id: NodeId,
}

#[allow(dead_code)]
impl Builder {
    pub fn new() -> Builder {
        Builder {
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            mutbl: Mutability::Not,
            generics: Generics::default(),
            unsafety: Unsafe::No,
            constness: Const::No,
            ext: Extern::None,
            inline: Inline::No,
            attrs: AttrVec::new(),
            span: DUMMY_SP,
            id: DUMMY_NODE_ID,
        }
    }

    // Modifier updates.

    pub fn vis<V: Make<Visibility>>(self, vis: V) -> Self {
        let vis = vis.make(&self);
        Builder { vis: vis, ..self }
    }

    pub fn pub_(self) -> Self {
        self.vis("pub")
    }

    pub fn inline(self) -> Self {
        Builder {
            inline: Inline::Yes,
            ..self
        }
    }

    pub fn set_mutbl<M: Make<Mutability>>(self, mutbl: M) -> Self {
        let mutbl = mutbl.make(&self);
        Builder {
            mutbl: mutbl,
            ..self
        }
    }

    pub fn mutbl(self) -> Self {
        self.set_mutbl(Mutability::Mut)
    }

    pub fn unsafety<U: Make<Unsafe>>(self, unsafety: U) -> Self {
        let unsafety = unsafety.make(&self);
        Builder {
            unsafety: unsafety,
            ..self
        }
    }

    pub fn unsafe_(self) -> Self {
        self.unsafety(Unsafe::Yes(DUMMY_SP))
    }

    pub fn constness<C: Make<Const>>(self, constness: C) -> Self {
        let constness = constness.make(&self);
        Builder {
            constness: constness,
            ..self
        }
    }

    pub fn const_(self) -> Self {
        self.constness(Const::Yes(DUMMY_SP))
    }

    pub fn extern_<A: Make<Extern>>(self, ext: A) -> Self {
        let ext = ext.make(&self);
        Builder { ext: ext, ..self }
    }

    pub fn span<S: Make<Span>>(self, span: S) -> Self {
        let span = span.make(&self);
        Builder { span: span, ..self }
    }

    /// Set the `NodeId` of the constructed AST.
    ///
    /// **Warning**: Be careful with this option!  Parts of the rewriter expect nodes with matching
    /// NodeIds to be identical in other ways as well.  For best results, only call this method
    /// with fresh NodeIds, like those returned by `st.next_node_id()`.
    pub fn id(self, id: NodeId) -> Self {
        Builder { id: id, ..self }
    }

    pub fn str_attr<K, V>(mut self, key: K, value: V) -> Self
    where
        K: Make<Path>,
        V: IntoSymbol,
    {
        let key = key.make(&self);

        let mut attrs = std::mem::take(&mut self.attrs);
        attrs.push(Attribute {
            id: AttrId::from_u32(0),
            style: AttrStyle::Outer,
            kind: AttrKind::Normal(
                AttrItem {
                    path: key,
                    args: MacArgs::Eq(
                        DUMMY_SP,
                        MacArgsEq::Ast(mk().lit_expr(mk().str_lit(value).as_lit())),
                    ),
                    tokens: None,
                },
                None,
            ),
            span: DUMMY_SP,
        });
        Builder {
            attrs: attrs,
            ..self
        }
    }

    pub fn single_attr<K>(mut self, key: K) -> Self
    where
        K: Make<PathSegment>,
    {
        let key: Path = vec![key].make(&self);

        let mut attrs = std::mem::take(&mut self.attrs);
        attrs.push(Attribute {
            id: AttrId::from_u32(0),
            style: AttrStyle::Outer,
            kind: AttrKind::Normal(
                AttrItem {
                    path: key,
                    args: MacArgs::Empty,
                    tokens: None,
                },
                None,
            ),
            span: DUMMY_SP,
        });
        Builder {
            attrs: attrs,
            ..self
        }
    }

    pub fn call_attr<K, V>(mut self, func: K, arguments: Vec<V>) -> Self
    where
        K: Make<PathSegment>,
        V: Make<Ident>,
    {
        let func: Path = vec![func].make(&self);

        let args = MacArgs::Delimited(DelimSpan::dummy(), MacDelimiter::Parenthesis, {
            let mut builder = TokenStreamBuilder::new();

            let mut is_first = true;
            for argument in arguments {
                if is_first {
                    is_first = false;
                } else {
                    builder.push(TokenStream::token_alone(TokenKind::Comma, DUMMY_SP));
                }

                let argument: Ident = argument.make(&self);
                let token_kind = TokenKind::Ident(argument.name, argument.is_raw_guess());
                builder.push(TokenStream::token_alone(token_kind, DUMMY_SP));
            }

            builder.build()
        });

        let mut attrs = std::mem::take(&mut self.attrs);
        attrs.push(Attribute {
            id: AttrId::from_u32(0),
            style: AttrStyle::Outer,
            kind: AttrKind::Normal(
                AttrItem {
                    path: func,
                    args,
                    tokens: None,
                },
                None,
            ),
            span: DUMMY_SP,
        });
        Builder {
            attrs: attrs,
            ..self
        }
    }

    // Path segments with parameters

    pub fn path_segment_with_args<I, P>(self, identifier: I, args: P) -> PathSegment
    where
        I: Make<Ident>,
        P: Make<GenericArgs>,
    {
        let identifier = identifier.make(&self);
        let args = args.make(&self);
        PathSegment {
            id: DUMMY_NODE_ID,
            ident: identifier,
            args: Some(P(args)),
        }
    }

    pub fn parenthesized_args<Ts>(self, tys: Ts) -> ParenthesizedArgs
    where
        Ts: Make<Vec<P<Ty>>>,
    {
        let tys = tys.make(&self);
        ParenthesizedArgs {
            span: self.span,
            inputs: tys,
            inputs_span: DUMMY_SP,
            output: FnRetTy::Default(DUMMY_SP),
        }
    }

    pub fn angle_bracketed_args<A>(self, args: Vec<A>) -> AngleBracketedArgs
    where
        A: Make<GenericArg>,
    {
        let args = args
            .into_iter()
            .map(|arg| AngleBracketedArg::Arg(arg.make(&self)))
            .collect();
        AngleBracketedArgs {
            span: self.span,
            args: args,
        }
    }

    pub fn generic_arg<A>(self, arg: A) -> GenericArg
    where
        A: Make<GenericArg>,
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
        K: Make<UseTreeKind>,
    {
        UseTree {
            span: DUMMY_SP,
            prefix: prefix.make(&self),
            kind: kind.make(&self),
        }
    }

    //    pub fn abs_path<Pa>(self, path: Pa) -> Path
    //    where
    //        Pa: Make<Path>,
    //    {
    //        let mut p = path.make(&self);
    //        if !p
    //            .segments
    //            .get(0)
    //            .map_or(false, |s| s.ident.name == kw::Crate)
    //        {
    //            p.segments.insert(0, kw::Crate.ident().make(&self));
    //        }
    //        p
    //    }

    pub fn anon_const<E>(self, expr: E) -> AnonConst
    where
        E: Make<P<Expr>>,
    {
        AnonConst {
            id: DUMMY_NODE_ID,
            value: expr.make(&self),
        }
    }

    pub fn spanned<T, U: Make<T>>(self, x: U) -> Spanned<T> {
        Spanned {
            node: x.make(&self),
            span: self.span,
        }
    }

    // Exprs
    // These are sorted in the same order as the corresponding ExprKind variants, with additional
    // variant-specific details following each variant.

    pub fn array_expr<A>(self, args: Vec<A>) -> P<Expr>
    where
        A: Make<P<Expr>>,
    {
        let args = args.into_iter().map(|a| a.make(&self)).collect();
        P(Expr {
            id: self.id,
            kind: ExprKind::Array(args),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn call_expr<F, A>(self, func: F, args: Vec<A>) -> P<Expr>
    where
        F: Make<P<Expr>>,
        A: Make<P<Expr>>,
    {
        let func = func.make(&self);
        let args = args.into_iter().map(|a| a.make(&self)).collect();
        P(Expr {
            id: self.id,
            kind: ExprKind::Call(func, args),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn method_call_expr<E, S, A>(self, expr: E, seg: S, args: Vec<A>) -> P<Expr>
    where
        E: Make<P<Expr>>,
        S: Make<PathSegment>,
        A: Make<P<Expr>>,
    {
        let expr = expr.make(&self);
        let seg = seg.make(&self);

        let mut all_args = Vec::with_capacity(args.len() + 1);
        all_args.push(expr);
        for arg in args {
            all_args.push(arg.make(&self));
        }

        P(Expr {
            id: self.id,
            kind: ExprKind::MethodCall(seg, all_args, DUMMY_SP),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn tuple_expr<E>(self, exprs: Vec<E>) -> P<Expr>
    where
        E: Make<P<Expr>>,
    {
        let exprs: Vec<P<Expr>> = exprs.into_iter().map(|x| x.make(&self)).collect();
        P(Expr {
            id: self.id,
            kind: ExprKind::Tup(exprs),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn binary_expr<O, E>(self, op: O, lhs: E, rhs: E) -> P<Expr>
    where
        O: Make<BinOpKind>,
        E: Make<P<Expr>>,
    {
        let op = op.make(&self);
        let op_ = mk().spanned(op);
        let mut lhs = lhs.make(&self);
        let rhs = rhs.make(&self);

        match op {
            BinOpKind::Lt | BinOpKind::Shl if has_rightmost_cast(&*lhs) => {
                lhs = mk().paren_expr(lhs)
            }
            _ => {}
        }

        P(Expr {
            id: self.id,
            kind: ExprKind::Binary(op_, lhs, rhs),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn unary_expr<O, E>(self, op: O, a: E) -> P<Expr>
    where
        O: Make<UnOp>,
        E: Make<P<Expr>>,
    {
        let op = op.make(&self);
        let a = a.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Unary(op, a),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn lit_expr<L>(self, lit: L) -> P<Expr>
    where
        L: Make<Lit>,
    {
        let lit = lit.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Lit(lit),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn cast_expr<E, T>(self, e: E, t: T) -> P<Expr>
    where
        E: Make<P<Expr>>,
        T: Make<P<Ty>>,
    {
        let e = e.make(&self);
        let t = t.make(&self);

        P(Expr {
            id: self.id,
            kind: ExprKind::Cast(e, t),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn type_expr<E, T>(self, e: E, t: T) -> P<Expr>
    where
        E: Make<P<Expr>>,
        T: Make<P<Ty>>,
    {
        let e = e.make(&self);
        let t = t.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Type(e, t),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn block_expr<B>(self, blk: B) -> P<Expr>
    where
        B: Make<P<Block>>,
    {
        let blk = blk.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Block(blk, None),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn labelled_block_expr<B, L>(self, blk: B, lbl: L) -> P<Expr>
    where
        B: Make<P<Block>>,
        L: Make<Label>,
    {
        let blk = blk.make(&self);
        let lbl = lbl.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            kind: ExprKind::Block(blk, Some(lbl)),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn assign_expr<E1, E2>(self, lhs: E1, rhs: E2) -> P<Expr>
    where
        E1: Make<P<Expr>>,
        E2: Make<P<Expr>>,
    {
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Assign(lhs, rhs, DUMMY_SP),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn assign_op_expr<O, E1, E2>(self, op: O, lhs: E1, rhs: E2) -> P<Expr>
    where
        O: Make<BinOpKind>,
        E1: Make<P<Expr>>,
        E2: Make<P<Expr>>,
    {
        let op = dummy_spanned(op.make(&self));
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::AssignOp(op, lhs, rhs),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn index_expr<E1, E2>(self, lhs: E1, rhs: E2) -> P<Expr>
    where
        E1: Make<P<Expr>>,
        E2: Make<P<Expr>>,
    {
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Index(lhs, rhs),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn path_expr<Pa>(self, path: Pa) -> P<Expr>
    where
        Pa: Make<Path>,
    {
        self.qpath_expr(None, path)
    }

    pub fn qpath_expr<Pa>(self, qself: Option<QSelf>, path: Pa) -> P<Expr>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Path(qself, path),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    /// An array literal constructed from one repeated element.
    /// `[expr; n]`
    pub fn repeat_expr<E, N>(self, expr: E, n: N) -> P<Expr>
    where
        E: Make<P<Expr>>,
        N: Make<P<Expr>>,
    {
        let expr = expr.make(&self);
        let n = mk().anon_const(n.make(&self));
        P(Expr {
            id: self.id,
            kind: ExprKind::Repeat(expr, n),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn paren_expr<E>(self, e: E) -> P<Expr>
    where
        E: Make<P<Expr>>,
    {
        let e = e.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Paren(e),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    // Special case of path_expr
    pub fn ident_expr<I>(self, name: I) -> P<Expr>
    where
        I: Make<Ident>,
    {
        self.path_expr(vec![name])
    }

    pub fn addr_of_expr<E>(self, e: E) -> P<Expr>
    where
        E: Make<P<Expr>>,
    {
        let e = e.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::AddrOf(BorrowKind::Ref, self.mutbl, e),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn mac_expr<M>(self, mac: M) -> P<Expr>
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::MacCall(mac),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn struct_expr<Pa>(self, path: Pa, fields: Vec<ExprField>) -> P<Expr>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Struct(P(StructExpr {
                qself: None,
                path,
                fields,
                rest: StructRest::None,
            })),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    // struct_expr, but with optional base expression
    pub fn struct_expr_base<Pa, E>(
        self,
        path: Pa,
        fields: Vec<ExprField>,
        base: Option<E>,
    ) -> P<Expr>
    where
        Pa: Make<Path>,
        E: Make<P<Expr>>,
    {
        let path = path.make(&self);
        let base = match base {
            Some(e) => StructRest::Base(e.make(&self)),
            None => StructRest::None,
        };
        P(Expr {
            id: self.id,
            kind: ExprKind::Struct(P(StructExpr {
                qself: None,
                path,
                fields,
                rest: base,
            })),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn field_expr<E, F>(self, val: E, field: F) -> P<Expr>
    where
        E: Make<P<Expr>>,
        F: Make<Ident>,
    {
        let val = val.make(&self);
        let field = field.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Field(val, field),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn field<I, E>(self, ident: I, expr: E) -> ExprField
    where
        I: Make<Ident>,
        E: Make<P<Expr>>,
    {
        let ident = ident.make(&self);
        let expr = expr.make(&self);
        ExprField {
            ident,
            expr: expr,
            span: self.span,
            is_shorthand: false,
            attrs: self.attrs.into(),
            id: self.id,
            is_placeholder: false,
        }
    }

    pub fn match_expr<E>(self, cond: E, arms: Vec<Arm>) -> P<Expr>
    where
        E: Make<P<Expr>>,
    {
        let cond = cond.make(&self);
        let arms = arms.into_iter().map(|arm| arm.make(&self)).collect();
        P(Expr {
            id: self.id,
            kind: ExprKind::Match(cond, arms),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn arm<Pa, E>(self, pat: Pa, guard: Option<E>, body: E) -> Arm
    where
        E: Make<P<Expr>>,
        Pa: Make<P<Pat>>,
    {
        let pat = pat.make(&self);
        let guard = guard.map(|g| g.make(&self));
        let body = body.make(&self);
        Arm {
            id: self.id,
            attrs: self.attrs,
            pat,
            guard,
            body,
            span: DUMMY_SP,
            is_placeholder: false,
        }
    }

    // Literals

    pub fn str_lit<S>(self, s: S) -> StrLit
    where
        S: IntoSymbol,
    {
        let symbol = s.into_symbol();
        StrLit {
            style: StrStyle::Cooked,
            suffix: None,
            span: self.span,
            symbol_unescaped: symbol.clone(),
            symbol,
        }
    }

    pub fn int_lit<T>(self, i: u128, ty: T) -> Lit
    where
        T: Make<LitIntType>,
    {
        let ty = ty.make(&self);
        Lit::from_lit_kind(LitKind::Int(i, ty), self.span)
    }

    pub fn float_lit<S, T>(self, s: S, ty: T) -> Lit
    where
        S: IntoSymbol,
        T: Make<FloatTy>,
    {
        let s = s.into_symbol();
        let ty = ty.make(&self);
        Lit::from_lit_kind(LitKind::Float(s, LitFloatType::Suffixed(ty)), self.span)
    }

    pub fn float_unsuffixed_lit<S>(self, s: S) -> Lit
    where
        S: IntoSymbol,
    {
        let s = s.into_symbol();
        Lit::from_lit_kind(LitKind::Float(s, LitFloatType::Unsuffixed), self.span)
    }

    pub fn bool_lit(self, b: bool) -> Lit {
        Lit::from_lit_kind(LitKind::Bool(b), self.span)
    }

    pub fn ifte_expr<C, T, E>(self, cond: C, then_case: T, else_case: Option<E>) -> P<Expr>
    where
        C: Make<P<Expr>>,
        T: Make<P<Block>>,
        E: Make<P<Expr>>,
    {
        let cond = cond.make(&self);
        let then_case = then_case.make(&self);
        let else_case = else_case.map(|x| {
            let e = x.make(&self);

            // The else branch in libsyntax must be one of these three cases,
            // otherwise we have to manually add the block around the else expression
            match e.kind {
                ExprKind::If { .. } | ExprKind::Block(_, None) => e,
                _ => mk().block_expr(mk().block(vec![mk().expr_stmt(e)])),
            }
        });

        P(Expr {
            id: self.id,
            kind: ExprKind::If(cond, then_case, else_case),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn while_expr<C, B, I>(self, cond: C, body: B, label: Option<I>) -> P<Expr>
    where
        C: Make<P<Expr>>,
        B: Make<P<Block>>,
        I: Make<Ident>,
    {
        let cond = cond.make(&self);
        let body = body.make(&self);
        let label = label.map(|l| Label {
            ident: l.make(&self),
        });

        P(Expr {
            id: self.id,
            kind: ExprKind::While(cond, body, label),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn loop_expr<B, I>(self, body: B, label: Option<I>) -> P<Expr>
    where
        B: Make<P<Block>>,
        I: Make<Ident>,
    {
        let body = body.make(&self);
        let label = label.map(|l| Label {
            ident: l.make(&self),
        });

        P(Expr {
            id: self.id,
            kind: ExprKind::Loop(body, label),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn for_expr<Pa, E, B, I>(self, pat: Pa, expr: E, body: B, label: Option<I>) -> P<Expr>
    where
        Pa: Make<P<Pat>>,
        E: Make<P<Expr>>,
        B: Make<P<Block>>,
        I: Make<Ident>,
    {
        let pat = pat.make(&self);
        let expr = expr.make(&self);
        let body = body.make(&self);
        let label = label.map(|l| Label {
            ident: l.make(&self),
        });

        P(Expr {
            id: self.id,
            kind: ExprKind::ForLoop(pat, expr, body, label),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    // Patterns

    pub fn ident_pat<I>(self, name: I) -> P<Pat>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        P(Pat {
            id: self.id,
            kind: PatKind::Ident(BindingMode::ByValue(self.mutbl), name, None),
            span: self.span,
            tokens: None,
        })
    }

    pub fn tuple_pat<Pa>(self, pats: Vec<Pa>) -> P<Pat>
    where
        Pa: Make<P<Pat>>,
    {
        let pats: Vec<P<Pat>> = pats.into_iter().map(|x| x.make(&self)).collect();
        P(Pat {
            id: self.id,
            kind: PatKind::Tuple(pats),
            span: self.span,
            tokens: None,
        })
    }

    pub fn qpath_pat<Pa>(self, qself: Option<QSelf>, path: Pa) -> P<Pat>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        P(Pat {
            id: self.id,
            kind: PatKind::Path(qself, path),
            span: self.span,
            tokens: None,
        })
    }

    pub fn wild_pat(self) -> P<Pat> {
        P(Pat {
            id: self.id,
            kind: PatKind::Wild,
            span: self.span,
            tokens: None,
        })
    }

    pub fn lit_pat<L>(self, lit: L) -> P<Pat>
    where
        L: Make<P<Expr>>,
    {
        let lit = lit.make(&self);
        P(Pat {
            id: self.id,
            kind: PatKind::Lit(lit),
            span: self.span,
            tokens: None,
        })
    }

    pub fn mac_pat<M>(self, mac: M) -> P<Pat>
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        P(Pat {
            id: self.id,
            kind: PatKind::MacCall(mac),
            span: self.span,
            tokens: None,
        })
    }

    pub fn ident_ref_pat<I>(self, name: I) -> P<Pat>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        P(Pat {
            id: self.id,
            kind: PatKind::Ident(BindingMode::ByRef(self.mutbl), name, None),
            span: self.span,
            tokens: None,
        })
    }

    pub fn or_pat<Pa>(self, pats: Vec<Pa>) -> P<Pat>
    where
        Pa: Make<P<Pat>>,
    {
        let pats: Vec<P<Pat>> = pats.into_iter().map(|p| p.make(&self)).collect();
        P(Pat {
            id: self.id,
            kind: PatKind::Or(pats),
            span: self.span,
            tokens: None,
        })
    }

    // Types

    pub fn barefn_ty<T>(self, decl: T) -> P<Ty>
    where
        T: Make<P<FnDecl>>,
    {
        let decl = decl.make(&self);

        let barefn = BareFnTy {
            unsafety: self.unsafety,
            ext: self.ext,
            generic_params: vec![],
            decl,
            decl_span: DUMMY_SP,
        };

        P(Ty {
            id: self.id,
            kind: TyKind::BareFn(P(barefn)),
            span: self.span,
            tokens: None,
        })
    }

    pub fn array_ty<T, E>(self, ty: T, len: E) -> P<Ty>
    where
        T: Make<P<Ty>>,
        E: Make<P<Expr>>,
    {
        let ty = ty.make(&self);
        let len = mk().anon_const(len.make(&self));
        P(Ty {
            id: self.id,
            kind: TyKind::Array(ty, len),
            span: self.span,
            tokens: None,
        })
    }

    pub fn slice_ty<T>(self, ty: T) -> P<Ty>
    where
        T: Make<P<Ty>>,
    {
        let ty = ty.make(&self);
        P(Ty {
            id: self.id,
            kind: TyKind::Slice(ty),
            span: self.span,
            tokens: None,
        })
    }

    pub fn ptr_ty<T>(self, ty: T) -> P<Ty>
    where
        T: Make<P<Ty>>,
    {
        let ty = ty.make(&self);
        P(Ty {
            id: self.id,
            kind: TyKind::Ptr(MutTy {
                ty: ty,
                mutbl: self.mutbl,
            }),
            span: self.span,
            tokens: None,
        })
    }

    pub fn ref_ty<T>(self, ty: T) -> P<Ty>
    where
        T: Make<P<Ty>>,
    {
        let ty = ty.make(&self);
        P(Ty {
            id: self.id,
            kind: TyKind::Rptr(
                None,
                MutTy {
                    ty: ty,
                    mutbl: self.mutbl,
                },
            ),
            span: self.span,
            tokens: None,
        })
    }

    pub fn ref_lt_ty<L, T>(self, lt: L, ty: T) -> P<Ty>
    where
        L: Make<Lifetime>,
        T: Make<P<Ty>>,
    {
        let lt = lt.make(&self);
        let ty = ty.make(&self);
        P(Ty {
            id: self.id,
            kind: TyKind::Rptr(
                Some(lt),
                MutTy {
                    ty: ty,
                    mutbl: self.mutbl,
                },
            ),
            span: self.span,
            tokens: None,
        })
    }

    pub fn never_ty(self) -> P<Ty> {
        P(Ty {
            id: self.id,
            kind: TyKind::Never,
            span: self.span,
            tokens: None,
        })
    }

    pub fn tuple_ty<T>(self, elem_tys: Vec<T>) -> P<Ty>
    where
        T: Make<P<Ty>>,
    {
        let elem_tys = elem_tys.into_iter().map(|ty| ty.make(&self)).collect();
        P(Ty {
            id: self.id,
            kind: TyKind::Tup(elem_tys),
            span: self.span,
            tokens: None,
        })
    }

    pub fn path_ty<Pa>(self, path: Pa) -> P<Ty>
    where
        Pa: Make<Path>,
    {
        self.qpath_ty(None, path)
    }

    pub fn qpath_ty<Pa>(self, qself: Option<QSelf>, path: Pa) -> P<Ty>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        P(Ty {
            id: self.id,
            kind: TyKind::Path(qself, path),
            span: self.span,
            tokens: None,
        })
    }

    pub fn ident_ty<I>(self, name: I) -> P<Ty>
    where
        I: Make<Ident>,
    {
        self.path_ty(vec![name])
    }

    pub fn infer_ty(self) -> P<Ty> {
        P(Ty {
            id: self.id,
            kind: TyKind::Infer,
            span: self.span,
            tokens: None,
        })
    }

    pub fn mac_ty<M>(self, mac: M) -> P<Ty>
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        P(Ty {
            id: self.id,
            kind: TyKind::MacCall(mac),
            span: self.span,
            tokens: None,
        })
    }

    pub fn cvar_args_ty(self) -> P<Ty> {
        P(Ty {
            id: self.id,
            kind: TyKind::CVarArgs,
            span: self.span,
            tokens: None,
        })
    }

    // Stmts

    pub fn local_stmt<L>(self, local: L) -> Stmt
    where
        L: Make<P<Local>>,
    {
        let local = local.make(&self);
        Stmt {
            id: self.id,
            kind: StmtKind::Local(local),
            span: self.span,
        }
    }

    pub fn expr_stmt<E>(self, expr: E) -> Stmt
    where
        E: Make<P<Expr>>,
    {
        let expr = expr.make(&self);
        Stmt {
            id: self.id,
            kind: StmtKind::Expr(expr),
            span: self.span,
        }
    }

    pub fn semi_stmt<E>(self, expr: E) -> Stmt
    where
        E: Make<P<Expr>>,
    {
        let expr = expr.make(&self);
        Stmt {
            id: self.id,
            kind: StmtKind::Semi(expr),
            span: self.span,
        }
    }

    pub fn item_stmt<I>(self, item: I) -> Stmt
    where
        I: Make<P<Item>>,
    {
        let item = item.make(&self);
        Stmt {
            id: self.id,
            kind: StmtKind::Item(item),
            span: self.span,
        }
    }

    pub fn mac_stmt<M>(self, mac: M) -> Stmt
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        Stmt {
            id: self.id,
            kind: StmtKind::MacCall(P(MacCallStmt {
                mac,
                style: MacStmtStyle::Semicolon,
                attrs: ThinVec::new(),
                tokens: None,
            })),
            span: self.span,
        }
    }

    // Items

    fn item(
        name: Ident,
        attrs: AttrVec,
        vis: Visibility,
        span: Span,
        id: NodeId,
        kind: ItemKind,
    ) -> P<Item> {
        P(Item {
            ident: name,
            attrs: attrs.to_vec(),
            id: id,
            kind: kind,
            vis: vis,
            span: span,
            tokens: None,
        })
    }

    pub fn static_item<I, T, E>(self, name: I, ty: T, init: E) -> P<Item>
    where
        I: Make<Ident>,
        T: Make<P<Ty>>,
        E: Make<P<Expr>>,
    {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.make(&self);
        Self::item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Static(ty, self.mutbl, Some(init)),
        )
    }

    pub fn const_item<I, T, E>(self, name: I, ty: T, init: Option<E>) -> P<Item>
    where
        I: Make<Ident>,
        T: Make<P<Ty>>,
        E: Make<P<Expr>>,
    {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.map(|i| i.make(&self));
        Self::item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Const(Defaultness::Final, ty, init),
        )
    }

    pub fn fn_item<I, S, B>(self, name: I, sig: S, block: Option<B>) -> P<Item>
    where
        I: Make<Ident>,
        S: Make<FnSig>,
        B: Make<P<Block>>,
    {
        let name = name.make(&self);
        let sig = sig.make(&self);
        let block = block.map(|b| b.make(&self));
        Self::item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Fn(Box::new(Fn {
                defaultness: Defaultness::Final,
                sig,
                generics: self.generics,
                body: block,
            })),
        )
    }

    pub fn fn_decl(self, inputs: Vec<Param>, output: FnRetTy) -> P<FnDecl> {
        P(FnDecl { inputs, output })
    }

    pub fn struct_item<I>(self, name: I, fields: Vec<FieldDef>, tuple: bool) -> P<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        let variant_data = if tuple {
            VariantData::Tuple(fields, DUMMY_NODE_ID)
        } else {
            VariantData::Struct(fields, false)
        };
        Self::item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Struct(variant_data, self.generics),
        )
    }

    pub fn union_item<I>(self, name: I, fields: Vec<FieldDef>) -> P<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Self::item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Union(VariantData::Struct(fields, false), self.generics),
        )
    }

    pub fn enum_item<I>(self, name: I, fields: Vec<Variant>) -> P<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Self::item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Enum(EnumDef { variants: fields }, self.generics),
        )
    }

    pub fn type_item<I, T>(self, name: I, ty: Option<T>) -> P<Item>
    where
        I: Make<Ident>,
        T: Make<P<Ty>>,
    {
        let ty = ty.map(|ty| ty.make(&self));
        let name = name.make(&self);
        let kind = ItemKind::TyAlias(Box::new(TyAlias {
            defaultness: Defaultness::Final,
            generics: self.generics,
            where_clauses: (TyAliasWhereClause::default(), TyAliasWhereClause::default()),
            where_predicates_split: 0,
            bounds: vec![],
            ty,
        }));
        Self::item(name, self.attrs, self.vis, self.span, self.id, kind)
    }

    pub fn mod_item<I>(self, name: I, m: ModKind) -> P<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        let kind = ItemKind::Mod(self.unsafety, m);
        Self::item(name, self.attrs, self.vis, self.span, self.id, kind)
    }

    pub fn mod_<I>(self, items: Vec<I>) -> ModKind
    where
        I: Make<P<Item>>,
    {
        let items = items.into_iter().map(|i| i.make(&self)).collect();
        let spans = ModSpans {
            inner_span: self.span,
            inject_use_span: DUMMY_SP,
        };
        ModKind::Loaded(items, self.inline, spans)
    }

    pub fn mac_item<M>(self, mac: M) -> P<Item>
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        let kind = ItemKind::MacCall(mac);
        Self::item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            kind,
        )
    }

    pub fn variant<I>(self, name: I, dat: VariantData) -> Variant
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        Variant {
            ident: name,
            attrs: self.attrs,
            id: DUMMY_NODE_ID,
            data: dat,
            disr_expr: None,
            span: self.span,
            vis: self.vis,
            is_placeholder: false,
        }
    }

    pub fn unit_variant<I, E>(self, name: I, disc: Option<E>) -> Variant
    where
        I: Make<Ident>,
        E: Make<P<Expr>>,
    {
        let name = name.make(&self);
        let disc = disc.map(|d| AnonConst {
            id: DUMMY_NODE_ID,
            value: d.make(&self),
        });
        Variant {
            ident: name,
            attrs: self.attrs,
            id: DUMMY_NODE_ID,
            data: VariantData::Unit(self.id),
            disr_expr: disc,
            span: self.span,
            vis: self.vis,
            is_placeholder: false,
        }
    }

    pub fn impl_item<T>(self, ty: T, items: Vec<P<AssocItem>>) -> P<Item>
    where
        T: Make<P<Ty>>,
    {
        let self_ty = ty.make(&self);
        Self::item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Impl(Box::new(Impl {
                defaultness: Defaultness::Final,
                unsafety: self.unsafety,
                generics: self.generics,
                constness: self.constness,
                polarity: ImplPolarity::Positive,
                of_trait: None, // not a trait implementation
                self_ty,
                items,
            })),
        )
    }

    pub fn extern_crate_item<I>(self, name: I, rename: Option<I>) -> P<Item>
    where
        I: Make<Ident>,
    {
        let name = name.make(&self);
        let rename = rename.map(|n| n.make(&self).name);
        Self::item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::ExternCrate(rename),
        )
    }

    pub fn use_item<U>(self, tree: U) -> P<Item>
    where
        U: Make<UseTree>,
    {
        let use_tree = tree.make(&self);
        Self::item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Use(use_tree),
        )
    }

    // `use <path>;` item
    pub fn use_simple_item<Pa, I>(self, path: Pa, rename: Option<I>) -> P<Item>
    where
        Pa: Make<Path>,
        I: Make<Ident>,
    {
        let path = path.make(&self);
        let rename = rename.map(|n| n.make(&self));
        let use_tree = UseTree {
            span: DUMMY_SP,
            prefix: path,
            kind: UseTreeKind::Simple(rename, DUMMY_NODE_ID, DUMMY_NODE_ID),
        };
        Self::item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Use(use_tree),
        )
    }

    pub fn use_multiple_item<Pa, I, It>(self, path: Pa, inner: It) -> P<Item>
    where
        Pa: Make<Path>,
        I: Make<Ident>,
        It: Iterator<Item = I>,
    {
        let path = path.make(&self);
        let inner_trees = inner
            .map(|i| {
                (
                    UseTree {
                        span: DUMMY_SP,
                        prefix: Path::from_ident(i.make(&self)),
                        kind: UseTreeKind::Simple(None, DUMMY_NODE_ID, DUMMY_NODE_ID),
                    },
                    DUMMY_NODE_ID,
                )
            })
            .collect();
        let use_tree = UseTree {
            span: DUMMY_SP,
            prefix: path,
            kind: UseTreeKind::Nested(inner_trees),
        };
        Self::item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Use(use_tree),
        )
    }

    pub fn use_glob_item<Pa>(self, path: Pa) -> P<Item>
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        let use_tree = UseTree {
            span: DUMMY_SP,
            prefix: path,
            kind: UseTreeKind::Glob,
        };
        Self::item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::Use(use_tree),
        )
    }

    pub fn foreign_items(self, items: Vec<P<ForeignItem>>) -> P<Item> {
        let fgn_mod = ForeignMod {
            unsafety: self.unsafety,
            abi: match self.ext {
                Extern::None | Extern::Implicit(_) => None,
                Extern::Explicit(s, _) => Some(s),
            },
            items,
        };
        Self::item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ItemKind::ForeignMod(fgn_mod),
        )
    }

    // Impl Items

    /// Called `impl_item_` because `impl_item` is already used for "Item, of ItemKind::Impl".
    fn impl_item_(
        ident: Ident,
        attrs: AttrVec,
        vis: Visibility,
        span: Span,
        id: NodeId,
        kind: AssocItemKind,
    ) -> P<AssocItem> {
        P(AssocItem {
            attrs: attrs.to_vec(),
            id,
            span,
            vis,
            ident,
            kind,
            tokens: None,
        })
    }

    pub fn mac_impl_item<M>(self, mac: M) -> P<AssocItem>
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        let kind = AssocItemKind::MacCall(mac);
        Self::impl_item_(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            kind,
        )
    }

    // Trait Items

    /// Called `trait_item_` because `trait_item` is already used for "Item, of ItemKind::Trait".
    fn trait_item_(
        ident: Ident,
        attrs: AttrVec,
        span: Span,
        vis: Visibility,
        id: NodeId,
        kind: AssocItemKind,
    ) -> P<AssocItem> {
        P(AssocItem {
            attrs: attrs.to_vec(),
            id,
            span,
            vis,
            ident,
            kind,
            tokens: None,
        })
    }

    pub fn mac_trait_item<M>(self, mac: M) -> P<AssocItem>
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        let kind = AssocItemKind::MacCall(mac);
        Self::trait_item_(
            Ident::empty(),
            self.attrs,
            self.span,
            self.vis,
            self.id,
            kind,
        )
    }

    // Foreign Items

    fn foreign_item(
        name: Ident,
        attrs: AttrVec,
        vis: Visibility,
        span: Span,
        id: NodeId,
        kind: ForeignItemKind,
    ) -> P<ForeignItem> {
        P(ForeignItem {
            ident: name,
            attrs: attrs.to_vec(),
            id: id,
            kind: kind,
            vis: vis,
            span: span,
            tokens: None,
        })
    }

    pub fn fn_foreign_item<I, S>(self, name: I, sig: S) -> P<ForeignItem>
    where
        I: Make<Ident>,
        S: Make<FnSig>,
    {
        let name = name.make(&self);
        let sig = sig.make(&self);
        Self::foreign_item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ForeignItemKind::Fn(Box::new(Fn {
                defaultness: Defaultness::Final,
                generics: self.generics,
                sig,
                body: None,
            })),
        )
    }

    pub fn static_foreign_item<I, T>(self, name: I, ty: T) -> P<ForeignItem>
    where
        I: Make<Ident>,
        T: Make<P<Ty>>,
    {
        let name = name.make(&self);
        let ty = ty.make(&self);
        Self::foreign_item(
            name,
            self.attrs,
            self.vis,
            self.span,
            self.id,
            ForeignItemKind::Static(ty, self.mutbl, None),
        )
    }

    pub fn mac_foreign_item<M>(self, mac: M) -> P<ForeignItem>
    where
        M: Make<MacCall>,
    {
        let mac = mac.make(&self);
        let kind = ForeignItemKind::MacCall(mac);
        Self::foreign_item(
            Ident::empty(),
            self.attrs,
            self.vis,
            self.span,
            self.id,
            kind,
        )
    }

    // struct fields

    pub fn field_def<I, T>(self, ident: I, ty: T) -> FieldDef
    where
        I: Make<Ident>,
        T: Make<P<Ty>>,
    {
        let ident = ident.make(&self);
        let ty = ty.make(&self);
        FieldDef {
            span: self.span,
            ident: Some(ident),
            vis: self.vis,
            id: self.id,
            ty: ty,
            attrs: self.attrs,
            is_placeholder: false,
        }
    }

    pub fn enum_field<T>(self, ty: T) -> FieldDef
    where
        T: Make<P<Ty>>,
    {
        let ty = ty.make(&self);
        FieldDef {
            span: self.span,
            ident: None,
            vis: self.vis,
            id: self.id,
            ty: ty,
            attrs: self.attrs,
            is_placeholder: false,
        }
    }

    // Misc nodes

    pub fn block<S>(self, stmts: Vec<S>) -> P<Block>
    where
        S: Make<Stmt>,
    {
        let stmts = stmts.into_iter().map(|s| s.make(&self)).collect();
        P(Block {
            stmts: stmts,
            id: self.id,
            rules: match self.unsafety {
                Unsafe::Yes(_) => BlockCheckMode::Unsafe(UnsafeSource::UserProvided),
                Unsafe::No => BlockCheckMode::Default,
            },
            span: self.span,
            tokens: None,
            could_be_bare_literal: false,
        })
    }

    pub fn label<L>(self, lbl: L) -> Label
    where
        L: Make<Label>,
    {
        lbl.make(&self)
    }

    pub fn break_expr_value<L, E>(self, label: Option<L>, value: Option<E>) -> P<Expr>
    where
        L: Make<Label>,
        E: Make<P<Expr>>,
    {
        let label = label.map(|l| l.make(&self));
        let value = value.map(|v| v.make(&self));
        P(Expr {
            id: DUMMY_NODE_ID,
            kind: ExprKind::Break(label, value),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn arg<T, Pt>(self, ty: T, pat: Pt) -> Param
    where
        T: Make<P<Ty>>,
        Pt: Make<P<Pat>>,
    {
        let ty = ty.make(&self);
        let pat = pat.make(&self);
        Param {
            attrs: ThinVec::new(),
            ty: ty,
            pat: pat,
            id: self.id,
            span: DUMMY_SP,
            is_placeholder: false,
        }
    }

    pub fn self_arg<S>(self, kind: S) -> Param
    where
        S: Make<SelfKind>,
    {
        let eself = dummy_spanned(kind.make(&self));
        let ident = "self".make(&self);
        let attrs = ThinVec::new();
        Param::from_self(attrs, eself, ident)
    }

    pub fn ty_param<I>(self, ident: I) -> GenericParam
    where
        I: Make<Ident>,
    {
        let ident = ident.make(&self);
        GenericParam {
            attrs: self.attrs.into(),
            ident: ident,
            id: self.id,
            bounds: vec![],
            kind: GenericParamKind::Type { default: None },
            is_placeholder: false,
            colon_span: None,
        }
    }

    pub fn ty<T>(self, kind: TyKind) -> Ty {
        Ty {
            id: self.id,
            kind,
            span: self.span,
            tokens: None,
        }
    }

    pub fn attribute<Pa, Ma>(self, style: AttrStyle, path: Pa, args: Ma) -> Attribute
    where
        Pa: Make<Path>,
        Ma: Make<MacArgs>,
    {
        let path = path.make(&self);
        let args = args.make(&self).into();
        Attribute {
            id: AttrId::from_u32(0),
            style,
            kind: AttrKind::Normal(
                AttrItem {
                    path,
                    args,
                    tokens: None,
                },
                None,
            ),
            span: self.span,
        }
    }

    pub fn meta_item_attr(mut self, style: AttrStyle, meta_item: MetaItem) -> Self {
        let mut attr = mk_attr_inner(meta_item);
        attr.style = style;
        self.attrs.push(attr);
        self
    }

    pub fn meta_item<I, K>(self, path: I, kind: K) -> MetaItem
    where
        I: Make<Path>,
        K: Make<MetaItemKind>,
    {
        let path = path.make(&self);
        let kind = kind.make(&self);
        MetaItem {
            path: path,
            kind: kind,
            span: DUMMY_SP,
        }
    }

    pub fn nested_meta_item<K>(self, kind: K) -> NestedMetaItem
    where
        K: Make<NestedMetaItem>,
    {
        kind.make(&self)
    }

    // Convert the current internal list of outer attributes
    // into a vector of inner attributes, e.g.:
    // `#[foo]` => `#![foo]`
    pub fn as_inner_attrs(self) -> AttrVec {
        self.attrs
            .into_iter()
            .map(|outer_attr| Attribute {
                style: AttrStyle::Inner,
                ..outer_attr
            })
            .collect::<AttrVec>()
    }

    pub fn into_attrs(self) -> AttrVec {
        self.attrs
    }

    pub fn empty_mac<Pa>(self, path: Pa) -> MacCall
    where
        Pa: Make<Path>,
    {
        let path = path.make(&self);
        MacCall {
            path: path,
            args: P(MacArgs::Empty),
            prior_type_ascription: None,
        }
    }

    pub fn mac<Pa, Ts>(self, func: Pa, arguments: Ts, delim: MacDelimiter) -> MacCall
    where
        Pa: Make<Path>,
        Ts: Make<TokenStream>,
    {
        let func: Path = func.make(&self);

        let args = MacArgs::Delimited(DelimSpan::dummy(), delim, arguments.make(&self));

        MacCall {
            path: func,
            args: P(args),
            prior_type_ascription: None,
        }
    }

    /// Create a local variable
    pub fn local<V, T, E>(self, pat: V, ty: Option<T>, init: Option<E>) -> Local
    where
        V: Make<P<Pat>>,
        T: Make<P<Ty>>,
        E: Make<P<Expr>>,
    {
        let pat = pat.make(&self);
        let ty = ty.map(|x| x.make(&self));
        let kind = match init {
            Some(x) => LocalKind::Init(x.make(&self)),
            None => LocalKind::Decl,
        };
        Local {
            id: self.id,
            pat,
            ty,
            kind,
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        }
    }

    pub fn return_expr<E>(self, val: Option<E>) -> P<Expr>
    where
        E: Make<P<Expr>>,
    {
        let val = val.map(|x| x.make(&self));
        P(Expr {
            id: self.id,
            kind: ExprKind::Ret(val),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn continue_expr<I>(self, label: Option<I>) -> P<Expr>
    where
        I: Make<Ident>,
    {
        let label = label.map(|l| Label {
            ident: l.make(&self),
        });

        P(Expr {
            id: self.id,
            kind: ExprKind::Continue(label),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn break_expr<I>(self, label: Option<I>) -> P<Expr>
    where
        I: Make<Ident>,
    {
        let label = label.map(|l| Label {
            ident: l.make(&self),
        });

        P(Expr {
            id: self.id,
            kind: ExprKind::Break(label, None),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }

    pub fn closure_expr<D, E>(
        self,
        capture: CaptureBy,
        mov: Movability,
        decl: D,
        body: E,
    ) -> P<Expr>
    where
        D: Make<P<FnDecl>>,
        E: Make<P<Expr>>,
    {
        let decl = decl.make(&self);
        let body = body.make(&self);
        P(Expr {
            id: self.id,
            kind: ExprKind::Closure(
                ClosureBinder::NotPresent,
                capture,
                Async::No,
                mov,
                decl,
                body,
                DUMMY_SP,
            ),
            span: self.span,
            attrs: self.attrs.into(),
            tokens: None,
        })
    }
}

pub fn mk() -> Builder {
    Builder::new()
}

/// Detect a cast that would create a syntax error when it was the left
/// argument to a less-than operator. This is a work-around for an upstream
/// libsyntax bug.
fn has_rightmost_cast(expr: &Expr) -> bool {
    match &expr.kind {
        &ExprKind::Cast(..) => true,
        &ExprKind::Unary(_, ref arg) => has_rightmost_cast(&**arg),
        &ExprKind::Binary(_, _, ref rhs) => has_rightmost_cast(&**rhs),
        _ => false,
    }
}
