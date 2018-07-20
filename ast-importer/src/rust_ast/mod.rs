//! Helpers for building AST nodes.  Normally used by calling `mk().some_node(args...)`.
use syntax::ast::*;
use syntax_pos::symbol::Symbol;
use syntax::codemap::{DUMMY_SP, dummy_spanned, Span};
use syntax::parse::token::{self, Token, DelimToken};
use syntax::attr::{mk_attr_inner};
use syntax::ptr::P;
use syntax::tokenstream::{TokenTree, TokenStream, TokenStreamBuilder, ThinTokenStream};
use syntax::symbol::keywords;
use std::rc::Rc;
use rustc_target::spec::abi::{lookup, Abi};

pub mod comment_store;
pub mod traverse;

/// Conversion of string-like values into interned `Symbol`s.
pub trait IntoSymbol {
    fn into_symbol(self) -> Symbol;
}

impl IntoSymbol for Symbol {
    fn into_symbol(self) -> Symbol {
        self
    }
}

impl<'a> IntoSymbol for &'a str {
    fn into_symbol(self) -> Symbol {
        Symbol::intern(self)
    }
}

impl IntoSymbol for String {
    fn into_symbol(self) -> Symbol {
        Symbol::intern(&self)
    }
}

impl<'a> IntoSymbol for &'a String {
    fn into_symbol(self) -> Symbol {
        <&str as IntoSymbol>::into_symbol(self)
    }
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


impl<S: IntoSymbol> Make<Ident> for S {
    fn make(self, _mk: &Builder) -> Ident {
        Ident::with_empty_ctxt(self.into_symbol())
    }
}

impl<L: Make<Ident>> Make<Label> for L {
    fn make(self, mk: &Builder) -> Label {
        Label { ident: self.make(mk) }
    }
}


impl<'a> Make<Visibility> for &'a str {
    fn make(self, _mk: &Builder) -> Visibility {
        let kind = match self {
            "pub" => VisibilityKind::Public,
            "priv" | "" | "inherit" => VisibilityKind::Inherited,
            "crate" => VisibilityKind::Crate(CrateSugar::JustCrate),
            "pub(crate)" => VisibilityKind::Crate(CrateSugar::PubCrate),
            _ => panic!("unrecognized string for Visibility: {:?}", self),
        };
        dummy_spanned(kind)
    }
}

impl<'a> Make<Abi> for &'a str {
    fn make(self, _mk: &Builder) -> Abi {
        lookup(self).expect(&format!("unrecognized string for Abi: {:?}", self))
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


impl<I: Make<Ident>> Make<PathSegment> for I {
    fn make(self, mk: &Builder) -> PathSegment {
        PathSegment {
            ident: self.make(mk),
            parameters: None,
        }
    }
}


impl<S: Make<PathSegment>> Make<Path> for Vec<S> {
    fn make(self, mk: &Builder) -> Path {
        Path {
            span: DUMMY_SP,
            segments: self.into_iter().map(|s| s.make(mk)).collect(),
        }
    }
}


impl Make<ThinTokenStream> for TokenStream {
    fn make(self, _mk: &Builder) -> ThinTokenStream {
        self.into()
    }
}

impl Make<ThinTokenStream> for Vec<TokenTree> {
    fn make(self, _mk: &Builder) -> ThinTokenStream {
        self.into_iter().collect::<TokenStream>().into()
    }
}

impl Make<TokenTree> for Token {
    fn make(self, _mk: &Builder) -> TokenTree {
        TokenTree::Token(DUMMY_SP, self)
    }
}

impl Make<PathParameters> for AngleBracketedParameterData {
    fn make(self, _mk: &Builder) -> PathParameters {
        AngleBracketed(self)
    }
}

impl Make<PathParameters> for ParenthesizedParameterData {
    fn make(self, _mk: &Builder) -> PathParameters {
        Parenthesized(self)
    }
}

impl Make<NestedMetaItemKind> for MetaItem {
    fn make(self, _mk: &Builder) -> NestedMetaItemKind {
        NestedMetaItemKind::MetaItem(self)
    }
}

impl Make<NestedMetaItemKind> for Lit {
    fn make(self, _mk: &Builder) -> NestedMetaItemKind {
        NestedMetaItemKind::Literal(self)
    }
}

impl Make<MetaItemKind> for Lit {
    fn make(self, _mk: &Builder) -> MetaItemKind {
        MetaItemKind::NameValue(self)
    }
}

#[derive(Clone)]
pub struct Builder {
    // The builder holds a set of "modifiers", such as visibility and mutability.  Functions for
    // building AST nodes don't take arguments of these types, but instead use any applicable
    // modifiers from the builder to set the node's visibility, mutability, etc.
    vis: Visibility,
    mutbl: Mutability,
    generics: Generics,
    unsafety: Unsafety,
    constness: Constness,
    abi: Abi,
    attrs: Vec<Attribute>,
    span: Span,
}

#[allow(dead_code)]
impl Builder {
    pub fn new() -> Builder {
        Builder {
            vis: dummy_spanned(VisibilityKind::Inherited),
            mutbl: Mutability::Immutable,
            generics: Generics::default(),
            unsafety: Unsafety::Normal,
            constness: Constness::NotConst,
            abi: Abi::Rust,
            attrs: Vec::new(),
            span: DUMMY_SP,
        }
    }


    // Modifier updates.

    pub fn vis<V: Make<Visibility>>(self, vis: V) -> Self {
        let vis = vis.make(&self);
        Builder {
            vis: vis,
            ..self
        }
    }

    pub fn pub_(self) -> Self {
        self.vis("pub")
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

    pub fn abi<A: Make<Abi>>(self, abi: A) -> Self {
        let abi = abi.make(&self);
        Builder {
            abi: abi,
            ..self
        }
    }

    pub fn span<S: Make<Span>>(self, span: S) -> Self {
        let span = span.make(&self);
        Builder {
            span: span,
            ..self
        }
    }


    pub fn str_attr<K, V>(self, key: K, value: V) -> Self
        where K: Make<PathSegment>, V: IntoSymbol {
        let key = vec![key].make(&self);

        let mut attrs = self.attrs;
        attrs.push(Attribute {
            id: AttrId(0),
            style: AttrStyle::Outer,
            path: key,
            tokens: vec![
                Token::Eq,
                Token::Literal(token::Lit::Str_(value.into_symbol()), None),
            ].into_iter().collect(),
            is_sugared_doc: false,
            span: DUMMY_SP,
        });
        Builder {
            attrs: attrs,
            ..self
        }
    }

    pub fn single_attr<K>(self, key: K) -> Self
        where K: Make<PathSegment> {
        let key: Path = vec![key].make(&self);

        let mut attrs = self.attrs;
        attrs.push(Attribute {
            id: AttrId(0),
            style: AttrStyle::Outer,
            path: key,
            tokens: TokenStream::empty(),
            is_sugared_doc: false,
            span: DUMMY_SP,
        });
        Builder {
            attrs: attrs,
            ..self
        }
    }

    pub fn call_attr<K,V>(self, func: K, arguments: Vec<V>) -> Self
        where K: Make<PathSegment>, V: Make<Ident> {

        let func: Path = vec![func].make(&self);

        let tokens: TokenStream = {
            let mut builder = TokenStreamBuilder::new();
            builder.push(Token::OpenDelim(DelimToken::Paren));

            let mut is_first = true;
            for argument in arguments {
                if is_first {
                    is_first = false;
                } else {
                    builder.push(Token::Comma);
                }

                let argument: Ident = argument.make(&self);
                builder.push(Token::from_ast_ident(argument));
            }

            builder.push(Token::CloseDelim(DelimToken::Paren));
            builder.build()
        };

        let mut attrs = self.attrs;
        attrs.push(Attribute {
            id: AttrId(0),
            style: AttrStyle::Outer,
            path: func,
            tokens: tokens,
            is_sugared_doc: false,
            span: DUMMY_SP,
        });
        Builder {
            attrs: attrs,
            ..self
        }
    }

    // Path segments with parameters

    pub fn path_segment_with_params<I,P>(self, identifier: I, parameters: P) -> PathSegment
        where I: Make<Ident>, P: Make<PathParameters> {
        let identifier = identifier.make(&self);
        let parameters = parameters.make(&self);
        PathSegment {
            ident: identifier,
            parameters: Some(P(parameters)),
        }
    }

    pub fn parenthesized_param_types<Ps>(self, params: Ps) -> ParenthesizedParameterData
        where Ps: Make<Vec<P<Ty>>> {

        let params = params.make(&self);
        ParenthesizedParameterData {
            span: DUMMY_SP,
            inputs: params,
            output: None,
        }
    }

    pub fn angle_bracketed_param_types<Ps>(self, params: Ps) -> AngleBracketedParameterData
        where Ps: Make<Vec<P<Ty>>> {

        let params = params.make(&self);
        AngleBracketedParameterData {
            span: DUMMY_SP,
            lifetimes: vec![],
            types: params,
            bindings: vec![],
        }
    }

    // Simple nodes

    pub fn ident<I>(self, name: I) -> Ident
        where I: Make<Ident> {
        name.make(&self)
    }

    pub fn path_segment<S>(self, seg: S) -> PathSegment
        where S: Make<PathSegment> {
        seg.make(&self)
    }

    pub fn path<Pa>(self, path: Pa) -> Path
        where Pa: Make<Path> {
        path.make(&self)
    }

    pub fn anon_const<E>(self, expr: E) -> AnonConst
        where E: Make<P<Expr>> {
        AnonConst {
            id: DUMMY_NODE_ID,
            value: expr.make(&self),
        }
    }

    // Exprs
    // These are sorted in the same order as the corresponding ExprKind variants, with additional
    // variant-specific details following each variant.

    pub fn array_expr<A>(self, args: Vec<A>) -> P<Expr>
        where A: Make<P<Expr>> {
        let args = args.into_iter().map(|a| a.make(&self)).collect();
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Array(args),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn call_expr<F, A>(self, func: F, args: Vec<A>) -> P<Expr>
        where F: Make<P<Expr>>, A: Make<P<Expr>> {
        let func = func.make(&self);
        let args = args.into_iter().map(|a| a.make(&self)).collect();
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Call(func, args),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn method_call_expr<E, S, A>(self, expr: E, seg: S, args: Vec<A>) -> P<Expr>
        where E: Make<P<Expr>>, S: Make<PathSegment>, A: Make<P<Expr>> {
        let expr = expr.make(&self);
        let seg = seg.make(&self);

        let mut all_args = Vec::with_capacity(args.len() + 1);
        all_args.push(expr);
        for arg in args {
            all_args.push(arg.make(&self));
        }

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::MethodCall(seg, all_args),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn tuple_expr<E>(self, exprs: Vec<E>) -> P<Expr> where E: Make<P<Expr>> {
        let exprs: Vec<P<Expr>> = exprs.into_iter().map(|x| x.make(&self)).collect();
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Tup(exprs),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn binary_expr<O, E>(self, op: O, lhs: E, rhs: E) -> P<Expr>
        where O: Make<BinOpKind>, E: Make<P<Expr>> {
        let op = op.make(&self);
        let mut lhs = lhs.make(&self);
        let rhs = rhs.make(&self);

        match op {
            BinOpKind::Lt | BinOpKind::Shl if has_rightmost_cast(&*lhs) =>
            lhs = mk().paren_expr(lhs),
            _ => {}
        }

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Binary(dummy_spanned(op), lhs, rhs),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn unary_expr<O, E>(self, op: O, a: E) -> P<Expr>
        where O: Make<UnOp>, E: Make<P<Expr>> {
        let op = op.make(&self);
        let a = a.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Unary(op, a),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn lit_expr<L>(self, lit: L) -> P<Expr>
        where L: Make<P<Lit>> {
        let lit = lit.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Lit(lit),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn cast_expr<E, T>(self, e: E, t: T) -> P<Expr>
        where E: Make<P<Expr>>, T: Make<P<Ty>> {
        let mut e = e.make(&self);
        let t = t.make(&self);

        // Workaround for a bug in libsyntax require us to emit extra parentheses
        let cast_if = if let ExprKind::If(..) = e.node { true } else { false };
        if cast_if { e = mk().paren_expr(e); }

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Cast(e, t),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn type_expr<E, T>(self, e: E, t: T) -> P<Expr>
        where E: Make<P<Expr>>, T: Make<P<Ty>> {
        let e = e.make(&self);
        let t = t.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Type(e, t),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn block_expr<B>(self, blk: B) -> P<Expr>
        where B: Make<P<Block>> {
        let blk = blk.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Block(blk, None),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn assign_expr<E1, E2>(self, lhs: E1, rhs: E2) -> P<Expr>
        where E1: Make<P<Expr>>, E2: Make<P<Expr>> {
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Assign(lhs, rhs),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn assign_op_expr<O, E1, E2>(self, op: O, lhs: E1, rhs: E2) -> P<Expr>
        where O: Make<BinOpKind>, E1: Make<P<Expr>>, E2: Make<P<Expr>> {
        let op = dummy_spanned(op.make(&self));
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::AssignOp(op, lhs, rhs),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn index_expr<E1, E2>(self, lhs: E1, rhs: E2) -> P<Expr>
        where E1: Make<P<Expr>>, E2: Make<P<Expr>> {
        let lhs = lhs.make(&self);
        let rhs = rhs.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Index(lhs, rhs),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn path_expr<Pa>(self, path: Pa) -> P<Expr>
        where Pa: Make<Path> {
        self.qpath_expr(None, path)
    }

    pub fn qpath_expr<Pa>(self, qself: Option<QSelf>, path: Pa) -> P<Expr>
        where Pa: Make<Path> {
        let path = path.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Path(qself, path),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    /// An array literal constructed from one repeated element.
    /// `[expr; n]`
    pub fn repeat_expr<E, N>(self, expr: E, n: N) -> P<Expr>
        where E: Make<P<Expr>>, N: Make<P<Expr>> {
        let expr = expr.make(&self);
        let n = mk().anon_const(n.make(&self));
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Repeat(expr, n),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn paren_expr<E>(self, e: E) -> P<Expr>
        where E: Make<P<Expr>> {
        let e = e.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Paren(e),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    // Special case of path_expr
    pub fn ident_expr<I>(self, name: I) -> P<Expr>
        where I: Make<Ident> {
        self.path_expr(vec![name])
    }

    pub fn addr_of_expr<E>(self, e: E) -> P<Expr>
        where E: Make<P<Expr>> {
        let e = e.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::AddrOf(self.mutbl, e),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn mac_expr<M>(self, mac: M) -> P<Expr>
        where M: Make<Mac> {
        let mut mac = mac.make(&self);
        mac.node.delim = MacDelimiter::Parenthesis;
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Mac(mac),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn struct_expr<Pa>(self, path: Pa, fields: Vec<Field>) -> P<Expr>
        where Pa: Make<Path> {
        let path = path.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Struct(path, fields, None),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    // struct_expr, but with optional base expression
    pub fn struct_expr_base<Pa, E>(self, path: Pa, fields: Vec<Field>, base: Option<E>) -> P<Expr>
        where Pa: Make<Path>, E: Make<P<Expr>> {
        let path = path.make(&self);
        let base = base.map(|e| e.make(&self));
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Struct(path, fields, base),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn field_expr<E, F>(self, val: E, field: F) -> P<Expr>
        where E: Make<P<Expr>>, F: Make<Ident> {
        let val = val.make(&self);
        let field = field.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Field(val, field),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }


    pub fn field<I, E>(self, ident: I, expr: E) -> Field
        where I: Make<Ident>, E: Make<P<Expr>> {
        let ident = ident.make(&self);
        let expr = expr.make(&self);
        Field {
            ident,
            expr,
            span: DUMMY_SP,
            is_shorthand: false,
            attrs: self.attrs.into(),
        }
    }

    pub fn match_expr<E>(self, cond: E, arms: Vec<Arm>) -> P<Expr>
        where E: Make<P<Expr>> {
        let cond = cond.make(&self);
        let arms = arms.into_iter().map(|arm| arm.make(&self)).collect();
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Match(cond, arms),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn arm<Pa, E>(self, pats: Vec<Pa>, guard: Option<E>, body: E) -> Arm
        where E: Make<P<Expr>>, Pa: Make<P<Pat>> {
        let pats = pats.into_iter().map(|pat| pat.make(&self)).collect();
        let guard = guard.map(|g| g.make(&self));
        let body = body.make(&self);
        Arm {
            attrs: self.attrs.into(),
            pats: pats,
            guard,
            body,
        }

    }


    // Literals

    pub fn bytestr_lit(self, s: Vec<u8>) -> P<Lit> {
        P(Lit {
            node: LitKind::ByteStr(Rc::new(s)),
            span: DUMMY_SP,
        })
    }

    pub fn str_lit<S>(self, s: S) -> P<Lit>
        where S: IntoSymbol {
        let s = s.into_symbol();
        P(Lit {
            node: LitKind::Str(s, StrStyle::Cooked),
            span: DUMMY_SP,
        })
    }

    pub fn byte_lit(self, b: u8) -> P<Lit> {
        P(Lit {
            node: LitKind::Byte(b),
            span: DUMMY_SP,
        })
    }

    pub fn char_lit(self, c: char) -> P<Lit> {
        P(Lit {
            node: LitKind::Char(c),
            span: DUMMY_SP,
        })
    }

    pub fn int_lit<T>(self, i: u128, ty: T) -> P<Lit>
        where T: Make<LitIntType> {
        let ty = ty.make(&self);
        P(Lit {
            node: LitKind::Int(i, ty),
            span: DUMMY_SP,
        })
    }

    pub fn float_lit<S, T>(self, s: S, ty: T) -> P<Lit>
        where S: IntoSymbol, T: Make<FloatTy> {
        let s = s.into_symbol();
        let ty = ty.make(&self);
        P(Lit {
            node: LitKind::Float(s, ty),
            span: DUMMY_SP,
        })
    }

    pub fn float_unsuffixed_lit<S>(self, s: S) -> P<Lit>
        where S: IntoSymbol {
        let s = s.into_symbol();
        P(Lit {
            node: LitKind::FloatUnsuffixed(s),
            span: DUMMY_SP,
        })
    }

    pub fn bool_lit(self, b: bool) -> P<Lit> {
        P(Lit {
            node: LitKind::Bool(b),
            span: DUMMY_SP,
        })
    }

    pub fn ifte_expr<C, T, E>(self, cond: C, then_case: T, else_case: Option<E>) -> P<Expr>
        where C: Make<P<Expr>>, T: Make<P<Block>>, E: Make<P<Expr>> {
        let cond = cond.make(&self);
        let then_case = then_case.make(&self);
        let else_case =
            else_case.map(|x| {

                let e = x.make(&self);

                // The else branch in libsyntax must be one of these three cases,
                // otherwise we have to manually add the block around the else expression
                match e.node {
                    ExprKind::If{..} | ExprKind::IfLet{..} | ExprKind::Block{..} => e,
                    _ => mk().block_expr(mk().block(vec![mk().expr_stmt(e)])),
                }
            });

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::If(cond, then_case, else_case),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn while_expr<C, B, L>(self, cond: C, body: B, label: Option<L>) -> P<Expr>
        where C: Make<P<Expr>>, B: Make<P<Block>>, L: Make<Label> {
        let cond = cond.make(&self);
        let body = body.make(&self);
        let label = label.map(|l| l.make(&self));

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::While(cond, body, label),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn loop_expr<B, L>(self, body: B, label: Option<L>) -> P<Expr>
        where B: Make<P<Block>>, L: Make<Label> {
        let body = body.make(&self);
        let label = label.map(|l| l.make(&self));

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Loop(body, label),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn for_expr<Pa, E, B, L>(self, pat: Pa, expr: E, body: B, label: Option<L>) -> P<Expr>
        where Pa: Make<P<Pat>>, E: Make<P<Expr>>, B: Make<P<Block>>, L: Make<Label> {
        let pat = pat.make(&self);
        let expr = expr.make(&self);
        let body = body.make(&self);
        let label = label.map(|l| l.make(&self));

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::ForLoop(pat, expr, body, label),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }


    // Patterns

    pub fn ident_pat<I>(self, name: I) -> P<Pat>
        where I: Make<Ident> {
        let name = name.make(&self);
        P(Pat {
            id: DUMMY_NODE_ID,
            node: PatKind::Ident(BindingMode::ByValue(self.mutbl), name, None),
            span: DUMMY_SP,
        })
    }

    pub fn tuple_pat<Pa>(self, pats: Vec<Pa>) -> P<Pat> where Pa: Make<P<Pat>> {
        let pats: Vec<P<Pat>> = pats.into_iter().map(|x| x.make(&self)).collect();
        P(Pat {
            id: DUMMY_NODE_ID,
            node: PatKind::Tuple(pats, None),
            span: DUMMY_SP,
        })
    }

    pub fn qpath_pat<Pa>(self, qself: Option<QSelf>, path: Pa) -> P<Pat>
        where Pa: Make<Path> {
        let path = path.make(&self);
        P(Pat {
            id: DUMMY_NODE_ID,
            node: PatKind::Path(qself, path),
            span: DUMMY_SP,
        })
    }

    pub fn wild_pat(self) -> P<Pat> {
        P(Pat {
            id: DUMMY_NODE_ID,
            node: PatKind::Wild,
            span: DUMMY_SP,
        })
    }

    pub fn lit_pat<L>(self, lit: L) -> P<Pat>
        where L: Make<P<Expr>> {
        let lit = lit.make(&self);
        P(Pat {
            id: DUMMY_NODE_ID,
            node: PatKind::Lit(lit),
            span: DUMMY_SP,
        })
    }

    pub fn ident_ref_pat<I>(self, name: I) -> P<Pat>
        where I: Make<Ident> {
        let name = name.make(&self);
        P(Pat {
            id: DUMMY_NODE_ID,
            node: PatKind::Ident(BindingMode::ByRef(self.mutbl), name, None),
            span: DUMMY_SP,
        })
    }


    // Types

    pub fn barefn_ty<T>(self, decl: T) -> P<Ty>
        where T: Make<P<FnDecl>> {

        let decl = decl.make(&self);

        let barefn = BareFnTy {
            unsafety: self.unsafety,
            abi: self.abi,
            generic_params: vec![],
            decl,
        };

        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::BareFn(P(barefn)),
            span: DUMMY_SP,
        })

    }

    pub fn array_ty<T, E>(self, ty: T, len: E) -> P<Ty>
        where T: Make<P<Ty>>, E: Make<P<Expr>> {
        let ty = ty.make(&self);
        let len = mk().anon_const(len.make(&self));
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Array(ty, len),
            span: DUMMY_SP,
        })
    }

    pub fn slice_ty<T>(self, ty: T) -> P<Ty>
        where T: Make<P<Ty>> {
        let ty = ty.make(&self);
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Slice(ty),
            span: DUMMY_SP,
        })
    }

    pub fn ptr_ty<T>(self, ty: T) -> P<Ty>
        where T: Make<P<Ty>> {
        let ty = ty.make(&self);
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Ptr(MutTy { ty: ty, mutbl: self.mutbl }),
            span: DUMMY_SP,
        })
    }

    pub fn ref_ty<T>(self, ty: T) -> P<Ty>
        where T: Make<P<Ty>> {
        let ty = ty.make(&self);
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Rptr(None, MutTy { ty: ty, mutbl: self.mutbl }),
            span: DUMMY_SP,
        })
    }

    pub fn ref_lt_ty<L,T>(self, lt: L, ty: T) -> P<Ty>
        where L: Make<Lifetime>, T: Make<P<Ty>> {
        let lt = lt.make(&self);
        let ty = ty.make(&self);
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Rptr(Some(lt), MutTy { ty: ty, mutbl: self.mutbl }),
            span: DUMMY_SP,
        })
    }

    pub fn never_ty(self) -> P<Ty> {
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Never,
            span: DUMMY_SP,
        })
    }

    pub fn tuple_ty<T>(self, elem_tys: Vec<T>) -> P<Ty>
        where T: Make<P<Ty>> {
        let elem_tys = elem_tys.into_iter().map(|ty| ty.make(&self)).collect();
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Tup(elem_tys),
            span: DUMMY_SP,
        })
    }

    pub fn path_ty<Pa>(self, path: Pa) -> P<Ty>
        where Pa: Make<Path> {
        self.qpath_ty(None, path)
    }

    pub fn qpath_ty<Pa>(self, qself: Option<QSelf>, path: Pa) -> P<Ty>
        where Pa: Make<Path> {
        let path = path.make(&self);
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Path(qself, path),
            span: DUMMY_SP,
        })
    }

    pub fn ident_ty<I>(self, name: I) -> P<Ty>
        where I: Make<Ident> {
        self.path_ty(vec![name])
    }

    pub fn infer_ty(self) -> P<Ty> {
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Infer,
            span: DUMMY_SP,
        })
    }


    // Stmts

    pub fn local_stmt<L>(self, local: L) -> Stmt
        where L: Make<P<Local>> {
        let local = local.make(&self);
        Stmt {
            id: DUMMY_NODE_ID,
            node: StmtKind::Local(local),
            span: self.span,
        }
    }

    pub fn expr_stmt<E>(self, expr: E) -> Stmt
        where E: Make<P<Expr>> {
        let expr = expr.make(&self);
        Stmt {
            id: DUMMY_NODE_ID,
            node: StmtKind::Expr(expr),
            span: self.span,
        }
    }

    pub fn semi_stmt<E>(self, expr: E) -> Stmt
        where E: Make<P<Expr>> {
        let expr = expr.make(&self);
        Stmt {
            id: DUMMY_NODE_ID,
            node: StmtKind::Semi(expr),
            span: self.span,
        }
    }

    pub fn item_stmt<I>(self, item: I)  -> Stmt
        where I: Make<P<Item>> {
        let item = item.make(&self);
        Stmt {
            id: DUMMY_NODE_ID,
            node: StmtKind::Item(item),
            span: self.span,
        }
    }


    // Items

    fn item(name: Ident, attrs: Vec<Attribute>, vis: Visibility, span: Span, node: ItemKind) -> P<Item> {
        P(Item {
            ident: name,
            attrs: attrs,
            id: DUMMY_NODE_ID,
            node: node,
            vis: vis,
            span: span,
            tokens: None,
        })
    }

    pub fn static_item<I, T, E>(self, name: I, ty: T, init: E) -> P<Item>
        where I: Make<Ident>, T: Make<P<Ty>>, E: Make<P<Expr>> {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.make(&self);
        Self::item(name, self.attrs, self.vis, self.span,
                   ItemKind::Static(ty, self.mutbl, init))
    }

    pub fn const_item<I, T, E>(self, name: I, ty: T, init: E) -> P<Item>
        where I: Make<Ident>, T: Make<P<Ty>>, E: Make<P<Expr>> {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.make(&self);
        Self::item(name, self.attrs, self.vis, self.span, ItemKind::Const(ty, init))
    }

    pub fn fn_item<I, D, B>(self, name: I, decl: D, block: B) -> P<Item>
        where I: Make<Ident>, D: Make<P<FnDecl>>, B: Make<P<Block>> {
        let name = name.make(&self);
        let decl = decl.make(&self);
        let block = block.make(&self);
        Self::item(name, self.attrs, self.vis, self.span,
                   ItemKind::Fn(decl,
                                self.unsafety,
                                dummy_spanned(self.constness),
                                self.abi,
                                self.generics,
                                block))
    }

    pub fn fn_decl(self, inputs: Vec<Arg>, output: FunctionRetTy, variadic: bool) -> P<FnDecl>
    {
        P(FnDecl {
            inputs,
            output,
            variadic,
        })
    }

    pub fn struct_item<I>(self, name: I, fields: Vec<StructField>) -> P<Item>
        where I: Make<Ident> {
        let name = name.make(&self);
        Self::item(name, self.attrs, self.vis, self.span,
                   ItemKind::Struct(VariantData::Struct(fields, DUMMY_NODE_ID),
                                    self.generics))
    }

    pub fn struct_field<I, T>(self, ident: I, ty: T) -> StructField
        where I: Make<Ident>, T: Make<P<Ty>> {
        let ident = ident.make(&self);
        let ty = ty.make(&self);
        StructField {
            span: DUMMY_SP,
            ident: Some(ident),
            vis: self.vis,
            id: DUMMY_NODE_ID,
            ty: ty,
            attrs: self.attrs,
        }
    }

    pub fn union_item<I>(self, name: I, fields: Vec<StructField>) -> P<Item>
        where I: Make<Ident> {
        let name = name.make(&self);
        Self::item(name, self.attrs, self.vis, self.span,
                   ItemKind::Union(VariantData::Struct(fields, DUMMY_NODE_ID),
                                    self.generics))
    }

    pub fn enum_item<I>(self, name: I, fields: Vec<Variant>) -> P<Item>
        where I: Make<Ident> {
        let name = name.make(&self);
        Self::item(name, self.attrs, self.vis, self.span,
                   ItemKind::Enum(EnumDef { variants: fields }, self.generics))
    }

    pub fn enum_field<T>(self, ty: T) -> StructField
        where T: Make<P<Ty>> {
        let ty = ty.make(&self);
        StructField {
            span: DUMMY_SP,
            ident: None,
            vis: self.vis,
            id: DUMMY_NODE_ID,
            ty: ty,
            attrs: self.attrs,
        }
    }

    pub fn type_item<I, T>(self, name: I, ty: T) -> P<Item>
        where I: Make<Ident>, T: Make<P<Ty>> {
        let ty = ty.make(&self);
        let name = name.make(&self);
        let kind = ItemKind::Ty(ty, self.generics);
        Self::item(name, self.attrs, self.vis, self.span, kind)
    }

    pub fn variant<I>(self, ident: I, data: VariantData) -> Variant
        where I: Make<Ident> {
        let ident = ident.make(&self);
        dummy_spanned(Variant_ {
            ident,
            attrs: self.attrs,
            data,
            disr_expr: None,
        })
    }

    pub fn unit_variant<I,E>(self, name: I, disr_expr: Option<E>) -> Variant
        where I: Make<Ident>, E: Make<P<Expr>> {
        let ident = name.make(&self);
        let disr_expr = disr_expr.map(|d| mk().anon_const(d.make(&self)));
        dummy_spanned(Variant_ {
            ident,
            attrs: self.attrs,
            data: VariantData::Unit(DUMMY_NODE_ID),
            disr_expr,
        })
    }

    pub fn impl_item<T>(self, ty: T, items: Vec<ImplItem>) -> P<Item>
        where T: Make<P<Ty>>
    {
        let ty = ty.make(&self);
        Self::item(keywords::Invalid.ident(), self.attrs, self.vis, self.span,
                   ItemKind::Impl(self.unsafety,
                                  ImplPolarity::Positive,
                                  Defaultness::Final,
                                  self.generics,
                                  None, // not a trait implementation
                                  ty,
                                  items))
    }

    pub fn extern_crate_item<I>(self, name: I, rename: Option<I>) -> P<Item>
        where I: Make<Ident>
    {
        let name = name.make(&self);
        let rename = rename.map(|n| n.make(&self).name);
        Self::item(name, self.attrs, self.vis, self.span, ItemKind::ExternCrate(rename))
    }

    // `use <path>;` item
    // TODO: for now, we only support simple paths with an optional rename;
    // if we ever need them, we should add support for globs and nested trees,
    // e.g., `use foo::*;` and `use foo::{a, b, c};`
    pub fn use_item<Pa, I>(self, path: Pa, rename: Option<I>) -> P<Item>
        where Pa: Make<Path>, I: Make<Ident>
    {
        let path = path.make(&self);
        let rename = rename.map(|n| n.make(&self));
        let use_tree = UseTree {
            span: DUMMY_SP,
            prefix: path,
            kind: UseTreeKind::Simple(rename, DUMMY_NODE_ID, DUMMY_NODE_ID),
        };
        Self::item(keywords::Invalid.ident(), self.attrs, self.vis, self.span,
                   ItemKind::Use(P(use_tree)))
    }

    pub fn foreign_items(self, items: Vec<ForeignItem>) -> P<Item>
    {
        let fgn_mod = ForeignMod { abi: self.abi, items };
        Self::item(keywords::Invalid.ident(), self.attrs, self.vis, self.span, ItemKind::ForeignMod(fgn_mod))
    }

    pub fn module<I, T>(self, name: I, items: T) -> P<Item>
        where I: Make<Ident>, T: Make<Vec<P<Item>>> {

        let name = name.make(&self);
        let items = items.make(&self);
        let module = Mod {
            inner: DUMMY_SP,
            items: items,
        };
        Self::item(name, self.attrs, self.vis, self.span, ItemKind::Mod(module))
    }

    // Foreign Items

    fn foreign_item(name: Ident, attrs: Vec<Attribute>, vis: Visibility, span: Span, node: ForeignItemKind) -> ForeignItem {
        ForeignItem {
            ident: name,
            attrs: attrs,
            id: DUMMY_NODE_ID,
            node: node,
            vis: vis,
            span: span,
        }
    }

    pub fn foreign_fn<I, D>(self, name: I, decl: D) -> ForeignItem
        where I: Make<Ident>, D: Make<P<FnDecl>> {
        let name = name.make(&self);
        let decl = decl.make(&self);
        Self::foreign_item(name, self.attrs, self.vis, self.span, ForeignItemKind::Fn(decl, self.generics))
    }

    pub fn foreign_static<I, T>(self, name: I, ty: T) -> ForeignItem
        where I: Make<Ident>, T: Make<P<Ty>> {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let is_mut = self.mutbl == Mutability::Mutable;
        Self::foreign_item(name, self.attrs, self.vis, self.span, ForeignItemKind::Static(ty, is_mut))
    }

    pub fn foreign_ty<I>(self, name: I) -> ForeignItem
        where I: Make<Ident> {
        let name = name.make(&self);
        Self::foreign_item(name, self.attrs, self.vis, self.span, ForeignItemKind::Ty)
    }


    // Misc nodes

    pub fn block<S>(self, stmts: Vec<S>) -> P<Block>
        where S: Make<Stmt> {
        let stmts = stmts.into_iter().map(|s| s.make(&self)).collect();
        P(Block {
            stmts: stmts,
            id: DUMMY_NODE_ID,
            rules: match self.unsafety {
                Unsafety::Unsafe => BlockCheckMode::Unsafe(UnsafeSource::UserProvided),
                Unsafety::Normal => BlockCheckMode::Default,
            },
            span: DUMMY_SP,
            recovered: false,
        })
    }

    pub fn arg<T, Pt>(self, ty: T, pat: Pt) -> Arg
        where T: Make<P<Ty>>, Pt: Make<P<Pat>> {
        let ty = ty.make(&self);
        let pat = pat.make(&self);
        Arg {
            ty: ty,
            pat: pat,
            id: DUMMY_NODE_ID,
        }
    }

    pub fn self_arg<S>(self, kind: S) -> Arg
        where S: Make<SelfKind> {
        let kind = kind.make(&self);
        let eself = dummy_spanned(kind);
        let ident = "self".make(&self);
        Arg::from_self(eself, ident)
    }

    pub fn ty_param<I>(self, ident: I) -> TyParam
        where I: Make<Ident> {
        let ident = ident.make(&self);
        TyParam {
            attrs: self.attrs.into(),
            ident: ident,
            id: DUMMY_NODE_ID,
            bounds: vec![],
            default: None,
        }
    }

    pub fn ty<T>(self, node: TyKind) -> Ty {
        Ty {
            id: DUMMY_NODE_ID,
            node,
            span: DUMMY_SP,
        }
    }

    pub fn attribute<Pa, Ts>(self, style: AttrStyle, path: Pa, tokens: Ts) -> Attribute
        where Pa: Make<Path>, Ts: Make<ThinTokenStream>
    {
        let path = path.make(&self);
        let tokens = tokens.make(&self).into();
        Attribute {
            id: AttrId(0),
            style,
            path,
            tokens,
            is_sugared_doc: false,
            span: DUMMY_SP,
        }
    }

    pub fn meta_item_attr(mut self, style: AttrStyle, meta_item: MetaItem) -> Self
    {
        let mut attr = mk_attr_inner(DUMMY_SP, AttrId(0), meta_item);
        attr.style = style;
        self.attrs.push(attr);
        self
    }

    pub fn meta_item<I,K>(self, path: I, kind: K) -> MetaItem
        where I: Make<Path>, K: Make<MetaItemKind> {
        
        let path = path.make(&self);
        let kind = kind.make(&self);
        MetaItem {
            ident: path,
            node: kind,
            span: DUMMY_SP,
        }
    }

    pub fn nested_meta_item<K>(self, kind: K) -> NestedMetaItem
        where K: Make<NestedMetaItemKind>
     {
        let kind = kind.make(&self);
        dummy_spanned(kind)
    }

    // Convert the current internal list of outer attributes
    // into a vector of inner attributes, e.g.:
    // `#[foo]` => `#![foo]`
    pub fn as_inner_attrs(self) -> Vec<Attribute> {
        self.attrs.into_iter().map(|outer_attr| {
            Attribute {
                style: AttrStyle::Inner,
                ..outer_attr
            }
        }).collect::<Vec<Attribute>>()
    }

    pub fn mac<Pa, Ts>(self, path: Pa, tts: Ts) -> Mac
        where Pa: Make<Path>, Ts: Make<ThinTokenStream> {
        let path = path.make(&self);
        let tts = tts.make(&self);
        dummy_spanned(Mac_ {
            delim: MacDelimiter::Brace,
            path: path,
            tts: tts,
        })
    }

    pub fn local<V, T, E>(self, pat: V, ty: Option<T>, init: Option<E>) -> Local
        where V: Make<P<Pat>>, T: Make<P<Ty>>, E: Make<P<Expr>> {
        let pat = pat.make(&self);
        let ty = ty.map(|x| x.make(&self));
        let init = init.map(|x| x.make(&self));
        Local {
            pat,
            ty,
            init,
            id: DUMMY_NODE_ID,
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        }
    }

    pub fn return_expr<E>(self, val: Option<E>) -> P<Expr>
        where E: Make<P<Expr>> {
        let val = val.map(|x| x.make(&self));
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Ret(val),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn continue_expr<L>(self, label: Option<L>) -> P<Expr>
        where L: Make<Label> {
        let label = label.map(|l| l.make(&self));

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Continue(label),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn break_expr<L>(self, label: Option<L>) -> P<Expr>
        where L: Make<Label> {
        let label = label.map(|l| l.make(&self));

        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Break(label, None),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
        })
    }

    pub fn closure_expr<D,E>(self, capture: CaptureBy, mov: Movability, decl: D, body: E) -> P<Expr>
        where D: Make<P<FnDecl>>, E: Make<P<Expr>>
    {
        let decl = decl.make(&self);
        let body = body.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Closure(capture, mov, decl, body, DUMMY_SP),
            span: DUMMY_SP,
            attrs: self.attrs.into(),
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
    match &expr.node {
        &ExprKind::Cast(..) => true,
        &ExprKind::Unary(_,ref arg) => has_rightmost_cast(&**arg),
        &ExprKind::Binary(_,_,ref rhs) => has_rightmost_cast(&**rhs),
        _ => false,
    }
}
