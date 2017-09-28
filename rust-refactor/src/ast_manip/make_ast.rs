//! Helpers for building AST nodes.  Normally used by calling `mk().some_node(args...)`.
use rustc::hir;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::parse::token::{self, Token};
use syntax::ptr::P;
use syntax::tokenstream::{TokenTree, TokenStream, ThinTokenStream};
use syntax::symbol::keywords;

use util::IntoSymbol;


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


impl<'a> Make<Visibility> for &'a str {
    fn make(self, _mk: &Builder) -> Visibility {
        match self {
            "pub" => Visibility::Public,
            "priv" | "" | "inherit" => Visibility::Inherited,
            _ => panic!("unrecognized string for Visibility: {:?}", self),
        }
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

impl<'a> Make<Mutability> for hir::Mutability {
    fn make(self, _mk: &Builder) -> Mutability {
        match self {
            hir::Mutability::MutMutable => Mutability::Mutable,
            hir::Mutability::MutImmutable => Mutability::Immutable,
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
            "is" | "isize" => LitIntType::Signed(IntTy::Is),
            "i8" => LitIntType::Signed(IntTy::I8),
            "i16" => LitIntType::Signed(IntTy::I16),
            "i32" => LitIntType::Signed(IntTy::I32),
            "i64" => LitIntType::Signed(IntTy::I64),
            "i128" => LitIntType::Signed(IntTy::I128),

            "us" | "usize" => LitIntType::Unsigned(UintTy::Us),
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
            identifier: self.make(mk),
            span: DUMMY_SP,
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
            abi: Abi::Rust,
            attrs: Vec::new(),
        }
    }


    // Modifier updates.

    pub fn vis<V: Make<Visibility>>(self, vis: V) -> Self {
        let vis = vis.make(&self);
        Builder {
            vis: vis,
            .. self
        }
    }

    pub fn pub_(self) -> Self {
        self.vis(Visibility::Public)
    }

    pub fn set_mutbl<M: Make<Mutability>>(self, mutbl: M) -> Self {
        let mutbl = mutbl.make(&self);
        Builder {
            mutbl: mutbl,
            .. self
        }
    }

    pub fn mutbl(self) -> Self {
        self.set_mutbl(Mutability::Mutable)
    }

    pub fn unsafety<U: Make<Unsafety>>(self, unsafety: U) -> Self {
        let unsafety = unsafety.make(&self);
        Builder {
            unsafety: unsafety,
            .. self
        }
    }

    pub fn unsafe_(self) -> Self {
        self.unsafety(Unsafety::Unsafe)
    }

    pub fn constness<C: Make<Constness>>(self, constness: C) -> Self {
        let constness = constness.make(&self);
        Builder {
            constness: constness,
            .. self
        }
    }

    pub fn const_(self) -> Self {
        self.constness(Constness::Const)
    }

    pub fn abi<A: Make<Abi>>(self, abi: A) -> Self {
        let abi = abi.make(&self);
        Builder {
            abi: abi,
            .. self
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
            .. self
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

    pub fn spanned<T, U>(self, x: U) -> Spanned<T>
            where U: Make<T> {
        let x = x.make(&self);
        Spanned {
            node: x,
            span: DUMMY_SP,
        }
    }


    // Exprs
    // These are sorted in the same order as the corresponding ExprKind variants, with additional
    // variant-specific details following each variant.

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
        let e = e.make(&self);
        let t = t.make(&self);
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
            node: ExprKind::Block(blk),
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
        let mac = mac.make(&self);
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

    pub fn field<I, E>(self, ident: I, expr: E) -> Field
            where I: Make<Ident>, E: Make<P<Expr>> {
        let ident = ident.make(&self);
        let expr = expr.make(&self);
        Field {
            ident: Spanned {
                node: ident,
                span: DUMMY_SP,
            },
            expr: expr,
            span: DUMMY_SP,
            is_shorthand: false,
            attrs: self.attrs.into(),
        }
    }


    // Literals

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
            where T: Make<LitIntType>{
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

    pub fn bool_lit(self, b: bool) -> P<Lit> {
        P(Lit {
            node: LitKind::Bool(b),
            span: DUMMY_SP,
        })
    }


    // Patterns

    pub fn ident_pat<I>(self, name: I) -> P<Pat>
            where I: Make<Ident> {
        let name = name.make(&self);
        P(Pat {
            id: DUMMY_NODE_ID,
            node: PatKind::Ident(BindingMode::ByValue(self.mutbl),
                                 Spanned { node: name, span: DUMMY_SP },
                                 None),
            span: DUMMY_SP,
        })
    }


    // Types

    pub fn array_ty<T, E>(self, ty: T, len: E) -> P<Ty>
            where T: Make<P<Ty>>, E: Make<P<Expr>> {
        let ty = ty.make(&self);
        let len = len.make(&self);
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
            span: DUMMY_SP,
        }
    }

    pub fn expr_stmt<E>(self, expr: E) -> Stmt
            where E: Make<P<Expr>> {
        let expr = expr.make(&self);
        Stmt {
            id: DUMMY_NODE_ID,
            node: StmtKind::Expr(expr),
            span: DUMMY_SP,
        }
    }

    pub fn semi_stmt<E>(self, expr: E) -> Stmt
            where E: Make<P<Expr>> {
        let expr = expr.make(&self);
        Stmt {
            id: DUMMY_NODE_ID,
            node: StmtKind::Semi(expr),
            span: DUMMY_SP,
        }
    }


    // Items

    fn item(name: Ident, attrs: Vec<Attribute>, vis: Visibility, node: ItemKind) -> P<Item> {
        P(Item {
            ident: name,
            attrs: attrs,
            id: DUMMY_NODE_ID,
            node: node,
            vis: vis,
            span: DUMMY_SP,
            tokens: None,
        })
    }

    pub fn static_item<I, T, E>(self, name: I, ty: T, init: E) -> P<Item>
            where I: Make<Ident>, T: Make<P<Ty>>, E: Make<P<Expr>> {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.make(&self);
        Self::item(name, self.attrs, self.vis,
                   ItemKind::Static(ty, self.mutbl, init))
    }

    pub fn fn_item<I, D, B>(self, name: I, decl: D, block: B) -> P<Item>
            where I: Make<Ident>, D: Make<P<FnDecl>>, B: Make<P<Block>> {
        let name = name.make(&self);
        let decl = decl.make(&self);
        let block = block.make(&self);
        Self::item(name, self.attrs, self.vis,
                   ItemKind::Fn(decl,
                                self.unsafety,
                                Spanned { span: DUMMY_SP, node: self.constness },
                                self.abi,
                                self.generics,
                                block))
    }

    pub fn struct_item<I>(self, name: I, fields: Vec<StructField>) -> P<Item>
            where I: Make<Ident> {
        let name = name.make(&self);
        Self::item(name, self.attrs, self.vis,
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

    pub fn enum_item<I>(self, name: I, fields: Vec<Variant>) -> P<Item>
        where I: Make<Ident> {
        let name = name.make(&self);
        Self::item(name, self.attrs, self.vis,
                   ItemKind::Enum(EnumDef {variants: fields}, self.generics))
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

    pub fn variant<I>(self, name: I, dat: VariantData) -> Variant
      where I: Make<Ident> {
        let name = name.make(&self);
        Spanned {
            span: DUMMY_SP,
            node: Variant_ {
                name: name,
                attrs: self.attrs,
                data: dat,
                disr_expr: None,
            },
        }
    }

    pub fn impl_item<T>(self, ty: T, items: Vec<ImplItem>) -> P<Item>
        where T: Make<P<Ty>>
    {
        let ty = ty.make(&self);
        Self::item(keywords::Invalid.ident(), self.attrs, self.vis,
                   ItemKind::Impl(self.unsafety,
                                  ImplPolarity::Positive,
                                  Defaultness::Final,
                                  self.generics,
                                  None, // not a trait implementation
                                  ty,
                                  items))
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
        let eself = Spanned { node: kind, span: DUMMY_SP };
        let ident = Spanned { node: "self".make(&self), span: DUMMY_SP };
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
            span: DUMMY_SP,
        }
    }

    pub fn ty<T>(self, node: TyKind) -> Ty {
        Ty {
            id: DUMMY_NODE_ID,
            node,
            span: DUMMY_SP,
        }
    }

    pub fn mac<Pa, Ts>(self, path: Pa, tts: Ts) -> Mac
            where Pa: Make<Path>, Ts: Make<ThinTokenStream> {
        let path = path.make(&self);
        let tts = tts.make(&self);
        Spanned {
            node: Mac_ {
                path: path,
                tts: tts,
            },
            span: DUMMY_SP,
        }
    }
}

pub fn mk() -> Builder {
    Builder::new()
}
