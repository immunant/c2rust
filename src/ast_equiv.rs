use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ext::hygiene::SyntaxContext;
use syntax::ptr::P;
use syntax::tokenstream::{TokenStream, ThinTokenStream};

/// Trait for checking equivalence of AST nodes.  This is similar to `PartialEq`, but less strict,
/// as it ignores some fields that have no bearing on the semantics of the AST (particularly
/// `Span`s and `NodeId`s).
pub trait AstEquiv {
    fn ast_equiv(&self, other: &Self) -> bool;
}

macro_rules! ast_equiv_ignore {
    ($T:ty) => {
        impl AstEquiv for $T {
            fn ast_equiv(&self, _other: &Self) -> bool {
                true
            }
        }
    };
}

macro_rules! ast_equiv_eq {
    ($T:ty) => {
        impl AstEquiv for $T {
            fn ast_equiv(&self, other: &Self) -> bool {
                self == other
            }
        }
    };
}

macro_rules! ast_equiv_struct {
    ($T:path { $($id:ident),* }) => {
        impl AstEquiv for $T {
            fn ast_equiv(&self, other: &Self) -> bool {
                // Exhaustiveness check
                match *self {
                    $T { $(ref $id),* } => {}
                }

                $( self.$id.ast_equiv(&other.$id) && )*
                true
            }
        }
    };
}

macro_rules! ast_equiv_enum_exhaustiveness_pattern {
    ($T:ident $V:ident ()) => {
        $T::$V
    };
    ($T:ident $V:ident ( $($id1:ident $id2:ident),* )) => {
        $T::$V ( $( ref $id1 ),* )
    };
}

macro_rules! ast_equiv_enum_match_pattern {
    ($T:ident $V:ident ()) => {
        (&$T::$V, &$T::$V)
    };
    ($T:ident $V:ident ( $($id1:ident $id2:ident),* )) => {
        (&$T::$V( $(ref $id1),* ),
         &$T::$V( $(ref $id2),* ))
    };
}

macro_rules! ast_equiv_enum_match_body {
    (( $($id1:ident $id2:ident),* )) => {
        $( $id1.ast_equiv($id2) && )*
        true
    };
}

macro_rules! ast_equiv_enum {
    ($T:ident {
        $( $V:ident $args:tt, )*
    }) => {
        impl AstEquiv for $T {
            fn ast_equiv(&self, other: &Self) -> bool {
                // Exhaustiveness check
                match *self {
                    $( ast_equiv_enum_exhaustiveness_pattern!($T $V $args) => {}, )*
                }

                match (self, other) {
                    $(
                    ast_equiv_enum_match_pattern!($T $V $args) => {
                        ast_equiv_enum_match_body!($args)
                    },
                    )*
                    (_, _) => false,
                }
            }
        }
    };
}



ast_equiv_struct!(Crate { module, attrs, span });
ast_equiv_struct!(Mod { inner, items });


ast_equiv_struct!(Item { ident, attrs, id, node, vis, span });
ast_equiv_enum!(
    ItemKind {
        ExternCrate(name1 name2),
        Use(vp1 vp2),
        Static(ty1 ty2, mut1 mut2, init1 init2),
        Const(ty1 ty2, init1 init2),
        Fn(decl1 decl2, unsafe1 unsafe2, const1 const2, abi1 abi2,
           generics1 generics2, block1 block2),
        Mod(mod1 mod2),
        ForeignMod(fm1 fm2),
        GlobalAsm(asm1 asm2),
        Ty(ty1 ty2, generics1 generics2),
        Enum(def1 def2, generics1 generics2),
        Struct(vd1 vd2, generics1 generics2),
        Union(vd1 vd2, generics1 generics2),
        Trait(unsafe1 unsafe2, generics1 generics2,
              bounds1 bounds2, items1 items2),
        DefaultImpl(unsafe1 unsafe2, trait_ref1 trait_ref2),
        Impl(unsafe1 unsafe2, polarity1 polarity2,
             generics1 generics2, trait_ref1 trait_ref2,
             ty1 ty2, items1 items2),
        Mac(mac1 mac2),
        MacroDef(tts1 tts2),
    }
);

ast_equiv_struct!(TraitItem {id, ident, attrs, node, span });
ast_equiv_enum!(
    TraitItemKind {
        Const(ty1 ty2, init1 init2),
        Method(sig1 sig2, body1 body2),
        Type(bounds1 bounds2, ty1 ty2),
        Macro(mac1 mac2),
    }
);

ast_equiv_struct!(ImplItem { id, ident, vis, defaultness, attrs, node, span });
ast_equiv_enum!(
    ImplItemKind {
        Const(ty1 ty2, init1 init2),
        Method(sig1 sig2, body1 body2),
        Type(ty1 ty2),
        Macro(mac1 mac2),
    }
);
ast_equiv_struct!(TraitRef { path, ref_id });

ast_equiv_struct!(EnumDef { variants });
ast_equiv_struct!(Variant_ { name, attrs, data, disr_expr });
ast_equiv_enum!(
    VariantData {
        Struct(fields1 fields2, id1 id2),
        Tuple(fields1 fields2, id1 id2),
        Unit(id1 id2),
    }
);
ast_equiv_struct!(StructField { span, ident, vis, id, ty, attrs });

ast_equiv_struct!(MethodSig { unsafety, constness, abi, decl, generics });

ast_equiv_struct!(ForeignMod { abi, items });
ast_equiv_struct!(ForeignItem { ident, attrs, node, id, span, vis });
ast_equiv_enum!(
    ForeignItemKind {
        Fn(decl1 decl2, generics1 generics2),
        Static(ty1 ty2, mut1 mut2),
    }
);

// Has to be implemented manually because it uses struct variants
impl AstEquiv for Visibility {
    fn ast_equiv(&self, other: &Visibility) -> bool {
        match (self, other) {
            (&Visibility::Public,
             &Visibility::Public) => true,
            (&Visibility::Crate(ref _sp1),
             &Visibility::Crate(ref _sp2)) => true,
            (&Visibility::Restricted { path: ref path1, id: ref _id1 },
             &Visibility::Restricted { path: ref path2, id: ref _id2 }) => {
                path1.ast_equiv(path2)
            },
            (&Visibility::Inherited,
             &Visibility::Inherited) => true,
            (_, _) => false,
        }
    }
}

ast_equiv_struct!(Generics { lifetimes, ty_params, where_clause, span });
ast_equiv_struct!(TyParam { attrs, ident, id, bounds, default, span });
ast_equiv_struct!(LifetimeDef { attrs, lifetime, bounds });
ast_equiv_struct!(WhereClause { id, predicates });
ast_equiv_enum!(
    WherePredicate {
        BoundPredicate(pred1 pred2),
        RegionPredicate(pred1 pred2),
        EqPredicate(pred1 pred2),
    }
);
ast_equiv_struct!(WhereBoundPredicate { span, bound_lifetimes, bounded_ty, bounds });
ast_equiv_struct!(WhereRegionPredicate { span, lifetime, bounds });
ast_equiv_struct!(WhereEqPredicate { id, span, lhs_ty, rhs_ty });
ast_equiv_eq!(TraitBoundModifier);

ast_equiv_enum!(
    ViewPath_ {
        ViewPathSimple(name1 name2, path1 path2),
        ViewPathGlob(path1 path2),
        ViewPathList(path1 path2, list1 list2),
    }
);
ast_equiv_struct!(PathListItem_ { name, rename, id });


ast_equiv_struct!(Ty { id, node, span });
ast_equiv_struct!(MutTy {ty, mutbl});
ast_equiv_enum!(
    TyKind {
        Slice(ty1 ty2),
        Array(ty1 ty2, len1 len2),
        Ptr(mty1 mty2),
        Rptr(lt1 lt2, mty1 mty2),
        BareFn(ty1 ty2),
        Never(),
        Tup(tys1 tys2),
        Path(qself1 qself2, path1 path2),
        TraitObject(bounds1 bounds2),
        ImplTrait(bounds1 bounds2),
        Paren(ty1 ty2),
        Typeof(expr1 expr2),
        Infer(),
        ImplicitSelf(),
        Mac(mac1 mac2),
        Err(),
    }
);

ast_equiv_eq!(LitIntType);
ast_equiv_eq!(FloatTy);

ast_equiv_struct!(BareFnTy { unsafety, abi, lifetimes, decl });
ast_equiv_struct!(Lifetime { id, span, name });
ast_equiv_enum!(
    TyParamBound {
        TraitTyParamBound(poly1 poly2, mod1 mod2),
        RegionTyParamBound(lt1 lt2),
    }
);
ast_equiv_struct!(PolyTraitRef { bound_lifetimes, trait_ref, span });

ast_equiv_struct!(FnDecl { inputs, output, variadic });
ast_equiv_struct!(Arg { ty, pat, id });
ast_equiv_enum!(
    FunctionRetTy {
        Default(sp1 sp2),
        Ty(ty1 ty2),
    }
);

ast_equiv_struct!(TypeBinding { id, ident, ty, span });


ast_equiv_struct!(Stmt { id, node, span });
ast_equiv_enum!(
    StmtKind {
        Local(local1 local2),
        Item(item1 item2),
        Expr(expr1 expr2),
        Semi(expr1 expr2),
        Mac(mac1 mac2),
    }
);
ast_equiv_struct!(Local { pat, ty, init, id, span, attrs });


ast_equiv_struct!(Expr { id, node, span, attrs });
ast_equiv_enum!(
    ExprKind {
        Box(expr1 expr2),
        InPlace(place1 place2, expr1 expr2),
        Array(elems1 elems2),
        Call(func1 func2, args1 args2),
        MethodCall(ident1 ident2, tys1 tys2, args1 args2),
        Tup(elems1 elems2),
        Binary(op1 op2, a1 a2, b1 b2),
        Unary(op1 op2, a1 a2),
        Lit(lit1 lit2),
        Cast(expr1 expr2, ty1 ty2),
        Type(expr1 expr2, ty1 ty2),
        If(cond1 cond2, then1 then2, else1 else2),
        IfLet(pat1 pat2, expr1 expr2, then1 then2, else1 else2),
        While(cond1 cond2, body1 body2, label1 label2),
        WhileLet(pat1 pat2, expr1 expr2, body1 body2, label1 label2),
        ForLoop(pat1 pat2, iter1 iter2, body1 body2, label1 label2),
        Loop(body1 body2, label1 label2),
        Match(target1 target2, arms1 arms2),
        Closure(cap1 cap2, decl1 decl2, body1 body2, span1 span2),
        Block(body1 body2),
        Catch(body1 body2),
        Assign(lhs1 lhs2, rhs1 rhs2),
        AssignOp(op1 op2, lhs1 lhs2, rhs1 rhs2),
        Field(expr1 expr2, ident1 ident2),
        TupField(expr1 expr2, idx1 idx2),
        Index(arr1 arr2, idx1 idx2),
        Range(lo1 lo2, hi1 hi2, limits1 limits2),
        Path(self1 self2, path1 path2),
        AddrOf(mut1 mut2, expr1 expr2),
        Break(label1 label2, expr1 expr2),
        Continue(label1 label2),
        Ret(expr1 expr2),
        InlineAsm(asm1 asm2),
        Mac(mac1 mac2),
        Struct(path1 path2, fields1 fields2, base1 base2),
        Repeat(item1 item2, count1 count2),
        Paren(expr1 expr2),
        Try(expr1 expr2),
    }
);

ast_equiv_eq!(UnOp);
ast_equiv_eq!(BinOpKind);
ast_equiv_struct!(Field { ident, expr, span, is_shorthand, attrs });
ast_equiv_struct!(Arm { attrs, pats, guard, body });
ast_equiv_struct!(Block { stmts, id, rules, span });


ast_equiv_struct!(Pat { id, node, span });
ast_equiv_enum!(
    PatKind {
        Wild(),
        Ident(mode1 mode2, id1 id2, pat1 pat2),
        Struct(path1 path2, fields1 fields2, dotdot1 dotdot2),
        TupleStruct(path1 path2, fields1 fields2, dotdot1 dotdot2),
        Path(qself1 qself2, path1 path2),
        Tuple(pats1 pats2, dotdot1 dotdot2),
        Box(pat1 pat2),
        Ref(pat1 pat2, mut1 mut2),
        Lit(expr1 expr2),
        Range(lo1 lo2, hi1 hi2, end1 end2),
        Slice(start1 start2, mid1 mid2, end1 end2),
        Mac(mac1 mac2),
    }
);
ast_equiv_struct!(FieldPat { ident, pat, is_shorthand, attrs });


ast_equiv_enum!(
    LitKind {
        Str(sym1 sym2, style1 style2),
        ByteStr(bytes1 bytes2),
        Byte(x1 x2),
        Char(x1 x2),
        Int(x1 x2, type1 type2),
        Float(sym1 sym2, type1 type2),
        FloatUnsuffixed(sym1 sym2),
        Bool(x1 x2),
    }
);


ast_equiv_eq!(Defaultness);
ast_equiv_eq!(Constness);
ast_equiv_eq!(ImplPolarity);
ast_equiv_eq!(Unsafety);
ast_equiv_eq!(Abi);
ast_equiv_eq!(Mutability);
ast_equiv_eq!(RangeEnd);
ast_equiv_eq!(BindingMode);
ast_equiv_eq!(CaptureBy);
ast_equiv_eq!(BlockCheckMode);
ast_equiv_eq!(StrStyle);
ast_equiv_eq!(AsmDialect);
ast_equiv_eq!(RangeLimits);


ast_equiv_struct!(Attribute { id, style, path, tokens, is_sugared_doc, span });
ast_equiv_eq!(AttrStyle);


ast_equiv_struct!(Path { span, segments });
ast_equiv_struct!(PathSegment { identifier, span, parameters });
ast_equiv_enum!(
    PathParameters {
        AngleBracketed(abpd1 abpd2),
        Parenthesized(ppd1 ppd2),
    }
);
ast_equiv_struct!(AngleBracketedParameterData { lifetimes, types, bindings });
ast_equiv_struct!(ParenthesizedParameterData { span, inputs, output });
ast_equiv_struct!(QSelf { ty, position });


ast_equiv_struct!(Mac_ { path, tts });
ast_equiv_eq!(MacStmtStyle);
ast_equiv_eq!(TokenStream);
ast_equiv_eq!(ThinTokenStream);


ast_equiv_struct!(InlineAsm {
    asm, asm_str_style, outputs, inputs, clobbers,
    volatile, alignstack, dialect, ctxt
});
ast_equiv_struct!(GlobalAsm { asm, ctxt });
ast_equiv_struct!(InlineAsmOutput { constraint, expr, is_rw, is_indirect });


ast_equiv_eq!(Ident);
ast_equiv_eq!(Name);
ast_equiv_eq!(SyntaxContext);

ast_equiv_ignore!(Span);
ast_equiv_ignore!(NodeId);
ast_equiv_ignore!(AttrId);

ast_equiv_eq!(usize);
ast_equiv_eq!(bool);
ast_equiv_eq!(u128);
ast_equiv_eq!(u8);
ast_equiv_eq!(char);


impl<'a, T: AstEquiv> AstEquiv for &'a T {
    fn ast_equiv(&self, other: &&'a T) -> bool {
        <T as AstEquiv>::ast_equiv(*self, *other)
    }
}

impl<T: AstEquiv> AstEquiv for P<T> {
    fn ast_equiv(&self, other: &P<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Spanned<T> {
    fn ast_equiv(&self, other: &Spanned<T>) -> bool {
        self.node.ast_equiv(&other.node)
    }
}

impl<T: AstEquiv> AstEquiv for [T] {
    fn ast_equiv(&self, other: &[T]) -> bool {
        for (l, r) in self.iter().zip(other.iter()) {
            if !l.ast_equiv(r) {
                return false;
            }
        }
        true
    }
}

impl<T: AstEquiv> AstEquiv for Vec<T> {
    fn ast_equiv(&self, other: &Vec<T>) -> bool {
        <[T] as AstEquiv>::ast_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for ThinVec<T> {
    fn ast_equiv(&self, other: &ThinVec<T>) -> bool {
        <[T] as AstEquiv>::ast_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Option<T> {
    fn ast_equiv(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref x), &Some(ref y)) => x.ast_equiv(y),
            (&None, &None) => true,
            (_, _) => false,
        }
    }
}

impl<A: AstEquiv, B: AstEquiv> AstEquiv for (A, B) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) &&
        self.1.ast_equiv(&other.1)
    }
}

impl<A: AstEquiv, B: AstEquiv, C: AstEquiv> AstEquiv for (A, B, C) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) &&
        self.1.ast_equiv(&other.1) &&
        self.2.ast_equiv(&other.2)
    }
}

