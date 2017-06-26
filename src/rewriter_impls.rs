use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ptr::P;
use syntax::print::pprust;
use syntax::tokenstream::ThinTokenStream;

use syntax::codemap::DUMMY_SP;

use rewriter::{self, Rewrite, RewriteCtxt};


/// Generate a match statement for rewriting a pair of enum values.  If `$lhs` and `$rhs` are built
/// from the same enum variant, it recursively rewrites their corresponding fields.  If they are
/// different variants, it records a rewrite in `$rcx` with the given`$span` and `$text`.
/// Syntax for variants looks like this:
///
///     VariantName(a1 ~ a2, b1 ~ b2, c1 ~ c2, ...),
///
/// The provided idents (`a1` etc.) will be used for the LHS and RHS instances of the field.  The
/// operator between them can be either `~`, meaning we should recursively rewrite those fields,
/// `==`, meaning we should require those fields to be equal (otherwise behave as if the LHS and
/// RHS are different variants entirely), or `_`, meaning we should ignore those fields.
///
/// In general, `~` is used for fields whose values have their own `Span`s, and `==` is used for
/// other fields (mostly enums, such as `Mutability` or `Constness`).
macro_rules! variant_rewrite {
    (lhs $lhs:expr;
     rhs $rhs:expr;
     rcx $rcx:expr;
     text $text:expr;
     span $span:expr;
     $( $Variant:ident ( $($field1:ident $op:tt $field2:ident),* ), )*) => {

        // Exhaustiveness check
        match $lhs {
            $(
                &$Variant( $(ref $field1),* ) => {},
            )*
        }

        match ($lhs, $rhs) {
            $(
                (&$Variant( $(ref $field1),* ),
                 &$Variant( $(ref $field2),* ))
                        if $( eq_or_true!($field1 $op $field2) && )* true => {
                    $( rewrite_or_unit!($rcx; $field1 $op $field2); )*
                },
            )*

            (_, _) => {
                let text = $text;
                let span = $span;
                $rcx.record(span, text);
            },
        }
    };
}

macro_rules! eq_or_true {
    ($lhs:ident == $rhs:ident) => {
        $lhs == $rhs
    };
    ($lhs:ident ~ $rhs:ident) => {
        true
    };
    ($lhs:ident _ $rhs:ident) => {
        true
    };
}

macro_rules! rewrite_or_unit {
    ($rcx:expr; $lhs:ident == $rhs:ident) => {
        ()
    };
    ($rcx:expr; $lhs:ident ~ $rhs:ident) => {
        $rcx.rewrite($lhs, $rhs)
    };
    ($rcx:expr; $lhs:ident _ $rhs:ident) => {
        ()
    };
}


impl<'ast> Rewrite<'ast> for Crate {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite(&self.module, &new.module);
    }
}

impl<'ast> Rewrite<'ast> for Mod {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite(&self.items, &new.items);
    }
}

impl<'ast> Rewrite<'ast> for Item {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite_in_item(self, &self.node, &new.node);
    }
}

impl<'ast> Rewrite<'ast> for ItemKind {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        use syntax::ast::ItemKind::*;
        // Some variants have fields that have no spans of their own.  This means we can't easily
        // rewrite just those fields.  If there is a mismatch on such a field, we rewrite the whole
        // item instead.
        variant_rewrite!(
            lhs self;
            rhs new;
            rcx rcx;
            text pprust::item_to_string(rcx.parent_as_item());
            span rcx.parent_span();

            ExternCrate(name1 == name2),
            Use(vp1 ~ vp2),
            Static(ty1 ~ ty2, mut1 == mut2, init1 ~ init2),
            Const(ty1 ~ ty2, init1 ~ init2),
            Fn(decl1 ~ decl2, unsafe1 == unsafe2, const1 ~ const2, abi1 == abi2,
               generics1 ~ generics2, block1 ~ block2),
            Mod(mod1 ~ mod2),
            ForeignMod(fm1 ~ fm2),
            GlobalAsm(asm1 ~ asm2),
            Ty(ty1 ~ ty2, generics1 ~ generics2),
            Enum(def1 ~ def2, generics1 ~ generics2),
            Struct(vd1 ~ vd2, generics1 ~ generics2),
            Union(vd1 ~ vd2, generics1 ~ generics2),
            Trait(unsafe1 == unsafe2, generics1 ~ generics2,
                  bounds1 ~ bounds2, items1 ~ items2),
            DefaultImpl(unsafe1 == unsafe2, trait_ref1 ~ trait_ref2),
            Impl(unsafe1 == unsafe2, polarity1 == polarity2,
                 generics1 ~ generics2, trait_ref1 ~ trait_ref2,
                 ty1 ~ ty2, items1 ~ items2),
            Mac(mac1 ~ mac2),
            MacroDef(tts1 ~ tts2),
        );
    }
}

impl<'ast> Rewrite<'ast> for Block {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite(&self.stmts, &new.stmts);
    }
}

impl<'ast> Rewrite<'ast> for Stmt {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite_in_stmt(self, &self.node, &new.node);
    }
}

impl<'ast> Rewrite<'ast> for StmtKind {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        use syntax::ast::StmtKind::*;
        variant_rewrite!(
            lhs self;
            rhs new;
            rcx rcx;
            text pprust::stmt_to_string(rcx.parent_as_stmt());
            span rcx.parent_span();

            Local(local1 ~ local2),
            Item(item1 ~ item2),
            Expr(expr1 ~ expr2),
            Semi(semi1 ~ semi2),
            Mac(mac1 ~ mac2),
        );
    }
}

impl<'ast> Rewrite<'ast> for Local {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite(&self.init, &new.init);
    }
}

impl<'ast> Rewrite<'ast> for Expr {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite_in_expr(self, &self.node, &new.node);
    }
}

impl<'ast> Rewrite<'ast> for ExprKind {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        use syntax::ast::ExprKind::*;
        // TODO: handle attrs
        variant_rewrite!(
            lhs self;
            rhs new;
            rcx rcx;
            text pprust::expr_to_string(rcx.parent_as_expr());
            span rcx.parent_span();

            Box(expr1 ~ expr2),
            InPlace(place1 ~ place2, expr1 ~ expr2),
            Array(elems1 ~ elems2),
            Call(func1 ~ func2, args1 ~ args2),
            MethodCall(ident1 ~ ident2, tys1 ~ tys2, args1 ~ args2),
            Tup(elems1 ~ elems2),
            Binary(op1 == op2, a1 ~ a2, b1 ~ b2),
            Unary(op1 == op2, a1 ~ a2),
            Lit(lit1 ~ lit2),
            Cast(expr1 ~ expr2, ty1 ~ ty2),
            Type(expr1 ~ expr2, ty1 ~ ty2),
            If(cond1 ~ cond2, then1 ~ then2, else1 ~ else2),
            IfLet(pat1 ~ pat2, expr1 ~ expr2, then1 ~ then2, else1 ~ else2),
            While(cond1 ~ cond2, body1 ~ body2, label1 ~ label2),
            WhileLet(pat1 ~ pat2, expr1 ~ expr2, body1 ~ body2, label1 ~ label2),
            ForLoop(pat1 ~ pat2, iter1 ~ iter2, body1 ~ body2, label1 ~ label2),
            Loop(body1 ~ body2, label1 ~ label2),
            Match(target1 ~ target2, arms1 ~ arms2),
            Closure(cap1 == cap2, decl1 ~ decl2, body1 ~ body2, span1 _ span2),
            Block(body1 ~ body2),
            Catch(body1 ~ body2),
            Assign(lhs1 ~ lhs2, rhs1 ~ rhs2),
            AssignOp(op1 == op2, lhs1 ~ lhs2, rhs1 ~ rhs2),
            Field(expr1 ~ expr2, ident1 ~ ident2),
            TupField(expr1 ~ expr2, idx1 ~ idx2),
            Index(arr1 ~ arr2, idx1 ~ idx2),
            Range(lo1 ~ lo2, hi1 ~ hi2, limits1 == limits2),
            Path(self1 ~ self2, path1 ~ path2),
            AddrOf(mut1 == mut2, expr1 ~ expr2),
            Break(label1 ~ label2, expr1 ~ expr2),
            Continue(label1 ~ label2),
            Ret(expr1 ~ expr2),
            InlineAsm(asm1 ~ asm2),
            Mac(mac1 ~ mac2),
            Struct(path1 ~ path2, fields1 ~ fields2, base1 ~ base2),
            Repeat(item1 ~ item2, count1 ~ count2),
            Paren(expr1 ~ expr2),
            Try(expr1 ~ expr2),
        );
    }
}


macro_rules! rewrite_nyi {
    ($($ty:ty,)*) => {
        $(
            impl<'ast> Rewrite<'ast> for $ty {
                fn rewrite(&'ast self, _new: &'ast Self, _rcx: &mut RewriteCtxt<'ast>) {
                    println!(concat!(" ** NYI: rewriting of ", stringify!($ty)));
                }
            }
        )*
    };
}

rewrite_nyi! {
    ViewPath_,
    FnDecl,
    Constness,
    Generics,
    ForeignMod,
    GlobalAsm,
    Ty,
    EnumDef,
    VariantData,
    TyParamBound,
    TraitItem,
    TraitRef,
    ImplItem,
    Mac_,
    ThinTokenStream,

    (Spanned<Mac_>, MacStmtStyle, ThinVec<Attribute>),
    Ident,
    LitKind,
    Pat,
    Arm,
    usize,
    QSelf,
    Path,
    InlineAsm,
    Field,
}


impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for [T] {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        assert!(self.len() == new.len(),
                "not sure how to handle different-length vecs of rewritables");

        for i in 0 .. self.len() {
            rcx.rewrite(&self[i], &new[i]);
        }
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for Vec<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        <[T] as Rewrite<'ast>>::rewrite(self, new, rcx)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for ThinVec<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        <[T] as Rewrite<'ast>>::rewrite(self, new, rcx)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for P<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        <T as Rewrite<'ast>>::rewrite(self, new, rcx)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for Spanned<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        rcx.rewrite(&self.node, &new.node)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for Option<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) {
        match (self, new) {
            (&Some(ref x1),
             &Some(ref x2)) => {
                rcx.rewrite(x1, x2);
            }
            (&None, &None) => {},
            (_, _) => {}, //TODO
        }
    }
}
