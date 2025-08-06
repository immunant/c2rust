pub use c2rust_ast_printer::pprust::BytePos;
use proc_macro2::Span;
use syn::*;

/// Set the span of an AST node.
pub trait SetSpan {
    fn set_span(&mut self, s: Span);
}

macro_rules! set_span_impl {
    ( enum $spanned_ty:ident, $span_ident:ident via $( ( $variant_name:ident => $($where:tt)+ ) ) , + $(,)? ) => {
        impl SetSpan for $spanned_ty {
            fn set_span(&mut self, $span_ident: Span) {
                use $spanned_ty::*;
                match self {
                    $(
                        $variant_name(inner) => inner.$($where)+,
                    )+
                    #[allow(unreachable_patterns)]
                    _ => panic!("could not set span on {} {:?}", stringify!($spanned_ty), self),
                }
            }
        }
    };
    ( enum $spanned_ty:ident, punct $( $variant_name:ident ) , + ) => {
        impl SetSpan for $spanned_ty {
            fn set_span(&mut self, s: Span) {
                use $spanned_ty::*;
                match self {
                    $(
                        $variant_name(inner) => inner.spans[0] = s,
                    )+
                    #[allow(unreachable_patterns)]
                    _ => panic!("could not set span on {} {:?}", stringify!($spanned_ty), self),
                }
            }
        }
    };
    ( enum $spanned_ty:ident, $( $variant_name:ident ) , + ) => {
        impl SetSpan for $spanned_ty {
            fn set_span(&mut self, s: Span) {
                use $spanned_ty::*;
                match self {
                    $(
                        $variant_name(inner) => inner.set_span(s),
                    )+
                    #[allow(unreachable_patterns)]
                    _ => panic!("could not set span on {} {:?}", stringify!($spanned_ty), self),
                }
            }
        }
    };
    ( struct $spanned_ty:ident, punct $field:ident ) => {
        impl SetSpan for $spanned_ty {
            fn set_span(&mut self, s: Span) {
                self.$field.spans[0] = s;
            }
        }
    };
    ( struct $spanned_ty:ident, kw $field:ident ) => {
        impl SetSpan for $spanned_ty {
            fn set_span(&mut self, s: Span) {
                self.$field.span = s;
            }
        }
    };
    ( struct $spanned_ty:ident, field $field:ident ) => {
        impl SetSpan for $spanned_ty {
            fn set_span(&mut self, s: Span) {
                self.$field.set_span(s)
            }
        }
    };
    ( struct $spanned_ty:ident, via $($where:tt) + ) => {
        impl SetSpan for $spanned_ty {
            fn set_span(&mut self, s: Span) {
                self.$($where)+ = s
            }
        }
    };
}

/* required impls:
syn::Stmt
syn::Expr
syn::TraitItem
syn::ImplItem
syn::Block
syn::Local
syn::FieldValue
syn::Item
syn::ForeignItem
*/

impl SetSpan for Stmt {
    fn set_span(&mut self, s: Span) {
        match self {
            Stmt::Expr(e, _semi) => e.set_span(s),
            Stmt::Local(l) => l.set_span(s),
            Stmt::Item(i) => i.set_span(s),
            Stmt::Macro(m) => m.mac.bang_token.span = s,
        }
    }
}

// NOTE: DelimSpan don't have set_span(),so for now ignore it
//       fix this when comment machinary is working again...
impl SetSpan for Expr {
    fn set_span(&mut self, s: Span) {
        match self {
            // DelimSpan
            Expr::Array(..)
            | Expr::Index(..)
            | Expr::Paren(..)
            | Expr::Repeat(..)
            | Expr::Struct(..)
            | Expr::Tuple(..) => {}

            Expr::Assign(e) => e.eq_token.spans[0] = s,
            Expr::Await(e) => e.await_token.span = s,
            Expr::Binary(e) => e.op.set_span(s),
            Expr::Block(e) => e.block.set_span(s),
            Expr::Break(e) => e.break_token.span = s,
            Expr::Call(e) => e.func.set_span(s),
            Expr::Cast(e) => e.as_token.span = s,
            Expr::Closure(e) => e.or1_token.span = s,
            Expr::Continue(e) => e.continue_token.span = s,
            Expr::Field(e) => e.dot_token.span = s,
            Expr::ForLoop(e) => e.for_token.span = s,
            Expr::Group(e) => e.group_token.span = s,
            Expr::If(e) => e.if_token.span = s,
            Expr::Let(e) => e.let_token.span = s,
            Expr::Lit(e) => e.lit.set_span(s),
            Expr::Loop(e) => e.loop_token.span = s,
            Expr::Macro(e) => e.mac.path.set_span(s),
            Expr::Match(e) => e.match_token.span = s,
            Expr::MethodCall(e) => e.dot_token.span = s,
            Expr::Path(e) => e.path.set_span(s),
            Expr::Range(e) => match e.limits {
                RangeLimits::Closed(mut r) => r.spans[0] = s,
                RangeLimits::HalfOpen(mut r) => r.spans[0] = s,
            },
            Expr::Reference(e) => e.and_token.span = s,
            Expr::Return(e) => e.return_token.span = s,
            Expr::Try(e) => e.question_token.span = s,
            Expr::Unary(e) => e.op.set_span(s),
            Expr::Unsafe(e) => e.unsafe_token.span = s,
            Expr::Verbatim(..) => {}
            Expr::While(e) => e.while_token.span = s,
            Expr::Yield(e) => e.yield_token.span = s,
            e => panic!("Expr set_span, {:?}", e),
        }
    }
}

impl SetSpan for Path {
    fn set_span(&mut self, s: Span) {
        if let Some(mut tok) = self.leading_colon {
            tok.spans[0] = s;
        } else if let Some(tok) = self.segments.first_mut() {
            tok.ident.set_span(s)
        }
    }
}

set_span_impl!(enum RangeLimits, s via
    (HalfOpen => spans[0] = s),
    (Closed => spans[0] = s),
);

set_span_impl!(struct Signature, kw fn_token);

set_span_impl!(enum TraitItem, s via
    (Const => const_token.span = s),
    (Fn => sig.set_span(s)),
    (Type => type_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
);

set_span_impl!(enum ImplItem, s via
    (Const => const_token.span = s),
    (Fn => sig.set_span(s)),
    (Type => type_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
);

impl SetSpan for Block {
    fn set_span(&mut self, s: Span) {
        if !self.stmts.is_empty() {
            self.stmts[0].set_span(s);
        }
    }
}

set_span_impl!(struct Local, kw let_token);

set_span_impl!(enum Member, s via
    (Named => set_span(s)),
    (Unnamed => span = s),
);

set_span_impl!(struct FieldValue, field member);

set_span_impl!(enum Item, s via
    (Const => const_token.span = s),
    (Enum => enum_token.span = s),
    (ExternCrate => extern_token.span = s),
    (Fn => sig.set_span(s)),
    (ForeignMod => abi.extern_token.span = s),
    (Impl => impl_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
    (Mod => mod_token.span = s),
    (Static => static_token.span = s),
    (Struct => struct_token.span = s),
    (Trait => trait_token.span = s),
    (TraitAlias => trait_token.span = s),
    (Type => type_token.span = s),
    (Union => union_token.span = s),
    (Use => use_token.span = s),
);

set_span_impl!(enum ForeignItem, s via
    (Fn => sig.set_span(s)),
    (Static => static_token.span = s),
    (Type => type_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
);

//lit required for expr

set_span_impl!(enum BinOp, punct Add, Sub, Mul, Div, Rem, And, Or, BitXor, BitAnd, BitOr, Shl, Shr, Eq, Lt, Le, Ne, Ge, Gt, AddAssign, SubAssign, MulAssign, DivAssign, RemAssign, BitXorAssign, BitAndAssign, BitOrAssign, ShlAssign, ShrAssign);
set_span_impl!(enum UnOp, punct Deref, Not, Neg);
