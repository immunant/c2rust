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
            Stmt::Expr(e) => e.set_span(s),
            Stmt::Local(l) => l.set_span(s),
            Stmt::Item(i) => i.set_span(s),
            Stmt::Semi(e, _) => e.set_span(s),
        }
    }
}

set_span_impl!(enum Expr, s via
    (Array => bracket_token.span = s),
    (Assign => eq_token.spans[0] = s),
    (AssignOp => op.set_span(s)),
    (Await => await_token.span = s),
    (Binary => op.set_span(s)),
    (Block => block.set_span(s)),
    (Box => box_token.span = s),
    (Break => break_token.span = s),
    (Call => paren_token.span = s),
    (Cast => as_token.span = s),
    (Closure => or1_token.spans[0] = s),
    (Continue => continue_token.span = s),
    (Field => dot_token.spans[0] = s),
    (ForLoop => for_token.span = s),
    (Group => group_token.span = s),
    (If => if_token.span = s),
    (Index => bracket_token.span = s),
    (Let => let_token.span = s),
    (Lit => lit.set_span(s)),
    (Loop => loop_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
    (Match => match_token.span = s),
    (MethodCall => dot_token.spans[0] = s),
    (Paren => paren_token.span = s),
    (Path => path.set_span(s)),
    (Range => limits.set_span(s)),
    (Reference => and_token.spans[0] = s),
    (Repeat => bracket_token.span = s),
    (Return => return_token.span = s),
    (Struct => brace_token.span = s),
    (Try => question_token.spans[0] = s),
    (TryBlock => try_token.span = s),
    (Tuple => paren_token.span = s),
    (Type => colon_token.spans[0] = s),
    (Unary => op.set_span(s)),
    (Unsafe => unsafe_token.span = s),
    (While => while_token.span = s),
    (Yield => yield_token.span = s),
);

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
    (Method => sig.set_span(s)),
    (Type => type_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
);

set_span_impl!(enum ImplItem, s via
    (Const => const_token.span = s),
    (Method => sig.set_span(s)),
    (Type => type_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
);

set_span_impl!(struct Block, kw brace_token);
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
    (ForeignMod => brace_token.span = s),
    (Impl => impl_token.span = s),
    (Macro => mac.bang_token.spans[0] = s),
    (Macro2 => macro_token.span = s),
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

set_span_impl!(enum BinOp, punct Add, Sub, Mul, Div, Rem, And, Or, BitXor, BitAnd, BitOr, Shl, Shr, Eq, Lt, Le, Ne, Ge, Gt, AddEq, SubEq, MulEq, DivEq, RemEq, BitXorEq, BitAndEq, BitOrEq, ShlEq, ShrEq);
set_span_impl!(enum UnOp, punct Deref, Not, Neg);
