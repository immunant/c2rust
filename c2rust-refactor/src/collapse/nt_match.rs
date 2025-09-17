use std::collections::HashMap;

use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::{DelimSpan, LazyTokenStream, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_span::source_map::{Span, Spanned, SyntaxContext};
use rustc_span::symbol::{Ident, Symbol};
use rustc_target::spec::abi::Abi;

use crate::ast_builder::mk;
use rustc_ast::ptr::P;
use std::rc::Rc;

use crate::ast_manip::{GetSpan, MaybeGetNodeId};

pub struct Ctxt {
    nts: Vec<(Span, Nonterminal)>,
}

impl Ctxt {
    fn record(&mut self, span: Span, nt: Nonterminal) {
        self.nts.push((span, nt));
    }
}

pub trait NtMatch {
    /// Match up subtrees of `old` and `new`, and record `Nonterminal`s that may be useful for
    /// macro collapsing.  `old` should be a node from the expanded AST, and `new` should be the
    /// corresponding node (same `NodeId`) from the transformed AST.
    ///
    /// Specifically, this operation traverses the corresponding portions of the two trees, looking
    /// for `old` nodes that are not macro-generated (empty `SyntaxContext`).  When it finds one,
    /// it records the old node's span (which should point to code inside a macro argument list)
    /// along with the new node wrapped up as a `Nonterminal`.  Editing the collapsed AST to
    /// replace tokens in the old span with the new nonterminal (as is done by `collapse::macros`)
    /// should cause later expansions to produce `new` in place of `old`.
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt);
}

include!(concat!(env!("OUT_DIR"), "/nt_match_gen.inc.rs"));

impl<T: NtMatch + ?Sized> NtMatch for P<T> {
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {
        <T as NtMatch>::nt_match(old, new, cx);
    }
}

impl<T: NtMatch + ?Sized> NtMatch for Box<T> {
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {
        <T as NtMatch>::nt_match(old, new, cx);
    }
}

impl<T: NtMatch + ?Sized> NtMatch for Rc<T> {
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {
        <T as NtMatch>::nt_match(old, new, cx);
    }
}

impl<T: NtMatch> NtMatch for Spanned<T> {
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {
        <T as NtMatch>::nt_match(&old.node, &new.node, cx);
    }
}

impl<T: NtMatch> NtMatch for Option<T> {
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {
        match (old, new) {
            (&Some(ref old), &Some(ref new)) => {
                <T as NtMatch>::nt_match(old, new, cx);
            }
            (_, _) => {}
        }
    }
}

impl<A: NtMatch, B: NtMatch> NtMatch for (A, B) {
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {
        <A as NtMatch>::nt_match(&old.0, &new.0, cx);
        <B as NtMatch>::nt_match(&old.1, &new.1, cx);
    }
}

impl<A: NtMatch, B: NtMatch, C: NtMatch> NtMatch for (A, B, C) {
    fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {
        <A as NtMatch>::nt_match(&old.0, &new.0, cx);
        <B as NtMatch>::nt_match(&old.1, &new.1, cx);
        <C as NtMatch>::nt_match(&old.2, &new.2, cx);
    }
}

impl<T: NtMatch + MaybeGetNodeId> NtMatch for [T] {
    fn nt_match(old_seq: &Self, new_seq: &Self, cx: &mut Ctxt) {
        if <T as MaybeGetNodeId>::supported() {
            // For each item in `old_seq`, `nt_match` it against the item with the same NodeId in
            // `new_seq`, if such an item exists.
            let new_id_map = new_seq
                .iter()
                .map(|new| (new.get_node_id(), new))
                .filter(|p| p.0 != DUMMY_NODE_ID)
                .collect::<HashMap<_, _>>();

            for old in old_seq {
                let old_id = old.get_node_id();
                if let Some(&new) = new_id_map.get(&old_id) {
                    NtMatch::nt_match(old, new, cx);
                }
            }
        } else {
            for (old, new) in old_seq.iter().zip(new_seq.iter()) {
                NtMatch::nt_match(old, new, cx);
            }
        }
    }
}

impl<T: NtMatch + MaybeGetNodeId> NtMatch for Vec<T> {
    fn nt_match(old_seq: &Self, new_seq: &Self, cx: &mut Ctxt) {
        <[T] as NtMatch>::nt_match(old_seq, new_seq, cx);
    }
}

impl<T: NtMatch + MaybeGetNodeId> NtMatch for ThinVec<T> {
    fn nt_match(old_seq: &Self, new_seq: &Self, cx: &mut Ctxt) {
        <[T] as NtMatch>::nt_match(old_seq, new_seq, cx);
    }
}

trait AsNonterminal {
    fn as_nonterminal(&self) -> Nonterminal;
}

macro_rules! as_nonterminal_impl {
    ($Ty:ty, $Variant:ident) => {
        impl AsNonterminal for $Ty {
            fn as_nonterminal(&self) -> Nonterminal {
                Nonterminal::$Variant(self.clone())
            }
        }
    };

    ($Ty:ty, $Variant:ident, P) => {
        impl AsNonterminal for $Ty {
            fn as_nonterminal(&self) -> Nonterminal {
                Nonterminal::$Variant(P(self.clone()))
            }
        }
    };
}

as_nonterminal_impl!(Item, NtItem, P);
as_nonterminal_impl!(Block, NtBlock, P);
as_nonterminal_impl!(Stmt, NtStmt, P);
as_nonterminal_impl!(Pat, NtPat, P);
as_nonterminal_impl!(Expr, NtExpr, P);
as_nonterminal_impl!(Ty, NtTy, P);
//as_nonterminal_impl!(Ident, NtIdent);
//as_nonterminal_impl!(Lifetime, NtLifetime);
//as_nonterminal_impl!(Expr??, NtLiteral, P);
as_nonterminal_impl!(AttrItem, NtMeta, P);
as_nonterminal_impl!(Path, NtPath, P);
as_nonterminal_impl!(Visibility, NtVis, P);

impl AsNonterminal for Ident {
    fn as_nonterminal(&self) -> Nonterminal {
        Nonterminal::NtIdent(*self, false)
    }
}

impl AsNonterminal for Lifetime {
    fn as_nonterminal(&self) -> Nonterminal {
        Nonterminal::NtLifetime(self.ident)
    }
}

impl AsNonterminal for Lit {
    fn as_nonterminal(&self) -> Nonterminal {
        Nonterminal::NtLiteral(mk().lit_expr(self.clone()))
    }
}

fn check_nonterminal<T>(old: &T, new: &T, cx: &mut Ctxt) -> bool
where
    T: GetSpan + AsNonterminal,
{
    let empty_ctxt = !old.get_span().from_expansion();
    if empty_ctxt {
        cx.record(old.get_span(), new.as_nonterminal());
    }
    empty_ctxt
}

pub fn match_nonterminals<T: NtMatch>(old: &T, new: &T) -> Vec<(Span, Nonterminal)> {
    let mut cx = Ctxt { nts: Vec::new() };
    NtMatch::nt_match(old, new, &mut cx);
    cx.nts
}
