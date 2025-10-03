use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::{DelimSpan, Spacing, TokenTree};
use rustc_ast::util::parser;
use rustc_ast::*;
use rustc_span::symbol::Ident;

use crate::ast_manip::{GetNodeId, GetSpan};
use crate::rewrite::base::{binop_left_prec, binop_right_prec, calc_outer_span, rewrite_seq};
use crate::rewrite::{ExprPrec, Rewrite, RewriteCtxtRef};

/// Try rewriting every child of `old` into the corresponding child of `new`.  Fails if `old` and
/// `new` don't have the same structure (for example, if they are different variants of an enum),
/// or if rewriting fails on any child.
pub fn rewrite<T: Recursive>(old: &T, new: &T, rcx: RewriteCtxtRef) -> bool {
    <T as Recursive>::recursive(old, new, rcx)
}

/// Generated trait for recursive rewriting.  `Recursive::recursive` calls `Rewrite::rewrite` on
/// each pair of corresponding children of `old` and `new`.
pub trait Recursive {
    fn recursive(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool;
}

include!(concat!(env!("OUT_DIR"), "/rewrite_recursive_gen.inc.rs"));
