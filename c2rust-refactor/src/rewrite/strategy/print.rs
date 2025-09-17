//! Pretty-printer based rewriting strategy.
//!
//! The basic idea here is to pretty-print the new AST, which gives text that is guaranteed to
//! parse to the new AST (assuming the printer is correct), and then splice that text into the
//! source in place of the old AST's text.
//!
//! It turns out that in many cases, some subtrees of the new AST actually came from the old AST,
//! and thus have source available.  We'd prefer to use that old source text instead of the
//! pretty-printer output, since it likely has nicer formatting, comments, etc.  So there is some
//! logic in this module for "recovering" from needing to use this strategy by splicing old AST
//! text back into the new AST's pretty printer output.
use log::{info, warn};
use rustc_ast::attr;
use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::{DelimSpan, LazyTokenStream, Spacing, TokenStream, TokenTree};
use rustc_ast::util::comments::CommentStyle;
use rustc_ast::util::parser;
use rustc_ast::*;
use rustc_ast_pretty::pprust::{self, PrintState};
use rustc_data_structures::sync::Lrc;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_parse::parser::attr::InnerAttrPolicy;
use rustc_session::Session;
use rustc_span::hygiene::SyntaxContext;
use rustc_span::source_map::{BytePos, FileName, RealFileName, SourceFile, Span, Spanned};
use rustc_span::symbol::{Ident, Symbol};
use rustc_span::DUMMY_SP;
use rustc_target::spec::abi::Abi;
use std::fmt::Debug;
use std::fs;
use std::path;
use std::rc::Rc;

use crate::ast_manip::util::extend_span_attrs;
use crate::ast_manip::NodeTable;
use crate::ast_manip::{AstDeref, GetSpan, MaybeGetNodeId};
use crate::driver;
use crate::rewrite::base::{binop_left_prec, binop_right_prec};
use crate::rewrite::base::{
    describe, extend_span_comments, extend_span_comments_strict, is_rewritable,
    rewind_span_over_whitespace,
};
use crate::rewrite::{ExprPrec, Rewrite, RewriteCtxt, RewriteCtxtRef, TextAdjust, TextRewrite};
use crate::util::Lone;

// PrintParse

/// Trait for nodes that can be printed and reparsed.
///
/// Someday it may be useful to separate this into `Print` and `Parse` traits (and move them out of
/// this module to a more general location), but right everything we care to print is also pretty
/// easy to parse.
pub trait PrintParse {
    /// Pretty print this node.
    fn to_string(&self) -> String;

    /// The result type of `Self::parse`.
    type Parsed: AstDeref<Target = Self>;
    /// Parse a string to a node of this type.  Panics if parsing fails.
    fn parse(sess: &Session, src: &str) -> Self::Parsed;
}

impl PrintParse for Expr {
    fn to_string(&self) -> String {
        pprust::expr_to_string(self)
    }

    type Parsed = P<Expr>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_expr(sess, src)
    }
}

impl PrintParse for Pat {
    fn to_string(&self) -> String {
        pprust::pat_to_string(self)
    }

    type Parsed = P<Pat>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_pat(sess, src)
    }
}

impl PrintParse for Ty {
    fn to_string(&self) -> String {
        pprust::ty_to_string(self)
    }

    type Parsed = P<Ty>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_ty(sess, src)
    }
}

impl PrintParse for Stmt {
    fn to_string(&self) -> String {
        // pprust::stmt_to_string appends a semicolon to Expr kind statements,
        // not just to Semi kind statements. We want to differentiate these
        // nodes.
        match self.kind {
            StmtKind::Expr(ref expr) => pprust::expr_to_string(expr),
            _ => pprust::to_string(|s| {
                s.stmt_to_string(self);
            }),
        }
    }

    type Parsed = Stmt;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_stmts(sess, src).lone()
    }
}

impl PrintParse for Item {
    fn to_string(&self) -> String {
        pprust::item_to_string(self)
    }

    type Parsed = P<Item>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_items(sess, src).lone()
    }
}

// TODO: ImplItem

impl PrintParse for ForeignItem {
    fn to_string(&self) -> String {
        pprust::to_string(|s| {
            s.foreign_item_to_string(self);
        })
    }

    type Parsed = P<ForeignItem>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_foreign_items(sess, src).lone()
    }
}

impl PrintParse for Block {
    fn to_string(&self) -> String {
        pprust::to_string(|s| {
            s.block_to_string(self);
        })
    }

    type Parsed = P<Block>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_block(sess, src)
    }
}

impl PrintParse for Param {
    fn to_string(&self) -> String {
        pprust::to_string(|s| {
            s.param_to_string(self);
        })
    }

    type Parsed = Param;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_arg(sess, src)
    }
}

impl PrintParse for Attribute {
    fn to_string(&self) -> String {
        pprust::attribute_to_string(self)
    }

    type Parsed = Attribute;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::run_parser(sess, src, |p| {
            match p.token.kind {
                // `parse_attribute` doesn't handle inner or outer doc comments.
                TokenKind::DocComment(kind, style, s) => {
                    assert!(src.ends_with('\n'));
                    // Expand the `span` to include the trailing \n.  Otherwise multiple spliced
                    // doc comments will run together into a single line.
                    let span = p.token.span.with_hi(p.token.span.hi() + BytePos(1));
                    let attr = attr::mk_doc_comment(kind, style, s, span);
                    p.bump();
                    return Ok(attr);
                }
                _ => p.parse_attribute(InnerAttrPolicy::Permitted),
            }
        })
    }
}

// Splice

/// Node types for which we can splice the node text into/out of the source.
pub trait Splice {
    /// Get a span that covers the entire text of the node.  This is used as the source or
    /// destination span when splicing text.
    fn splice_span(&self) -> Span;

    /// Get the text adjustment (such as parenthesization) to apply to the printed text before
    /// splicing it in.  This relies on the `RewriteCtxt` accurately tracking the `ExprPrec`s of
    /// the parent nodes of the destination location.
    fn get_adjustment(&self, _rcx: &RewriteCtxt) -> TextAdjust {
        TextAdjust::None
    }
}

impl Splice for Expr {
    fn splice_span(&self) -> Span {
        extend_span_attrs(self.span, &self.attrs)
    }

    fn get_adjustment(&self, rcx: &RewriteCtxt) -> TextAdjust {
        // Check for cases where we can safely omit parentheses.
        let prec = self.precedence();
        let need_parens = match rcx.expr_prec() {
            ExprPrec::Normal(min_prec) => prec.order() < min_prec,
            ExprPrec::Cond(min_prec) => {
                prec.order() < min_prec || parser::contains_exterior_struct_lit(self)
            }
            ExprPrec::Callee(min_prec) => match self.kind {
                ExprKind::Field(..) => true,
                _ => prec.order() < min_prec,
            },
            ExprPrec::LeftLess(min_prec) => match self.kind {
                ExprKind::Cast(..) | ExprKind::Type(..) => true,
                _ => prec.order() < min_prec,
            },
        };

        if need_parens {
            TextAdjust::Parenthesize
        } else {
            TextAdjust::None
        }
    }
}

impl Splice for Pat {
    fn splice_span(&self) -> Span {
        self.span
    }
}

impl Splice for Ty {
    fn splice_span(&self) -> Span {
        self.span
    }
}

impl Splice for Stmt {
    fn splice_span(&self) -> Span {
        self.span
    }
}

impl Splice for Item {
    fn splice_span(&self) -> Span {
        extend_span_attrs(self.span, &self.attrs)
    }
}

impl Splice for ForeignItem {
    fn splice_span(&self) -> Span {
        extend_span_attrs(self.span, &self.attrs)
    }
}

impl Splice for Block {
    fn splice_span(&self) -> Span {
        self.span
    }
}

impl Splice for Param {
    fn splice_span(&self) -> Span {
        self.pat.span.to(self.ty.span)
    }
}

impl Splice for Attribute {
    fn splice_span(&self) -> Span {
        self.span
    }
}

// Recover

/// Node types for which we can recover an old AST that has associated text.
pub trait Recover {
    /// Obtain from the `RewriteCtxt` the table of old nodes of this type.
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self>;
}

impl Recover for Expr {
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self> {
        &rcx.old_nodes().exprs
    }
}

impl Recover for Pat {
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self> {
        &rcx.old_nodes().pats
    }
}

impl Recover for Ty {
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self> {
        &rcx.old_nodes().tys
    }
}

impl Recover for Stmt {
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self> {
        &rcx.old_nodes().stmts
    }
}

impl Recover for Item {
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self> {
        &rcx.old_nodes().items
    }
}

impl Recover for ForeignItem {
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self> {
        &rcx.old_nodes().foreign_items
    }
}

impl Recover for Block {
    fn node_table<'a, 's>(rcx: &'a RewriteCtxt<'s>) -> &'a NodeTable<'s, Self> {
        &rcx.old_nodes().blocks
    }
}

// RecoverChildren

/// Codegenned trait for recursively traversing new and reparsed ASTs, looking for places we can
/// invoke `recover`.
pub trait RecoverChildren: Debug {
    /// Recursively attempt to `recover()` descendants of `reparsed`/`new`, not including
    /// `reparsed`/`new` itself.
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef);

    /// Try to `recover` the node itself (if this node type implements `Recover`), then try to
    /// `recover_children`.
    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef);

    /// Attempt "restricted recovery" of the node itself, then try to `recover_children`.
    /// Restricted recovery succeeds only if the recovered AST has a different span than the old
    /// AST (otherwise we would get stuck in an infinite loop, replacing the old AST and old text
    /// with identical copies of themselves).
    fn recover_node_restricted(old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef);
}

impl<T: RecoverChildren + ?Sized> RecoverChildren for P<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_children(reparsed, new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_and_children(reparsed, new, rcx)
    }

    fn recover_node_restricted(old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_restricted(old_span, reparsed, new, rcx)
    }
}

impl<T: RecoverChildren + ?Sized> RecoverChildren for Box<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_children(reparsed, new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_and_children(reparsed, new, rcx)
    }

    fn recover_node_restricted(old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_restricted(old_span, reparsed, new, rcx)
    }
}

impl<T: RecoverChildren + ?Sized> RecoverChildren for Rc<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_children(reparsed, new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_and_children(reparsed, new, rcx)
    }

    fn recover_node_restricted(old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_restricted(old_span, reparsed, new, rcx)
    }
}

impl<T: RecoverChildren> RecoverChildren for Spanned<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_children(&reparsed.node, &new.node, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_and_children(&reparsed.node, &new.node, rcx)
    }

    fn recover_node_restricted(old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_restricted(old_span, &reparsed.node, &new.node, rcx)
    }
}

impl<T: RecoverChildren> RecoverChildren for Option<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        match (reparsed, new) {
            (&Some(ref x1), &Some(ref x2)) => {
                RecoverChildren::recover_children(x1, x2, rcx);
            }
            (_, _) => {}
        }
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        match (reparsed, new) {
            (&Some(ref x1), &Some(ref x2)) => {
                RecoverChildren::recover_node_and_children(x1, x2, rcx);
            }
            (_, _) => {}
        }
    }

    fn recover_node_restricted(_old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        // This type never implements `Recover`, so just call `recover_children`.
        RecoverChildren::recover_children(reparsed, new, rcx);
    }
}

impl<A: RecoverChildren, B: RecoverChildren> RecoverChildren for (A, B) {
    fn recover_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        <A as RecoverChildren>::recover_children(&reparsed.0, &new.0, rcx.borrow());
        <B as RecoverChildren>::recover_children(&reparsed.1, &new.1, rcx.borrow());
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        <A as RecoverChildren>::recover_node_and_children(&reparsed.0, &new.0, rcx.borrow());
        <B as RecoverChildren>::recover_node_and_children(&reparsed.1, &new.1, rcx.borrow());
    }

    fn recover_node_restricted(_old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        // This type never implements `Recover`, so just call `recover_children`.
        RecoverChildren::recover_children(reparsed, new, rcx);
    }
}

impl<A: RecoverChildren, B: RecoverChildren, C: RecoverChildren> RecoverChildren for (A, B, C) {
    fn recover_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        <A as RecoverChildren>::recover_children(&reparsed.0, &new.0, rcx.borrow());
        <B as RecoverChildren>::recover_children(&reparsed.1, &new.1, rcx.borrow());
        <C as RecoverChildren>::recover_children(&reparsed.2, &new.2, rcx.borrow());
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        <A as RecoverChildren>::recover_node_and_children(&reparsed.0, &new.0, rcx.borrow());
        <B as RecoverChildren>::recover_node_and_children(&reparsed.1, &new.1, rcx.borrow());
        <C as RecoverChildren>::recover_node_and_children(&reparsed.2, &new.2, rcx.borrow());
    }

    fn recover_node_restricted(_old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        // This type never implements `Recover`, so just call `recover_children`.
        RecoverChildren::recover_children(reparsed, new, rcx);
    }
}

impl<T: RecoverChildren> RecoverChildren for [T] {
    fn recover_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        assert!(
            reparsed.len() == new.len(),
            "new and reparsed ASTs don't match: {:?} != {:?}",
            new,
            reparsed
        );
        for i in 0..reparsed.len() {
            RecoverChildren::recover_children(&reparsed[i], &new[i], rcx.borrow());
        }
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        assert!(
            reparsed.len() == new.len(),
            "new and reparsed ASTs don't match: {:?} != {:?}",
            new,
            reparsed
        );
        for i in 0..reparsed.len() {
            RecoverChildren::recover_node_and_children(&reparsed[i], &new[i], rcx.borrow());
        }
    }

    fn recover_node_restricted(_old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        // This type never implements `Recover`, so just call `recover_children`.
        RecoverChildren::recover_children(reparsed, new, rcx);
    }
}

impl<T: RecoverChildren> RecoverChildren for Vec<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_children(&reparsed, &new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_node_and_children(&reparsed, &new, rcx)
    }

    fn recover_node_restricted(old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_node_restricted(old_span, &reparsed, &new, rcx)
    }
}

impl<T: RecoverChildren> RecoverChildren for ThinVec<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_children(&reparsed, &new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_node_and_children(&reparsed, &new, rcx)
    }

    fn recover_node_restricted(old_span: Span, reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_node_restricted(old_span, &reparsed, &new, rcx)
    }
}

include!(concat!(
    env!("OUT_DIR"),
    "/rewrite_recover_children_gen.inc.rs"
));

/// Try to replace the text for `reparsed` with recovered text for `new`.  This works as
/// follows:
///
///  1. Find a node that has text available and whose `NodeId` matches `new.id`.  This is the
///     "old" node.  Get the old node's text and AST.
///  2. Rewrite the old text to match the new AST.  This is necessary because things other than
///     the `NodeId` may differ between the old and new ASTs.
///  3. Splice the rewritten text in place of the text for `reparsed`.
///
/// Returns `true` if all steps succeed.  Returns `false` if it fails to find an old node or if
/// it fails to rewrite the old node to match `new`.
fn recover<'s, T>(
    maybe_restricted_span: Option<Span>,
    reparsed: &T,
    new: &T,
    mut rcx: RewriteCtxtRef<'s, '_>,
) -> bool
where
    T: MaybeGetNodeId + Recover + Rewrite + Splice + 's,
{
    // Find a node with ID matching `new.id`, after accounting for renumbering of NodeIds.
    let old_id = rcx.new_to_old_id(new.get_node_id());
    let old = match <T as Recover>::node_table(&rcx).get(old_id) {
        Some(x) => x,
        None => {
            return false;
        }
    };

    let old_span = rewind_span_over_whitespace(old.splice_span(), &rcx);
    let old_span = match extend_span_comments_strict(&old_id, old_span, &rcx) {
        Ok(span) => span,
        Err(_) => return false,
    };
    let reparsed_span = extend_span_comments(&old_id, reparsed.splice_span(), &rcx);

    if !is_rewritable(old_span) {
        return false;
    }

    let sm = rcx.session().source_map();
    if sm.is_imported(old_span) {
        return false;
    }

    // If `maybe_restricted_span` is set, then we can only proceed if `old.splice_span() !=
    // restricted_span`.  What's really going on here is that `restricted_span` is the `old_span`
    // of the enclosing `rewrite_at`, and we need to avoid infinitely recursing through
    // `rewrite_at` and `recover` on the same node.
    if let Some(restricted_span) = maybe_restricted_span {
        if old.splice_span() == restricted_span {
            return false;
        }
    }

    info!("REVERT {}", describe(rcx.session(), reparsed_span));
    info!("    TO {}", describe(rcx.session(), old_span));

    let mut rw = TextRewrite::adjusted(reparsed_span, old_span, new.get_adjustment(&rcx));
    let mark = rcx.mark();
    let ok = Rewrite::rewrite(old, new, rcx.enter(&mut rw));
    if !ok {
        rcx.rewind(mark);
        return false;
    }

    rcx.record(rw);
    true
}

pub fn rewrite<T>(old: &T, new: &T, rcx: RewriteCtxtRef) -> bool
where
    T: PrintParse + RecoverChildren + Splice + MaybeGetNodeId,
{
    if !is_rewritable(old.splice_span()) {
        // If we got here, it means rewriting failed somewhere inside macro-generated code, and
        // outside any chunks of AST that the macro copied out of its arguments (those chunks
        // would have non-dummy spans, and would be spliced in already).  We give up on this
        // part of the rewrite when this happens, because rewriting inside the RHS of a
        // macro_rules! macro would be very difficult, and for procedural macros it's just
        // impossible.  But we still report success (`return true`) because we don't want to force
        // replacement of the macro with its expansion.
        warn!("can't splice in fresh text for a non-rewritable node");
        return true;
    }
    new.rewrite_at(old.splice_span(), rcx)
}

fn describe_rewrite(old_span: Span, new_span: Span, rcx: &RewriteCtxt) {
    if old_span.lo() != old_span.hi() {
        info!("REWRITE {}", describe(rcx.session(), old_span));
        info!("   INTO {}", describe(rcx.session(), new_span));
    } else {
        info!("INSERT AT {}", describe(rcx.session(), old_span));
        info!("     TEXT {}", describe(rcx.session(), new_span));
    }
}

fn add_comments<T>(s: String, node: &T, rcx: &RewriteCtxt) -> String
where
    T: MaybeGetNodeId,
{
    if <T as MaybeGetNodeId>::supported() {
        if let Some(comments) = rcx.comments().get(&rcx.new_to_old_id(node.get_node_id())) {
            let mut new_s = String::new();
            let mut sorted_comments = comments.iter().collect::<Vec<_>>();
            sorted_comments.sort_by_key(|c| c.pos);
            for comment in &sorted_comments {
                if comment.style == CommentStyle::Isolated {
                    new_s.push('\n');
                    comment.lines.iter().for_each(|s| {
                        new_s.push_str(s.as_str());
                        new_s.push('\n');
                    });
                }
            }
            new_s.push_str(&s);
            for comment in &sorted_comments {
                if comment.style == CommentStyle::Trailing {
                    comment.lines.iter().for_each(|s| {
                        new_s.push_str(s.as_str());
                        new_s.push('\n');
                    });
                }
            }

            return new_s;
        }
    }

    s
}

fn rewrite_at_impl<T>(old_span: Span, new: &T, mut rcx: RewriteCtxtRef) -> bool
where
    T: PrintParse + RecoverChildren + Splice + MaybeGetNodeId,
{
    let printed = add_comments(new.to_string(), new, &rcx);
    let reparsed = T::parse(rcx.session(), &printed);
    let reparsed = reparsed.ast_deref();

    let mut expanded_old_span = old_span;
    let mut reparsed_span = reparsed.splice_span();

    if <T as MaybeGetNodeId>::supported() {
        let old_id = rcx.new_to_old_id(new.get_node_id());
        expanded_old_span = extend_span_comments(&old_id, old_span, &rcx);
        reparsed_span = match extend_span_comments_strict(&old_id, reparsed_span, &rcx) {
            Ok(span) => rewind_span_over_whitespace(span, &rcx),
            Err(_) => return false,
        };
    }
    describe_rewrite(expanded_old_span, reparsed_span, &rcx);

    let mut rw = TextRewrite::adjusted(expanded_old_span, reparsed_span, new.get_adjustment(&rcx));
    // Try recovery, starting in "restricted mode" to avoid infinite recursion.
    // The guarantee of `recover_node_restricted` is that if it calls into
    // `Rewrite::rewrite(old2, new2, ...)`, then `old2.splice_span() !=
    // old_span`, so we won't end up back here in `rewrite_at` with identical
    // arguments.
    RecoverChildren::recover_node_restricted(old_span, reparsed, new, rcx.enter(&mut rw));

    rcx.record(rw);
    true
}

pub trait RewriteAt {
    fn rewrite_at(&self, old_span: Span, rcx: RewriteCtxtRef) -> bool;
}

impl<T> RewriteAt for T
where
    T: PrintParse + RecoverChildren + Splice + MaybeGetNodeId,
{
    default fn rewrite_at(&self, old_span: Span, rcx: RewriteCtxtRef) -> bool {
        rewrite_at_impl(old_span, self, rcx)
    }
}

fn create_file_for_module(
    module_item: &Item,
    old_span: Span,
    sess: &Session,
) -> (Lrc<SourceFile>, Option<Attribute>) {
    let source_map = sess.source_map();
    let old_sf = source_map.lookup_byte_offset(old_span.lo()).sf;
    let mut path_attr = None;
    let filename = match old_sf.name.clone() {
        FileName::Real(path) => {
            let mod_file_name = format!("{}.rs", module_item.ident.to_string());
            let mut path = path
                .into_local_path()
                .expect("module file has no local path");
            if let Some(path_attr) = crate::util::first_attr_value_str_by_name(
                &module_item.attrs,
                Symbol::intern("path"),
            ) {
                path.pop();
                path.push(path_attr.to_string());
            } else {
                if sess
                    .local_crate_source_file
                    .as_ref()
                    .map_or(false, |f| *f == path)
                {
                    path.pop();
                    if path.file_name().map_or(true, |path| path != "src") {
                        path.push("src");

                        // Attempt to create the output directory, if it doesn't
                        // exist
                        let _ = fs::create_dir_all(&path);

                        // Add a #[path = "..."] attribute
                        let path_item = attr::mk_name_value_item_str(
                            Ident::from_str("path"),
                            Symbol::intern(&format!(
                                "src{}{}",
                                path::MAIN_SEPARATOR,
                                mod_file_name,
                            )),
                            DUMMY_SP,
                        );
                        path_attr = Some(attr::mk_attr_outer(path_item));
                    }
                } else {
                    if path.file_name().unwrap() == "mod.rs" {
                        path.pop();
                    } else {
                        let parent_name = path.file_stem().unwrap().to_os_string();
                        path.pop();
                        path.push(parent_name);
                    }
                }
            }
            path.push(mod_file_name);
            path
        }

        _ => panic!(
            "Could not construct file path for external module {:?}",
            module_item.ident
        ),
    };

    let rfn = RealFileName::LocalPath(filename);
    let sf = source_map.new_source_file(FileName::Real(rfn), String::new());
    (sf, path_attr)
}

impl RewriteAt for Item {
    fn rewrite_at(&self, old_span: Span, mut rcx: RewriteCtxtRef) -> bool {
        if let ItemKind::Mod(_, ModKind::Loaded(m_items, m_inline, m_spans)) = &self.kind {
            if *m_inline == Inline::No {
                // We need to print the `mod name;` in the parent and the module
                // contents in its own file. If there are no items, delete the
                // `mod name;`.

                if m_items.is_empty() {
                    info!("DELETE {}", describe(rcx.session(), old_span));
                    rcx.record(TextRewrite::new(old_span, DUMMY_SP));
                    return true;
                }

                let mut item = self.clone();

                // let old_span = rewind_span_over_whitespace(old_span, &rcx);

                // Where should the module contents be printed?
                let inner_span = if !is_rewritable(m_spans.inner_span) {
                    // Need to create a new file
                    let (sf, path_attr) = create_file_for_module(self, old_span, rcx.session());
                    if let Some(attr) = path_attr {
                        item.attrs.push(attr);
                    }
                    Span::new(sf.start_pos, sf.end_pos, SyntaxContext::root(), None)
                } else {
                    m_spans.inner_span
                };

                // Print the module (mod foo;) in the parent
                let printed = add_comments(item.to_string(), &item, &rcx);
                let reparsed = Self::parse(rcx.session(), &printed);
                let reparsed = reparsed.ast_deref();

                describe_rewrite(old_span, reparsed.splice_span(), &rcx);

                let rw = TextRewrite::adjusted(
                    old_span,
                    reparsed.splice_span(),
                    self.get_adjustment(&rcx),
                );
                rcx.record(rw);

                // Print the module items in the external file
                let mut printed = pprust::to_string(|s| {
                    s.print_inner_attributes(&self.attrs);
                });
                for item in m_items {
                    printed.push_str(&add_comments(item.to_string(), item, &rcx));
                }
                let reparsed = driver::parse_items(rcx.session(), &printed);

                // Extend span to cover comments before and after items
                let first_node = reparsed.first().unwrap();
                let first_span =
                    extend_span_comments(&first_node.get_node_id(), first_node.splice_span(), &rcx);
                let last_node = reparsed.last().unwrap();
                let last_span =
                    extend_span_comments(&last_node.get_node_id(), last_node.splice_span(), &rcx);
                let reparsed_span = first_span.with_hi(last_span.hi());

                describe_rewrite(inner_span, reparsed_span, &rcx);
                let mut rw =
                    TextRewrite::adjusted(inner_span, reparsed_span, self.get_adjustment(&rcx));
                RecoverChildren::recover_children(&reparsed, &m_items, rcx.enter(&mut rw));
                rcx.record(rw);

                return true;
            }
        }

        // Default to rewrite_at_impl for inline modules and other items
        rewrite_at_impl(old_span, self, rcx)
    }
}
