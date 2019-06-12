//! Basic rewriting definitions.
//!
//! # Sequence Rewriting
//!
//! During rewriting, the case where the old and new "nodes" are in fact sequences of nodes (such
//! as `&[Item]`) is somewhat complicated.  We provide two implementations of sequence rewriting.
//! The limited implementation (`rewrite_seq_unsupported`) works for all `Rewrite`-able types, but
//! doesn't handle sequences that differ in length or make any attempt to match up nodes from the
//! old and new sequences.  The full implementation (`rewrite_seq`) intelligently handles
//! insertions, deletions, and changes between the old and new ASTs, but requires additional traits
//! to be implemented on the node type.  The generic `impl Rewrite for [T]` uses the full
//! implementation if the type `T` supports it, and otherwise falls back on the limited one.
//! Rewrite strategies for specific types can call directly into `rewrite_seq` (or an even more
//! specialized function, such as `rewrite_seq_comma_sep`) to get better results than the generic
//! `[T]` implementation.
use rustc_target::spec::abi::Abi;
use syntax::ast::*;
use syntax::parse::token::{DelimToken, Nonterminal, Token};
use syntax::source_map::{Span, SyntaxContext};
use syntax::tokenstream::{DelimSpan, TokenStream, TokenTree};
use syntax::ThinVec;

use diff;
use rustc::session::Session;
use std::fmt::Debug;
use std::rc::Rc;
use syntax::ptr::P;
use syntax::source_map::{Spanned, DUMMY_SP};
use syntax::util::parser::{AssocOp, Fixity};
use syntax_pos::{BytePos, Pos};

use crate::ast_manip::{AstDeref, CommentStyle, GetSpan};

use super::strategy;
use super::strategy::print;
use super::{ExprPrec, RewriteCtxtRef, SeqItemId, TextRewrite};

pub trait Rewrite {
    /// Given an old AST, a new AST, and text corresponding to the old AST, transform the text into
    /// text corresponding to the new AST.  `rcx` tracks the old text (in the form of a `SourceMap`)
    /// and the rewrites.  Returns `true` if rewriting succeeded, `false` otherwise.
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool;
}

include!(concat!(env!("OUT_DIR"), "/rewrite_rewrite_gen.inc.rs"));

// Generic Rewrite impls

impl<T: Rewrite> Rewrite for P<T> {
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool {
        <T as Rewrite>::rewrite(old, new, rcx)
    }
}

impl<T: Rewrite> Rewrite for Rc<T> {
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool {
        <T as Rewrite>::rewrite(old, new, rcx)
    }
}

impl<T: Rewrite> Rewrite for Spanned<T> {
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool {
        <T as Rewrite>::rewrite(&old.node, &new.node, rcx)
    }
}

impl<T: Rewrite> Rewrite for Option<T> {
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool {
        match (old, new) {
            (&Some(ref x1), &Some(ref x2)) => Rewrite::rewrite(x1, x2, rcx),
            (&None, &None) => true,
            (_, _) => false,
        }
    }
}

impl<A: Rewrite, B: Rewrite> Rewrite for (A, B) {
    fn rewrite(old: &Self, new: &Self, mut rcx: RewriteCtxtRef) -> bool {
        <A as Rewrite>::rewrite(&old.0, &new.0, rcx.borrow())
            && <B as Rewrite>::rewrite(&old.1, &new.1, rcx.borrow())
            && true
    }
}

impl<A: Rewrite, B: Rewrite, C: Rewrite> Rewrite for (A, B, C) {
    fn rewrite(old: &Self, new: &Self, mut rcx: RewriteCtxtRef) -> bool {
        <A as Rewrite>::rewrite(&old.0, &new.0, rcx.borrow())
            && <B as Rewrite>::rewrite(&old.1, &new.1, rcx.borrow())
            && <C as Rewrite>::rewrite(&old.2, &new.2, rcx.borrow())
            && true
    }
}

impl<T: MaybeRewriteSeq> Rewrite for [T] {
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool {
        MaybeRewriteSeq::maybe_rewrite_seq(old, new, DUMMY_SP, rcx)
    }
}

impl<T: MaybeRewriteSeq> Rewrite for Vec<T> {
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool {
        <[T] as Rewrite>::rewrite(&old, &new, rcx)
    }
}

impl<T: MaybeRewriteSeq> Rewrite for ThinVec<T> {
    fn rewrite(old: &Self, new: &Self, rcx: RewriteCtxtRef) -> bool {
        <[T] as Rewrite>::rewrite(&old, &new, rcx)
    }
}

// Sequence rewriting

/// Node types that are acceptable as the item type in sequence rewriting.  Sequence rewriting
/// needs a `SeqItemId` for each item in the sequence.  The additional traits support inserting,
/// deleting, and moving around of individual items.
pub trait SeqItem {
    fn seq_item_id(&self) -> SeqItemId;
}

include!(concat!(env!("OUT_DIR"), "/rewrite_seq_item_gen.inc.rs"));

impl SeqItem for Attribute {
    fn seq_item_id(&self) -> SeqItemId {
        SeqItemId::Attr(self.id)
    }
}

impl SeqItem for Arg {
    fn seq_item_id(&self) -> SeqItemId {
        SeqItemId::Node(self.id)
    }
}

impl<T: SeqItem> SeqItem for P<T> {
    fn seq_item_id(&self) -> SeqItemId {
        <T as SeqItem>::seq_item_id(self)
    }
}

/// This trait is implemented (using generated impls) for every node type.  On types that implement
/// `SeqItem`, `maybe_rewrite_seq` dispatches to `rewrite_seq`; on other types, it dispatches to
/// `rewrite_seq_unsupported`.  The only purpose of this trait is to let `impl Rewrite for [T]`
/// call one of those two functions depending on whether the type `T` implements `SeqItem`.
pub trait MaybeRewriteSeq: Rewrite + Sized {
    fn maybe_rewrite_seq(
        old: &[Self],
        new: &[Self],
        _outer_span: Span,
        rcx: RewriteCtxtRef,
    ) -> bool {
        rewrite_seq_unsupported(old, new, rcx)
    }
}

include!(concat!(
    env!("OUT_DIR"),
    "/rewrite_maybe_rewrite_seq_gen.inc.rs"
));

impl<T: Rewrite> MaybeRewriteSeq for Spanned<T> {}
impl<A: Rewrite, B: Rewrite> MaybeRewriteSeq for (A, B) {}

/// Fallback case for `rewrite_seq` on unsupported types.
pub fn rewrite_seq_unsupported<T: Rewrite>(old: &[T], new: &[T], mut rcx: RewriteCtxtRef) -> bool {
    if old.len() != new.len() {
        // Give up - hope to recover at a higher level
        return false;
    } else {
        for i in 0..old.len() {
            if !Rewrite::rewrite(&old[i], &new[i], rcx.borrow()) {
                return false;
            }
        }
        true
    }
}

/// Implementation of sequence rewriting.  In addition to the usual rewrite arguments, it accepts
/// an `outer_span`, which should cover the entire text of the old sequence, or (if the old
/// sequence is empty) should be an empty span located at the place where new items should be
/// inserted (for example, for the argument sequence of `fn foo()`, `outer_span` should be placed
/// between the empty parens).  `outer_span` can also be `DUMMY_SP` if no reasonable outer span is
/// available.
///
/// The `calc_outer_span` function helps to compute a reasonable `outer_span`.
pub fn rewrite_seq<T, R>(old: &[R], new: &[R], outer_span: Span, mut rcx: RewriteCtxtRef) -> bool
where
    T: SeqItem + print::RewriteAt + print::Splice + print::PrintParse + print::RecoverChildren + Rewrite + Debug,
    R: AstDeref<Target = T>,
{
    if old.len() == 0 && new.len() != 0 && !is_rewritable(outer_span) {
        // We can't handle this case because it provides us with no span information about the
        // `old` side.  We need at least one span so we know where to splice in any new items.
        return false;
    }

    fn ast<T: AstDeref>(x: &T) -> &<T as AstDeref>::Target {
        x.ast_deref()
    }

    // We diff the sequences of `NodeId`s to match up nodes on the left and the right.  This works
    // because the old AST has `NodeId`s assigned properly.  (The new AST might not, but in that
    // case we will properly detect a change.)
    //
    // Note we map the new IDs to corresponding old IDs, to account for NodeId renumbering.
    let new_ids = new
        .iter()
        .map(|x| rcx.new_to_old_id(ast(x).seq_item_id()))
        .collect::<Vec<_>>();
    let old_ids = old.iter().map(|x| ast(x).seq_item_id()).collect::<Vec<_>>();

    let mut i = 0;
    let mut j = 0;

    for step in diff::slice(&old_ids, &new_ids) {
        match step {
            diff::Result::Left(_) => {
                // There's an item on the left corresponding to nothing on the right.
                // Delete the item from the left.
                let old_span = ast(&old[i]).splice_span();
                let old_span = match old_ids[i] {
                    SeqItemId::Node(id) => extend_span_comments(&id, old_span, rcx.borrow()),
                    _ => old_span,
                };

                info!(
                    "DELETE {}",
                    describe(rcx.session(), old_span)
                );
                rcx.record(TextRewrite::new(old_span, DUMMY_SP));
                i += 1;
            }
            diff::Result::Right(_) => {
                // There's an item on the right corresponding to nothing on the left.
                // Insert the item before the current item on the left, rewriting
                // recursively.
                let before = if i > 0 {
                    ast(&old[i - 1]).splice_span()
                } else {
                    outer_span.shrink_to_lo()
                };
                let after = if i < old.len() {
                    ast(&old[i]).splice_span()
                } else {
                    outer_span.shrink_to_hi()
                };

                let old_span = if is_rewritable(before) {
                    before.with_lo(before.hi())
                } else if is_rewritable(after) {
                    after.with_hi(after.lo())
                } else {
                    warn!("can't insert new node between two non-rewritable nodes");
                    return true;
                };

                let ok = ast(&new[j]).rewrite_at(old_span, rcx.borrow());
                if !ok {
                    return false;
                }
                j += 1;
            }
            diff::Result::Both(_, _) => {
                let ok = Rewrite::rewrite(ast(&old[i]), ast(&new[j]), rcx.borrow());
                if !ok {
                    return false;
                }
                i += 1;
                j += 1;
            }
        }
    }

    true
}

/// Compute an `outer_span` value for performing rewriting on `seq`.  The resulting span will
/// enclose all rewritable spans found in `seq`, as well as `default`.  `default` should be a
/// reasonable insertion point when `seq` is empty; when `seq` is non-empty, it only needs to point
/// somewhere within the text of the sequence.
pub fn calc_outer_span<T: GetSpan>(seq: &[T], default: Span) -> Span {
    let mut sp = default;
    for node in seq {
        let node_sp = node.get_span();
        if is_rewritable(node_sp) {
            sp = sp.to(node_sp);
        }
    }
    sp
}

/// Like normal sequence rewriting, but on a list of comma-separated items.  Also requires a span
/// for each `old` item that covers the item itself along with its trailing comma (if any), and a
/// span covering the entire sequence (for cases where `old` is empty).
pub fn rewrite_seq_comma_sep<T, R>(
    old: &[R],
    new: &[R],
    old_spans: &[Span],
    outer_span: Span,
    has_trailing_comma: bool,
    mut rcx: RewriteCtxtRef,
) -> bool
where
    T: SeqItem + print::RewriteAt + print::Splice + print::PrintParse + print::RecoverChildren + Rewrite + Debug,
    R: AstDeref<Target = T>,
{
    fn ast<T: AstDeref>(x: &T) -> &<T as AstDeref>::Target {
        x.ast_deref()
    }

    let new_ids = new
        .iter()
        .map(|x| rcx.new_to_old_id(ast(x).seq_item_id()))
        .collect::<Vec<_>>();
    let old_ids = old.iter().map(|x| ast(x).seq_item_id()).collect::<Vec<_>>();

    let mut i = 0;
    let mut j = 0;

    let mut comma_before = true;

    for step in diff::slice(&old_ids, &new_ids) {
        match step {
            diff::Result::Left(_) => {
                // There's an item on the left corresponding to nothing on the right.
                // Delete the item from the left.
                let old_span = match old_ids[i] {
                    SeqItemId::Node(id) => extend_span_comments(&id, old_spans[i], rcx.borrow()),
                    _ => old_spans[i],
                };
                info!("DELETE {}", describe(rcx.session(), old_span));
                rcx.record(TextRewrite::new(old_span, DUMMY_SP));
                i += 1;

                if i == old.len() && !has_trailing_comma {
                    comma_before = false;
                }
            }
            diff::Result::Right(_) => {
                // There's an item on the right corresponding to nothing on the left.
                // Insert the item before the current item on the left, rewriting
                // recursively.
                let before = if i > 0 {
                    old_spans[i - 1]
                } else {
                    outer_span.shrink_to_lo()
                };
                let after = if i < old.len() {
                    old_spans[i]
                } else {
                    outer_span.shrink_to_hi()
                };

                let old_span = if is_rewritable(before) {
                    before.with_lo(before.hi())
                } else if is_rewritable(after) {
                    after.with_hi(after.lo())
                } else {
                    warn!("can't insert new node between two non-rewritable nodes");
                    return false;
                };

                if !comma_before {
                    rcx.record_text(old_span, ", ");
                }
                let ok = ast(&new[j]).rewrite_at(old_span, rcx.borrow());
                if !ok {
                    return false;
                }
                j += 1;

                if j == new.len() {
                    // Don't insert a trailing comma
                    comma_before = false;
                } else {
                    rcx.record_text(old_span, ", ");
                    comma_before = true;
                }
            }
            diff::Result::Both(_, _) => {
                let ok = Rewrite::rewrite(ast(&old[i]), ast(&new[j]), rcx.borrow());
                if !ok {
                    return false;
                }
                i += 1;
                j += 1;

                if i == old.len() && !has_trailing_comma {
                    comma_before = false;
                }
            }
        }
    }

    true
}

// Misc helpers

pub fn binop_left_prec(op: &BinOp) -> ExprPrec {
    let assoc_op = AssocOp::from_ast_binop(op.node);
    let prec = assoc_op.precedence() as i8;
    let fixity = assoc_op.fixity();

    let prec = match fixity {
        Fixity::Left => prec,
        Fixity::Right => prec + 1,
        Fixity::None => prec + 1,
    };

    match assoc_op {
        AssocOp::Less | AssocOp::LessEqual | AssocOp::ObsoleteInPlace => ExprPrec::LeftLess(prec),
        _ => ExprPrec::Normal(prec),
    }
}

pub fn binop_right_prec(op: &BinOp) -> ExprPrec {
    let assoc_op = AssocOp::from_ast_binop(op.node);
    let prec = assoc_op.precedence() as i8;
    let fixity = assoc_op.fixity();

    let prec = match fixity {
        Fixity::Left => prec + 1,
        Fixity::Right => prec,
        Fixity::None => prec + 1,
    };
    ExprPrec::Normal(prec)
}

/// Checks if a span has corresponding source text that we can rewrite (or use as source text to
/// rewrite something else).  Rewriting macro bodies would be very complicated, so we just declare
/// all macro-generated code to be non-rewritable.
///
/// Note that this does not require the source text to exist in a real (non-virtual) file - there
/// just has to be text somewhere in the `SourceMap`.
pub fn is_rewritable(sp: Span) -> bool {
    sp != DUMMY_SP &&
    // If it has a non-default SyntaxContext, it was generated as part of a macro expansion.
    sp.ctxt() == SyntaxContext::empty()
}

pub fn describe(sess: &Session, span: Span) -> String {
    let cm = sess.source_map();
    let loc = cm.span_to_string(span);
    let src = cm.span_to_snippet(span);

    if let Ok(src) = src {
        format!("{}: {}", loc, src)
    } else {
        loc
    }
}

/// Extend a node span to cover comments around it.
pub fn extend_span_comments(id: &NodeId, mut span: Span, rcx: RewriteCtxtRef) -> Span {
    let comments = match rcx.comments().get(id) {
        Some(comments) if comments.is_empty() => return span,
        Some(comments) => comments,
        None => return span,
    };

    debug!("Extending span comments for {:?} for comments: {:?}", span, comments);

    let mut before = vec![];
    let mut after = vec![];
    for comment in comments {
        match comment.style {
            CommentStyle::Isolated => {
                before.push(comment);
            }

            CommentStyle::Trailing => {
                after.push(comment);
            }

            _ => unimplemented!("Mixed and BlankLine comment styles are not implemented"),
        }
    }

    before.sort_by_key(|c| c.pos);
    after.sort_by_key(|c| c.pos);

    before.reverse();

    for comment in &before {
        let comment_span = span.shrink_to_lo().with_lo(comment.pos);
        let source = rcx.session().source_map().span_to_snippet(comment_span).unwrap();
        let matches = source.lines().zip(&comment.lines).all(|(src_line, comment_line)| {
            src_line.trim() == comment_line.trim()
        });
        if matches {
            let mut comment_pos = comment.pos;

            // Extend to previous newline because this is an isolated comment
            let comment_begin = rcx.session().source_map().lookup_byte_offset(comment.pos);
            let mut extend_comment_pos = |src: &str| {
                if let Some(newline_index) = src[..comment_begin.pos.to_usize()].rfind('\n') {
                    comment_pos = BytePos::from_usize(newline_index) + comment_begin.sf.start_pos;
                }
            };
            if let Some(ref src) = comment_begin.sf.src {
                extend_comment_pos(src);
            } else if let Some(src) = comment_begin.sf.external_src.borrow().get_source() {
                extend_comment_pos(src);
            }

            span = span.with_lo(comment_pos);
        } else {
            debug!("comment {:?} did not match source {:?}", comment, source);
            break;
        }
    }

    for comment in &after {
        for comment_line in &comment.lines {
            let line_end = if comment_line.starts_with("//") {
                BytePos::from_usize(span.hi().to_usize() + comment_line.len() + 1)
            } else {
                BytePos::from_usize(span.hi().to_usize() + comment_line.len())
            };
            let line_span = span.shrink_to_hi().with_hi(line_end);
            let src_line = rcx.session().source_map().span_to_snippet(line_span).unwrap();
            if comment_line.trim() == src_line.trim() {
                span = span.with_hi(line_end);
            } else {
                // We need to break out of processing any after comments because
                // a line didn't match.
                debug!("comment {:?} did not match line {:?}", comment_line, src_line);
                return span;
            }
        }
    }

    span
}
