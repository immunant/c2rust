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
use std::rc::Rc;
use rustc::session::Session;
use rustc_target::spec::abi::Abi;
use syntax::ast::*;
use syntax::attr;
use syntax::source_map::{Span, Spanned, BytePos, FileName};
use syntax::ext::hygiene::SyntaxContext;
use syntax::parse::token::{Token, DelimToken, Nonterminal};
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::tokenstream::{TokenTree, Delimited, TokenStream, ThinTokenStream};
use syntax::util::parser;

use ast_manip::{GetNodeId, AstDeref};
use ast_manip::util::extended_span;
use driver;
use rewrite::{Rewrite, RewriteCtxt, RewriteCtxtRef, TextAdjust, ExprPrec, NodeTable};
use rewrite::base::{is_rewritable, describe};
use rewrite::base::{binop_left_prec, binop_right_prec};
use util::Lone;


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
    type Parsed: AstDeref<Target=Self>;
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
        match self.node {
            StmtKind::Expr(ref expr) => pprust::expr_to_string(expr),
            _ => pprust::stmt_to_string(self),
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
        pprust::to_string(|s| s.print_foreign_item(self))
    }

    type Parsed = ForeignItem;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_foreign_items(sess, src).lone()
    }
}

impl PrintParse for Block {
    fn to_string(&self) -> String {
        pprust::block_to_string(self)
    }

    type Parsed = P<Block>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_block(sess, src)
    }
}

impl PrintParse for Arg {
    fn to_string(&self) -> String {
        pprust::arg_to_string(self)
    }

    type Parsed = Arg;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_arg(sess, src)
    }
}

impl PrintParse for Attribute {
    fn to_string(&self) -> String {
        pprust::attr_to_string(self)
    }

    type Parsed = Attribute;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::run_parser(sess, src, |p| {
            match p.token {
                // `parse_attribute` doesn't handle inner or outer doc comments.
                Token::DocComment(s) => {
                    assert!(src.ends_with('\n'));
                    // Expand the `span` to include the trailing \n.  Otherwise multiple spliced
                    // doc comments will run together into a single line.
                    let span = p.span.with_hi(p.span.hi() + BytePos(1));
                    let attr = attr::mk_sugared_doc_attr(attr::mk_attr_id(), s, span);
                    p.bump();
                    return Ok(attr);
                },
                _ => p.parse_attribute(true),
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
        extended_span(self.span, &self.attrs)
    }

    fn get_adjustment(&self, rcx: &RewriteCtxt) -> TextAdjust {
        // Check for cases where we can safely omit parentheses.
        let prec = self.precedence();
        let need_parens = match rcx.expr_prec() {
            ExprPrec::Normal(min_prec) => prec.order() < min_prec,
            ExprPrec::Cond(min_prec) =>
                prec.order() < min_prec || parser::contains_exterior_struct_lit(self),
            ExprPrec::Callee(min_prec) => match self.node {
                ExprKind::Field(..) => true,
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
        extended_span(self.span, &self.attrs)
    }
}

impl Splice for ForeignItem {
    fn splice_span(&self) -> Span {
        extended_span(self.span, &self.attrs)
    }
}

impl Splice for Block {
    fn splice_span(&self) -> Span {
        self.span
    }
}

impl Splice for Arg {
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
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self>;
}

impl Recover for Expr {
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_exprs()
    }
}

impl Recover for Pat {
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_pats()
    }
}

impl Recover for Ty {
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_tys()
    }
}

impl Recover for Stmt {
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_stmts()
    }
}

impl Recover for Item {
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_items()
    }
}

impl Recover for ForeignItem {
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_foreign_items()
    }
}

impl Recover for Block {
    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_blocks()
    }
}


// RecoverChildren

/// Codegenned trait for recursively traversing new and reparsed ASTs, looking for places we can
/// invoke `recover`.
pub trait RecoverChildren {
    /// Recursively attempt to `recover()` descendants of `reparsed`/`new`, not including
    /// `reparsed`/`new` itself.
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef);

    /// Try to `recover` the node itself (if this node type implements `Recover`), then try to
    /// `recover_children`.
    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef);
}

impl<T: RecoverChildren> RecoverChildren for P<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_children(reparsed, new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_and_children(reparsed, new, rcx)
    }
}

impl<T: RecoverChildren> RecoverChildren for Rc<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_children(reparsed, new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_and_children(reparsed, new, rcx)
    }
}

impl<T: RecoverChildren> RecoverChildren for Spanned<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_children(&reparsed.node, &new.node, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <T as RecoverChildren>::recover_node_and_children(&reparsed.node, &new.node, rcx)
    }
}

impl<T: RecoverChildren> RecoverChildren for Option<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        match (reparsed, new) {
            (&Some(ref x1),
             &Some(ref x2)) => {
                RecoverChildren::recover_children(x1, x2, rcx);
            }
            (_, _) => {},
        }
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        match (reparsed, new) {
            (&Some(ref x1),
             &Some(ref x2)) => {
                RecoverChildren::recover_node_and_children(x1, x2, rcx);
            }
            (_, _) => {},
        }
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
}

impl<T: RecoverChildren> RecoverChildren for [T] {
    fn recover_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        assert!(reparsed.len() == new.len(),
                "new and reprinted ASTs don't match");
        for i in 0 .. reparsed.len() {
            RecoverChildren::recover_children(&reparsed[i], &new[i], rcx.borrow());
        }
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, mut rcx: RewriteCtxtRef) {
        assert!(reparsed.len() == new.len(),
                "new and reprinted ASTs don't match");
        for i in 0 .. reparsed.len() {
            RecoverChildren::recover_node_and_children(&reparsed[i], &new[i], rcx.borrow());
        }
    }
}

impl<T: RecoverChildren> RecoverChildren for Vec<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_children(&reparsed, &new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_node_and_children(&reparsed, &new, rcx)
    }
}

impl<T: RecoverChildren> RecoverChildren for ThinVec<T> {
    fn recover_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_children(&reparsed, &new, rcx)
    }

    fn recover_node_and_children(reparsed: &Self, new: &Self, rcx: RewriteCtxtRef) {
        <[T] as RecoverChildren>::recover_node_and_children(&reparsed, &new, rcx)
    }
}

include!(concat!(env!("OUT_DIR"), "/rewrite_recover_children_gen.inc.rs"));



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
fn recover<'s, T>(reparsed: &T, new: &T, mut rcx: RewriteCtxtRef<'s, '_>) -> bool
        where T: GetNodeId + Recover + Rewrite + Splice + 's {
    // Don't try to replace the entire fresh subtree with old text.   This breaks an infinite
    // recursion when a non-splice-point child differs between the old and new ASTs.  In such a
    // situation, `splice_recycled` wants to replace the old text with newly printed text
    // (because `old != new`), but `splice_fresh` wants to replace the printed text with the
    // old text (because `new` still has a source span covering the old text).  It's always
    // safe to use printed text instead of old text, so we bail out here if we detect this.
    if new.splice_span() == rcx.fresh_start() {
        return false;
    }

    // Find a node with ID matching `new.id`, after accounting for renumbering of NodeIds.
    let old_id = rcx.new_to_old_id(new.get_node_id());
    let old = match <T as Recover>::node_table(&mut rcx).get(old_id) {
        Some(x) => x,
        None => {
            return false;
        },
    };


    if !is_rewritable(old.splice_span()) {
        return false;
    }

    let fm = rcx.session().codemap().lookup_byte_offset(old.splice_span().lo()).fm;
    if let FileName::Macros(..) = fm.name {
        return false;
    }

    info!("REVERT {}", describe(rcx.session(), reparsed.splice_span()));
    info!("    TO {}", describe(rcx.session(), old.splice_span()));

    let mut rewrites = Vec::new();
    let mark = rcx.mark();
    let ok = Rewrite::rewrite(old, new, rcx.with_rewrites(&mut rewrites));
    if !ok {
        rcx.rewind(mark);
        return false;
    }

    let adj = new.get_adjustment(&rcx);
    rcx.record(reparsed.splice_span(), old.splice_span(), rewrites, adj);
    true
}

pub fn rewrite<T>(old: &T, new: &T, rcx: RewriteCtxtRef) -> bool
        where T: PrintParse + RecoverChildren + Splice {
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
    rewrite_at(old.splice_span(), new, rcx)
}

pub fn rewrite_at<T>(old_span: Span, new: &T, mut rcx: RewriteCtxtRef) -> bool
        where T: PrintParse + RecoverChildren + Splice {
    let printed = new.to_string();
    let reparsed = T::parse(rcx.session(), &printed);
    let reparsed = reparsed.ast_deref();

    if old_span.lo() != old_span.hi() {
        info!("REWRITE {}", describe(rcx.session(), old_span));
        info!("   INTO {}", describe(rcx.session(), reparsed.splice_span()));
    } else {
        info!("INSERT AT {}", describe(rcx.session(), old_span));
        info!("     TEXT {}", describe(rcx.session(), reparsed.splice_span()));
    }

    let mut rewrites = Vec::new();
    let old_fs = rcx.replace_fresh_start(new.splice_span());
    RecoverChildren::recover_node_and_children(reparsed, new, rcx.with_rewrites(&mut rewrites));
    rcx.replace_fresh_start(old_fs);

    let adj = new.get_adjustment(&rcx);
    rcx.record(old_span, reparsed.splice_span(), rewrites, adj);

    true
}
