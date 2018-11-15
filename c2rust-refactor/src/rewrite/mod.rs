//! Module for rewriting source text to reflect changes in the AST.
//!
//! Rewriting takes as input an old AST, a new AST, and source text that parses to the old AST, and
//! transforms that source text into text that parses to the new AST.  Rewriting is designed to
//! preserve comments and whitespace whenever possible.
//!
//! At a high level, rewriting is a recursive traversal on the old and new ASTs.  Everywhere the
//! two are equal, there is no work to do.  But where they differ, it applies a number of "rewrite
//! strategies" that attempt to turn the old text into new text.  In cases where no strategy can
//! perform the rewrite, it propagates the error upward, trying the available strategies to rewrite
//! enclosing nodes of the ASTs.
//!
//! The core of the actual implementation is the `Rewrite::rewrite(old, new, rcx) -> bool` method,
//! which attempts to rewrite the `old` AST into the `new` AST.  The implementation of this method
//! for each node type simply tries each applicable strategy for the node type until either one of
//! the strategies succeeds or it runs out of strategies to try.  `Rewrite::rewrite` is not
//! (directly) recursive - the recursive traversal is handled by the `recursive` strategy.
//!
//! There are three core rewrite strategies:
//!
//!  * `equal`: If the two nodes are equal, rewriting succeeds.  If they aren't, it fails.  In
//!    either case, this strategy performs no actual rewrites.
//!
//!    For leaf nodes, this strategy is tried first.
//!
//!  * `recursive`: If every child of the first can be rewritten to the corresponding child of the
//!    second, then rewriting succeeds.  For nodes of enum type, the two nodes must be instances of
//!    the same variant (otherwise there would be no correspondence between the old and new nodes'
//!    children).  If the variants are unequal or rewriting of any child fails, then the overall
//!    rewrite fails.  This strategy performs no rewrites beyond those performed by its recursive
//!    calls.
//!
//!    This is where the recursion happens in the actual implementation.  Since it implements a
//!    superset of `equal`'s functionality, it replaces `equal` as the first strategy to try for
//!    all non-leaf node types.
//!
//!  * `print`: Pretty-prints the new node, and performs a rewrite to replace the old source with
//!    this new source text.  This strategy always succeeds, but is only implemented for a few node
//!    types (mostly major ones such as `Item`, `Expr`, etc).
//!
//!    Since pretty-printer's output is cosmetically quite bad (it includes no comments, prints
//!    macros in expanded form, and sometimes makes questionable decisions regarding whitespace),
//!    the `print` strategy tries to replace pretty-printer output with original (user-written)
//!    source text whenever possible.  See the `rewrite::strategy::print` module docs for details.
//!
//!    Since this strategy always succeeds, but often produces bad results, it is tried last for
//!    any node types that support it.
//!
//! Since `print` and the more specialized (non-core) strategies only work for a small set of node
//! types, for most nodes `Rewrite::rewrite` simply tries `equal` (leaf nodes) or `recursive`
//! (non-leaf nodes), and fails if the strategy fails.  This failure will cause a failure in the
//! enclosing `recursive`, and will propagate upward until it reaches a node type that actually
//! does support another strategy, such as `Item`.  This is the point where rewriting actually
//! happens: when `recursive` fails, `Rewrite::rewrite` will try the next strategy (such as
//! `print`), which can perform rewrites to correct the error at this higher level.


use std::collections::HashMap;
use std::mem;
use std::ops::{Deref, DerefMut};
use rustc::session::Session;
use syntax::ast::*;
use syntax::source_map::{Span, DUMMY_SP};
use syntax::util::parser;

use ast_manip::Visit;
use ast_manip::ast_map::{AstMap, map_ast};
use driver;

mod cleanup;
pub mod files;
pub mod json;

mod base;
mod strategy;

pub use self::base::Rewrite;


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TextAdjust {
    None,
    Parenthesize,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TextRewrite {
    pub old_span: Span,
    pub new_span: Span,
    pub rewrites: Vec<TextRewrite>,
    pub adjust: TextAdjust,
}


/// Common ID type for nodes and `Attribute`s.  Both are sequence items, but `Attribute`s have
/// their own custom ID type for some reason.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SeqItemId {
    Node(NodeId),
    Attr(AttrId),
}

trait MappableId {
    fn map_id(self, rcx: &RewriteCtxt) -> Self;
}

impl MappableId for NodeId {
    fn map_id(self, rcx: &RewriteCtxt) -> Self {
        rcx.node_id_map.get(&self).map_or(DUMMY_NODE_ID, |&x| x)
    }
}

impl MappableId for AttrId {
    fn map_id(self, _rcx: &RewriteCtxt) -> Self {
        self
    }
}

impl MappableId for SeqItemId {
    fn map_id(self, rcx: &RewriteCtxt) -> Self {
        match self {
            SeqItemId::Node(id) => SeqItemId::Node(id.map_id(rcx)),
            SeqItemId::Attr(id) => SeqItemId::Attr(id.map_id(rcx)),
        }
    }
}


/// Precedence information about the context surrounding an expression.  Used to determine whether
/// an expr needs to be parenthesized.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ExprPrec {
    /// Normal behavior.  Parenthesize expr if its precedence is less than the given value.
    Normal(i8),
    /// Conditional-like position.  Parenthesize lower precedence, and also parenthesize exprs with
    /// outer struct literals.
    Cond(i8),
    /// Callee position.  Parenthesize lower precedence, and also parenthesize struct and tuple
    /// field expressions (so the call is not mistaken for a method call).
    Callee(i8),
}


pub struct RewriteCtxt<'s> {
    sess: &'s Session,
    old_nodes: AstMap<'s>,
    text_span_cache: HashMap<String, Span>,

    /// The span of the new AST the last time we entered "fresh" mode.  This lets us avoid infinite
    /// recursion - see comment in `splice_fresh`.
    fresh_start: Span,

    /// Precedence of the current expression context.  If we splice in an expression of lower
    /// precedence, it will be parenthesized.
    expr_prec: ExprPrec,

    /// Mapping from NodeIds in the new AST to corresponding NodeIds in the old AST.  This has two
    /// purposes.  (1) If `node_id_map[new_node.id] == old_node.id`, then `new_node` and `old_node`
    /// are considered "the same node" for sequence rewriting purposes.  This affects the
    /// rewriter's decisions about where to insert/delete sequence elements, as opposed to
    /// rewriting the old node to the new one.  (2) When the rewriter is in "fresh" mode and
    /// looking for recycled text to splice in, it checks `old_nodes` for a node whose ID is
    /// `node_id_map[new_node.id]`.
    node_id_map: HashMap<NodeId, NodeId>,
}

impl<'s> RewriteCtxt<'s> {
    fn new(sess: &'s Session,
           old_nodes: AstMap<'s>,
           node_id_map: HashMap<NodeId, NodeId>) -> RewriteCtxt<'s> {
        RewriteCtxt {
            sess,
            old_nodes,
            text_span_cache: HashMap::new(),

            fresh_start: DUMMY_SP,
            expr_prec: ExprPrec::Normal(parser::PREC_RESET),
            node_id_map,
        }
    }

    pub fn session(&self) -> &'s Session {
        self.sess
    }

    pub fn old_nodes(&self) -> &AstMap<'s> {
        &self.old_nodes
    }

    pub fn fresh_start(&self) -> Span {
        self.fresh_start
    }

    pub fn replace_fresh_start(&mut self, span: Span) -> Span {
        mem::replace(&mut self.fresh_start, span)
    }

    pub fn expr_prec(&self) -> ExprPrec {
        self.expr_prec
    }

    pub fn replace_expr_prec(&mut self, prec: ExprPrec) -> ExprPrec {
        mem::replace(&mut self.expr_prec, prec)
    }

    fn new_to_old_id<Id: MappableId>(&self, id: Id) -> Id {
        id.map_id(self)
    }

    pub fn with_rewrites<'b>(&'b mut self,
                             rewrites: &'b mut Vec<TextRewrite>)
                             -> RewriteCtxtRef<'s, 'b> {
        RewriteCtxtRef {
            rewrites: rewrites,
            cx: self,
        }
    }

    pub fn text_span(&mut self, s: &str) -> Span {
        if let Some(&sp) = self.text_span_cache.get(s) {
            return sp;
        }

        let sp = driver::make_span_for_text(self.sess.source_map(), s);
        self.text_span_cache.insert(s.to_owned(), sp);
        sp
    }
}


pub struct RewriteCtxtRef<'s: 'a, 'a> {
    rewrites: &'a mut Vec<TextRewrite>,
    cx: &'a mut RewriteCtxt<'s>,
}

impl<'s, 'a> Deref for RewriteCtxtRef<'s, 'a> {
    type Target = RewriteCtxt<'s>;

    fn deref(&self) -> &RewriteCtxt<'s> {
        self.cx
    }
}

impl<'s, 'a> DerefMut for RewriteCtxtRef<'s, 'a> {
    fn deref_mut(&mut self) -> &mut RewriteCtxt<'s> {
        self.cx
    }
}

impl<'s, 'a> RewriteCtxtRef<'s, 'a> {
    pub fn borrow<'b>(&'b mut self) -> RewriteCtxtRef<'s, 'b> {
        RewriteCtxtRef {
            rewrites: self.rewrites,
            cx: self.cx,
        }
    }

    pub fn with_rewrites<'b>(&'b mut self,
                             rewrites: &'b mut Vec<TextRewrite>)
                             -> RewriteCtxtRef<'s, 'b> {
        RewriteCtxtRef {
            rewrites: rewrites,
            cx: self.cx,
        }
    }

    pub fn mark(&self) -> usize {
        self.rewrites.len()
    }

    pub fn rewind(&mut self, mark: usize) {
        self.rewrites.truncate(mark);
    }

    pub fn record(&mut self,
                  old_span: Span,
                  new_span: Span,
                  rewrites: Vec<TextRewrite>,
                  adjust: TextAdjust) {
        self.rewrites.push(TextRewrite {
            old_span: old_span,
            new_span: new_span,
            rewrites: rewrites,
            adjust: adjust,
        });
    }

    pub fn record_text(&mut self,
                       old_span: Span,
                       text: &str) {
        let new_span = self.text_span(text);
        self.record(old_span, new_span, Vec::new(), TextAdjust::None);
    }
}


pub fn rewrite<'s, T: Rewrite+Visit>(sess: &Session,
                                     old: &'s T,
                                     new: &T,
                                     node_id_map: HashMap<NodeId, NodeId>,
                                     map_extra_ast: impl FnOnce(&mut AstMap<'s>))
                                     -> Vec<TextRewrite> {
    let mut map = map_ast(old);
    map_extra_ast(&mut map);

    let mut rcx = RewriteCtxt::new(sess, map, node_id_map);
    let mut rewrites = Vec::new();
    let ok = Rewrite::rewrite(old, new, rcx.with_rewrites(&mut rewrites));
    assert!(ok, "rewriting did not complete");
    rewrites
}
