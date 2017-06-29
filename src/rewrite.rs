//! Module for rewriting source text to reflect changes in the AST.
//!
//! We get two ASTs as input: an "old" AST, which was obtained by parsing some source text, and a
//! "new" AST, constructed by performing some arbitrary transformations on the old AST.  The goal
//! is to produce new source text corresponding to the new AST, while preserving as much as
//! possible the formatting, comments, and macro invocations as they appeared in the old source.
//!
//! Note that pretty-printing is a problem for macro invocations in particular - since the old AST
//! is macro-expanded before being transformed, each invocation would be replaced by its expansion.
//! Our goal is to preserve macro invocations as they appeared in the original source, but rewrite
//! inside macro arguments whenever possible.
//!
//! The strategy here is to traverse the new AST, with two modes of operation, depending on whether
//! we are in recycled code (nodes copied from the old AST) or fresh code (nodes copied from
//! replacements or generated from scratch).  The process also has a current output buffer, which
//! begins with the old source but is gradually transformed to reflect the structure of the new
//! AST.  If recycled code contains fresh code, then we need to delete the corresponding piece of
//! the output buffer and replace it with newly generated source for the fresh code.  Similarly, if
//! fresh code contains recycled code, we need to delete a piece and replace it with the
//! appropriate piece of the old source text.
//!
//! The process begins in "recycled" mode.  We traverse old and new ASTs together, checking for
//! differences between them.  If one is found, then the new AST has fresh code at this position.
//! We delete the buffer contents corresponding to the old AST node, and substitute in the result
//! of pretty-printing the new (fresh) node.  We then switch to "fresh" mode and recurse on the new
//! node, since it may contain additional chunks of recycled code as subtrees.
//!
//! "Fresh" mode is similar but uses a different condition to trigger rewriting.  When entering
//! "fresh" mode, we re-parse the pretty-printed text that was just substituted in, and traverse
//! the new AST and the reparsed AST.  These ASTs should be identical, but if a node in the new AST
//! has source information pointing to the old source text, then it is recycled code, and its text
//! in the output buffer needs to be replaced with the old source text.  We locate the piece of
//! text to be replaced by consulting the source information of the reparsed AST.  As before, when
//! rewriting occurs, we switch to "recycled" mode, in case the fresh node containis recycled code
//! as a subtree.  "Recycled" mode requires a copy of the old AST, which we obtain by looking up
//! the new node's source information in a precomputed table.


use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::result;
use rustc::session::Session;
use syntax::ast::{Ident, Expr, ExprKind, Stmt, Item, Crate, Mac};
use syntax::codemap::{Span, Spanned, DUMMY_SP};
use syntax::symbol::Symbol;
use syntax::ptr::P;
use syntax::visit::{self, Visitor};

use visit::Visit;


pub trait Rewrite {
    /// Rewrite inside recycled code.  `self` is a node from the new AST; `old` is the
    /// corresponding node from the old AST.  Returns `true` if some rewriting of the nodes or
    /// their children is required, which happens if the nodes are unequal in visible ways.
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool;

    /// Rewrite inside fresh code.  `self` is a node from the new AST; `reparsed` is the
    /// corresponding node from the result of printing and then parsing the new AST.  Rewriting
    /// happens if the new node has a span referring to the old source code and there is an AST
    /// available for that span, but rewriting is handled immediately when needed and there is no
    /// need to propagate the information upward.
    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef);
}


#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TextRewrite {
    pub old_span: Span,
    pub new_span: Span,
    pub rewrites: Vec<TextRewrite>,
}


struct SpanMap<'s> {
    exprs: HashMap<Span, &'s Expr>,
}

impl<'s> SpanMap<'s> {
    fn new() -> SpanMap<'s> {
        SpanMap {
            exprs: HashMap::new(),
        }
    }
}


struct SpanMapVisitor<'s> {
    sm: SpanMap<'s>,
}

impl<'s> Visitor<'s> for SpanMapVisitor<'s> {
    fn visit_expr(&mut self, x: &'s Expr) {
        if !self.sm.exprs.contains_key(&x.span) {
            self.sm.exprs.insert(x.span, x);
        }
        visit::walk_expr(self, x);
    }
}


pub struct RewriteCtxt<'s> {
    sess: &'s Session,
    span_map: SpanMap<'s>,
}

impl<'s> RewriteCtxt<'s> {
    pub fn session(&self) -> &'s Session {
        self.sess
    }

    pub fn get_expr(&self, span: Span) -> Option<&'s Expr> {
        self.span_map.exprs.get(&span).map(|&x| x)
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

    pub fn record(&mut self, old_span: Span, new_span: Span, rewrites: Vec<TextRewrite>) {
        self.rewrites.push(TextRewrite {
            old_span: old_span,
            new_span: new_span,
            rewrites: rewrites,
        });
    }
}



pub fn rewrite<T: Rewrite+Visit>(sess: &Session, old: &T, new: &T) -> Vec<TextRewrite> {
    let mut smv = SpanMapVisitor { sm: SpanMap::new() };
    old.visit(&mut smv);

    let mut rcx = RewriteCtxt {
        sess: sess,
        span_map: smv.sm,
    };
    let mut rewrites = Vec::new();
    {
        let mut rcx_ref = RewriteCtxtRef {
            rewrites: &mut rewrites,
            cx: &mut rcx,
        };
        let need_rewrite = Rewrite::rewrite_recycled(new, old, rcx_ref);
        assert!(!need_rewrite, "rewriting did not complete");
    }
    rewrites
}
