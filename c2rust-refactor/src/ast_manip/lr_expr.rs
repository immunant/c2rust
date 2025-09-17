//! `fold_expr_with_context` function, for rewriting exprs with knowledge of their contexts (rvalue
//! / lvalue / mut lvalue).
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::{DelimSpan, LazyTokenStream, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_span::hygiene::SyntaxContext;
use rustc_span::source_map::{Span, Spanned};
use rustc_span::symbol::{Ident, Symbol};
use rustc_target::spec::abi::Abi;
use smallvec::SmallVec;
use std::rc::Rc;

use crate::ast_manip::MutVisit;

// TODO: Check for autoborrow adjustments.  Some method receivers are actually Lvalue / LvalueMut
// contexts, but currently they're all treated as Rvalues.

// TODO: Handle match inputs properly.  The target expression of a match could be any context,
// depending on whether `ref` / `ref mut` appears in any of the patterns.

/// Trait implemented by all AST types, allowing folding over exprs while tracking the context.
trait LRExpr {
    fn fold_rvalue<LR: LRRewrites>(&mut self, lr: &mut LR);
    fn fold_lvalue<LR: LRRewrites>(&mut self, lr: &mut LR);
    fn fold_lvalue_mut<LR: LRRewrites>(&mut self, lr: &mut LR);
}

/// A set of expr rewrites, one for each kind of context where an expr may appear.
trait LRRewrites {
    fn fold_rvalue(&mut self, e: &mut P<Expr>);
    fn fold_lvalue(&mut self, e: &mut P<Expr>);
    fn fold_lvalue_mut(&mut self, e: &mut P<Expr>);
}

// Helper macro for generating LRExpr instances.
macro_rules! lr_expr_fn {
    (($slf:ident, $next:ident($T:ty)) => $e:expr) => {
        #[allow(unused_mut)]
        fn fold_rvalue<LR: LRRewrites>(&mut $slf, lr: &mut LR) {
            let mut $next = |x: &mut $T| x.fold_rvalue(lr);
            $e
        }

        #[allow(unused_mut)]
        fn fold_lvalue<LR: LRRewrites>(&mut $slf, lr: &mut LR) {
            let mut $next = |x: &mut $T| x.fold_lvalue(lr);
            $e
        }

        #[allow(unused_mut)]
        fn fold_lvalue_mut<LR: LRRewrites>(&mut $slf, lr: &mut LR) {
            let mut $next = |x: &mut $T| x.fold_lvalue_mut(lr);
            $e
        }
    };
}

impl<T: LRExpr> LRExpr for Vec<T> {
    lr_expr_fn!((self, next(T)) => {
        mut_visit::visit_vec(self, next)
    });
}

impl<T: LRExpr> LRExpr for ThinVec<T> {
    lr_expr_fn!((self, next(T)) => {
        for x in self.iter_mut() {
            next(x);
        }
    });
}

impl<T: LRExpr + ?Sized + 'static> LRExpr for P<T> {
    lr_expr_fn!((self, next(T)) => {
        next(self);
    });
}

impl<T: LRExpr + ?Sized + Clone> LRExpr for Rc<T> {
    lr_expr_fn!((self, next(T)) => {
        next(Rc::make_mut(self));
    });
}

impl<T: LRExpr> LRExpr for Spanned<T> {
    lr_expr_fn!((self, next(T)) => {
        next(&mut self.node)
    });
}

impl<T: LRExpr> LRExpr for Option<T> {
    lr_expr_fn!((self, next(T)) => {
        mut_visit::visit_opt(self, next)
    });
}

impl<A: LRExpr, B: LRExpr> LRExpr for (A, B) {
    fn fold_rvalue<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.0.fold_rvalue(lr);
        self.1.fold_rvalue(lr);
    }

    fn fold_lvalue<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.0.fold_lvalue(lr);
        self.1.fold_lvalue(lr);
    }

    fn fold_lvalue_mut<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.0.fold_lvalue_mut(lr);
        self.1.fold_lvalue_mut(lr);
    }
}

impl<A: LRExpr, B: LRExpr, C: LRExpr> LRExpr for (A, B, C) {
    fn fold_rvalue<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.0.fold_rvalue(lr);
        self.1.fold_rvalue(lr);
        self.2.fold_rvalue(lr);
    }

    fn fold_lvalue<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.0.fold_lvalue(lr);
        self.1.fold_lvalue(lr);
        self.2.fold_lvalue(lr);
    }

    fn fold_lvalue_mut<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.0.fold_lvalue_mut(lr);
        self.1.fold_lvalue_mut(lr);
        self.2.fold_lvalue_mut(lr);
    }
}

impl LRExpr for P<Expr> {
    fn fold_rvalue<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.kind.fold_rvalue(lr);
        lr.fold_rvalue(self)
    }
    fn fold_lvalue<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.kind.fold_lvalue(lr);
        lr.fold_lvalue(self)
    }
    fn fold_lvalue_mut<LR: LRRewrites>(&mut self, lr: &mut LR) {
        self.kind.fold_lvalue_mut(lr);
        lr.fold_lvalue_mut(self)
    }
}

include!(concat!(env!("OUT_DIR"), "/lr_expr_gen.inc.rs"));

/// Kinds of contexts where exprs can appear.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Context {
    Rvalue,
    Lvalue,
    LvalueMut,
}

struct Rewrites<F: FnMut(&mut P<Expr>, Context)> {
    callback: F,
}

impl<F> LRRewrites for Rewrites<F>
where
    F: FnMut(&mut P<Expr>, Context),
{
    fn fold_rvalue(&mut self, e: &mut P<Expr>) {
        (self.callback)(e, Context::Rvalue)
    }

    fn fold_lvalue(&mut self, e: &mut P<Expr>) {
        (self.callback)(e, Context::Lvalue)
    }

    fn fold_lvalue_mut(&mut self, e: &mut P<Expr>) {
        (self.callback)(e, Context::LvalueMut)
    }
}

/// Perform a bottom-up rewrite of an `Expr`, indicating at each step whether the expr is in an
/// rvalue, (immutable) lvalue, or mutable lvalue context.
///
/// `start` is the context of the outermost expression `e`.
pub fn fold_expr_with_context<F>(e: &mut P<Expr>, start: Context, callback: F)
where
    F: FnMut(&mut P<Expr>, Context),
{
    let mut lr = Rewrites { callback };
    match start {
        Context::Rvalue => e.fold_rvalue(&mut lr),
        Context::Lvalue => e.fold_lvalue(&mut lr),
        Context::LvalueMut => e.fold_lvalue_mut(&mut lr),
    }
}

// MutVisitor for rewriting exprs that aren't children of other exprs.
struct TopExprFolder<F> {
    callback: F,
    in_expr: bool,
}

impl<F> TopExprFolder<F> {
    fn in_expr<G: FnOnce(&mut Self) -> R, R>(&mut self, in_expr: bool, callback: G) -> R {
        let old_in_expr = self.in_expr;
        self.in_expr = in_expr;
        let r = callback(self);
        self.in_expr = old_in_expr;
        r
    }
}

impl<F: FnMut(&mut P<Expr>)> MutVisitor for TopExprFolder<F> {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        self.in_expr(true, |this| mut_visit::noop_visit_expr(e, this));
        if !self.in_expr {
            (self.callback)(e);
        }
    }

    // Clear the `in_expr` flag upon entry to a non-expr node that may contain exprs.
    fn visit_ty(&mut self, ty: &mut P<Ty>) {
        self.in_expr(false, |this| mut_visit::noop_visit_ty(ty, this))
    }

    fn visit_pat(&mut self, p: &mut P<Pat>) {
        self.in_expr(false, |this| mut_visit::noop_visit_pat(p, this))
    }

    fn flat_map_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]> {
        self.in_expr(false, |this| mut_visit::noop_flat_map_stmt(s, this))
    }
}

fn fold_top_exprs<T, F>(x: &mut T, callback: F)
where
    T: MutVisit,
    F: FnMut(&mut P<Expr>),
{
    let mut f = TopExprFolder {
        callback,
        in_expr: false,
    };
    x.visit(&mut f)
}

pub fn fold_exprs_with_context<T, F>(x: &mut T, mut callback: F)
where
    T: MutVisit,
    F: FnMut(&mut P<Expr>, Context),
{
    fold_top_exprs(x, |e| {
        fold_expr_with_context(e, Context::Rvalue, |e, ctx| callback(e, ctx))
    })
}
