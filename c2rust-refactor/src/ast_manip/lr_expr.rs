//! `fold_expr_with_context` function, for rewriting exprs with knowledge of their contexts (rvalue
//! / lvalue / mut lvalue).
use std::rc::Rc;
use rustc_target::spec::abi::Abi;
use smallvec::SmallVec;
use syntax::ThinVec;
use syntax::ast::*;
use syntax::fold::{self, Folder};
use syntax::parse::token::{Token, DelimToken, Nonterminal};
use syntax::ptr::P;
use syntax::source_map::{Span, Spanned};
use syntax::tokenstream::{TokenTree, Delimited, DelimSpan, TokenStream};
use syntax::util::move_map::MoveMap;
use syntax_pos::hygiene::SyntaxContext;

use crate::ast_manip::Fold;


// TODO: Check for autoborrow adjustments.  Some method receivers are actually Lvalue / LvalueMut
// contexts, but currently they're all treated as Rvalues.

// TODO: Handle match inputs properly.  The target expression of a match could be any context,
// depending on whether `ref` / `ref mut` appears in any of the patterns.


/// Trait implemented by all AST types, allowing folding over exprs while tracking the context.
trait LRExpr {
    fn fold_rvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self;
    fn fold_lvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self;
    fn fold_lvalue_mut<LR: LRRewrites>(self, lr: &mut LR) -> Self;
}

/// A set of expr rewrites, one for each kind of context where an expr may appear.
trait LRRewrites {
    fn fold_rvalue(&mut self, e: Expr) -> Expr;
    fn fold_lvalue(&mut self, e: Expr) -> Expr;
    fn fold_lvalue_mut(&mut self, e: Expr) -> Expr;
}



// Helper macro for generating LRExpr instances.
macro_rules! lr_expr_fn {
    (($slf:ident, $next:ident($T:ty)) => $e:expr) => {
        #[allow(unused_mut)]
        fn fold_rvalue<LR: LRRewrites>($slf, lr: &mut LR) -> Self {
            let mut $next = |x: $T| x.fold_rvalue(lr);
            $e
        }

        #[allow(unused_mut)]
        fn fold_lvalue<LR: LRRewrites>($slf, lr: &mut LR) -> Self {
            let mut $next = |x: $T| x.fold_lvalue(lr);
            $e
        }

        #[allow(unused_mut)]
        fn fold_lvalue_mut<LR: LRRewrites>($slf, lr: &mut LR) -> Self {
            let mut $next = |x: $T| x.fold_lvalue_mut(lr);
            $e
        }
    };
}

impl<T: LRExpr> LRExpr for Vec<T> {
    lr_expr_fn!((self, next(T)) => {
        self.move_map(next)
    });
}

impl<T: LRExpr> LRExpr for ThinVec<T> {
    lr_expr_fn!((self, next(Vec<T>)) => {
        next(self.into()).into()
    });
}

impl<T: LRExpr+'static> LRExpr for P<T> {
    lr_expr_fn!((self, next(T)) => {
        self.map(next)
    });
}

impl<T: LRExpr + Clone> LRExpr for Rc<T> {
    lr_expr_fn!((self, next(T)) => {
        Rc::new(next((*self).clone()))
    });
}

impl<T: LRExpr> LRExpr for Spanned<T> {
    lr_expr_fn!((self, next(T)) => {
        Spanned { node: next(self.node), ..self }
    });
}

impl<T: LRExpr> LRExpr for Option<T> {
    lr_expr_fn!((self, next(T)) => {
        match self {
            Some(x) => Some(next(x)),
            None => None,
        }
    });
}

impl<A: LRExpr, B: LRExpr> LRExpr for (A, B) {
    fn fold_rvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {
        let (a, b) = self;
        (a.fold_rvalue(lr),
         b.fold_rvalue(lr))
    }

    fn fold_lvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {
        let (a, b) = self;
        (a.fold_lvalue(lr),
         b.fold_lvalue(lr))
    }

    fn fold_lvalue_mut<LR: LRRewrites>(self, lr: &mut LR) -> Self {
        let (a, b) = self;
        (a.fold_lvalue_mut(lr),
         b.fold_lvalue_mut(lr))
    }
}

impl<A: LRExpr, B: LRExpr, C: LRExpr> LRExpr for (A, B, C) {
    fn fold_rvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {
        let (a, b, c) = self;
        (a.fold_rvalue(lr),
         b.fold_rvalue(lr),
         c.fold_rvalue(lr))
    }

    fn fold_lvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {
        let (a, b, c) = self;
        (a.fold_lvalue(lr),
         b.fold_lvalue(lr),
         c.fold_lvalue(lr))
    }

    fn fold_lvalue_mut<LR: LRRewrites>(self, lr: &mut LR) -> Self {
        let (a, b, c) = self;
        (a.fold_lvalue_mut(lr),
         b.fold_lvalue_mut(lr),
         c.fold_lvalue_mut(lr))
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


struct Rewrites<F: FnMut(P<Expr>, Context) -> P<Expr>> {
    callback: F,
}

impl<F> LRRewrites for Rewrites<F>
        where F: FnMut(P<Expr>, Context) -> P<Expr> {
    fn fold_rvalue(&mut self, e: Expr) -> Expr {
        (self.callback)(P(e), Context::Rvalue).into_inner()
    }

    fn fold_lvalue(&mut self, e: Expr) -> Expr {
        (self.callback)(P(e), Context::Lvalue).into_inner()
    }

    fn fold_lvalue_mut(&mut self, e: Expr) -> Expr {
        (self.callback)(P(e), Context::LvalueMut).into_inner()
    }
}

/// Perform a bottom-up rewrite of an `Expr`, indicating at each step whether the expr is in an
/// rvalue, (immutable) lvalue, or mutable lvalue context.
///
/// `start` is the context of the outermost expression `e`.
pub fn fold_expr_with_context<F>(e: P<Expr>, start: Context, callback: F) -> P<Expr>
        where F: FnMut(P<Expr>, Context) -> P<Expr> {
    let mut lr = Rewrites { callback: callback };
    match start {
        Context::Rvalue => e.fold_rvalue(&mut lr),
        Context::Lvalue => e.fold_lvalue(&mut lr),
        Context::LvalueMut => e.fold_lvalue_mut(&mut lr),
    }
}


// Folder for rewriting exprs that aren't children of other exprs.
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

impl<F: FnMut(P<Expr>) -> P<Expr>> Folder for TopExprFolder<F> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        let e = self.in_expr(true, |this| e.map(|e| fold::noop_fold_expr(e, this)));
        if !self.in_expr {
            (self.callback)(e)
        } else {
            e
        }
    }


    // Clear the `in_expr` flag upon entry to a non-expr node that may contain exprs.
    fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty> {
        self.in_expr(false, |this| fold::noop_fold_ty(ty, this))
    }

    fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> {
        self.in_expr(false, |this| fold::noop_fold_pat(p, this))
    }

    fn fold_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]> {
        self.in_expr(false, |this| fold::noop_fold_stmt(s, this))
    }
}

fn fold_top_exprs<T, F>(x: T, callback: F) -> <T as Fold>::Result
        where T: Fold, F: FnMut(P<Expr>) -> P<Expr> {
    let mut f = TopExprFolder { callback: callback, in_expr: false };
    x.fold(&mut f)
}

pub fn fold_exprs_with_context<T, F>(x: T, mut callback: F) -> <T as Fold>::Result
        where T: Fold, F: FnMut(P<Expr>, Context) -> P<Expr> {
    fold_top_exprs(x, |e| {
        fold_expr_with_context(e, Context::Rvalue, |e, ctx| callback(e, ctx))
    })
}
