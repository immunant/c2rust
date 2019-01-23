//! `fold_output_exprs` function, for visiting return-value expressions.
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::move_map::MoveMap;

use crate::ast_manip::Fold;
use crate::util::Lone;


struct OutputFolder<F> {
    callback: F,

    /// Are we in the trailing expr of a function?
    trailing: bool,
}

impl<F: FnMut(P<Expr>) -> P<Expr>> OutputFolder<F> {
    /// Change the value of `self.trailing` for the duration of the callback `g`.
    fn with_trailing<G: FnOnce(&mut Self) -> R, R>(&mut self, trailing: bool, g: G) -> R {
        let old = self.trailing;
        self.trailing = trailing;
        let r = g(self);
        self.trailing = old;
        r
    }
}

impl<F: FnMut(P<Expr>) -> P<Expr>> Folder for OutputFolder<F> {
    fn fold_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        // The expr within the fn is always trailing
        match i.node {
            ItemKind::Fn(..) =>
                self.with_trailing(true, |f| fold::noop_fold_item(i, f)),
            _ => fold::noop_fold_item(i, self),
        }
    }

    fn fold_impl_item(&mut self, i: ImplItem) -> SmallVec<[ImplItem; 1]> {
        match i.node {
            ImplItemKind::Method(..) =>
                self.with_trailing(true, |f| fold::noop_fold_impl_item(i, f)),
            _ => fold::noop_fold_impl_item(i, self),
        }
    }

    fn fold_trait_item(&mut self, i: TraitItem) -> SmallVec<[TraitItem; 1]> {
        match i.node {
            TraitItemKind::Method(..) =>
                self.with_trailing(true, |f| fold::noop_fold_trait_item(i, f)),
            _ => fold::noop_fold_trait_item(i, self),
        }
    }

    fn fold_block(&mut self, b: P<Block>) -> P<Block> {
        if b.stmts.len() == 0 {
            return b;
        }

        b.map(|b| {
            let last = b.stmts.len() - 1;
            let new_stmts = b.stmts.into_iter().enumerate().map(|(i, s)| {
                if i != last {
                    self.with_trailing(false, |f| f.fold_stmt(s)).lone()
                } else {
                    // Last stmt is trailing if the block is trailing
                    self.fold_stmt(s).lone()
                }
            }).collect();

            Block {
                stmts: new_stmts,
                .. b
            }
        })
    }

    fn fold_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]> {
        match s.node {
            StmtKind::Expr(..) => fold::noop_fold_stmt(s, self),
            _ => self.with_trailing(false, |f| fold::noop_fold_stmt(s, f)),
        }
    }

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        e.map(|e| {
            let node = match e.node {
                ExprKind::If(cond, then, rest) => ExprKind::If(
                    self.with_trailing(false, |f| f.fold_expr(cond)),
                    self.fold_block(then),
                    rest.map(|e| self.fold_expr(e)),
                ),

                ExprKind::IfLet(pats, cond, then, rest) => ExprKind::IfLet(
                    pats.move_map(|p| self.fold_pat(p)),
                    self.with_trailing(false, |f| f.fold_expr(cond)),
                    self.fold_block(then),
                    rest.map(|e| self.fold_expr(e)),
                ),

                // TODO: Handle `loop` + `break`-with-expr.  If the `loop` is a trailing
                // expression, then a `break` targeting its label should be treated as a return
                // expression.
                //ExprKind::Loop(body) => { TODO },

                ExprKind::Match(target, arms) => ExprKind::Match(
                    self.with_trailing(false, |f| f.fold_expr(target)),
                    arms.move_map(|arm| self.fold_arm(arm)),
                ),

                ExprKind::Block(b, lbl) => ExprKind::Block(self.fold_block(b), lbl),

                ExprKind::Try(_) => {
                    // Explicitly unimplemented.  Depending on whether `try` winds up
                    // auto-wrapping its "return" value in `Ok`, we may need to treat the trailing
                    // expr of a `catch` specially.
                    panic!("output_exprs: ExprKind::Try is not supported")
                },

                ExprKind::Ret(None) => ExprKind::Ret(None),
                ExprKind::Ret(Some(ret)) => {
                    let ret = self.with_trailing(false, |f| f.fold_expr(ret));
                    let ret = (self.callback)(ret);
                    ExprKind::Ret(Some(ret))
                },

                //ExprKind::Break(Some(label), Some(expr)) => { TODO },

                // Not sure what to do with ExprKind::Try.  It can return (on error), but doesn't
                // have an actual output expression.

                node => {
                    let e = Expr { node: node, .. e };
                    let e = self.with_trailing(false, |f| fold::noop_fold_expr(e, f));
                    if self.trailing {
                        return (self.callback)(P(e)).into_inner();
                    } else {
                        return e
                    }
                },
            };

            Expr {
                node: node,
                .. e
            }
        })
    }
}


/// Rewrite function output/return expressions.
///
/// For the trailing expression of a block, only the leaf expressions will be visited - for
/// example, in `fn f() { if c { x } else { y } }`, only `x` and `y` will be visited, not `{ x }`,
/// `{ y }`, or the `if`.
pub fn fold_output_exprs<T, F>(target: T, trailing: bool, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(P<Expr>) -> P<Expr> {
    let mut f = OutputFolder {
        callback: callback,
        trailing: trailing,
    };
    target.fold(&mut f)
}
