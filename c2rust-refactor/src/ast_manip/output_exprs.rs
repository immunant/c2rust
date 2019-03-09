//! `fold_output_exprs` function, for visiting return-value expressions.
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::mut_visit::{self, MutVisitor, visit_opt, visit_vec};
use syntax::ptr::P;

use crate::ast_manip::MutVisit;
use crate::util::Lone;


struct OutputFolder<F> {
    callback: F,

    /// Are we in the trailing expr of a function?
    trailing: bool,
}

impl<F: FnMut(&mut P<Expr>)> OutputFolder<F> {
    /// Change the value of `self.trailing` for the duration of the callback `g`.
    fn with_trailing<G: FnOnce(&mut Self) -> R, R>(&mut self, trailing: bool, g: G) -> R {
        let old = self.trailing;
        self.trailing = trailing;
        let r = g(&mut self);
        self.trailing = old;
        r
    }
}

impl<F: FnMut(&mut P<Expr>)> MutVisitor for OutputFolder<F> {
    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        // The expr within the fn is always trailing
        match i.node {
            ItemKind::Fn(..) =>
                self.with_trailing(true, |f| mut_visit::noop_flat_map_item(i, f)),
            _ => mut_visit::noop_flat_map_item(i, self),
        }
    }

    fn flat_map_impl_item(&mut self, i: ImplItem) -> SmallVec<[ImplItem; 1]> {
        match i.node {
            ImplItemKind::Method(..) =>
                self.with_trailing(true, |f| mut_visit::noop_flat_map_impl_item(i, f)),
            _ => mut_visit::noop_flat_map_impl_item(i, self),
        }
    }

    fn flat_map_trait_item(&mut self, i: TraitItem) -> SmallVec<[TraitItem; 1]> {
        match i.node {
            TraitItemKind::Method(..) =>
                self.with_trailing(true, |f| mut_visit::noop_flat_map_trait_item(i, f)),
            _ => mut_visit::noop_flat_map_trait_item(i, self),
        }
    }

    fn visit_block(&mut self, b: &mut P<Block>) {
        if b.stmts.len() == 0 {
            return;
        }

        let last = b.stmts.len() - 1;
        let new_stmts = b.stmts.into_iter().enumerate().map(|(i, s)| {
            if i != last {
                self.with_trailing(false, |f| f.flat_map_stmt(s)).lone()
            } else {
                // Last stmt is trailing if the block is trailing
                self.flat_map_stmt(s).lone()
            }
        }).collect();

        b.stmts = new_stmts;
    }

    fn flat_map_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]> {
        match s.node {
            StmtKind::Expr(..) => mut_visit::noop_flat_map_stmt(s, self),
            _ => self.with_trailing(false, |f| mut_visit::noop_flat_map_stmt(s, f)),
        }
    }

    fn visit_expr(&mut self, e: &mut P<Expr>) {
        match &mut e.node {
            ExprKind::If(cond, then, rest) => {
                self.with_trailing(false, |f| f.visit_expr(cond));
                self.visit_block(then);
                visit_opt(rest, |rest| self.visit_expr(rest));
            }

            ExprKind::IfLet(pats, cond, then, rest) => {
                visit_vec(pats, |pat| self.visit_pat(pat));
                self.with_trailing(false, |f| f.visit_expr(cond));
                self.visit_block(then);
                visit_opt(rest, |rest| self.visit_expr(rest));
            }

            // TODO: Handle `loop` + `break`-with-expr.  If the `loop` is a trailing
            // expression, then a `break` targeting its label should be treated as a return
            // expression.
            //ExprKind::Loop(body) => { TODO },

            ExprKind::Match(target, arms) => {
                self.with_trailing(false, |f| f.visit_expr(target));
                visit_vec(arms, |arm| self.visit_arm(arm));
            }

            ExprKind::Block(b, lbl) => {
                self.visit_block(b);
            }

            ExprKind::Try(_) => {
                // Explicitly unimplemented.  Depending on whether `try` winds up
                // auto-wrapping its "return" value in `Ok`, we may need to treat the trailing
                // expr of a `catch` specially.
                panic!("output_exprs: ExprKind::Try is not supported")
            }

            ExprKind::Ret(ret) => {
                visit_opt(ret, |ret| self.with_trailing(false, |f| f.visit_expr(ret)));
            }

            //ExprKind::Break(Some(label), Some(expr)) => { TODO },

            // Not sure what to do with ExprKind::Try.  It can return (on error), but doesn't
            // have an actual output expression.

            node => {
                self.with_trailing(false, |f| mut_visit::noop_visit_expr(e, f));
                if self.trailing {
                    (self.callback)(e);
                }
            },
        };
    }
}


/// Rewrite function output/return expressions.
///
/// For the trailing expression of a block, only the leaf expressions will be visited - for
/// example, in `fn f() { if c { x } else { y } }`, only `x` and `y` will be visited, not `{ x }`,
/// `{ y }`, or the `if`.
pub fn fold_output_exprs<T, F>(target: &mut T, trailing: bool, callback: F)
        where T: MutVisit,
              F: FnMut(&mut P<Expr>) {
    let mut f = OutputFolder {
        callback: callback,
        trailing: trailing,
    };
    target.visit(&mut f)
}
