use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ptr::P;
use syntax::print::pprust;

use rewriter::{self, Rewrite, RewriteCtxt};


impl Rewrite for Crate {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        rcx.rewrite(&self.module, &new.module);
        //rcx.rewrite(&self.attrs, &new.attrs); //TODO
    }
}

impl Rewrite for Mod {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        rcx.rewrite(&self.items, &new.items);
    }
}

impl Rewrite for Item {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        // TODO
        rcx.rewrite(&self.node, &new.node);
    }
}

impl Rewrite for ItemKind {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        match (self, new) {
            (&ItemKind::Fn(_, _, _, _, _, ref block1),
             &ItemKind::Fn(_, _, _, _, _, ref block2)) => {
                rcx.rewrite(block1, block2);
            },
            (_, _) => { /* TODO */ },
        }
    }
}

impl Rewrite for Block {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        rcx.rewrite(&self.stmts, &new.stmts);
    }
}

impl Rewrite for Stmt {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        // TODO
        rcx.rewrite(&self.node, &new.node);
    }
}

impl Rewrite for StmtKind {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        match (self, new) {
            (&StmtKind::Local(ref local1),
             &StmtKind::Local(ref local2)) => {
                rcx.rewrite(local1, local2);
            },
            (_, _) => { /* TODO */ },
        }
    }
}

impl Rewrite for Local {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        rcx.rewrite(&self.init, &new.init);
    }
}

impl Rewrite for Expr {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        if self.node != new.node {
            rcx.record(self.span, pprust::expr_to_string(new));
        } else {
            // TODO
        }
    }
}


impl<T: Rewrite> Rewrite for [T] {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        assert!(self.len() == new.len(),
                "not sure how to handle different-length vecs of rewritables");

        for i in 0 .. self.len() {
            rcx.rewrite(&self[i], &new[i]);
        }
    }
}

impl<T: Rewrite> Rewrite for Vec<T> {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        <[T] as Rewrite>::rewrite(self, new, rcx)
    }
}

impl<T: Rewrite> Rewrite for ThinVec<T> {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        <[T] as Rewrite>::rewrite(self, new, rcx)
    }
}

impl<T: Rewrite> Rewrite for P<T> {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        <T as Rewrite>::rewrite(self, new, rcx)
    }
}

impl<T: Rewrite> Rewrite for Spanned<T> {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        rcx.rewrite(&self.node, &new.node)
    }
}

impl<T: Rewrite> Rewrite for Option<T> {
    fn rewrite(&self, new: &Self, rcx: &mut RewriteCtxt) {
        match (self, new) {
            (&Some(ref x1),
             &Some(ref x2)) => {
                rcx.rewrite(x1, x2);
            }
            (&None, &None) => {},
            (_, _) => {}, //TODO
        }
    }
}
