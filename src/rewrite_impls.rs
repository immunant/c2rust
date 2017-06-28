use std::rc::Rc;
use syntax::ast::*;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ext::hygiene::SyntaxContext;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::tokenstream::{TokenStream, ThinTokenStream};

use rewrite::{self, Rewrite, RewriteCtxt};


fn recover_expr(old: &Expr, new: &Expr, rcx: &mut RewriteCtxt) {
    rcx.record(old.span, pprust::expr_to_string(new));
}

fn recover_stmt(old: &Stmt, new: &Stmt, rcx: &mut RewriteCtxt) {
    rcx.record(old.span, pprust::stmt_to_string(new));
}



impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for [T] {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        if self.len() != new.len() {
            return false;
        }

        for i in 0 .. self.len() {
            if !rcx.rewrite(&self[i], &new[i]) {
                return false;
            }
        }
        true
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for Vec<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        <[T] as Rewrite<'ast>>::rewrite(self, new, rcx)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for ThinVec<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        <[T] as Rewrite<'ast>>::rewrite(self, new, rcx)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for P<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        <T as Rewrite<'ast>>::rewrite(self, new, rcx)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for Rc<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        <T as Rewrite<'ast>>::rewrite(self, new, rcx)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for Spanned<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        rcx.rewrite(&self.node, &new.node)
    }
}

impl<'ast, T: Rewrite<'ast>> Rewrite<'ast> for Option<T> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        match (self, new) {
            (&Some(ref x1),
             &Some(ref x2)) => {
                rcx.rewrite(x1, x2)
            }
            (&None, &None) => true,
            (_, _) => false,
        }
    }
}

impl<'ast, A: Rewrite<'ast>, B: Rewrite<'ast>> Rewrite<'ast> for (A, B) {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        rcx.rewrite(&self.0, &new.0) &&
        rcx.rewrite(&self.1, &new.1) &&
        true
    }
}

impl<'ast, A: Rewrite<'ast>, B: Rewrite<'ast>, C: Rewrite<'ast>> Rewrite<'ast> for (A, B, C) {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {
        rcx.rewrite(&self.0, &new.0) &&
        rcx.rewrite(&self.1, &new.1) &&
        rcx.rewrite(&self.2, &new.2) &&
        true
    }
}


include!("rewrite_impls_gen.inc.rs");
