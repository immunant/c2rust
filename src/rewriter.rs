use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;
use std::result;
use syntax::ast::{Ident, Expr, ExprKind, Stmt, Item, Crate, Mac};
use syntax::codemap::{Span, Spanned};
use syntax::symbol::Symbol;
use syntax::ptr::P;
use syntax::visit::{self, Visitor};


#[derive(Debug)]
pub struct RewriteCtxt {
    rewrites: Vec<(Span, String)>,
}

impl RewriteCtxt {
    pub fn new() -> RewriteCtxt {
        RewriteCtxt {
            rewrites: Vec::new(),
        }
    }

    pub fn record(&mut self, sp: Span, new_text: String) {
        self.rewrites.push((sp, new_text));
    }

    pub fn rewrite<R: Rewrite>(&mut self, sp: Span, old: &R, new: &R) {
        old.rewrite(sp, new, self);
    }

    pub fn rewrites(&self) -> &[(Span, String)] {
        &self.rewrites
    }
}


pub trait Rewrite {
    fn rewrite(&self, sp: Span, new: &Self, rcx: &mut RewriteCtxt);
}
