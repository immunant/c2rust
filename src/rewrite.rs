use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;
use std::result;
use syntax::ast::{Ident, Expr, ExprKind, Stmt, Item, Crate, Mac};
use syntax::codemap::{Span, Spanned, DUMMY_SP};
use syntax::symbol::Symbol;
use syntax::ptr::P;
use syntax::visit::{self, Visitor};


#[derive(Debug)]
enum Context<'ast> {
    Expr(&'ast Expr, &'ast Expr),
    Stmt(&'ast Stmt, &'ast Stmt),
    Item(&'ast Item, &'ast Item),
}

#[derive(Debug)]
pub struct RewriteCtxt<'ast> {
    rewrites: Vec<(Span, String)>,
    stack: Vec<Context<'ast>>,
}

impl<'ast> RewriteCtxt<'ast> {
    pub fn new() -> RewriteCtxt<'ast> {
        RewriteCtxt {
            rewrites: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn mark(&self) -> usize {
        self.rewrites.len()
    }

    pub fn rewind(&mut self, i: usize) {
        self.rewrites.truncate(i);
    }

    pub fn record(&mut self, sp: Span, new_text: String) {
        self.rewrites.push((sp, new_text));
    }


    pub fn rewrite<R: Rewrite<'ast>>(&mut self, old: &'ast R, new: &'ast R) -> bool {
        old.rewrite(new, self)
    }

    fn in_context<F>(&mut self,
                     context: Context<'ast>,
                     f: F) -> bool
            where F: FnOnce(&mut RewriteCtxt<'ast>) -> bool {
        self.stack.push(context);
        let result = f(self);
        self.stack.pop();
        result
    }


    pub fn in_expr<F>(&mut self,
                      old_expr: &'ast Expr,
                      new_expr: &'ast Expr,
                      f: F) -> bool
            where F: FnOnce(&mut RewriteCtxt<'ast>) -> bool {
        self.in_context(Context::Expr(old_expr, new_expr), f)
    }


    pub fn rewrites(&self) -> &[(Span, String)] {
        &self.rewrites
    }


    // Get the old span of the parent node.
    pub fn parent_span(&self) -> Span {
        for ctx in self.stack.iter().rev() {
            match ctx {
                &Context::Expr(e, _) => return e.span,
                &Context::Stmt(s, _) => return s.span,
                &Context::Item(i, _) => return i.span,
            }
        }
        panic!("no enclosing context has a span");
    }


    // Get the new parent node as an expr/stmt/item.

    pub fn parent_as_expr(&self) -> &'ast Expr {
        match self.stack.last() {
            Some(&Context::Expr(_, e)) => e,
            _ => panic!("parent context is not an expr"),
        }
    }

    pub fn parent_as_stmt(&self) -> &'ast Stmt {
        match self.stack.last() {
            Some(&Context::Stmt(_, s)) => s,
            _ => panic!("parent context is not a stmt"),
        }
    }

    pub fn parent_as_item(&self) -> &'ast Item {
        match self.stack.last() {
            Some(&Context::Item(_, i)) => i,
            _ => panic!("parent context is not an item"),
        }
    }
}


pub trait Rewrite<'ast> {
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool;
}


pub fn rewrite<'ast, T: Rewrite<'ast>>(old: &'ast T, new: &'ast T) -> Vec<(Span, String)> {
    let mut rcx = RewriteCtxt::new();
    assert!(rcx.rewrite(old, new), "rewriting failed");
    rcx.rewrites
}
