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

    pub fn record(&mut self, sp: Span, new_text: String) {
        self.rewrites.push((sp, new_text));
    }

    pub fn rewrite<R: Rewrite<'ast>>(&mut self, old: &'ast R, new: &'ast R) {
        old.rewrite(new, self);
    }

    fn rewrite_in_context<R: Rewrite<'ast>>(&mut self,
                                            context: Context<'ast>,
                                            old: &'ast R,
                                            new: &'ast R) {
        self.stack.push(context);
        old.rewrite(new, self);
        self.stack.pop();
    }


    pub fn rewrite_in_expr<R: Rewrite<'ast>>(&mut self,
                                             old_expr: &'ast Expr,
                                             new_expr: &'ast Expr,
                                             old: &'ast R,
                                             new: &'ast R) {
        self.rewrite_in_context(Context::Expr(old_expr, new_expr), old, new);
    }

    pub fn rewrite_in_stmt<R: Rewrite<'ast>>(&mut self,
                                             old_stmt: &'ast Stmt,
                                             new_stmt: &'ast Stmt,
                                             old: &'ast R,
                                             new: &'ast R) {
        self.rewrite_in_context(Context::Stmt(old_stmt, new_stmt), old, new);
    }

    pub fn rewrite_in_item<R: Rewrite<'ast>>(&mut self,
                                             old_item: &'ast Item,
                                             new_item: &'ast Item,
                                             old: &'ast R,
                                             new: &'ast R) {
        self.rewrite_in_context(Context::Item(old_item, new_item), old, new);
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
    fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>);
}
