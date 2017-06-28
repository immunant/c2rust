use std::collections::hash_map::{HashMap, Entry};
use std::result;
use syntax::ast::{Ident, Expr, Pat, Stmt, Item, Crate, Mac};
use syntax::symbol::Symbol;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::visit::{self, Visitor};

use bindings::Bindings;
use fold::Fold;


pub type Result = result::Result<(), Error>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Error {
    NotImplemented,
    VariantMismatch,
    LengthMismatch,
    SymbolMismatch,

    // For nonlinear patterns, it's possible that the 2nd+ occurrence of the variable in the
    // pattern matches a different ident/expr/stmt than the 1st occurrence.
    NonlinearMismatch,
}

#[derive(Debug)]
pub struct MatchCtxt {
    pub bindings: Bindings,
}

impl MatchCtxt {
    pub fn new() -> MatchCtxt {
        MatchCtxt {
            bindings: Bindings::new(),
        }
    }

    pub fn from_match<T: TryMatch>(pat: &T, target: &T) -> result::Result<MatchCtxt, Error> {
        let mut m = MatchCtxt::new();
        m.try_match(pat, target)?;
        Ok(m)
    }

    pub fn try_match<T: TryMatch>(&mut self, pat: &T, target: &T) -> Result {
        let r = pat.try_match(target, self);
        r
    }

    pub fn try_capture_ident(&mut self, sym: Symbol, ident: &Ident) -> Result {
        let ok = self.bindings.try_add_ident(sym, ident.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn try_capture_expr(&mut self, sym: Symbol, expr: &P<Expr>) -> Result {
        let ok = self.bindings.try_add_expr(sym, expr.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn try_capture_pat(&mut self, sym: Symbol, pat: &P<Pat>) -> Result {
        let ok = self.bindings.try_add_pat(sym, pat.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn try_capture_stmt(&mut self, sym: Symbol, stmt: &P<Stmt>) -> Result {
        let ok = self.bindings.try_add_stmt(sym, stmt.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }
}

pub trait TryMatch {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> Result;
}



pub trait Pattern<'a, F>: TryMatch+Fold+Sized
        where F: FnMut(Self, Bindings) -> <Self as Fold>::Result + 'a {
    type Folder: Folder;
    fn make_folder(self, callback: F) -> Self::Folder;
}

pub struct ExprPatternFolder<F>
        where F: FnMut(P<Expr>, Bindings) -> P<Expr> {
    pattern: P<Expr>,
    callback: F,
}

impl<'a, F> Folder for ExprPatternFolder<F>
        where F: FnMut(P<Expr>, Bindings) -> P<Expr> + 'a {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        let e =
            if let Ok(mcx) = MatchCtxt::from_match(&self.pattern, &e) {
                (self.callback)(e, mcx.bindings)
            } else {
                e
            };

        e.map(|e| fold::noop_fold_expr(e, self))
    }
}

impl<'a, F> Pattern<'a, F> for P<Expr>
        where F: FnMut(P<Expr>, Bindings) -> P<Expr> + 'a {
    type Folder = ExprPatternFolder<F>;
    fn make_folder(self, callback: F) -> Self::Folder {
        ExprPatternFolder {
            pattern: self,
            callback: callback,
        }
    }
}

/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn fold_match<'a, P, T, F>(pattern: P, target: T, callback: F) -> <T as Fold>::Result
        where P: Pattern<'a, F>,
              T: Fold,
              F: FnMut(P, Bindings) -> <P as Fold>::Result + 'a {
    let mut f = pattern.make_folder(callback);
    target.fold(&mut f)
}






struct FirstExprVisitor<'p> {
    pat: &'p Expr,
    result: Option<MatchCtxt>,
}

impl<'a, 'p> Visitor<'a> for FirstExprVisitor<'p> {
    fn visit_item(&mut self, i: &'a Item) {
        if self.result.is_some() {
            return;
        }
        visit::walk_item(self, i);
    }

    fn visit_stmt(&mut self, s: &'a Stmt) {
        if self.result.is_some() {
            return;
        }
        visit::walk_stmt(self, s);
    }

    fn visit_expr(&mut self, e: &'a Expr) {
        if self.result.is_some() {
            return;
        }

        if let Ok(mcx) = MatchCtxt::from_match(self.pat, e) {
            self.result = Some(mcx);
        } else {
            visit::walk_expr(self, e);
        }
    }

    fn visit_mac(&mut self, mac: &'a Mac) {
        // TODO
    }
}

pub fn match_first_expr(pat: &Expr, ast: &Crate) -> Option<MatchCtxt> {
    let mut v = FirstExprVisitor {
        pat: pat,
        result: None
    };
    visit::walk_crate(&mut v, ast);
    v.result
}





