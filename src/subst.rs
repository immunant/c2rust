use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;
use std::result;
use syntax::ast::{Ident, Expr, Pat, Stmt, Item, Crate, Mac};
use syntax::fold::{self, Folder};
use syntax::symbol::Symbol;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use bindings::Bindings;
use fold::Fold;
use matcher::MatchCtxt;
use util;
use util::Lone;


struct SubstFolder<'a> {
    bindings: &'a Bindings,
}

impl<'a> Folder for SubstFolder<'a> {
    fn fold_ident(&mut self, i: Ident) -> Ident {
        if let Some(sym) = util::ident_sym(&i) {
            self.bindings.ident(sym).clone()
        } else {
            fold::noop_fold_ident(i, self)
        }
    }

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if let Some(sym) = util::expr_sym(&e) {
            self.bindings.expr(sym).clone()
        } else {
            e.map(|e| fold::noop_fold_expr(e, self))
        }
    }

    fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> {
        if let Some(sym) = util::pat_sym(&p) {
            self.bindings.pat(sym).clone()
        } else {
            fold::noop_fold_pat(p, self)
        }
    }

    fn fold_stmt(&mut self, s: Stmt) -> SmallVector<Stmt> {
        if let Some(sym) = util::stmt_sym(&s) {
            SmallVector::one((**self.bindings.stmt(sym)).clone())
        } else {
            fold::noop_fold_stmt(s, self)
        }
    }

    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
        if let Some(sym) = util::item_sym(&i) {
            SmallVector::one(self.bindings.item(sym).clone())
        } else {
            fold::noop_fold_item(i, self)
        }
    }
}


pub trait Subst {
    fn subst(self, bindings: &Bindings) -> Self;
}

macro_rules! subst_impl {
    ($ty:ty, $fold_func:ident) => {
        impl Subst for $ty {
            fn subst(self, bindings: &Bindings) -> Self {
                let mut f = SubstFolder { bindings: bindings };
                let result = self.fold(&mut f);
                result.lone()
            }
        }
    };
}

subst_impl!(Ident, fold_ident);
subst_impl!(P<Expr>, fold_expr);
subst_impl!(P<Pat>, fold_pat);
subst_impl!(Stmt, fold_stmt);
subst_impl!(P<Item>, fold_item);
