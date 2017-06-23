use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;
use std::result;
use syntax::ast::{Ident, Expr, ExprKind, Stmt, Item, Crate, Mac};
use syntax::fold::{self, Folder};
use syntax::symbol::Symbol;
use syntax::ptr::P;

use matcher::MatchCtxt;

#[derive(Debug)]
pub struct ReplaceCtxt {
    pub idents: HashMap<Symbol, P<Ident>>,
    pub exprs: HashMap<Symbol, P<Expr>>,
}

impl ReplaceCtxt {
    pub fn from_match_ctxt(mcx: MatchCtxt) -> ReplaceCtxt {
        ReplaceCtxt {
            idents: mcx.cap_idents,
            exprs: mcx.cap_exprs,
        }
    }

    pub fn try_replace_ident(&self, ident: &Ident) -> Option<P<Ident>> {
        self.idents.get(&ident.name).map(|x| x.clone())
    }

    pub fn try_replace_expr(&self, expr: &Expr) -> Option<P<Expr>> {
        MatchCtxt::expr_capture_sym(expr)
            .and_then(|sym| self.exprs.get(&sym))
            .map(|x| x.clone())
    }
}


struct BuildReplacementFolder<'p> {
    rcx: &'p ReplaceCtxt,
}

impl<'p> Folder for BuildReplacementFolder<'p> {
    fn fold_ident(&mut self, i: Ident) -> Ident {
        if let Some(repl) = self.rcx.try_replace_ident(&i) {
            (*repl).clone()
        } else {
            fold::noop_fold_ident(i, self)
        }
    }

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if let Some(repl) = self.rcx.try_replace_expr(&e) {
            repl
        } else {
            e.map(|e| fold::noop_fold_expr(e, self))
        }
    }
}


fn build_replacement_expr(mcx: MatchCtxt, repl: &Expr) -> P<Expr> {
    let rcx = ReplaceCtxt::from_match_ctxt(mcx);
    let mut f = BuildReplacementFolder { rcx: &rcx };
    f.fold_expr(P(repl.clone()))
}

struct FindAndReplaceExpr<'p> {
    pat: &'p Expr,
    repl: &'p Expr,
}

impl<'p> Folder for FindAndReplaceExpr<'p> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        let e =
            MatchCtxt::from_match(self.pat, &e).ok()
                .map(|mcx| build_replacement_expr(mcx, self.repl))
                .unwrap_or(e);
        e.map(|e| fold::noop_fold_expr(e, self))
    }
}

pub fn find_and_replace_expr(pat: &Expr, repl: &Expr, krate: &Crate) -> Crate {
    let mut f = FindAndReplaceExpr {
        pat: pat,
        repl: repl,
    };
    f.fold_crate(krate.clone())
}

