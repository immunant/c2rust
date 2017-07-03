use syntax::ast::{Ident, Expr, Pat, Ty, Stmt, Item};
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use bindings::Bindings;
use fold::Fold;
use util::AsSymbol;
use util::Lone;


struct SubstFolder<'a> {
    bindings: &'a Bindings,
}

impl<'a> Folder for SubstFolder<'a> {
    fn fold_ident(&mut self, i: Ident) -> Ident {
        // The `Ident` case is a bit different from the others.  If `fold_stmt` finds a non-`Stmt`
        // in `self.bindings`, it can ignore the problem and hope `fold_expr` or `fold_ident` will
        // find an `Expr`/`Ident` for the symbol later on.  If `fold_ident` fails, there is no
        // lower-level construct to try.  So we report an error if a binding exists at this point
        // but is not an `Ident`.

        if let Some(sym) = i.as_symbol() {
            if let Some(ident) = self.bindings.get_ident(sym) {
                return ident.clone();
            } else if let Some(ty) = self.bindings.get_type(sym) {
                panic!("binding {:?} (of type {:?}) has wrong type for hole", sym, ty);
            }
            // Otherwise, fall through
        }
        fold::noop_fold_ident(i, self)
    }

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if let Some(expr) = e.as_symbol().and_then(|sym| self.bindings.get_expr(sym)) {
            expr.clone()
        } else {
            e.map(|e| fold::noop_fold_expr(e, self))
        }
    }

    fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> {
        if let Some(pat) = p.as_symbol().and_then(|sym| self.bindings.get_pat(sym)) {
            pat.clone()
        } else {
            fold::noop_fold_pat(p, self)
        }
    }

    fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty> {
        if let Some(ty) = ty.as_symbol().and_then(|sym| self.bindings.get_ty(sym)) {
            ty.clone()
        } else {
            fold::noop_fold_ty(ty, self)
        }
    }

    fn fold_stmt(&mut self, s: Stmt) -> SmallVector<Stmt> {
        if let Some(stmt) = s.as_symbol().and_then(|sym| self.bindings.get_stmt(sym)) {
            SmallVector::one(stmt.clone())
        } else if let Some(stmts) = s.as_symbol()
                .and_then(|sym| self.bindings.get_multi_stmt(sym)) {
            SmallVector::many(stmts.clone())
        } else {
            fold::noop_fold_stmt(s, self)
        }
    }

    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
        if let Some(item) = i.as_symbol().and_then(|sym| self.bindings.get_item(sym)) {
            SmallVector::one(item.clone())
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

macro_rules! multi_subst_impl {
    ($ty:ty, $fold_func:ident) => {
        impl Subst for Vec<$ty> {
            fn subst(self, bindings: &Bindings) -> Self {
                let mut f = SubstFolder { bindings: bindings };
                let mut results = Vec::with_capacity(self.len());
                for x in self {
                    results.extend_from_slice(&x.fold(&mut f));
                }
                results
            }
        }
    };
}

subst_impl!(Ident, fold_ident);
subst_impl!(P<Expr>, fold_expr);
subst_impl!(P<Pat>, fold_pat);
subst_impl!(P<Ty>, fold_ty);
subst_impl!(Stmt, fold_stmt);
subst_impl!(P<Item>, fold_item);

multi_subst_impl!(Stmt, fold_stmt);
multi_subst_impl!(P<Item>, fold_item);
