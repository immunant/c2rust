use syntax::ast::*;
use syntax::fold::{self, Folder};
use syntax::ptr::P;

use fold::Fold;


/// AST fold for deleting `ExprKind::Paren` nodes.  These are used only for pretty-printing, but
/// cause problems when we compare ASTs (for example, "Mul(Add(x, y), z)" prints as "(x + y) * z",
/// which parses back as "Mul(Paren(Add(x, y)), z)").
struct RemoveParen;

impl Folder for RemoveParen {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        let mut e = e;
        while let ExprKind::Paren(_) = e.node {
            match e.unwrap().node {
                ExprKind::Paren(inner) => { e = inner; },
                _ => unreachable!(),
            }
        }
        e.map(|e| fold::noop_fold_expr(e, self))
    }

    fn fold_ty(&mut self, t: P<Ty>) -> P<Ty> {
        let mut t = t;
        while let TyKind::Paren(_) = t.node {
            match t.unwrap().node {
                TyKind::Paren(inner) => { t = inner; },
                _ => unreachable!(),
            }
        }
        fold::noop_fold_ty(t, self)
    }

    // Need a no-op implementation to avoid "fold_mac disabled by default" error.
    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}

pub fn remove_paren<T: Fold>(x: T) -> <T as Fold>::Result {
    x.fold(&mut RemoveParen)
}
