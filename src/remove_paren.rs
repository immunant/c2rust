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
        let e = match e.node {
            ExprKind::Paren(ref inner) => inner.clone(),
            _ => e.clone(),
        };
        e.map(|e| fold::noop_fold_expr(e, self))
    }
}

pub fn remove_paren<T: Fold>(x: T) -> <T as Fold>::Result {
    x.fold(&mut RemoveParen)
}
