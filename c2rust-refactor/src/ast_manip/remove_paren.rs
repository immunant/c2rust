//! `remove_paren` function, for removing unnecessary `ExprKind::Paren` nodes.
use syntax::ast::*;
use syntax::mut_visit::{self, MutVisitor};
use syntax::ptr::P;

use crate::ast_manip::MutVisit;


/// AST fold for deleting `ExprKind::Paren` nodes.  These are used only for pretty-printing, but
/// cause problems when we compare ASTs (for example, "Mul(Add(x, y), z)" prints as "(x + y) * z",
/// which parses back as "Mul(Paren(Add(x, y)), z)").
struct RemoveParen;

impl MutVisitor for RemoveParen {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        if let ExprKind::Paren(inner) = e.node {
            *e = inner;
        }
        mut_visit::noop_visit_expr(e, self);
    }

    fn visit_ty(&mut self, t: &mut P<Ty>) {
        if let TyKind::Paren(inner) = t.node {
            *t = inner;
        }
        mut_visit::noop_visit_ty(t, self)
    }

    // Need a no-op implementation to avoid "fold_mac disabled by default" error.
    fn visit_mac(&mut self, mac: &mut Mac) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

pub fn remove_paren<T: MutVisit>(x: &mut T) {
    x.visit(&mut RemoveParen)
}
