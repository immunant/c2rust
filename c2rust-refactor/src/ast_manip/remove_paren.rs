//! `remove_paren` function, for removing unnecessary `ExprKind::Paren` nodes.
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::*;

use crate::ast_manip::MutVisit;

/// AST fold for deleting `ExprKind::Paren` nodes.  These are used only for pretty-printing, but
/// cause problems when we compare ASTs (for example, "Mul(Add(x, y), z)" prints as "(x + y) * z",
/// which parses back as "Mul(Paren(Add(x, y)), z)").
struct RemoveParen;

impl MutVisitor for RemoveParen {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        if let ExprKind::Paren(ref inner) = e.kind {
            *e = inner.clone();
        }
        mut_visit::noop_visit_expr(e, self);
    }

    fn visit_ty(&mut self, t: &mut P<Ty>) {
        if let TyKind::Paren(ref inner) = t.kind {
            *t = inner.clone();
        }
        mut_visit::noop_visit_ty(t, self)
    }

    // Need a no-op implementation to avoid "fold_mac disabled by default" error.
    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

#[cfg_attr(feature = "profile", flame)]
pub fn remove_paren<T: MutVisit>(x: &mut T) {
    x.visit(&mut RemoveParen)
}
