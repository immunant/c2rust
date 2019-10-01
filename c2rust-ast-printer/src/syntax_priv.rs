//! Crate-local helpers from libsyntax needed for the pretty-printer

use syntax::ast;
use syntax::util::parser::{AssocOp};

// From libsyntax/src/util/parser.rs

/// In `let p = e`, operators with precedence `<=` this one requires parenthesis in `e`.
crate fn prec_let_scrutinee_needs_par() -> usize {
    AssocOp::LAnd.precedence()
}

/// Suppose we have `let _ = e` and the `order` of `e`.
/// Is the `order` such that `e` in `let _ = e` needs parenthesis when it is on the RHS?
///
/// Conversely, suppose that we have `(let _ = a) OP b` and `order` is that of `OP`.
/// Can we print this as `let _ = a OP b`?
crate fn needs_par_as_let_scrutinee(order: i8) -> bool {
    order <= prec_let_scrutinee_needs_par() as i8
}


// From libsyntax/src/parse/classify.rs

/// Does this expression require a semicolon to be treated
/// as a statement? The negation of this: 'can this expression
/// be used as a statement without a semicolon' -- is used
/// as an early-bail-out in the parser so that, for instance,
///     if true {...} else {...}
///      |x| 5
/// isn't parsed as (if true {...} else {...} | x) | 5
pub fn expr_requires_semi_to_be_stmt(e: &ast::Expr) -> bool {
    match e.kind {
        ast::ExprKind::If(..) |
        ast::ExprKind::Match(..) |
        ast::ExprKind::Block(..) |
        ast::ExprKind::While(..) |
        ast::ExprKind::Loop(..) |
        ast::ExprKind::ForLoop(..) |
        ast::ExprKind::TryBlock(..) => false,
        _ => true,
    }
}
