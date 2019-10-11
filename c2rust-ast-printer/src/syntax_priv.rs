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
