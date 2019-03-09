use syntax::ast::Crate;

use crate::command::{CommandState, Registry};
use crate::matcher::replace_expr;
use crate::transform::Transform;
use crate::RefactorCtxt;


/// # `wrapping_arith_to_normal` Command
/// 
/// Usage: `wrapping_arith_to_normal`
/// 
/// Replace all uses of wrapping arithmetic methods with ordinary arithmetic
/// operators.  For example, replace `x.wrapping_add(y)` with `x + y`.
pub struct WrappingToNormal;

impl Transform for WrappingToNormal {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &RefactorCtxt) -> Crate {
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_add($y:Expr)",
                                 "$x + $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_sub($y:Expr)",
                                 "$x - $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_mul($y:Expr)",
                                 "$x * $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_div($y:Expr)",
                                 "$x / $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_rem($y:Expr)",
                                 "$x % $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_neg()",
                                 "-$x");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_shl($y:Expr)",
                                 "$x << $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_shr($y:Expr)",
                                 "$x >> $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:Expr.wrapping_abs()",
                                 "$x:Expr.abs()");
        krate
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("wrapping_arith_to_normal", |_args| mk(WrappingToNormal));
}
