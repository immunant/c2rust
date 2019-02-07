use syntax::ast::Crate;

use crate::api::*;
use crate::command::{CommandState, Registry};
use crate::driver;
use crate::transform::Transform;


/// # `wrapping_arith_to_normal` Command
/// 
/// Usage: `wrapping_arith_to_normal`
/// 
/// Replace all uses of wrapping arithmetic methods with ordinary arithmetic
/// operators.  For example, replace `x.wrapping_add(y)` with `x + y`.
pub struct WrappingToNormal;

impl Transform for WrappingToNormal {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_add($y:expr)",
                                 "$x + $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_sub($y:expr)",
                                 "$x - $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_mul($y:expr)",
                                 "$x * $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_div($y:expr)",
                                 "$x / $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_rem($y:expr)",
                                 "$x % $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_neg()",
                                 "-$x");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_shl($y:expr)",
                                 "$x << $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_shr($y:expr)",
                                 "$x >> $y");
        let krate = replace_expr(st, cx, krate,
                                 "$x:expr.wrapping_abs()",
                                 "$x:expr.abs()");
        krate
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("wrapping_arith_to_normal", |_args| mk(WrappingToNormal));
}
