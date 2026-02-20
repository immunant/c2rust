use rustc_ast::ptr::P;
use rustc_ast::{Crate, Expr, ExprKind};
use std::collections::HashMap;

use crate::ast_builder::mk;
use crate::ast_manip::{visit_foreign_fns, MutVisitNodes};
use crate::command::{CommandState, Registry};
use crate::transform::Transform;
use crate::RefactorCtxt;

/// # `convert_math_funcs` Command
///
/// Usage: `convert_math_funcs`
///
/// Marks: none
///
/// Converts calls to libc math functions into calls to the corresponding
/// Rust standard library methods. For example, converts `sin(x)` to `x.sin()`.
///
/// This command checks that the callees are foreign functions imported using
/// `extern "C"` and marked `#[no_mangle]`, and are not defined in the current
/// crate, to ensure the caller is actually calling the libc functions.
///
/// - `sin(x)` / `sinf(x)` / `sinl(x)` -> `x.sin()`
/// - `cos(x)` / `cosf(x)` / `cosl(x)` -> `x.cos()`
/// - `tan(x)` / `tanf(x)` / `tanl(x)` -> `x.tan()`
/// - `asin(x)` / `asinf(x)` / `asinl(x)` -> `x.asin()`
/// - `acos(x)` / `acosf(x)` / `acosl(x)` -> `x.acos()`
/// - `atan(x)` / `atanf(x)` / `atanl(x)` -> `x.atan()`
/// - `sinh(x)` / `sinhf(x)` / `sinhl(x)` -> `x.sinh()`
/// - `cosh(x)` / `coshf(x)` / `coshl(x)` -> `x.cosh()`
/// - `tanh(x)` / `tanhf(x)` / `tanhl(x)` -> `x.tanh()`
/// - `asinh(x)` / `asinhf(x)` / `asinhl(x)` -> `x.asinh()`
/// - `acosh(x)` / `acoshf(x)` / `acoshl(x)` -> `x.acosh()`
/// - `atanh(x)` / `atanhf(x)` / `atanhl(x)` -> `x.atanh()`
/// - `sqrt(x)` / `sqrtf(x)` / `sqrtl(x)` -> `x.sqrt()`
/// - `cbrt(x)` / `cbrtf(x)` / `cbrtl(x)` -> `x.cbrt()`
/// - `log(x)` / `logf(x)` / `logl(x)` -> `x.ln()`
/// - `exp(x)` / `expf(x)` / `expl(x)` -> `x.exp()`
/// - `fabs(x)` / `fabsf(x)` / `fabsl(x)` -> `x.abs()`
/// - `abs(x)` / `labs(x)` / `llabs(x)` -> `x.abs()`
/// - `floor(x)` / `floorf(x)` / `floorl(x)` -> `x.floor()`
/// - `ceil(x)` / `ceilf(x)` / `ceill(x)` -> `x.ceil()`
/// - `round(x)` / `roundf(x)` / `roundl(x)` -> `x.round()`
/// - `trunc(x)` / `truncf(x)` / `truncl(x)` -> `x.trunc()`
/// - `pow(x, y)` / `powf(x, y)` / `powl(x, y)` -> `x.powf(y)`
/// - `atan2(y, x)` / `atan2f(y, x)` / `atan2l(y, x)` -> `y.atan2(x)`
/// - `hypot(x, y)` / `hypotf(x, y)` / `hypotl(x, y)` -> `x.hypot(y)`
/// - `copysign(x, y)` / `copysignf(x, y)` / `copysignl(x, y)` -> `x.copysign(y)`
/// - `fmin(x, y)` / `fminf(x, y)` / `fminl(x, y)` -> `x.min(y)`
/// - `fmax(x, y)` / `fmaxf(x, y)` / `fmaxl(x, y)` -> `x.max(y)`
///
/// Example:
///
/// ```ignore
/// let result = sin(angle);
/// let result_f32 = sinf(angle_f32);
/// ```
///
/// gets converted to:
///
/// ```ignore
/// let result = angle.sin();
/// let result_f32 = angle_f32.sin();
/// ```
pub struct ConvertMath;

impl Transform for ConvertMath {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        let mut unary_defs = HashMap::new();
        let mut binary_defs = HashMap::new();

        visit_foreign_fns(krate, cx, |fi, def_id| {
            let unary_defs = &mut unary_defs;
            let binary_defs = &mut binary_defs;
            let (defs, name) = match fi.ident.as_str() {
                "sin" | "sinf" | "sinl" => (unary_defs, "sin"),
                "cos" | "cosf" | "cosl" => (unary_defs, "cos"),
                "tan" | "tanf" | "tanl" => (unary_defs, "tan"),
                "asin" | "asinf" | "asinl" => (unary_defs, "asin"),
                "acos" | "acosf" | "acosl" => (unary_defs, "acos"),
                "atan" | "atanf" | "atanl" => (unary_defs, "atan"),
                "sinh" | "sinhf" | "sinhl" => (unary_defs, "sinh"),
                "cosh" | "coshf" | "coshl" => (unary_defs, "cosh"),
                "tanh" | "tanhf" | "tanhl" => (unary_defs, "tanh"),
                "asinh" | "asinhf" | "asinhl" => (unary_defs, "asinh"),
                "acosh" | "acoshf" | "acoshl" => (unary_defs, "acosh"),
                "atanh" | "atanhf" | "atanhl" => (unary_defs, "atanh"),
                "sqrt" | "sqrtf" | "sqrtl" => (unary_defs, "sqrt"),
                "cbrt" | "cbrtf" | "cbrtl" => (unary_defs, "cbrt"),
                "log" | "logf" | "logl" => (unary_defs, "ln"),
                "exp" | "expf" | "expl" => (unary_defs, "exp"),
                "fabs" | "fabsf" | "fabsl" => (unary_defs, "abs"),
                "abs" | "labs" | "llabs" => (unary_defs, "abs"),
                "floor" | "floorf" | "floorl" => (unary_defs, "floor"),
                "ceil" | "ceilf" | "ceill" => (unary_defs, "ceil"),
                "round" | "roundf" | "roundl" => (unary_defs, "round"),
                "trunc" | "truncf" | "truncl" => (unary_defs, "trunc"),
                "pow" | "powf" | "powl" => (binary_defs, "powf"),
                "atan2" | "atan2f" | "atan2l" => (binary_defs, "atan2"),
                "hypot" | "hypotf" | "hypotl" => (binary_defs, "hypot"),
                "copysign" | "copysignf" | "copysignl" => (binary_defs, "copysign"),
                "fmin" | "fminf" | "fminl" => (binary_defs, "min"),
                "fmax" | "fmaxf" | "fmaxl" => (binary_defs, "max"),
                _ => return,
            };
            defs.insert(def_id, name);
        });

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let ExprKind::Call(ref f, ref args) = e.kind else {
                return;
            };

            let Some(def_id) = cx.try_resolve_expr(f) else {
                return;
            };

            if let Some(&method) = unary_defs.get(&def_id) {
                let [receiver] = args.as_slice() else {
                    return;
                };
                *e = mk()
                    .span(e.span)
                    .method_call_expr(receiver, method, Vec::<P<Expr>>::new());
            } else if let Some(&method) = binary_defs.get(&def_id) {
                let [receiver, arg] = args.as_slice() else {
                    return;
                };
                *e = mk()
                    .span(e.span)
                    .method_call_expr(receiver, method, vec![arg]);
            }
        });
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("convert_math_funcs", |_args| mk(ConvertMath))
}
