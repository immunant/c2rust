use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_hir::Node;
use rustc_span::sym;
use std::collections::HashMap;

use crate::ast_builder::mk;
use crate::ast_manip::{visit_nodes, MutVisitNodes};
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
/// This command checks that the callees are foreign functions imported
/// using `extern "C"` and marked `#[no_mangle]`, to ensure the caller
/// is actually calling the libc functions.
///
/// - `sin(x)` / `sinf(x)` / `sinl(x)` -> `x.sin()`
/// - `cos(x)` / `cosf(x)` / `cosl(x)` -> `x.cos()`
/// - `tan(x)` / `tanf(x)` / `tanl(x)` -> `x.tan()`
/// - `sqrt(x)` / `sqrtf(x)` / `sqrtl(x)` -> `x.sqrt()`
/// - `log(x)` / `logf(x)` / `logl(x)` -> `x.ln()`
/// - `exp(x)` / `expf(x)` / `expl(x)` -> `x.exp()`
/// - `fabs(x)` / `fabsf(x)` / `fabsl(x)` -> `x.abs()`
/// - `abs(x)` / `labs(x)` / `llabs(x)` -> `x.abs()`
/// - `floor(x)` / `floorf(x)` / `floorl(x)` -> `x.floor()`
/// - `ceil(x)` / `ceilf(x)` / `ceill(x)` -> `x.ceil()`
/// - `trunc(x)` / `truncf(x)` / `truncl(x)` -> `x.trunc()`
/// - `pow(x, y)` / `powf(x, y)` / `powl(x, y)` -> `x.powf(y)`
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
        // Track unary and binary math function variants by DefId
        let mut unary_defs = HashMap::new();
        let mut binary_defs = HashMap::new();

        visit_nodes(krate, |fi: &ForeignItem| {
            if crate::util::contains_name(&fi.attrs, sym::no_mangle) {
                if let ForeignItemKind::Fn(_) = fi.kind {
                    let def_id = cx.node_def_id(fi.id);
                    match &*fi.ident.as_str() {
                        "sin" | "sinf" | "sinl" => {
                            unary_defs.insert(def_id, "sin");
                        }
                        "cos" | "cosf" | "cosl" => {
                            unary_defs.insert(def_id, "cos");
                        }
                        "tan" | "tanf" | "tanl" => {
                            unary_defs.insert(def_id, "tan");
                        }
                        "sqrt" | "sqrtf" | "sqrtl" => {
                            unary_defs.insert(def_id, "sqrt");
                        }
                        "log" | "logf" | "logl" => {
                            unary_defs.insert(def_id, "ln");
                        }
                        "exp" | "expf" | "expl" => {
                            unary_defs.insert(def_id, "exp");
                        }
                        "fabs" | "fabsf" | "fabsl" => {
                            unary_defs.insert(def_id, "abs");
                        }
                        "abs" | "labs" | "llabs" => {
                            unary_defs.insert(def_id, "abs");
                        }
                        "floor" | "floorf" | "floorl" => {
                            unary_defs.insert(def_id, "floor");
                        }
                        "ceil" | "ceilf" | "ceill" => {
                            unary_defs.insert(def_id, "ceil");
                        }
                        "trunc" | "truncf" | "truncl" => {
                            unary_defs.insert(def_id, "trunc");
                        }
                        "pow" | "powf" | "powl" => {
                            binary_defs.insert(def_id, "powf");
                        }
                        _ => {}
                    }
                }
            }
        });

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let ExprKind::Call(ref f, ref args) = e.kind else {
                return;
            };

            let Some(def_id) = cx.try_resolve_expr(f) else {
                return;
            };

            // Ignore functions that are defined locally since that has to be a
            // custom math function. We only want to translate calls to the libc
            // math functions.
            if def_id.is_local() {
                match cx.hir_map().get_if_local(def_id) {
                    Some(Node::ForeignItem(_)) => {}
                    _ => return,
                }
            }

            if let Some(&method) = unary_defs.get(&def_id) {
                if args.len() != 1 {
                    return;
                }
                let receiver = args[0].clone();
                *e = mk()
                    .span(e.span)
                    .method_call_expr(receiver, method, Vec::<P<Expr>>::new());
            } else if let Some(&method) = binary_defs.get(&def_id) {
                if args.len() != 2 {
                    return;
                }
                let receiver = args[0].clone();
                let method_args = vec![args[1].clone()];
                *e = mk()
                    .span(e.span)
                    .method_call_expr(receiver, method, method_args);
            }
        })
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("convert_math_funcs", |_args| mk(ConvertMath))
}
