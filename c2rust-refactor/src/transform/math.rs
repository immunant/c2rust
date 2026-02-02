use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_hir::def_id::DefId;
use rustc_span::sym;
use std::collections::HashSet;

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
/// Currently supports:
/// - `sin(x)` -> `x.sin()`
///
/// Example:
///
/// ```ignore
/// let result = sin(angle);
/// ```
///
/// gets converted to:
///
/// ```ignore
/// let result = angle.sin();
/// ```
pub struct ConvertMath;

impl Transform for ConvertMath {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        let mut sin_defs = HashSet::<DefId>::new();

        visit_nodes(krate, |fi: &ForeignItem| {
            if crate::util::contains_name(&fi.attrs, sym::no_mangle) {
                match (&*fi.ident.as_str(), &fi.kind) {
                    ("sin", ForeignItemKind::Fn(_)) => {
                        sin_defs.insert(cx.node_def_id(fi.id));
                    }
                    _ => {}
                }
            }
        });

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            match e.kind {
                ExprKind::Call(ref f, ref args) => {
                    if args.len() != 1 {
                        return;
                    }

                    // Check if this is a call to sin()
                    if let Some(def_id) = cx.try_resolve_expr(f) {
                        if sin_defs.contains(&def_id) {
                            // Convert sin(x) to x.sin()
                            let receiver = args[0].clone();
                            *e = mk().span(e.span).method_call_expr(
                                receiver,
                                "sin",
                                Vec::<P<Expr>>::new(),
                            );
                        }
                    }
                }
                _ => {}
            }
        })
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("convert_math_funcs", |_args| mk(ConvertMath))
}
