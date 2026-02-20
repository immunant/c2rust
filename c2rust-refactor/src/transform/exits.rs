use rustc_ast::ptr::P;
use rustc_ast::{Crate, Expr, ExprKind};
use std::collections::HashMap;

use crate::ast_builder::mk;
use crate::ast_manip::{visit_foreign_fns, MutVisitNodes};
use crate::command::{CommandState, Registry};
use crate::transform::Transform;
use crate::RefactorCtxt;

/// # `convert_exits` Command
///
/// Usage: `convert_exits`
///
/// Converts C-style abort and exit function calls to use the equivalent
/// Rust std::process functions:
///
/// - `abort()` becomes `std::process::abort()`
/// - `exit(status)` becomes `std::process::exit(status)`
///
/// This only transforms actual libc function calls, not locally-defined
/// functions with the same names.
pub struct ConvertExits;

impl Transform for ConvertExits {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        enum ExitFn {
            Abort,
            Exit,
        }

        let mut exit_defs = HashMap::new();
        visit_foreign_fns(krate, cx, |fi, def_id| match fi.ident.as_str() {
            "abort" => {
                exit_defs.insert(def_id, ExitFn::Abort);
            }
            "exit" => {
                exit_defs.insert(def_id, ExitFn::Exit);
            }
            _ => {}
        });

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let ExprKind::Call(f, args) = &e.kind else {
                return;
            };

            let Some(def_id) = cx.try_resolve_expr(f) else {
                return;
            };

            if let Some(func) = exit_defs.get(&def_id) {
                match func {
                    ExitFn::Abort => {
                        *e = mk().span(e.span).call_expr(
                            mk().path_expr(vec!["std", "process", "abort"]),
                            Vec::<P<Expr>>::new(),
                        );
                    }
                    ExitFn::Exit => {
                        let [status] = args.as_slice() else {
                            return;
                        };
                        *e = mk().span(e.span).call_expr(
                            mk().path_expr(vec!["std", "process", "exit"]),
                            vec![status],
                        );
                    }
                }
            }
        });
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;
    reg.register("convert_exits", |_args| mk(ConvertExits));
}
