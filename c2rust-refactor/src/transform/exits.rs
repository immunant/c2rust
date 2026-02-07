use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_hir::Node;
use rustc_span::sym;
use std::collections::{HashMap, HashSet};

use crate::ast_builder::mk;
use crate::ast_manip::{visit_nodes, MutVisitNodes};
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
        // Collect names of locally-defined #[no_mangle] functions that might
        // shadow libc functions.
        let mut local_no_mangle_names = HashSet::new();
        visit_nodes(krate, |item: &Item| {
            if let ItemKind::Fn(_) = item.kind {
                if crate::util::contains_name(&item.attrs, sym::no_mangle) {
                    local_no_mangle_names.insert(item.ident.name);
                }
            }
        });

        let mut exit_defs = HashMap::new();
        visit_nodes(krate, |fi: &ForeignItem| {
            if !crate::util::contains_name(&fi.attrs, sym::no_mangle) {
                return;
            }
            let ForeignItemKind::Fn(_) = fi.kind else {
                return;
            };

            let def_id = cx.node_def_id(fi.id);

            // Ignore functions that are defined locally, either directly or as
            // indirect `extern "C"` imports, since those have to be custom
            // functions. We only want to translate calls to the foreign libc
            // functions.
            if local_no_mangle_names.contains(&fi.ident.name) {
                return;
            }
            if def_id.is_local() {
                match cx.hir_map().get_if_local(def_id) {
                    Some(Node::ForeignItem(_)) => {}
                    _ => return,
                }
            }

            match &*fi.ident.as_str() {
                "abort" => {
                    exit_defs.insert(def_id, "abort");
                }
                "exit" => {
                    exit_defs.insert(def_id, "exit");
                }
                _ => {}
            }
        });

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let ExprKind::Call(ref f, ref args) = e.kind else {
                return;
            };

            let Some(def_id) = cx.try_resolve_expr(f) else {
                return;
            };

            if let Some(&func_name) = exit_defs.get(&def_id) {
                match func_name {
                    "abort" => {
                        *e = mk().span(e.span).call_expr(
                            mk().path_expr(vec!["std", "process", "abort"]),
                            Vec::<P<Expr>>::new(),
                        );
                    }
                    "exit" => {
                        if args.len() != 1 {
                            return;
                        }
                        let status = args[0].clone();
                        *e = mk().span(e.span).call_expr(
                            mk().path_expr(vec!["std", "process", "exit"]),
                            vec![status],
                        );
                    }
                    _ => {}
                }
            }
        });
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;
    reg.register("convert_exits", |_args| mk(ConvertExits));
}
