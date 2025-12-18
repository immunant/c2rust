use crate::ast_manip::visit_nodes;
use crate::ast_manip::util::{namespace, UseInfo};
use crate::command::{CommandState, Registry};
use crate::path_edit::fold_resolved_paths_with_id;
use crate::transform::Transform;
use crate::RefactorCtxt;
use log::debug;
use rustc_ast::*;
use std::collections::HashMap;

/// # `shorten_imported_paths` Command
///
/// Usage: `shorten_imported_paths`
///
/// Shorten all fully qualified paths where the identifier is already
/// imported into the current module with a `use` item.
///
/// Example:
///
/// ```ignore
///     use crate::foo::bar;
///     crate::foo::bar();
/// ```
///
/// After running `shorten_imported_paths`:
///
/// ```ignore
///     use crate::foo::bar();
///     bar();
/// ```
pub struct ShortenImportedPaths;

impl Transform for ShortenImportedPaths {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        let mut all_uses = HashMap::new();
        visit_nodes(krate, |i: &Item| {
            let ItemKind::Use(use_tree) = &i.kind else {
                return;
            };

            let uses_in_item = UseInfo::from_use_tree(use_tree, i.id);
            if uses_in_item.is_empty() {
                return;
            }

            let item_hir_id = cx.hir_map().node_to_hir_id(i.id);
            let item_mod_ldid = cx.ty_ctxt().parent_module(item_hir_id);
            for use_info in uses_in_item {
                for id in use_info.ids {
                    let Some(path) = cx.try_resolve_use_id(id) else {
                        continue;
                    };
                    let Some(ns) = namespace(&path.res) else {
                        continue;
                    };

                    let key = (item_mod_ldid, ns, use_info.ident.clone());
                    if let Some(did) = path.res.opt_def_id() {
                        let value = (id, did);
                        all_uses.insert(key, value);
                    }
                }
            }
        });

        // If any items are imported into the current module,
        // use the short paths, e.g., `bar` instead of `crate::foo::bar`.
        fold_resolved_paths_with_id(krate, cx, |id, qself, mut path, defs| {
            // Keep qualified paths with Self as they are
            if qself.is_some() {
                return (qself, path);
            }

            let item_hir_id = cx.hir_map().node_to_hir_id(id);
            let item_mod_ldid = cx.ty_ctxt().parent_module(item_hir_id);
            let last_seg = path.segments.last().expect("empty path");

            let mut is_use = false;
            let mut matches_use = false;
            debug!("Shortening {:?} (def: {:?})", path, defs);
            for def in defs {
                let (Some(path_def_id), Some(ns)) = (def.opt_def_id(), def.ns()) else {
                    continue;
                };

                let key = (item_mod_ldid, ns, last_seg.ident);
                let Some((use_id, use_did)) = all_uses.get(&key) else {
                    continue;
                };
                if id == *use_id {
                    // The current item is exactly the use in the map
                    is_use = true;
                    break;
                }
                if path_def_id == *use_did {
                    matches_use = true;
                    break;
                }
            }

            if matches_use && !is_use {
                // Current path matches one of the uses
                // in the current module; shorten it.
                path.segments.drain(..path.segments.len() - 1);
            }
            (None, path)
        });
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("shorten_imported_paths", |_args| mk(ShortenImportedPaths));
}
