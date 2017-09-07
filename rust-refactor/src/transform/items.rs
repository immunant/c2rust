use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use regex::Regex;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::util::small_vector::SmallVector;

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};
use transform::Transform;
use util::IntoSymbol;


/// Rename items using regex match and replace.
pub struct RenameRegex {
    pattern: String,
    repl: String,
    filter: Option<Symbol>,
}

impl Transform for RenameRegex {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let re = Regex::new(&self.pattern).unwrap();

        // (1) Fold over items and rewrite their `ident`s.  Records the new paths of modified items
        // into `new_paths`.

        let mut new_idents = HashMap::new();
        let krate = fold_nodes(krate, |i: P<Item>| {
            if let Some(label) = self.filter {
                if !st.marked(i.id, label) {
                    return SmallVector::one(i);
                }
            }

            let name = i.ident.name.as_str();
            let new_name = re.replace(&name, &self.repl as &str);
            if let Cow::Owned(new_name) = new_name {
                new_idents.insert(cx.node_def_id(i.id), mk().ident(&new_name));

                SmallVector::one(i.map(|i| {
                    Item {
                        ident: mk().ident(&new_name),
                        .. i
                    }
                }))
            } else {
                SmallVector::one(i)
            }
        });

        // (2) Rewrite paths referring to renamed defs

        let krate = fold_resolved_paths(krate, cx, |qself, mut path, def_id| {
            if let Some(new_ident) = new_idents.get(&def_id) {
                path.segments.last_mut().unwrap().identifier = new_ident.clone();
            }
            (qself, path)
        });

        krate
    }
}


/// Replace all uses of `target` items with references to the `repl` item, and remove all `target`
/// items.
pub struct ReplaceItems;

impl Transform for ReplaceItems {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Scan items for `target` and `repl` marks, collecting the relevant `DefId`s and
        // removing all `target` items.

        let mut target_ids = HashSet::new();
        let mut repl_id = None;

        // (1a) Top-level items
        let krate = fold_nodes(krate, |i: P<Item>| {
            if st.marked(i.id, "repl") {
                if repl_id.is_none() {
                    repl_id = Some(cx.node_def_id(i.id));
                } else {
                    panic!("found multiple `repl` items");
                }
            }

            if st.marked(i.id, "target") {
                target_ids.insert(cx.node_def_id(i.id));
                SmallVector::new()
            } else {
                SmallVector::one(i)
            }
        });

        // (1b) Impl items
        // TODO: Only inherent impls are supported for now.  May not work on trait impls.
        let krate = fold_nodes(krate, |i: ImplItem| {
            if st.marked(i.id, "repl") {
                if repl_id.is_none() {
                    repl_id = Some(cx.node_def_id(i.id));
                } else {
                    panic!("found multiple `repl` items");
                }
            }

            if st.marked(i.id, "target") {
                target_ids.insert(cx.node_def_id(i.id));
                SmallVector::new()
            } else {
                SmallVector::one(i)
            }
        });

        let repl_id = repl_id.expect("found no `repl` item");

        // (2) Rewrite references to `target` items to refer to `repl` instead.

        let krate = fold_resolved_paths(krate, cx, |qself, path, def_id| {
            if target_ids.contains(&def_id) {
                (None, cx.def_path(repl_id))
            } else {
                (qself, path)
            }
        });

        // (3) Find impls for `target` types, and remove them.  This way, if a struct is removed,
        // we also remove the associated `Clone` impl.

        let krate = fold_nodes(krate, |i: P<Item>| {
            let opt_def_id = match i.node {
                ItemKind::Impl(_, _, _, _, _, ref ty, _) => cx.try_resolve_ty(ty),
                _ => None,
            };

            if let Some(def_id) = opt_def_id {
                if target_ids.contains(&def_id) {
                    return SmallVector::new();
                }
            }
            SmallVector::one(i)
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("rename_items_regex", |args| mk(RenameRegex {
        pattern: args[0].clone(),
        repl: args[1].clone(),
        filter: args.get(2).map(|x| (x as &str).into_symbol()),
    }));

    reg.register("replace_items", |_args| mk(ReplaceItems));
}

