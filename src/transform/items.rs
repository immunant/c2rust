use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use regex::Regex;
use rustc::hir::def_id::DefId;
use rustc::ty::TypeVariants;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::attr;
use syntax::codemap::Spanned;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::util::small_vector::SmallVector;

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};
use fold::Fold;
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


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("rename_items_regex", |args| mk(RenameRegex {
        pattern: args[0].clone(),
        repl: args[1].clone(),
        filter: args.get(2).map(|x| (x as &str).into_symbol()),
    }));
}

