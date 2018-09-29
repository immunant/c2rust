use std::collections::HashMap;
use rustc::hir::def_id::DefId;
use syntax::codemap::{CodeMap, Span, DUMMY_SP};
use syntax::util::small_vector::SmallVector;
use syntax::ptr::P;
use syntax::ast::*;
use transform::Transform;
use command::{CommandState, Registry};

use api::*;
use driver::{self, Phase};
use util::{HirDefExt, IntoSymbol};

pub struct ReorganizeModules;

#[derive(Debug)]
pub struct ModInfo {
    pub id: NodeId,
    pub def_id: DefId,
    pub ident: Ident,
    pub items: Vec<P<Item>>,
    pub consumed: bool,
}

impl Transform for ReorganizeModules {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let mut modules = Vec::new();
        // Push the module into a vec
        visit_nodes(&krate, |i: &Item| {
            if !st.marked(i.id, "target") {
                return;
            }
            
            match i.node {
                ItemKind::Mod(ref m) => {
                    modules.push(ModInfo {
                        id: i.id,
                        def_id: cx.node_def_id(i.id),
                        ident: i.ident.clone(),
                        items: m.items.clone(),
                        consumed: false,
                    });
                },

                _ => {},
            }
        });


        // remove the content of the old module and the module itself
        let krate = fold_nodes(krate, |pi: P<Item>| {
            if st.marked(pi.id, "target") {
                return SmallVector::new();
            }
            SmallVector::one(pi)
        });

        let krate = fold_nodes(krate, |mut c: Crate| { 
            for module in &modules {
                let index = find_index(c.module.items.clone());
                c.module.items.insert(index, mk().pub_().module(module.ident, Vec::new(), c.span));
            }
            c
        });

        create_new_module(modules, st, cx);

        krate 
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

// Iterate through the vector and find the first position,
// where there isn't a dummy location
pub fn find_index(module_items: Vec<P<Item>>) -> usize {
    for (index, item) in module_items.iter().enumerate() {
        if item.span != DUMMY_SP {
            return index;
        }
    }
    module_items.len() 
}

pub fn create_new_module(modules: Vec<ModInfo>, st: &CommandState, cx: &driver::Ctxt) {

}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reorganize_modules", |_args| mk(ReorganizeModules)) 
}
