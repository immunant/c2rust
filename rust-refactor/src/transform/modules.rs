use std::collections::HashMap;
use std::path::Path;
use std::fs::File;
use std::io::Write;
use rustc::hir::def_id::DefId;
use rustc::session::Session;
use syntax::ast::*;
use syntax::tokenstream::*;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::print::pprust;
use syntax::util::small_vector::SmallVector;
use syntax::codemap::{Span, DUMMY_SP};
use transform::Transform;

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};

pub struct ReorganizeModules;

#[derive(Clone, Debug)]
pub struct ModInfo {
    pub id: NodeId,
    pub def_id: DefId,
    pub ident: Ident,
    pub items: Vec<P<Item>>,
    pub attrs: Vec<Attribute>,
    pub span: Span,
    pub consumed: bool,
}

impl Transform for ReorganizeModules {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let mut modules = Vec::new();
        let mut mod_names = HashMap::new();

        // Push the modules into a vec
        visit_nodes(&krate, |i: &Item| {
            match i.node {
                ItemKind::Mod(ref m) => {
                    if is_from_source_header(&i.attrs) {
                        modules.push(ModInfo {
                            id: i.id,
                            def_id: cx.node_def_id(i.id),
                            ident: i.ident.clone(),
                            items: m.items.clone(),
                            attrs: i.attrs.clone(),
                            span: m.inner,
                            consumed: false,
                        });
                    }
                },
                _ => {},
            }
        });

        let krate = match_modules(krate, modules.clone(), &mut mod_names);

        let krate = fold_nodes(krate, |pi: P<Item>| {
            // Remove the module, if it has the specific attribute
            if is_from_source_header(&pi.attrs) {
                return SmallVector::new();
            }
            SmallVector::one(pi)
        });

        /*
        let krate = fold_nodes(krate, |mut c: Crate| {
            for module in &modules {
                let index = find_index(c.module.items.clone());
                c.module.items.insert(index, mk().pub_().module(module.ident, Vec::new(), c.span));
            }
            c
        });
        */

        let krate = clean_module_items(krate, &mut mod_names);

        let krate = fold_nodes(krate, |pi: P<Item>| {
            match pi.node {
                ItemKind::Mod(ref m) => {
                    let pi_clone = pi.clone();
                    return SmallVector::one(pi_clone.map(|i| {
                        let mod_name = symbol_to_string(i.ident.name);
                        let modinfo = mod_names.get(&mod_name);
                        if modinfo.is_none() {
                            return i;
                        }

                        let modinfo_items = modinfo.unwrap().items.clone();
                        let mut m = m.clone();
                        let mut m_items = m.items.clone();
                        for item in modinfo_items {
                            m_items.push(item);
                        }
                        m = Mod {
                            items: m_items.clone(),
                            .. m
                        };

                        Item {
                            node: ItemKind::Mod(m),
                            .. i
                        }
                    }));
                },
                _ => {}
            }
            SmallVector::one(pi)
        });

        // print_module_info(modules);

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

// Iterate through the vector and find the first position,
// where there isn't a dummy location. A useful function for inserting
// at the beginning of a file.
pub fn find_index(module_items: Vec<P<Item>>) -> usize {
    for (index, item) in module_items.iter().enumerate() {
        if item.span != DUMMY_SP {
            return index;
        }
    }
    module_items.len()
}

// If a module already has an item, that is in the ModInfo item list,
// delete that value from the ModInfo list. We do not want to reprint that value.
pub fn clean_module_items(krate: Crate, mod_names: &mut HashMap<String, ModInfo>) -> Crate {
    let mut k = krate;
    for (_, module_to_move) in mod_names.into_iter() {
        k = fold_nodes(k, |pi: P<Item>| {
            module_to_move.items.retain(|pitem| {
                let item = pitem.clone().into_inner();
                if item.node.ast_equiv(&pi.node) {
                    return false;
                }
                true
            });

            SmallVector::one(pi)
        });
    }
    let krate = k;
    krate
}

// We should match possible modules together:
// test.rs should get the content of module test_h.
// So the hashmap should be something like "Test" => ModInfo { ..., "test_h"}
pub fn match_modules(krate: Crate, modules: Vec<ModInfo>, mod_names: &mut HashMap<String, ModInfo>) -> Crate {
    let krate = fold_nodes(krate, |pi: P<Item>| {
        match pi.node {
            ItemKind::Mod(_) => {
                if !is_from_source_header(&pi.attrs) {
                    let mod_name = symbol_to_string(pi.ident.name);
                    for module in &modules {
                        let mod_info_name = symbol_to_string(module.ident.name);
                        if mod_info_name.contains(&mod_name) && !mod_name.is_empty() {
                            mod_names.insert(mod_name.clone(), module.clone());
                        }
                    }
                }
            },
            _ => {}
        }
        SmallVector::one(pi)
    });
    krate
}

pub fn print_module_info(modules: Vec<ModInfo>) {
    for module in &modules {
        let filename_str = symbol_to_string(module.ident.name);
        let filename = Path::new(&filename_str).with_extension("new");
        let s = pprust::to_string(|s| {
            for p_item in &module.items {
                s.print_item(&*p_item)?;
            }
            Ok(())
        });

        let mut file = match File::create(filename) {
            Ok(file) => file,
            Err(e) => panic!("Unable to open file for writing: {}", e),
        };

        match file.write_all(s.as_bytes()) {
            Ok(()) => (),
            Err(e) => panic!("Unable to write translation to file: {}", e),
        };
    }
}

pub fn get_source_file(sess: &Session) -> String {
    let s = sess.local_crate_source_file.as_ref().cloned();
    s.unwrap().to_str().unwrap().to_string()
}

// This function is a check to ensure that the modules, we remove are ones translated.
// What this function is looking for is the ident, 'source_header'.
// Every translated file, that were translated with the correct option, should have:
// `#[cfg(not(source_header = "/some/path"))]`
pub fn is_from_source_header(attrs: &Vec<Attribute>) -> bool {

    fn parse_token_tree(tree: &TokenTree, is_source_header: &mut bool) {
        match tree {
            TokenTree::Delimited(_, delimited) => {
                let stream = delimited.stream();
                stream.map(|tree| {
                    parse_token_tree(&tree, is_source_header);
                    tree
                });
            },
            TokenTree::Token(_, token) => {
                if token.is_ident() {
                    let (ident, _) = token.ident().unwrap();
                    let name = symbol_to_string(ident.name);
                    if name.contains("source_header") {
                        *is_source_header = true;
                    }
                }
            }
        }
    }

    let mut is_source_header = false;
    for attr in attrs {
        let tokens = attr.tokens.clone();
        tokens.map(|tree| {
            parse_token_tree(&tree, &mut is_source_header);
            tree
        });
    }
    is_source_header
}

pub fn symbol_to_string(sym: Symbol) -> String {
    sym.as_str().to_string()
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reorganize_modules", |_args| mk(ReorganizeModules))
}
