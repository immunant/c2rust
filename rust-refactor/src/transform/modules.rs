use std::collections::HashMap;
use rustc::hir::def_id::DefId;
use rustc::session::Session;
use syntax::ast::*;
use syntax::tokenstream::*;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;
use syntax::codemap::{Span, DUMMY_SP};
use transform::Transform;

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};
use util::{IntoIdent, IntoString};

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

        let krate = match_modules(krate, modules.clone(), &mut mod_names, cx.session());

        let krate = fold_nodes(krate, |pi: P<Item>| {
            // Remove the module, if it has the specific attribute
            if is_from_source_header(&pi.attrs) {
                return SmallVector::new();
            }
            SmallVector::one(pi)
        });

        let krate = clean_module_items(krate, &mut mod_names);

        // This is where the `old module` items get moved into the `new modules`
        let krate = fold_nodes(krate, |pi: P<Item>| {
            if !matches!([pi.node] ItemKind::Mod(..)) {
                return SmallVector::one(pi);
            }

            SmallVector::one(pi.map(|i| {
                unpack!([i.node.clone()] ItemKind::Mod(m));
                let mut m = m;
                let mod_name = i.ident.into_string();
                let modinfos = mod_names.get(&mod_name);

                if let Some(modinfo_vec) = modinfos {
                    for modinfo in modinfo_vec {
                        for item in &modinfo.items {
                            m.items.push(item.clone());
                        }
                    }
                }


                Item {
                    node: ItemKind::Mod(m),
                    .. i
                }
            }))
        });

        let mut new_names = HashMap::new();

        // Iterate through mod_names and gather the `old module` identifier's,
        // use that as the key. Use the `new module` name as the value.
        for (new_mod_name, old_mods) in mod_names.iter() {
            for old_mod in old_mods.iter() {
                new_names.insert(old_mod.ident.into_string(), new_mod_name);
            }
        }

        // Change path segments of `old modules` to the to match the `new modules`.
        // Ex:
        // use foo_h::some_struct; -> use foo::some_struct;
        let krate = fold_nodes(krate, |mut p: Path| {
            for segment in &mut p.segments {
                let path_name = segment.ident.into_string();
                if let Some(new_path) = new_names.get(&path_name) {
                    segment.ident = new_path.to_string().into_ident();
                }
            }
            p
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
pub fn clean_module_items(krate: Crate, mod_names: &mut HashMap<String, Vec<ModInfo>>) -> Crate {
    let mut k = krate;
    for (_, modules_to_move) in mod_names.into_iter() {
        k = fold_nodes(k, |pi: P<Item>| {
            for module in modules_to_move.iter_mut() {
                module.items.retain(|pitem| {
                    let item = pitem.clone().into_inner();
                    if item.node.ast_equiv(&pi.node) {
                        return false;
                    }
                    true
                });
            }
            SmallVector::one(pi)
        });
    }
    let krate = k;
    krate
}

// We should match possible modules together:
// test.rs should get the content of module test_h.
// So the hashmap should be something like "Test" => ModInfo { ..., "test_h"}
pub fn match_modules(krate: Crate, modules: Vec<ModInfo>, mod_names: &mut HashMap<String, Vec<ModInfo>>,
                     sess: &Session) -> Crate {
    let krate = fold_nodes(krate, |pi: P<Item>| {
        match pi.node {
            ItemKind::Mod(_) => {
                if !is_from_source_header(&pi.attrs) {
                    let mut mod_name = pi.ident.into_string();
                    let mut temp_vec = Vec::new();
                    for module in &modules {
                        let mod_info_name = module.ident.into_string();
                        if mod_name.is_empty() {
                            mod_name = get_source_file(sess);
                        }

                        if mod_info_name.contains(&mod_name) {
                            temp_vec.push(module.clone());
                        }
                    }
                    mod_names.insert(mod_name.clone(), temp_vec.clone());
                }
            },
            _ => {}
        }
        SmallVector::one(pi)
    });
    krate
}

/*
pub fn print_module_info(modules: Vec<ModInfo>) {
    for module in &modules {
        let filename_str = symbol_to_string(module);
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
*/

pub fn get_source_file(sess: &Session) -> String {
    let s = sess.local_crate_source_file.as_ref().cloned();
    s.unwrap().to_str().unwrap().to_string()
}

// This function is a check to ensure that the modules, we remove are ones translated.
// What this function is looking for is the ident, 'source_header'.
// Every translated file, that were translated with the correct option, should have:
// `#[cfg(not(source_header = "/some/path"))]`
pub fn is_from_source_header(attrs: &Vec<Attribute>) -> bool {

    // Recurse down the `TokenTree` till the `Token` is reached,
    // if the token contains an Ident with `source_tree`, this should be a translated
    // `old module` then.
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
                    let name = ident.into_string();
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

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reorganize_modules", |_args| mk(ReorganizeModules))
}
