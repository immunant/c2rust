use rustc::session::Session;
use std::collections::{HashMap, HashSet};
use syntax::ast::DUMMY_NODE_ID;
use syntax::ast::*;
use syntax::codemap::{dummy_spanned, DUMMY_SP};
use syntax::parse::token::Lit::Str_;
use syntax::parse::token::Token::Literal;
use syntax::ptr::P;
use syntax::tokenstream::*;
use syntax::util::small_vector::SmallVector;
use transform::Transform;

use api::*;
use ast_manip::AstEquiv;
use command::{CommandState, Registry};
use driver::{self, Phase};
use util::{IntoIdent, IntoString, IntoSymbol};

pub struct ReorganizeModules;

impl Transform for ReorganizeModules {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let mut decl_destination_mod = HashMap::new();

        // Match the modules, using a mapping like:
        // NodeId -> NodeId
        // The key is the id of the old item to be moved, and the value is the NodeId of the module
        // the item will be moved to.
        visit_nodes(&krate, |i: &Item| {
            match i.node {
                ItemKind::Mod(ref m) => {
                    // All C standard library headers are going to be put into this arbitrary
                    // NodeId location.
                    if is_std(&i.attrs) {
                        for item in m.items.iter() {
                            decl_destination_mod.insert(item.id.as_u32(), DUMMY_NODE_ID.as_u32());
                        }
                    }

                    if has_source_header(&i.attrs) {
                        for item in m.items.iter() {
                            match_modules(
                                &krate,
                                &item.id,
                                i.ident.into_string(),
                                &mut decl_destination_mod,
                                cx.session(),
                            );
                        }
                    }
                }
                _ => {}
            }
        });

        // Clean paths
        let krate = fold_nodes(krate, |mut p: Path| {
            if p.segments.len() > 1 {
                p.segments.retain(|s| {
                    !(s.ident.into_string() == "super" || s.ident.into_string() == "self")
                });
            }
            p
        });

        // `new_module_decls`:
        // NodeId -> vec<NodeId>
        // The mapping is the destination module's `NodeId` to the items needing to be added to it.
        let new_module_decls = clean_module_items(&krate, &decl_destination_mod, cx);

        // This is where the `old module` items get moved into the `new modules`
        let crate_copy = krate.clone();
        let krate = fold_nodes(krate, |pi: P<Item>| {
            if !matches!([pi.node] ItemKind::Mod(..)) {
                return SmallVector::one(pi);
            }

            SmallVector::one(pi.map(|i| {
                unpack!([i.node.clone()] ItemKind::Mod(m));
                let mut m = m;

                let new_items_option = new_module_decls.get(&i.id.as_u32());
                if let Some(new_item_ids) = new_items_option {
                    for new_item_id in new_item_ids.iter() {
                        let new_item_option = find_item(&crate_copy, new_item_id);

                        if let Some(new_item) = new_item_option {
                            m.items.push(P(new_item.clone()));
                        }
                    }
                }

                Item {
                    node: ItemKind::Mod(m),
                    ..i
                }
            }))
        });

        // insert a new module for the C standard headers
        let krate = fold_nodes(krate, |mut c: Crate| {
            let c_std_items_option = new_module_decls.get(&DUMMY_NODE_ID.as_u32());
            if let Some(c_std_items) = c_std_items_option {
                let items: Vec<P<Item>> = c_std_items
                    .iter()
                    .map(|id| P(find_item(&crate_copy, id).unwrap()))
                    .collect();

                let new_mod = Mod {
                    inner: DUMMY_SP,
                    items,
                };

                let new_item = Item {
                    ident: Ident::new("stdlib".into_symbol(), DUMMY_SP),
                    attrs: Vec::new(),
                    id: DUMMY_NODE_ID,
                    node: ItemKind::Mod(new_mod),
                    vis: dummy_spanned(VisibilityKind::Public),
                    span: DUMMY_SP,
                    tokens: None,
                };
                c.module.items.push(P(new_item));
            }
            c
        });

        let mut new_names = HashMap::new();
        for (old_item_id, dest_mod_id) in decl_destination_mod.iter() {
            let old_module = get_module(&krate, cx, &old_item_id).unwrap();
            let old_module_id = get_id(&krate, &old_module).unwrap();
            let old_item = find_item(&krate, &old_module_id.as_u32()).unwrap();

            let dest_item = find_item(&krate, &dest_mod_id).unwrap();

            new_names.insert(old_item.ident.into_string(), dest_item.ident.into_string());
        }

        // We need to truncate the path from being `use self::some_h::foo;`,
        // to be `use some_h::foo;`
        let krate = fold_nodes(krate, |mut p: Path| {
            for segment in &mut p.segments {
                let path_name = segment.ident.into_string();
                if let Some(new_path_segment) = new_names.get(&path_name) {
                    segment.ident = new_path_segment.to_string().into_ident();
                }
            }
            p
        });

        // This will remove all the translated up modules.
        let krate = fold_nodes(krate, |pi: P<Item>| {
            // Remove the module, if it has the specific attribute
            if has_source_header(&pi.attrs) || is_std(&pi.attrs) {
                return SmallVector::new();
            }
            SmallVector::one(pi)
        });

        let krate = purge_duplicates(krate);

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn purge_duplicates(krate: Crate) -> Crate {
    // This may be too aggressive of a removal,
    // but the initial thought is that all `extern`'s that should be kept are
    // ffi's from C standard libraries like: `malloc`
    let krate = fold_nodes(krate, |pi: P<Item>| {
        match pi.node.clone() {
            ItemKind::Mod(ref m) => {
                return SmallVector::one(pi.clone().map(|i| {
                    let mut m = m.clone();

                    if pi.ident.into_string() == "stdlib" {
                        return i;
                    }

                    // iterate through the module items and look for any foreign modules,
                    // if found remove.
                    m.items.retain(|item| {
                        let mut result = true;
                        if matches!([item.node] ItemKind::ForeignMod(..)) {
                            result = false;
                        }
                        result
                    });

                    Item {
                        node: ItemKind::Mod(m),
                        ..i
                    }
                }));
            }
            _ => {
                return SmallVector::one(pi);
            }
        }
    });

    // Remove any foreign item duplicates within the `stdlib` module
    let mut seen_item = HashSet::new();
    let krate = fold_nodes(krate, |mut fm: ForeignMod| {
        fm.items.retain(|item| {
            let mut result = true;

            if seen_item.contains(&item.ident.into_string()) {
                result = false;
            } else {
                seen_item.insert(item.ident.into_string());
            }
            result
        });
        fm
    });

    krate
}

// We should match possible modules together:
// test.rs should get the content of module test_h.
// So the hashmap should be something like "Test" => ModInfo { ..., "test_h"}
fn match_modules(
    krate: &Crate,
    old_mod_item_id: &NodeId,
    old_mod_name: String,
    decl_destination_mod: &mut HashMap<u32, u32>,
    sess: &Session,
) {
    visit_nodes(krate, |i: &Item| {
        match i.node {
            ItemKind::Mod(_) => {
                if !has_source_header(&i.attrs) {
                    let mut dest_mod_name = i.ident.into_string();

                    // The main crate module is an empty string,
                    // so just give it it's original name
                    if dest_mod_name.is_empty() {
                        dest_mod_name = get_source_file(sess);
                    }

                    // TODO: This is a simple naive heuristic,
                    // and should be improved upon.
                    if old_mod_name.contains(&dest_mod_name) {
                        decl_destination_mod.insert(old_mod_item_id.as_u32(), i.id.as_u32());
                    }
                }
            }
            _ => {}
        }
    });
}

// `clean_module_items` should iterate through decl_destination_mod, and if the Node has a similar `Item` within
// the destination module do not insert it into to the vector of NodeId's.
fn clean_module_items(
    krate: &Crate,
    decl_destination_mod: &HashMap<u32, u32>,
    cx: &driver::Ctxt,
) -> HashMap<u32, Vec<u32>> {
    let mut dest_items_map = HashMap::new();

    for (old_item_id, dest_mod_id) in decl_destination_mod {
        let mut dest_vec = Vec::new();

        let old_item_option = find_item(krate, old_item_id);
        let dest_mod_option = find_item(krate, dest_mod_id);

        if dest_mod_option.is_some() && old_item_option.is_some() {
            let dest_mod_ = dest_mod_option.unwrap();
            let old_item = old_item_option.unwrap();

            unpack!([dest_mod_.node.clone()] ItemKind::Mod(dest_mod));

            // if the Module alrady has the item, no need to insert it.
            // '''
            // // dest_mod
            // Mod {
            //    pub struct some_struct {
            //      pub a: i32,
            //    }
            // }
            //
            // //item
            // pub struct some_struct {
            //    pub a: i32
            // } // should not be inserted
            // '''
            //
            // Use statement duplicates are taken care of here as well.
            let mut is_match = false;
            for dest_item in dest_mod.items.iter() {
                if dest_item.node.ast_equiv(&old_item.node) {
                    is_match = true;
                }
            }

            if !is_match {
                dest_vec.push(old_item.id.as_u32());
            }
        } else if dest_mod_option.is_none() && old_item_option.is_some() {
            // This is for DUMMY_NODE_ID's
            let old_item = old_item_option.unwrap();
            dest_vec.push(old_item.id.as_u32());
        }

        if !dest_items_map.contains_key(dest_mod_id) {
            dest_items_map.insert(*dest_mod_id, dest_vec);
        } else {
            if let Some(v) = dest_items_map.get_mut(dest_mod_id) {
                v.append(&mut dest_vec);
            }
        }
    }
    remove_duplicates(krate, &mut dest_items_map, cx);
    dest_items_map
}

// Remove any items that are duplicated throughout the process.
fn remove_duplicates(
    krate: &Crate,
    decl_destination_mod: &mut HashMap<u32, Vec<u32>>,
    cx: &driver::Ctxt,
) {
    let mut cloned_map = decl_destination_mod.clone();

    for (dest_mod_id, possible_duplicate_items_ids) in decl_destination_mod.iter_mut() {
        possible_duplicate_items_ids.retain(|item_id| {
            let cloned_item_ids = cloned_map.get_mut(&dest_mod_id).unwrap();

            let mut result = true;
            let mut id_to_remove: Option<u32> = None;
            for cloned_item_id in cloned_item_ids.iter() {
                // Make sure we aren't comparing the same items
                if *item_id != *cloned_item_id {
                    let item_a = find_item(krate, &item_id).unwrap();
                    let item_b = find_item(krate, &cloned_item_id).unwrap();

                    // There tends to be some flakyness around the `ast_equiv`,
                    // specifically when structs have corresponding fields.
                    // TODO: Fix ast_equiv, `Token` and `Symbol` seem to be the culprits.
                    if item_a.node.ast_equiv(&item_b.node) {
                        result = false;
                        id_to_remove = Some(item_id.clone());
                    }
                }
            }
            if let Some(id) = id_to_remove {
                let index = cloned_item_ids.iter().position(|&i| i == id).unwrap();
                // Remove the item that is deemed as a duplicate.
                cloned_item_ids.remove(index);
            }

            result
        });
    }
}

fn get_source_file(sess: &Session) -> String {
    let s = sess.local_crate_source_file.as_ref().cloned();
    s.unwrap().to_str().unwrap().to_string()
}

// This function is a check to ensure that the modules, we remove are ones translated.
// What this function is looking for is the ident, 'source_header'.
// Every translated file, that were translated with the correct option, should have:
// `#[cfg(not(source_header = "/some/path"))]`
fn has_source_header(attrs: &Vec<Attribute>) -> bool {
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
            }
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

fn is_std(attrs: &Vec<Attribute>) -> bool {
    // Recurse down the `TokenTree` till the `Token` is reached,
    // if the token contains an Ident with `source_tree`, this should be a translated
    // `old module` then.
    fn parse_token_tree(tree: &TokenTree, is_std: &mut bool) {
        match tree {
            TokenTree::Delimited(_, delimited) => {
                let stream = delimited.stream();
                stream.map(|tree| {
                    parse_token_tree(&tree, is_std);
                    tree
                });
            }
            TokenTree::Token(_, token) => match token {
                Literal(lit, _) => match lit {
                    Str_(name) => {
                        let path = name.into_string();
                        if path.contains("/usr/include") || path.contains("stddef") {
                            *is_std = true;
                        }
                    }
                    _ => {}
                },
                _ => {}
            },
        }
    }

    let mut is_std = false;
    for attr in attrs {
        let tokens = attr.tokens.clone();
        tokens.map(|tree| {
            parse_token_tree(&tree, &mut is_std);
            tree
        });
    }
    is_std
}

// Iterate through the crate, and look for the specified Node
// from the NodeId
fn find_item(krate: &Crate, id: &u32) -> Option<Item> {
    let mut some_i = Vec::new();
    visit_nodes(krate, |i: &Item| {
        if i.id == NodeId::from_u32(*id) {
            some_i.push(i.clone());
        }
    });

    if some_i.len() > 0 {
        let i = some_i.first().unwrap();
        return Some(i.clone());
    }
    None
}

// Get the `parent` module of the item.
fn get_module(krate: &Crate, cx: &driver::Ctxt, id: &u32) -> Option<Mod> {
    if NodeId::from_u32(*id) == DUMMY_NODE_ID {
        return None;
    }

    let parent_id = cx.hir_map().get_module_parent(NodeId::from_u32(*id));
    let item_id = cx.hir_map().as_local_node_id(parent_id).unwrap();

    let item_option = find_item(krate, &item_id.as_u32());
    if let Some(item) = item_option {
        if matches!([item.node.clone()] ItemKind::Mod(_)) {
            unpack!([item.node.clone()] ItemKind::Mod(m));
            return Some(m);
        }
        return None;
    }
    None
}

// Get the Id of a module, it may be benificial to do this with
// all `ItemKind`'s, as seeing that they do not include the NodeId
fn get_id(krate: &Crate, module: &Mod) -> Option<NodeId> {
    let mut node = NodeId::new(0);
    visit_nodes(krate, |i: &Item| match i.node {
        ItemKind::Mod(ref m) => {
            if m.ast_equiv(module) {
                node = i.id.clone();
            }
        }
        _ => {}
    });

    if node.as_u32() > 0 {
        return Some(node);
    }
    None
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reorganize_modules", |_args| mk(ReorganizeModules))
}
