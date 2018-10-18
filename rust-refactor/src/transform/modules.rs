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
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let stdlib_id = st.next_node_id();
        let mut item_map = HashMap::new();

        // Cleanse the paths of the super or self prefix.
        let krate = fold_nodes(krate, |mut p: Path| {
            if p.segments.len() > 1 {
                p.segments.retain(|s| {
                    !(s.ident.into_string() == "super" || s.ident.into_string() == "self")
                });
            }
            p
        });

        // Match the modules, using a mapping like:
        // NodeId -> NodeId
        // The key is the id of the old item to be moved, and the value is the NodeId of the module
        // the item will be moved to.
        // TODO: Try and utilize the Visit trait, instead of using a visit_node
        let mut decl_destination_mod = HashMap::new();
        let mut new_names = HashMap::new();
        visit_nodes(&krate, |i: &Item| {
            match i.node {
                // TODO: Move this into it's own function which accepts an Item and returns an
                // Optional decl_destination_mod
                ItemKind::Mod(ref m) => {
                    // All C standard library headers are going to be put into this arbitrary
                    // NodeId location.
                    if is_std(&i.attrs) {
                        for item in m.items.iter() {
                            decl_destination_mod.insert(item.id, stdlib_id);
                        }
                        new_names.insert(i.ident.into_string(), "stdlib".to_string());
                    }

                    if has_source_header(&i.attrs) {
                        for item in m.items.iter() {
                            match_modules(
                                &krate,
                                &item.id,
                                i.ident.into_string(),
                                &mut decl_destination_mod,
                                &mut new_names,
                                cx.session(),
                            );
                        }
                    }
                }
                _ => {}
            }
            item_map.insert(i.id, i.clone());
        });

        // `new_module_decls`:
        // NodeId -> vec<NodeId>
        // The mapping is the destination module's `NodeId` to the items needing to be added to it.
        let new_module_decls = clean_module_items(&krate, &decl_destination_mod, &item_map, &cx);

        // This is where the `old module` items get moved into the `new modules`
        let crate_copy = krate.clone();
        let krate = fold_nodes(krate, |pi: P<Item>| match pi.node.clone() {
            ItemKind::Mod(ref m) => {
                return SmallVector::one(pi.map(|i| {
                    let mut m = m.clone();

                    if let Some(new_item_ids) = new_module_decls.get(&i.id) {
                        for new_item_id in new_item_ids.iter() {
                            if let Some(new_item) = item_map.get(new_item_id) {
                                m.items.push(P(new_item.clone()));
                            }
                        }
                    }

                    Item {
                        node: ItemKind::Mod(m),
                        ..i
                    }
                }));
            }
            _ => {
                return SmallVector::one(pi);
            }
        });

        // insert a new module for the C standard headers
        let krate = extend_crate(krate, &new_module_decls, &item_map, &stdlib_id);

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

fn extend_crate(
    krate: Crate,
    new_module_decls: &HashMap<NodeId, Vec<NodeId>>,
    item_map: &HashMap<NodeId, Item>,
    stdlib_id: &NodeId,
) -> Crate {
    if let Some(c_std_items) = new_module_decls.get(&stdlib_id) {
        let items: Vec<P<Item>> = c_std_items
            .iter()
            .map(|id| P(item_map.get(id).unwrap().clone()))
            .collect();

        let stdlib_mod = Mod {
            inner: DUMMY_SP,
            items,
        };

        let new_item = Item {
            ident: Ident::new("stdlib".into_symbol(), DUMMY_SP),
            attrs: Vec::new(),
            id: *stdlib_id,
            node: ItemKind::Mod(stdlib_mod),
            vis: dummy_spanned(VisibilityKind::Public),
            span: DUMMY_SP,
            tokens: None,
        };

        let mut krate_mod = krate.module.clone();

        krate_mod.items.push(P(new_item));
        return Crate {
            module: krate_mod,
            ..krate
        };
    }
    krate
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

    // TODO: Since we move the content of an module out into a destination module,
    // that destination module may contain a `use` statement that allowed the use of the `to move`
    // module item. If this is the case the use statement needs to be removed.
    //
    // ```
    // pub mod buffer {
    //     use buffer::buffer_t;
    //     ...
    //     pub struct buffer_t; // moved from mod buffer_h
    // }
    // ```
    let krate = fold_nodes(krate, |pi: P<Item>| match pi.node.clone() {
        ItemKind::Mod(ref m) => {
            return SmallVector::one(pi.map(|item| {
                let mut m = m.clone();
                let cloned_items = m.items.clone();
                m.items.retain(|i| {
                    let mut result = true;
                    match i.node {
                        ItemKind::Use(ref usetree) => {
                            for cloned_item in cloned_items.iter() {
                                match cloned_item.node {
                                    ItemKind::Ty(..) | ItemKind::Fn(..) | ItemKind::Struct(..) => {
                                        let item_declaration = cloned_item.ident.into_string();
                                        let path_segments = usetree.prefix.segments.clone();
                                        if path_segments
                                            .iter()
                                            .any(|s| s.ident.into_string() == item_declaration)
                                        {
                                            result = false;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => {}
                    }
                    result
                });
                Item {
                    node: ItemKind::Mod(m),
                    ..item
                }
            }));
        }
        _ => {
            return SmallVector::one(pi);
        }
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
    decl_destination_mod: &mut HashMap<NodeId, NodeId>,
    new_names: &mut HashMap<String, String>,
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
                        decl_destination_mod.insert(*old_mod_item_id, i.id);
                        new_names.insert(old_mod_name.clone(), dest_mod_name);
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
    decl_destination_mod: &HashMap<NodeId, NodeId>,
    item_map: &HashMap<NodeId, Item>,
    cx: &driver::Ctxt,
) -> HashMap<NodeId, Vec<NodeId>> {
    let mut dest_items_map = HashMap::new();

    for (old_item_id, dest_mod_id) in decl_destination_mod {
        let mut dest_vec = Vec::new();

        // TODO: figure out why using item_map here fails.
        let old_item_option = item_map.get(old_item_id);
        let dest_mod_option = item_map.get(dest_mod_id);

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
                dest_vec.push(old_item.id);
            }
        } else if dest_mod_option.is_none() && old_item_option.is_some() {
            // This is for DUMMY_NODE_ID's
            let old_item = old_item_option.unwrap();
            dest_vec.push(old_item.id);
        }

        if !dest_items_map.contains_key(dest_mod_id) {
            dest_items_map.insert(*dest_mod_id, dest_vec);
        } else {
            if let Some(v) = dest_items_map.get_mut(dest_mod_id) {
                v.append(&mut dest_vec);
            }
        }
    }
    remove_duplicates(krate, &mut dest_items_map, &item_map, cx);
    dest_items_map
}

// Remove any items that are duplicated throughout the process.
fn remove_duplicates(
    krate: &Crate,
    decl_destination_mod: &mut HashMap<NodeId, Vec<NodeId>>,
    item_map: &HashMap<NodeId, Item>,
    cx: &driver::Ctxt,
) {
    let mut cloned_map = decl_destination_mod.clone();

    for (dest_mod_id, possible_duplicate_items_ids) in decl_destination_mod.iter_mut() {
        possible_duplicate_items_ids.retain(|item_id| {
            let cloned_item_ids = cloned_map.get_mut(&dest_mod_id).unwrap();

            let mut result = true;
            let mut id_to_remove: Option<NodeId> = None;
            for cloned_item_id in cloned_item_ids.iter() {
                // Make sure we aren't comparing the same items
                if *item_id != *cloned_item_id {
                    let item_a = item_map.get(&item_id).unwrap();
                    let item_b = item_map.get(&cloned_item_id).unwrap();

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

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reorganize_modules", |_args| mk(ReorganizeModules))
}
