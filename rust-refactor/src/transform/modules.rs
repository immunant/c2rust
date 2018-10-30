///! This is a transform for reorganizing definitions from a translated c2rust project.
///!
///! The main goal of this transform is unpollute the translated library from redefinitions.
///! What the c2rust transpiler does, is redefine every declaration from a header wherever that
///! header is included. Like so:
///! ```
///! mod buffer {
///!     struct buffer_t {
///!        data: i32,
///!     }
///! }
///! mod foo {
///!     struct buffer_t {
///!        data: i32,
///!     }
///! }
///! ```
///! ...
use rustc::session::Session;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};
use syntax::ast::*;
use syntax::parse::token::Lit::Str_;
use syntax::parse::token::Token::Literal;
use syntax::ptr::P;
use syntax::source_map::symbol::Symbol;
use syntax::source_map::{dummy_spanned, SyntaxContext, DUMMY_SP};
use syntax::symbol::keywords;
use syntax::tokenstream::*;
use syntax::visit::{self, Visitor};
use transform::Transform;

use api::*;
use ast_manip::AstEquiv;
use command::{CommandState, Registry};
use driver::{self, Phase};

pub struct ReorganizeModules;

pub struct ModuleInformation<'a, 'tcx: 'a, 'st> {
    // Mapping for fast item lookup, stops the need of having to search the entire Crate.
    item_map: HashMap<NodeId, Item>,

    // Maps a *to be moved Item to the destination module id
    // * meaning items that pass the `is_std` and `has_source_header` check
    item_to_dest_module: HashMap<NodeId, NodeId>,

    // This is used for mapping modules that need to be created to a new node id
    // e.g.: "stdlib" -> `NodeId`
    new_modules: HashMap<Ident, NodeId>,

    // Set of module `NodeId`'s where "old" module items will be sent to
    possible_destination_modules: HashSet<NodeId>,

    // Old path NodeId -> (New Path, Destination module id)
    path_mapping: HashMap<NodeId, (Path, NodeId)>,

    cx: &'a driver::Ctxt<'a, 'tcx>,
    st: &'st CommandState,
}

impl<'a, 'tcx, 'st> ModuleInformation<'a, 'tcx, 'st> {
    fn new(cx: &'a driver::Ctxt<'a, 'tcx>, st: &'st CommandState) -> Self {
        let mut new_modules = HashMap::new();
        new_modules.insert(Ident::from_str("stdlib"), st.next_node_id());
        ModuleInformation {
            item_map: HashMap::new(),
            item_to_dest_module: HashMap::new(),
            new_modules,
            path_mapping: HashMap::new(),
            possible_destination_modules: HashSet::new(),
            cx,
            st,
        }
    }

    fn find_destination_modules(&mut self, krate: &Crate) {
        // visit all the modules, and find potential destination module canidates
        // also build up the item map here
        //
        // TODO: move this into its own method
        // possible names:
        // find_destination_modules?
        visit_nodes(krate, |i: &Item| {
            match i.node {
                ItemKind::Mod(_) => {
                    if !has_source_header(&i.attrs) && !is_std(&i.attrs) {
                        self.possible_destination_modules.insert(i.id);
                    }
                }
                ItemKind::Use(ref ut) => {
                    // Don't insert any "dummy" spanned use statements
                    if i.span.ctxt() == SyntaxContext::empty() {
                        let mut prefix = ut.prefix.clone();

                        if prefix.segments.len() > 1 {
                            prefix.segments.retain(|segment| {
                                segment.ident.name != keywords::Super.name()
                                    && segment.ident.name != keywords::SelfValue.name()
                            });
                        }
                        self.path_mapping.insert(i.id, (prefix, DUMMY_NODE_ID));
                    }
                }
                _ => {}
            }
            self.item_map.insert(i.id, i.clone());
        });
    }

    // We should match possible modules together:
    // test.rs should get the content of module test_h.
    // So the hashmap should be something like "Test" => ModInfo { ..., "test_h"}
    //
    // TODO: Better variable naming; naming is too confusing.
    fn find_destination_id(
        &mut self,
        item_to_process: &NodeId,
        old_module: &Item, // Parent of `item_to_process`
    ) -> (NodeId, Ident) {
        // `old_mod` is an `Item` type
        let mut node_id = DUMMY_NODE_ID;
        let mut ident = Ident::from_str("");
        let mut found = false;

        // iterate through the set of possible destinations and try to find a possible match
        for dest_module_id in self.possible_destination_modules.iter() {
            if let Some(dest_module) = self.item_map.get(dest_module_id) {
                let mut dest_module_ident = dest_module.ident;

                if dest_module_ident.as_str().is_empty() {
                    dest_module_ident = Ident::from_str(&get_source_file(self.cx.session()));
                }

                // TODO: This is a simple naive heuristic,
                // and should be improved upon.
                if old_module
                    .ident
                    .as_str()
                    .contains(&*dest_module_ident.as_str())
                {
                    node_id = dest_module.id;
                    ident = dest_module_ident;
                    found = true;
                }
            }
        }

        if !self.item_to_dest_module.contains_key(item_to_process) && !found {
            let new_modules = self.new_modules.clone();
            if let Some(mod_id) = new_modules.get(&old_module.ident) {
                node_id = *mod_id;
                ident = old_module.ident;
            } else {
                let new_id = self.st.next_node_id();
                self.new_modules.insert(old_module.ident, new_id);
                node_id = new_id;
                ident = old_module.ident;
            }
        }
        if is_std(&old_module.attrs) {
            node_id = *self.new_modules.get(&Ident::from_str("stdlib")).unwrap();
            ident = Ident::from_str("stdlib");
        }
        (node_id, ident)
    }

    // `clean_module_items` should iterate through item_to_dest_module, and if the Node has a similar `Item` within
    // the destination module do not insert it into to the vector of NodeId's.
    fn create_dest_mod_map(&self) -> HashMap<NodeId, Vec<NodeId>> {
        let mut dest_mod_to_items: HashMap<NodeId, Vec<NodeId>> = HashMap::new();
        for (item_id, dest_mod_id) in self.item_to_dest_module.iter() {
            if let Some(vec_of_items) = dest_mod_to_items.get_mut(&dest_mod_id) {
                vec_of_items.push(*item_id);
            }

            if !dest_mod_to_items.contains_key(&dest_mod_id) {
                dest_mod_to_items.insert(*dest_mod_id, vec![*item_id]);
            }
        }
        dest_mod_to_items
    }

    fn extend_crate(
        &mut self,
        krate: Crate,
        dest_mod_to_items: &HashMap<NodeId, Vec<NodeId>>,
    ) -> Crate {
        let mut krate = krate;
        // inverse new_modules, so we can look up the ident by id
        let inverse_map = self
            .new_modules
            .iter()
            .map(|(ident, id)| (id.clone(), ident.clone()))
            .collect::<HashMap<_, _>>();

        for (dest_mod_id, vec_of_ids) in dest_mod_to_items.iter() {
            let items: Vec<P<Item>> = vec_of_ids
                .iter()
                .map(|id| P(self.item_map.get(id).unwrap().clone()))
                .collect();

            let new_mod = Mod {
                inner: DUMMY_SP,
                items,
                inline: true,
            };

            if let Some(ident) = inverse_map.get(dest_mod_id) {
                let sym = Symbol::intern(&ident.as_str());

                let new_item = Item {
                    ident: Ident::new(sym, DUMMY_SP),
                    attrs: Vec::new(),
                    id: *dest_mod_id,
                    node: ItemKind::Mod(new_mod),
                    vis: dummy_spanned(VisibilityKind::Public),
                    span: DUMMY_SP,
                    tokens: None,
                };

                let mut krate_mod = krate.module.clone();
                krate_mod.items.push(P(new_item));

                krate = Crate {
                    module: krate_mod,
                    ..krate
                };
            }
        }

        // Now that the items have
        self.item_map.clear();
        let krate = fold_nodes(krate, |pi: P<Item>| {
            if has_source_header(&pi.attrs) || is_std(&pi.attrs) {
                return SmallVec::new();
            }

            self.item_map.insert(pi.id, pi.clone().into_inner());
            let mut v = smallvec![];
            v.push(pi);
            v
        });

        krate
    }
}

impl<'ast, 'a, 'tcx, 'st> Visitor<'ast> for ModuleInformation<'a, 'tcx, 'st> {
    // Match the modules, using a mapping like:
    // NodeId -> NodeId
    // The key is the id of the old item to be moved, and the value is the NodeId of the module
    // the item will be moved to.
    fn visit_item(&mut self, old_module: &'ast Item) {
        match old_module.node {
            ItemKind::Mod(ref m) => {
                // All C standard library headers are going to be put into this arbitrary
                // NodeId location.
                for module_item in m.items.iter() {
                    // TODO:
                    // * Update path_mapping so we get the dest_module_id
                    let (dest_module_id, ident) =
                        self.find_destination_id(&module_item.id, &old_module);
                    self.item_to_dest_module
                        .insert(module_item.id, dest_module_id);

                    // Update the path_mapping to have the respective dest module id and the new
                    // path.
                    // TODO: If the dest module and path are the same what should happen?
                    for (path, dummy_node_id) in self.path_mapping.values_mut() {
                        for segment in &mut path.segments {
                            // Check to see if a segment within the path is getting moved.
                            // example_h -> example
                            // DUMMY_NODE_ID -> actual destination module id
                            //
                            // TODO: put the whole match for paths here from new,
                            // I can insert into path_mapping here.
                            if segment.ident == old_module.ident {
                                segment.ident = ident;
                                *dummy_node_id = dest_module_id;
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        visit::walk_item(self, old_module);
    }
}

impl Transform for ReorganizeModules {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let mut mod_info = ModuleInformation::new(cx, st);

        mod_info.find_destination_modules(&krate);

        krate.visit(&mut mod_info);

        // `dest_mod_to_items`:
        // NodeId -> vec<NodeId>
        // The mapping is the destination module's `NodeId` to the items needing to be added to it.
        let dest_mod_to_items = mod_info.create_dest_mod_map();

        // insert a new module for the C standard headers
        let krate = mod_info.extend_crate(krate, &dest_mod_to_items);


        // This is where the "old module" items get moved into the "new modules"
        // The items left in dest_mod_to_items are the `brand new` modules going to be inserted into
        // the crate, like `stdlib`.
        let krate = fold_nodes(krate, |pi: P<Item>| {
            let mut v = smallvec![];
            match pi.node.clone() {
                ItemKind::Mod(ref m) => {
                    let i = pi.map(|i| {
                        let mut m = m.clone();

                        if let Some(new_item_ids) = dest_mod_to_items.get(&i.id) {
                            for new_item_id in new_item_ids.iter() {
                                if let Some(mut new_item) = mod_info.item_map.get_mut(new_item_id) {
                                    let mut found = false;
                                    for item in m.items.iter() {
                                        if compare_items(&new_item, &item) {
                                            found = true;
                                        }

                                        if let ItemKind::ForeignMod(ref mut fm) = new_item.node {
                                            fm.items.retain(|fm_item| {
                                                let mut result = true;
                                                if fm_item.ident == item.ident {
                                                    result = false;
                                                }
                                                result
                                            });
                                        }
                                    }

                                    if !found {
                                        m.items.push(P(new_item.clone()));
                                    }
                                }
                            }
                        }

                        // TODO:
                        // turn these two iterations into one, using filter_map?
                        // file:///home/miguelsaldivar/workspace/rust/build/x86_64-unknown-linux-gnu/doc/std/iter/trait.Iterator.html#method.filter_map
                        m.items.retain(|m_item| {
                            let mut result = true;
                            // If the use statement is in the module we are in, and the items of the path
                            // are getting inserted into the module. Just remove the path.
                            if let Some((_, dest_module_id)) = mod_info.path_mapping.get(&m_item.id) {
                                if i.id == *dest_module_id {
                                    result = false;
                                }
                            }
                            result
                        });

                        // Update each path to have the new path
                        for module_item in m.items.iter_mut() {
                            let m_id = module_item.id.clone();
                            if let ItemKind::Use(ref mut ut) = module_item.node {
                                if let Some((new_path, _)) = mod_info.path_mapping.get(&m_id) {
                                    ut.prefix = new_path.clone();
                                }
                            }
                        }

                        let seen_item_ids =
                            m.items.iter().map(|item| item.id).collect::<HashSet<_>>();

                        let mut deleted_item_ids = HashSet::new();
                        let mut seen_paths: HashMap<Ident, HashSet<Ident>> = HashMap::new();

                        m.items.retain(|m_item| {
                            let mut result = true;
                            for item_id in &seen_item_ids {
                                if let Some(item) = mod_info.item_map.get(&item_id) {
                                    if item.id != m_item.id {
                                        // Items that are the "same" are deleted here
                                        if compare_items(&item, &m_item) && ! deleted_item_ids.contains(&item.id) {
                                            deleted_item_ids.insert(m_item.id);
                                            result = false;
                                        }
                                    }
                                }
                            }

                            // In some modules there are multiple nested use statements that may
                            // import differing prefixes, but also duplicate prefixes. So what
                            // happens here is if there is a nested use statement:
                            // 1. insert all the prefixes in a set
                            // 2. If the module name is already in seen_paths, create a union of
                            //    the existing set with the set of prefixes we just created and
                            //    override.
                            //    Else just insert that set into the map.
                            //    [foo_h] -> [item, item2, item3]
                            //  3. delete the nested use statement.
                            match m_item.node {
                                ItemKind::Use(ref ut) => {
                                    match ut.kind {
                                        UseTreeKind::Nested(ref use_trees) => {
                                            let mut prefixes = HashSet::new();
                                            for (use_tree, _) in use_trees {
                                                prefixes.insert(path_to_ident(&use_tree.prefix));
                                            }
                                            if let Some(set_of_prefixes) = seen_paths.get_mut(&path_to_ident(&ut.prefix)) {
                                                let union: HashSet<Ident> = set_of_prefixes.union(&prefixes).cloned().collect();
                                                *set_of_prefixes = union;
                                            }
                                            if !seen_paths.contains_key(&path_to_ident(&ut.prefix)) {
                                                seen_paths.insert(path_to_ident(&ut.prefix), prefixes);
                                            }
                                            result = false;
                                        },
                                        UseTreeKind::Simple(..) => {
                                            if ut.prefix.segments.len() > 1 {
                                                let mod_name = ut.prefix.segments.first().unwrap();
                                                let prefix = ut.prefix.segments.last().unwrap();

                                                if let Some(set_of_prefixes) = seen_paths.get_mut(&mod_name.ident) {
                                                    set_of_prefixes.insert(prefix.ident);
                                                }
                                                if !seen_paths.contains_key(&mod_name.ident) {
                                                    let mut prefixes = HashSet::new();
                                                    prefixes.insert(prefix.ident);
                                                    seen_paths.insert(mod_name.ident, prefixes);
                                                }
                                                result = false;
                                            }
                                        },
                                        _ => {}
                                    }
                                },
                                _ => {}
                            }

                            result
                        });

                        // Here is where the seen_paths map is read, and turned into paths
                        // [foo_h] -> [item, item2, item3] turns into `use foo_h::{item, item2, item3};`
                        // And that ast is pushed into the module
                        let item_idents =
                            m.items.iter().map(|item| item.ident).collect::<HashSet<_>>();
                        for (mod_name, mut prefixes) in seen_paths.iter_mut() {
                            let mut items: Vec<Ident> = prefixes.iter().map(|i| i).cloned().collect();
                            let mod_prefix = Path::from_ident(*mod_name);
                            prefixes.retain(|prefix| {
                                let mut result = true;
                                if item_idents.contains(&prefix) {
                                    result = false;
                                }
                                result
                            });
                            let use_stmt = mk().use_multiple_item(mod_prefix, items);
                            m.items.push(use_stmt);
                        }

                        // TODO:
                        // This is hacky, and more often than not, probably not necessary.
                        //      This should be removed then.
                        for module_item in m.items.iter_mut() {
                            for item_id in &seen_item_ids {
                                if let Some(item) = mod_info.item_map.get(&item_id) {
                                    if item.id != module_item.id && !deleted_item_ids.contains(&item_id) {
                                        if let ItemKind::ForeignMod(ref mut fm1) = module_item.node {
                                            if let ItemKind::ForeignMod(ref fm2) = item.node {
                                                fm1.items.retain(|fm_item| {
                                                    let mut result = true;
                                                    for fm2_item in fm2.items.iter() {
                                                        if fm_item.node.ast_equiv(&fm2_item.node) && !deleted_item_ids.contains(&fm2_item.id)
                                                            && fm_item.ident == fm2_item.ident {
                                                            deleted_item_ids.insert(fm_item.id);
                                                            result = false;
                                                        }
                                                    }
                                                    result
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Item {
                            node: ItemKind::Mod(m),
                            ..i
                        }
                    });
                    v.push(i);
                    return v;
                }
                _ => {
                    v.push(pi);
                    return v;
                }
            }
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn get_source_file(sess: &Session) -> String {
    let s = sess.local_crate_source_file.as_ref().cloned();
    s.unwrap().to_str().unwrap().to_string()
}

fn path_to_ident(path: &Path) -> Ident {
    Ident::from_str(&path.to_string())
}

// Compares an item, in mulitiple ways to see if they are the same
// TODO:
// * Check to see where exactly ast_equiv fails.
fn compare_items(new_item: &Item, module_item: &Item) -> bool {
    if new_item.node.ast_equiv(&module_item.node) && new_item.ident == module_item.ident {
        return true;
    }

    // The next two upper level if statements are a check for constant and type alias'.
    // So the renamer seems to give all unnamed types the variable name `unnamed`, and tacks on a
    // `_N` where N is the number of unnamed variables in the module/scope.
    //
    // So there are times where when moving items into modules where there are two of the same
    // type, but with differing names.
    // E.g:
    // ```
    // pub type Foo: unnamed = 0;
    // pub type Foo: unnamed_0 = 0;
    // ```
    // And both unnamed and unnamed_0 are both of type `libc::uint;`, so one of these `Foo`'s must
    // be removed.
    // TODO:
    // * Assure that these two items are in fact of the same type, just to be safe.
    if let ItemKind::Ty(_, _) = new_item.node {
        if let ItemKind::Ty(_, _) = module_item.node {
            if new_item.ident == module_item.ident {
                return true;
            }
        }
    }

    if let ItemKind::Const(_, _) = new_item.node {
        if let ItemKind::Const(_, _) = module_item.node {
            if new_item.ident == module_item.ident {
                return true;
            }
        }
    }

    if let ItemKind::Use(ref new) = new_item.node {
        if let ItemKind::Use(ref mo) = module_item.node {
            let mut new_copy = new.clone();
            let mut mo_copy = mo.clone();
            new_copy.prefix.segments.retain(|segment| {
                segment.ident.name != keywords::Super.name()
                    && segment.ident.name != keywords::SelfValue.name()
            });

            mo_copy.prefix.segments.retain(|segment| {
                segment.ident.name != keywords::Super.name()
                    && segment.ident.name != keywords::SelfValue.name()
            });

            if new_copy.ast_equiv(&mo_copy) {
                return true;
            }
        }
    }
    false
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
                    if ident.as_str().contains("source_header") {
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
                        if name.as_str().contains("/usr/include")
                            || name.as_str().contains("stddef")
                            || name.as_str().contains("vararg")
                        {
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
