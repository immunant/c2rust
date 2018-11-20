use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};
use syntax::ast::*;
use syntax::attr;
use syntax::ptr::P;
use syntax::symbol::keywords;
use syntax::visit::{self, Visitor};
use transform::Transform;
use indexmap::{IndexSet};

use api::*;
use ast_manip::AstEquiv;
use command::{CommandState, Registry};
use driver::{self, Phase};

/// This is a transform for reorganizing definitions from a translated c2rust project.
///
/// The main goal of this transform is unpollute the translated project from redefinitions.
/// Essentially what will happen is there will be a project like:
/// ```
/// mod buffer {
///     #[header_src="/some/path/buffer.h"]
///     mod buffer_h {
///         struct buffer_t {
///            data: i32,
///         }
///     }
/// }
/// ```
/// This will then turn into:
/// ```
/// mod buffer {
///     struct buffer_t {
///        data: i32,
///     }
/// }
/// ```
pub struct ReorganizeModules;

/// Holds the information of the current `Crate`, which includes a `HashMap` to look up Items
/// quickly, as well as other members that hold important information.
pub struct CrateInformation<'st> {
    /// Mapping for fast item lookup, stops the need of having to search the entire Crate.
    item_map: HashMap<NodeId, Item>,

    /// Maps a header declaration item id to a new destination module id.
    item_to_dest_module: HashMap<NodeId, NodeId>,

    /// This is used for mapping modules that need to be created to a new node id
    /// e.g.: "stdlib" -> `NodeId`
    new_modules: HashMap<Ident, NodeId>,

    /// Set of module `NodeId`'s where "old" module items will be sent to
    possible_destination_modules: HashSet<NodeId>,

    /// Old path NodeId -> (New Path, Destination module id)
    path_mapping: HashMap<NodeId, (Path, NodeId)>,

    /// Helper, to expedite the look up of paths
    path_ids: HashSet<NodeId>,

    st: &'st CommandState,
}

impl<'st> CrateInformation<'st> {
    fn new(st: &'st CommandState) -> Self {
        let mut new_modules = HashMap::new();
        new_modules.insert(Ident::from_str("stdlib"), st.next_node_id());
        CrateInformation {
            item_map: HashMap::new(),
            item_to_dest_module: HashMap::new(),
            new_modules,
            path_mapping: HashMap::new(),
            path_ids: HashSet::new(),
            possible_destination_modules: HashSet::new(),
            st,
        }
    }

    /// Iterates through the Crate, to find any potentential "destination modules",
    /// if one is found it is inserted into `possible_destination_modules`.
    /// Also since we iterate through the items, it is a good place to insert everything
    /// into `item_map`.
    fn find_destination_modules(&mut self, krate: &Crate) {
        // visit all the modules, and find potential destination module canidates
        // also build up the item map here
        visit_nodes(krate, |i: &Item| {
            match i.node {
                ItemKind::Mod(_) => {
                    if !has_source_header(&i.attrs) && !is_std(&i.attrs) {
                        self.possible_destination_modules.insert(i.id);
                    }
                },
                ItemKind::Use(_) => {
                    self.path_ids.insert(i.id);
                },
                _ => {}
            }
            self.item_map.insert(i.id, i.clone());
        });
    }

    /// In this function we try to match an item to a destination module,
    /// once we have a match, the NodeId and the Ident of the module is returned.
    fn find_destination_id(
        &mut self,
        item_to_process: &NodeId,
        old_module: &Item, // Parent of `item_to_process`
    ) -> (NodeId, Ident) {
        if is_std(&old_module.attrs) {
            let node_id = *self.new_modules.get(&Ident::from_str("stdlib")).unwrap();
            let ident = Ident::from_str("stdlib");
            return (node_id, ident);
        }

        // iterate through the set of possible destinations and try to find a possible match
        for dest_module_id in self.possible_destination_modules.iter() {
            if let Some(dest_module) = self.item_map.get(dest_module_id) {
                let mut dest_module_ident = dest_module.ident;

                // TODO: This is a simple naive heuristic,
                // and should be improved upon.
                if old_module
                    .ident
                    .as_str()
                    .contains(&*dest_module_ident.as_str())
                {
                    let node_id = dest_module.id;
                    let ident = dest_module_ident;
                    return (node_id, ident);
                }
            }
        }

        assert!(!self.item_to_dest_module.contains_key(item_to_process));
        let new_modules = &mut self.new_modules;
        let state = &self.st;
        let node_id = *new_modules
            .entry(old_module.ident)
            .or_insert_with(|| state.next_node_id());
        let ident = old_module.ident;
        (node_id, ident)
    }

    /// Iterates through `item_to_dest_mod`, and creates a reverse mapping of that HashMap
    /// `dest_node_id` -> `Vec<items_to_get_inserted>`
    fn create_dest_mod_map(&self) -> HashMap<NodeId, IndexSet<NodeId>> {
        let mut dest_mod_to_items: HashMap<NodeId, IndexSet<NodeId>> = HashMap::new();
        for (item_id, dest_mod_id) in self.item_to_dest_module.iter() {
            dest_mod_to_items.entry(*dest_mod_id).or_insert_with(IndexSet::new).insert(*item_id);
        }
        dest_mod_to_items
    }

    /// This function creates a `Mod` with its previous `Item`'s and inserts it into the
    /// `Crate`.
    fn extend_crate(
        &mut self,
        krate: Crate,
        dest_mod_to_items: &HashMap<NodeId, IndexSet<NodeId>>,
    ) -> Crate {
        let mut krate = krate;
        // inverse new_modules, so we can look up the ident by id
        let inverse_map = self
            .new_modules
            .iter()
            .map(|(ident, id)| (id, ident.clone()))
            .collect::<HashMap<_, _>>();

        // insert the "new modules" into the crate
        for (dest_mod_id, vec_of_ids) in dest_mod_to_items.iter() {
            let items: Vec<P<Item>> = vec_of_ids
                .iter()
                .map(|id| P(self.item_map.get(id).unwrap().clone()))
                .collect();

            if let Some(ident) = inverse_map.get(dest_mod_id) {
                let new_item = mk().id(*dest_mod_id).mod_item(ident, mk().mod_(items));
                krate.module.items.push(new_item);
            }
        }
        krate
    }

    /// Inserts `Item`'s into a previously existing `Mod`
    fn insert_items_into_dest(
        &mut self,
        krate: Crate,
        dest_mod_to_items: &HashMap<NodeId, IndexSet<NodeId>>,
    ) -> Crate {
        // This is where items get inserted into the corresponding
        // "destination module"
        self.item_map.clear();
        let krate = fold_nodes(krate, |pi: P<Item>| {
            if has_source_header(&pi.attrs) || is_std(&pi.attrs) {
                return SmallVec::new();
            }

            let pi = pi.map(|mut i| {
                i.node = match i.node {
                    ItemKind::Mod(mut m) => {
                        let new_item_ids = match dest_mod_to_items.get(&i.id) {
                            Some(x) => x,
                            None => {
                                return Item {
                                    node: ItemKind::Mod(m),
                                    ..i
                                };
                            },
                        };

                        let old_items: HashSet<Ident> = m.items.iter().map(|item| item.ident)
                            .collect::<HashSet<_>>();

                        for new_item_id in new_item_ids.iter() {
                            // `get_mut` is used to remove duplicates from incoming
                            // `ForeignMod`'s
                            // TODO:
                            //  * Utilize `remove` as opposed to `get`, so  there won't be a need for
                            // a clone.
                            //  * Investigate why there is a panic here when just unwrapping.
                            if let Some(new_item) = self.item_map.get_mut(new_item_id) {
                                let mut found = false;

                                if old_items.contains(&new_item.ident) {
                                    found = true;
                                }

                                if let ItemKind::ForeignMod(ref mut fm) = new_item.node {
                                    fm.items.retain(|fm_item| !old_items.contains(&fm_item.ident));
                                }

                                if !found {
                                    m.items.push(P(new_item.clone()));
                                }
                            }
                        }
                        ItemKind::Mod(m)
                    },
                    n => n,
                };
                i
            });

            self.item_map.insert(pi.id, pi.clone().into_inner());
            smallvec![pi]
        });
        krate
    }
}

impl<'ast, 'st> Visitor<'ast> for CrateInformation<'st> {
    // Match the modules, using a mapping like:
    // NodeId -> NodeId
    // The key is the id of the old item to be moved, and the value is the NodeId of the module
    // the item will be moved to.
    fn visit_item(&mut self, old_module: &'ast Item) {
        match old_module.node {
            ItemKind::Mod(ref m) => {
                for module_item in m.items.iter() {
                    let (dest_module_id, ident) =
                        self.find_destination_id(&module_item.id, &old_module);
                    self.item_to_dest_module
                        .insert(module_item.id, dest_module_id);

                    for use_id in self.path_ids.iter() {
                        let item = self.item_map.get(&use_id)
                            .unwrap_or_else(|| panic!("There should be an item here: {:#?}", use_id));
                        let ut = match item.node {
                            ItemKind::Use(ref ut) => ut,
                            _ => unreachable!(),
                        };

                        let (prefix, dest_id) = self.path_mapping.entry(*use_id).or_insert_with(|| {
                            let mut prefix = ut.prefix.clone();

                            // Remove super and self from the paths
                            if prefix.segments.len() > 1 {
                                prefix.segments.retain(|segment| {
                                    segment.ident.name != keywords::Super.name()
                                        && segment.ident.name != keywords::SelfValue.name()
                                });
                            }
                            (prefix, *use_id)
                        });

                        // Check to see if a segment within the path is getting moved.
                        // example_h -> example
                        // DUMMY_NODE_ID -> actual destination module id
                        for segment in &mut prefix.segments {
                            if segment.ident == old_module.ident {
                                segment.ident = ident;
                                *dest_id = dest_module_id;
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
    fn transform(&self, krate: Crate, st: &CommandState, _cx: &driver::Ctxt) -> Crate {
        let mut krate_info = CrateInformation::new(st);

        krate_info.find_destination_modules(&krate);

        krate.visit(&mut krate_info);

        // `dest_mod_to_items`:
        // NodeId -> vec<NodeId>
        // The mapping is the destination module's `NodeId` to the items needing to be added to it.
        let dest_mod_to_items = krate_info.create_dest_mod_map();

        // insert a new modules into the Crate
        let krate = krate_info.extend_crate(krate, &dest_mod_to_items);

        // insert all the items marked as to be moved, into the proper
        // "destination module"
        let krate = krate_info.insert_items_into_dest(krate, &dest_mod_to_items);

        // This is where a bulk of the duplication removal happens, as well as path clean up.
        // 1. Paths are updated, meaning either removed or changed to match module change.
        //      And then reinserted with the new set of prefixes.
        // 2. Removes duplicates from `ForeignMod`'s
        // 3. Also removes duplicate `Item`'s found within a module.
        let krate = fold_nodes(krate, |pi: P<Item>| {
            // let mut v = smallvec![];
            let pi = pi.map(|mut i| {
                i.node = match i.node {
                    ItemKind::Mod(ref m) => {
                        let mut m = m.clone();
                        // This iteration goes through the module items and finds use statements,
                        // and either removes use statements or modifies them to have correct the
                        // module name.
                        let mut seen_paths: HashMap<Ident, HashSet<Ident>> = HashMap::new();
                        m.items = m.items.into_iter().filter_map(|item| {
                            // Removes use statements that are no longer needed in a module
                            if let Some((_, dest_module_id)) = krate_info.path_mapping.get(&item.id) {
                                if i.id == *dest_module_id {
                                    return None;
                                }
                            }
                            let m_id = item.id;
                            if let ItemKind::Use(ref ut) = item.node {
                                if let Some((new_path, _)) = krate_info.path_mapping.get(&m_id) {
                                    let mut ut = ut.clone();
                                    ut.prefix = new_path.clone();
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
                                    match ut.kind {
                                        UseTreeKind::Nested(ref use_trees) => {
                                            let mut prefixes = HashSet::new();
                                            for (use_tree, _) in use_trees {
                                                prefixes.insert(path_to_ident(&use_tree.prefix));
                                            }

                                            seen_paths.entry(path_to_ident(&ut.prefix)).and_modify(|set_of_prefixes| {
                                                set_of_prefixes.extend(prefixes.clone().into_iter());
                                            }).or_insert_with(|| {
                                                prefixes
                                            });
                                        },
                                        UseTreeKind::Simple(..) => {
                                            if ut.prefix.segments.len() > 1 {
                                                let mod_name = ut.prefix.segments.first().unwrap();
                                                let prefix = ut.prefix.segments.last().unwrap();

                                                seen_paths.entry(mod_name.ident).and_modify(|set_of_prefixes| {
                                                    set_of_prefixes.insert(prefix.ident);
                                                }).or_insert_with(|| {
                                                    let mut prefixes = HashSet::new();
                                                    prefixes.insert(prefix.ident);
                                                    prefixes
                                                });
                                            } else {
                                                // one item use statements like: `use libc;`
                                                // can be returned
                                                return Some(P(Item {
                                                    node: ItemKind::Use(ut.clone()),
                                                    ..item.clone().into_inner()
                                                }))
                                            }
                                        },
                                        _ => {}
                                    }
                                    return None;
                                }
                            }
                            Some(item)
                        }).collect();


                        let seen_item_ids =
                            m.items.iter().map(|item| item.id).collect::<HashSet<_>>();
                        let mut deleted_item_ids = HashSet::new();

                        // TODO: Use a function for `filter_map`
                        m.items = m.items.into_iter().filter_map(|mut module_item| {
                            for item_id in &seen_item_ids {
                                let item = krate_info.item_map.get(&item_id)
                                    .unwrap_or_else(|| panic!("There should be an item here: {:#?}", item_id));
                                if item.id != module_item.id {
                                    if let ItemKind::ForeignMod(ref mut foreign_mod) = module_item.node {
                                        if let ItemKind::ForeignMod(ref other_foreign_mod) = item.node {
                                            foreign_mod.items.retain(|foreign_item| {
                                                let mut result = true;
                                                for other_item in other_foreign_mod.items.iter() {
                                                    // Make a `compare_items` for foreign items?
                                                    if compare_foreign_items(&foreign_item, &other_item) && !deleted_item_ids.contains(&other_item.id) {
                                                        deleted_item_ids.insert(foreign_item.id);
                                                        result = false;
                                                    }
                                                }
                                                result
                                            });
                                        }
                                    }
                                }
                            }
                            Some(module_item)
                        }).collect();

                        // TODO: instead of iterating through items twice, use a hashmap and
                        // utilize indexing to get O(n)
                        m.items = m.items.into_iter().filter_map(|module_item| {
                            for item_id in &seen_item_ids {
                                let item = krate_info.item_map.get(&item_id)
                                    .unwrap_or_else(|| panic!("There should be an item here: {:#?}", item_id));
                                if item.id != module_item.id {
                                    let m_copy = module_item.clone();
                                    match module_item.node {
                                        ItemKind::ForeignMod(_) => {},
                                        _ => {
                                            if compare_items(&item, &m_copy) && !deleted_item_ids.contains(&item.id) {
                                                deleted_item_ids.insert(module_item.id);
                                                return None;
                                            }
                                        }
                                    }
                                }
                            }
                            Some(module_item)
                        }).collect();

                        // Here is where the seen_paths map is read, and turned into paths
                        // [foo_h] -> [item, item2, item3] turns into `use foo_h::{item, item2, item3};`
                        // And that ast is pushed into the module
                        let item_idents: HashSet<Ident> =
                            m.items.iter().map(|item| item.ident).collect::<HashSet<_>>();
                        for (mod_name, mut prefixes) in seen_paths.iter_mut() {
                            let mut items: Vec<Ident> = prefixes.iter().map(|i| i).cloned().collect();
                            let mod_prefix = Path::from_ident(*mod_name);

                            // Removes duplicates from the nested use statement
                            prefixes.retain(|prefix| !item_idents.contains(&*prefix));

                            let use_stmt = mk().use_multiple_item(mod_prefix, items);
                            m.items.push(use_stmt);
                        }
                        ItemKind::Mod(m)
                    },
                    n => n,
                };
                i
            });
            smallvec![pi]
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

// TODO:
// There may be issues with multi-segment paths, if there is it probably best
// to use `Vec<PathSegment>` instead.
fn path_to_ident(path: &Path) -> Ident {
    Ident::from_str(&path.to_string())
}

fn compare_foreign_items(fm_item: &ForeignItem, fm_item2: &ForeignItem) -> bool {
    fm_item.node.ast_equiv(&fm_item2.node) && fm_item.ident == fm_item2.ident
}

/// Compares an item not only using `ast_equiv`, but also in a variety of different ways
/// to handle different cases where an item may be equivalent but not caught by `ast_equiv`.
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

/// A check that goes through an `Item`'s attributes, and if the module
/// has `#[header_src = "/some/path"]` the function return true.
fn has_source_header(attrs: &Vec<Attribute>) -> bool {
    attr::contains_name(attrs, "header_src")
}

/// A check that goes through an `Item`'s attributes, and if the module
/// has "/usr/include" in the path like: `#[header_src = "/usr/include/stdlib.h"]`
/// then function return true.
// TODO: In macOS mojave the system headers aren't in `/usr/include` anymore,
// so this needs to be updated.
fn is_std(attrs: &Vec<Attribute>) -> bool {
    attrs.into_iter().any(|attr| {
        if let Some(meta) = attr.meta() {
            if let Some(value_str) = meta.value_str() {
                return value_str.as_str().contains("/usr/include");
            }
        }
        false
    })
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reorganize_modules", |_args| mk(ReorganizeModules))
}
