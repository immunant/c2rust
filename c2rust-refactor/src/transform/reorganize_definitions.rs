use indexmap::IndexMap;
use std::collections::HashMap;

use crate::transform::Transform;
use rustc::hir::Node;
use rustc::hir::def::{Namespace, PerNS};
use rustc::hir::def_id::DefId;
use rustc_target::spec::abi::Abi;
use syntax::ast::*;
use syntax::attr;
use syntax::print::pprust::{foreign_item_to_string, item_to_string};
use syntax::ptr::P;
use syntax::symbol::keywords;

use c2rust_ast_builder::mk;
use crate::ast_manip::util::{join_visibility, is_relative_path, namespace, split_uses};
use crate::ast_manip::{AstEquiv, FlatMapNodes, visit_nodes};
use crate::command::{CommandState, Registry};
use crate::driver::{Phase};
use crate::path_edit::fold_resolved_paths_with_id;
use crate::RefactorCtxt;

/// # `reoganize_definitions` Command
///
/// Usage: `reorganize_definitions`
///
/// This refactoring operates on code transpiled with the
/// `--reorganize-definitions` flag.
///
/// This pass refactors a crate to de-duplicate declarations, move them into
/// their relevant modules and import the items as needed, rather than using
/// extern forward declarations for all types and functions in headers.
pub struct ReorganizeDefinitions;

/// Holds the information of the current `Crate`, which includes a `HashMap` to look up Items
/// quickly, as well as other members that hold important information.
pub struct Reorganizer<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    st: &'a CommandState,

    modules: HashMap<Ident, ModuleInfo>,

    path_mapping: HashMap<DefId, (Path, NodeId)>,
}

/// A ModuleInfo captures all information about a module that is needed to
/// determine which module a header declaration should be moved into.
#[derive(Copy, Clone)]
struct ModuleInfo {
    ident: Ident,
    id: NodeId,
    new: bool,
}

impl<'a, 'tcx> Reorganizer<'a, 'tcx> {
    fn new(st: &'a CommandState, cx: &'a RefactorCtxt<'a, 'tcx>) -> Self {
        let mut modules = HashMap::new();
        let stdlib_ident = Ident::from_str("stdlib");
        modules.insert(
            stdlib_ident,
            ModuleInfo::new(stdlib_ident, st.next_node_id()),
        );
        Reorganizer {
            st,
            cx,
            modules,
            path_mapping: HashMap::new(),
        }
    }

    /// Run the reorganization pass
    pub fn run(&mut self, krate: &mut Crate) {
        self.find_destination_modules(&krate);

        let mut module_items = HashMap::new();
        self.remove_header_items(krate, &mut module_items);

        self.move_items(krate, module_items);

        self.update_paths(krate)
    }

    /// Iterate through the Crate and enumerate potentential destination modules.
    fn find_destination_modules(&mut self, krate: &Crate) {
        visit_nodes(krate, |i: &Item| {
            if let ItemKind::Mod(_) = &i.node {
                if !has_source_header(&i.attrs) && !is_std(&i.attrs) {
                    self.modules.insert(i.ident, ModuleInfo::from_item(i));
                }
            }
        });
    }

    /// Pick a destination module for a header item
    fn find_destination_id(&mut self, _item: &Item, parent_module: &Item) -> (NodeId, Ident) {
        if is_std(&parent_module.attrs) {
            let mod_info = self.modules.get(&Ident::from_str("stdlib")).unwrap();
            return (mod_info.id, mod_info.ident);
        }

        // Try to find an existing module to put this item in
        let dest_module = self
            .modules
            .values()
            .find(|dest_module_info| {
                // TODO: This is a simple naive heuristic,
                // and should be improved upon.
                parent_module
                    .ident
                    .as_str()
                    .contains(&*dest_module_info.ident.as_str())
            })
            .cloned();
        let dest_module = dest_module.unwrap_or_else(|| {
            // We didn't find an existing module, just put it in a new module for
            // that header.
            let new_node_id = self.st.next_node_id();
            *self
                .modules
                .entry(parent_module.ident)
                .or_insert_with(|| ModuleInfo::new(parent_module.ident, new_node_id))
        });

        (dest_module.id, dest_module.ident)
    }

    /// Drop all header modules, storing their items into the `module_items`
    /// mapping.
    fn remove_header_items(
        &mut self,
        krate: &mut Crate,
        module_items: &mut HashMap<NodeId, ModuleDefines<'a, 'tcx>>,
    ) {
        FlatMapNodes::visit(krate, |mut item: P<Item>| {
            if has_source_header(&item.attrs) {
                let header_item = item.clone();
                if let ItemKind::Mod(module) = &mut item.node {
                    module.items.retain(|item| {
                        let (dest_module_id, dest_module_ident) =
                            self.find_destination_id(&item, &header_item);

                        // Move the item to the `module_items` mapping.
                        let items = module_items
                            .entry(dest_module_id)
                            .or_insert_with(|| ModuleDefines::new(self.cx));
                        if !items.insert(item.clone()) {
                            // We couldn't move this item, bail out and retain the item.
                            return true;
                        }

                        // We are visiting a foreign module. Add path mappings
                        // for all of its items.
                        if let ItemKind::ForeignMod(m) = &item.node {
                            for foreign_item in &m.items {
                                let dest_path = mk().path(vec![
                                    keywords::Crate.ident(),
                                    dest_module_ident,
                                    foreign_item.ident,
                                ]);
                                self.path_mapping.insert(
                                    self.cx.node_def_id(foreign_item.id),
                                    (dest_path, dest_module_id),
                                );
                            }
                        }

                        // WARNING: this assumes that the destination module is
                        // a simple path in the crate root and it is flat,
                        // i.e. has no submodules which contain target items.
                        let dest_path = mk().path(vec![
                            keywords::Crate.ident(),
                            dest_module_ident,
                            item.ident,
                        ]);
                        self.path_mapping
                            .insert(self.cx.node_def_id(item.id), (dest_path, dest_module_id));

                        // Delete the item from the header module
                        false
                    });

                    if module.items.is_empty() {
                        // Delete the header module
                        smallvec![]
                    } else {
                        // We keep the header module with a (hopefully) reduced
                        // list of items.
                        smallvec![item]
                    }
                } else {
                    panic!("Unexpected Item kind with header_src attribute");
                }
            } else {
                smallvec![item]
            }
        })
    }

    /// Add items in `module_items` to their respective modules and create any
    /// new modules.
    fn move_items(&self, krate: &mut Crate, mut module_items: HashMap<NodeId, ModuleDefines>) {
        FlatMapNodes::visit(krate, |item: P<Item>| {
            smallvec![if let Some(new_defines) = module_items.remove(&item.id) {
                new_defines.move_into_module(item)
            } else {
                item
            }]
        });

        for mod_info in self.modules.values() {
            if mod_info.new {
                if let Some(new_defines) = module_items.remove(&mod_info.id) {
                    let new_items = new_defines.into_items();
                    let mut new_mod = mk().mod_(new_items);
                    new_mod.inline = false;
                    let new_mod_item = mk()
                        .id(mod_info.id)
                        .mod_item(mod_info.ident, new_mod);
                    krate.module.items.push(new_mod_item);
                }
            }
        }
    }

    /// Update paths to moved items and remove redundant imports.
    fn update_paths(&self, krate: &mut Crate) {
        // Maps NodeId of an AST element with an updated path to the NodeId of
        // the module it's target is now located in.
        let mut remapped_path_nodes = HashMap::new();

        fold_resolved_paths_with_id(krate, self.cx, |id, qself, path, def| {
            debug!("Folding path {:?} (def: {:?})", path, def);
            if let Some(def_id) = def.opt_def_id() {
                if let Some((new_path, mod_id)) = self.path_mapping.get(&def_id) {
                    let insert_result = remapped_path_nodes.insert(id, *mod_id);
                    assert_eq!(insert_result, None);
                    debug!("  -> {:?}", new_path);
                    return (qself, new_path.clone());
                } else if is_relative_path(&path) {
                    // Canonicalize a new path from the crate root. Will rewrite
                    // any relative paths that we may have moved into absolute
                    // paths.
                    return self.cx.def_qpath(def_id);
                }
            }
            (qself, path)
        });

        // Remove use statements that now refer to their self module.
        FlatMapNodes::visit(krate, |mut item: P<Item>| {
            let parent_id = item.id;
            if let ItemKind::Mod(m) = &mut item.node {
                let mut uses: HashMap<Ident, Path> = HashMap::new();
                m.items.retain(|item| {
                    if let ItemKind::Use(u) = &item.node {
                        match u.kind {
                            // uses that rename need to be retained
                            UseTreeKind::Simple(Some(_), _, _) => {}

                            UseTreeKind::Glob => {
                                return true;
                            }

                            // We don't need to handle Nested uses here because
                            // `fold_resolved_paths_with_id` splits nested uses
                            // into Simple uses when it remaps a path.
                            _ => {
                                if let Some(dest_mod_id) = remapped_path_nodes.get(&item.id) {
                                    if *dest_mod_id == parent_id {
                                        return false;
                                    }
                                }
                            }
                        }

                        if let Some(existing_prefix) = uses.get(&u.ident()) {
                            if !existing_prefix.ast_equiv(&u.prefix) {
                                panic!(
                                    "Conflicting imports of {:?}: {:?} and {:?}",
                                    u.ident(),
                                    existing_prefix,
                                    u.prefix,
                                );
                            }
                            return false;
                        } else {
                            uses.insert(u.ident(), u.prefix.clone());
                        }
                    }
                    true
                });
            }
            smallvec![item]
        });
    }
}

impl ModuleInfo {
    fn new(ident: Ident, id: NodeId) -> Self {
        Self {
            ident,
            id,
            new: true,
        }
    }

    /// Create a ModuleInfo from a module `Item`
    fn from_item(item: &Item) -> Self {
        Self {
            ident: item.ident,
            id: item.id,
            new: false,
        }
    }
}

enum IdentDecl {
    Item(P<Item>),
    ForeignItem(ForeignItem, Abi),
}

/// Store and de-duplicate items in a singlea module
struct ModuleDefines<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    idents: PerNS<IndexMap<Ident, IdentDecl>>,
    impls: Vec<P<Item>>,
}

impl<'a, 'tcx> ModuleDefines<'a, 'tcx> {
    pub fn new(cx: &'a RefactorCtxt<'a, 'tcx>) -> Self {
        Self {
            cx,
            idents: PerNS::default(),
            impls: Vec::new(),
        }
    }

    /// Add an item into the module. If it has a name conflict with an existing
    /// item, choose the definition item over any declarations.
    pub fn insert(&mut self, item: P<Item>) -> bool {
        match &item.node {
            // We have to disambiguate anonymous items by contents,
            // since we don't have a proper Ident.

            // Uses are split into simple uses (no brackets) and added into
            // ident_map.
            ItemKind::Use(_) => {
                for u in split_uses(item).into_iter() {
                    let use_tree = expect!([&u.node] ItemKind::Use(u) => u);
                    let path = self.cx.resolve_use(&u);
                    let ns = namespace(&path.def).expect("Could not identify def namespace");
                    if !self.insert_ident(ns, use_tree.ident(), u) {
                        return false;
                    }
                }
                true
            }

            // Impls are just filtered on equivalence
            ItemKind::Impl(..) => {
                if self.impls.iter().find(|u| item.ast_equiv(u)).is_none() {
                    self.impls.push(item.clone());
                }
                true
            }

            // We collect all ForeignItems and later filter out any idents
            // defined in ident_map after processing the whole list of items.
            ItemKind::ForeignMod(f) => {
                f.items
                    .iter()
                    .for_each(|item| self.insert_foreign(item.clone(), f.abi));
                true
            }

            // We disambiguate named items by their names and check that
            // we don't have any items with the same name but different
            // contents.

            // Value namespace
            ItemKind::Static(..) | ItemKind::Const(..) | ItemKind::Fn(..) => {
                assert!(item.ident != keywords::Invalid.ident());
                self.insert_ident(Namespace::ValueNS, item.ident, item)
            }

            // Type namespace
            _ => {
                assert!(item.ident != keywords::Invalid.ident());
                self.insert_ident(Namespace::TypeNS, item.ident, item)
            }
        }
    }

    /// Merge with the defintions in the given module, returning a new module
    /// containing the de-duplicated union of items.
    pub fn move_into_module(mut self, mut mod_item: P<Item>) -> P<Item> {
        let module = expect!([&mut mod_item.node] ItemKind::Mod(m) => m);
        module.items = {
            for item in module.items.iter() {
                self.insert(item.clone());
            }
            self.into_items()
        };

        mod_item
    }

    /// Add a foreign item declaration into the module, making sure to not
    /// duplicate an existing definition.
    fn insert_foreign(&mut self, item: ForeignItem, abi: Abi) {
        let ns = match &item.node {
            ForeignItemKind::Fn(..) | ForeignItemKind::Static(..) => Namespace::ValueNS,
            ForeignItemKind::Ty => Namespace::TypeNS,
            ForeignItemKind::Macro(..) => unimplemented!(),
        };

        self.insert_ident_foreign(ns, item.ident, item, abi);
    }

    /// Insert an item with the given ident into a namespace. Helper for
    /// `insert`.
    fn insert_ident(&mut self, ns: Namespace, ident: Ident, mut new: P<Item>) -> bool {
        match self.idents[ns].get_mut(&ident) {
            Some(existing_decl) => match existing_decl {
                IdentDecl::Item(existing_item) => match (&existing_item.node, &new.node) {
                    // Replace a use with a real definition
                    (ItemKind::Use(..), _) => {
                        new.vis.node = join_visibility(&existing_item.vis.node, &new.vis.node);
                        *existing_decl = IdentDecl::Item(new);
                        true
                    }

                    // Make sure we use the widest visibility for this item
                    (_, ItemKind::Use(..)) => {
                        existing_item.vis.node =
                            join_visibility(&existing_item.vis.node, &new.vis.node);
                        true
                    }

                    // (ItemKind::Fn(..), ItemKind::Fn(..)) => {
                    //     panic!("Cannot redefine function {:?}", existing_item.ident);
                    // }

                    // Otherwise make sure these items are structurally
                    // equivalent.
                    _ => {
                        if self.cx.structural_eq(&new, &existing_item) {
                            true
                        } else {
                            warn!(
                                "Could not disambiguate item for ident: {:?}\n  {}\n  {}",
                                existing_item.ident,
                                item_to_string(&new),
                                item_to_string(&existing_item)
                            );
                            false
                        }
                    }
                },

                IdentDecl::ForeignItem(existing_foreign, _) => {
                    if let ItemKind::Use(..) = new.node {
                        // If the import refers to the existing foreign item, do
                        // not replace it.
                        let path = self.cx.resolve_use(&new);
                        if let Some(did) = path.def.opt_def_id() {
                            if let Some(Node::ForeignItem(_)) = self.cx.hir_map().get_if_local(did) {
                                existing_foreign.vis.node =
                                    join_visibility(&existing_foreign.vis.node, &new.vis.node);
                                return true;
                            }
                        }

                        // Otherwise we want to replace a foreign definition
                        // with a rust import.
                        new.vis.node = join_visibility(&existing_foreign.vis.node, &new.vis.node);
                        *existing_decl = IdentDecl::Item(new);
                        return true;
                    }
                    if foreign_equiv(&existing_foreign, &new) {
                        // This item is equivalent to an existing foreign item,
                        // modulo visibility.
                        new.vis.node = join_visibility(&existing_foreign.vis.node, &new.vis.node);
                        *existing_decl = IdentDecl::Item(new);
                        true
                    } else {
                        panic!(
                            "Couldn't find a matching item for ident: {:?}\n{:#?}\n{:#?}",
                            existing_foreign.ident, existing_foreign, new
                        );
                    }
                }
            },

            None => {
                self.idents[ns].insert(ident, IdentDecl::Item(new));
                true
            }
        }
    }

    /// Insert a foreign item with the given ident into a namespace. Helper for
    /// `insert_foreign`.
    fn insert_ident_foreign(&mut self, ns: Namespace, ident: Ident, new: ForeignItem, abi: Abi) {
        match self.idents[ns].get_mut(&ident) {
            Some(existing_decl) => match existing_decl {
                IdentDecl::Item(existing_item) => {
                    if foreign_equiv(&new, &existing_item) {
                        // This foreign declaration is equivalent to an existing
                        // item, modulo visibility.
                        existing_item.vis.node =
                            join_visibility(&existing_item.vis.node, &new.vis.node);
                    } else if let ItemKind::Use(_) = existing_item.node {
                        // A use takes precedence over a foreign declaration
                        // unless the use refers to the foreign declaration are
                        // attempting to insert.
                        let path = self.cx.resolve_use(&existing_item);
                        if let Some(did) = path.def.opt_def_id() {
                            if let Some(Node::ForeignItem(_)) = self.cx.hir_map().get_if_local(did) {
                                *existing_decl = IdentDecl::ForeignItem(new, abi);
                                return;
                            }
                        }
                        existing_item.vis.node =
                            join_visibility(&existing_item.vis.node, &new.vis.node);
                    } else {
                        panic!(
                            "Couldn't find a matching item for ident: {:?}\n{:#?}\n{:#?}",
                            ident, new, existing_item
                        );
                    }
                }

                IdentDecl::ForeignItem(existing_foreign, existing_abi) => {
                    if *existing_abi != abi {
                        panic!("A foreign item already exists for {:?} but it has the wrong abi ({:?} vs {:?})", ident, existing_abi, abi);
                    }
                    let matches_existing = match (&existing_foreign.node, &new.node) {
                        (ForeignItemKind::Fn(decl1, _), ForeignItemKind::Fn(decl2, _)) => {
                            self.cx.compatible_fn_prototypes(decl1, decl2)
                        }

                        _ => existing_foreign.ast_equiv(&new),
                    };
                    if !matches_existing {
                        panic!("A foreign item already exists for {:?} but it doesn't match the new item\nOld item: {}\nNew item: {}", ident, foreign_item_to_string(existing_foreign), foreign_item_to_string(&new));
                    }
                }
            },

            None => {
                self.idents[ns].insert(ident, IdentDecl::ForeignItem(new, abi));
            }
        }
    }

    /// Finalize and return a de-duplicated Vec of items
    fn into_items(self) -> Vec<P<Item>> {
        let Self {
            cx: _,
            idents,
            impls,
        } = self;

        let items_iter = idents
            .type_ns
            .into_iter()
            .map(|(_, v)| v)
            .chain(idents.value_ns.into_iter().map(|(_, v)| v));

        let mut items: Vec<P<Item>> = Vec::new();
        let mut foreign_items: HashMap<Abi, Vec<ForeignItem>> = HashMap::new();
        for item in items_iter {
            match item {
                IdentDecl::Item(i) => items.push(i),
                IdentDecl::ForeignItem(fi, abi) => {
                    foreign_items.entry(abi).or_default().push(fi);
                }
            }
        }

        let foreign_mods = foreign_items
            .into_iter()
            .map(|(abi, items)| mk().abi(abi).foreign_items(items));

        foreign_mods
            .chain(items.into_iter())
            .chain(impls.into_iter())
            .collect()
    }
}

/// Returns true if the given ForeignItem can be a declaration for the given
/// Item definition.
fn foreign_equiv(foreign: &ForeignItem, item: &Item) -> bool {
    match (&foreign.node, &item.node) {
        // We should never encounter a foreign function with the same name but a
        // different declaration. Nonetheless, the FnDecls in rust might be
        // slightly different (param name, mutability), so we can't do an
        // ast_equiv on the FnDecl. Might be worth writing a custom comparison
        // for a sanity check, but not doing that right now.
        (ForeignItemKind::Fn(..), ItemKind::Fn(..)) => true,

        (ForeignItemKind::Static(frn_ty, frn_mutbl), ItemKind::Static(ty, mutbl, _))
            if frn_ty.ast_equiv(&ty) =>
        {
            match (frn_mutbl, mutbl) {
                (true, Mutability::Mutable) | (false, Mutability::Immutable) => true,
                _ => false,
            }
        }

        // If we have a definition for this type name we can assume it is
        // equivalent.
        (ForeignItemKind::Ty, ItemKind::Ty(..))
        | (ForeignItemKind::Ty, ItemKind::Enum(..))
        | (ForeignItemKind::Ty, ItemKind::Struct(..))
        | (ForeignItemKind::Ty, ItemKind::Union(..)) => true,

        _ => false,
    }
}

/// Check if the `Item` has the `#[header_src = "/some/path"]` attribute
fn has_source_header(attrs: &Vec<Attribute>) -> bool {
    attr::contains_name(attrs, "header_src")
}

/// A complementary check to `has_source_header`. Checks if the header source
/// path contains `/usr/include`
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

impl Transform for ReorganizeDefinitions {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let mut reorg = Reorganizer::new(st, cx);
        reorg.run(krate)
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reorganize_definitions", |_args| mk(ReorganizeDefinitions))
}
