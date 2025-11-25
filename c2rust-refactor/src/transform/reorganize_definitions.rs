use derive_more::From;
use indexmap::IndexMap;
use log::{debug, trace, warn};
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet, hash_map::Entry};
use std::mem;

use crate::transform::Transform;
use rustc_hir::def::{DefKind, Namespace, PerNS, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{self as hir, Node};
use rustc_middle::metadata::ModChild;
use rustc_middle::ty::{self, ParamEnv};
use rustc_target::spec::abi::{self, Abi};
use rustc_ast::*;
use rustc_ast::util::comments::{Comment, CommentStyle};
use rustc_ast::ptr::P;
use rustc_ast_pretty::pprust::{self, PrintState, item_to_string};
use rustc_span::symbol::{Ident, kw};
use rustc_data_structures::map_in_place::MapInPlace;
use rustc_span::{BytePos, DUMMY_SP, Symbol};
use smallvec::smallvec;

use crate::ast_manip::util::{is_relative_path, join_visibility, namespace, split_uses, is_exported, is_c2rust_attr};
use crate::ast_manip::{visit_nodes, AstEquiv, FlatMapNodes, MutVisitNodes};
use crate::command::{CommandState, Registry};
use crate::driver::Phase;
use crate::{expect, match_or};
use crate::path_edit::fold_resolved_paths_with_id;
use crate::RefactorCtxt;
use crate::ast_builder::mk;

use super::externs;

/// # `reorganize_definitions` Command
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

    modules: IndexMap<NodeId, ModuleInfo>,

    stdlib_id: NodeId,

    // Mapping from replaced item DefId to the path of its replacement and the
    // replacements parent module NodeId
    path_mapping: HashMap<DefId, Replacement>,

    // Counter used by `unique_ident`
    ident_counter: HashMap<Ident, usize>,
}

#[derive(Clone)]
struct Replacement {
    /// Path to replacement item
    path: Path,

    /// NodeId of replacement's parent module
    parent: NodeId,

    /// DefId of replacement if available
    def: Option<DefId>,
}

/// A ModuleInfo captures all information about a module that is needed to
/// determine which module a header declaration should be moved into.
#[derive(Clone)]
struct ModuleInfo {
    orig_ident: Ident,
    unique_ident: Ident,
    id: NodeId,

    path: Vec<PathSegment>,

    /// Does this module have a main function
    has_main: bool,

    /// Mapping from header ident to the line it was included into this module
    header_lines: HashMap<Ident, usize>,

    /// Set of headers included by this module (as full paths)
    headers: HashSet<String>,

    /// Set of item idents defined in this module, per namespace
    items: PerNS<HashSet<Ident>>,
}

impl<'a, 'tcx> Reorganizer<'a, 'tcx> {
    fn new(st: &'a CommandState, cx: &'a RefactorCtxt<'a, 'tcx>) -> Self {
        Reorganizer {
            st,
            cx,
            modules: IndexMap::new(),
            path_mapping: HashMap::new(),
            stdlib_id: DUMMY_NODE_ID,
            ident_counter: HashMap::new(),
        }
    }

    /// Run the reorganization pass
    pub fn run(&mut self, krate: &mut Crate) {
        self.find_destination_modules(&krate);

        // let mut module_items = HashMap::new();
        let mut header_decls = self.remove_header_items(krate);

        self.match_defs(&mut header_decls, krate);
        self.update_module_info_items(krate);

        self.move_items(header_decls, krate);

        self.update_paths(krate)
    }

    /// Return a new unique identifier with the given prefix
    fn unique_ident(&mut self, ident: Ident) -> Ident {
        match self.ident_counter.entry(ident) {
            Entry::Vacant(e) => {
                e.insert(0);
                ident
            }
            Entry::Occupied(mut e) => {
                let ev = e.get_mut();
                let res = format!("{}_{}", ident.as_str(), *ev);
                *ev += 1;
                Ident::from_str(&res)
            }
        }
    }

    /// Iterate through the Crate and enumerate potentential destination modules.
    fn find_destination_modules(&mut self, krate: &Crate) {
        visit_nodes(krate, |i: &Item| {
            if let ItemKind::Mod(_, ModKind::Loaded(mod_items, _, _)) = &i.kind {
                if !has_source_header(&i.attrs)
                    && mod_items.iter().any(|child| {
                        if let ItemKind::Mod(_, _) = child.kind {
                            false
                        } else {
                            true
                        }
                    })
                {
                    self.modules.insert(i.id, ModuleInfo::from_item(i, self.cx));
                }
            }
        });

        // Create a new module for standard library headers
        let stdlib_ident = Ident::from_str("stdlib");
        match self.modules.values().find(|mod_info| mod_info.orig_ident == stdlib_ident) {
            Some(info) => self.stdlib_id = info.id,
            None => {
                self.stdlib_id = self.st.next_node_id();
                let unique_ident = self.unique_ident(stdlib_ident);
                // TODO: this builds a `ModuleInfo` with an empty `headers`,
                // which is fine because that doesn't ever get checked below
                // in `find_destination_id` if `is_std() == true`; if that ever
                // changes, we need to fix it here
                self.modules.entry(self.stdlib_id)
                    .or_insert(ModuleInfo::new(stdlib_ident, unique_ident, self.stdlib_id));
            }
        }
    }

    /// Pick a destination module for a header item
    fn find_destination_id(&mut self, declaration: &MovedDecl) -> NodeId {
        if declaration.parent_header.is_std() {
            let mod_info = self.modules.get(&self.stdlib_id).unwrap();
            return mod_info.id;
        }

        // Try to find an existing module to put this item in
        let dest_module = self.modules.values().find(|dest_module_info| {
            if dest_module_info.has_main {
                return false;
            }
            // TODO: This is a simple naive heuristic,
            // and should be improved upon.
            if !dest_module_info.headers.contains(&declaration.parent_header.path) {
                return false;
            }

            if dest_module_info.items[declaration.namespace].contains(&declaration.ident()) {
                return false;
            }

            let header_ident = declaration.parent_header.ident.as_str();
            let module_ident = dest_module_info.orig_ident.as_str();
            if header_ident.len() >= module_ident.len() {
                let (base, ext) = header_ident.split_at(module_ident.len());
                base == &*module_ident && (ext.is_empty() || ext == "_h")
            } else {
                false
            }
        });
        let dest_module = match dest_module {
            Some(m) => m,
            None => {
                // We didn't find an existing module, just put it in a new module for
                // that header.
                let new_node_id = self.st.next_node_id();
                let orig_ident = declaration.parent_header.ident;
                let unique_ident = self.unique_ident(orig_ident);
                self.modules
                    .entry(new_node_id)
                    .or_insert_with(|| {
                        let mut mod_info = ModuleInfo::new(orig_ident, unique_ident, new_node_id);
                        mod_info.headers.insert(declaration.parent_header.path.clone());
                        mod_info
                    })
            }
        };

        dest_module.id
    }

    /// Drop all header modules, storing their items into the `module_items`
    /// mapping.
    fn remove_header_items(
        &mut self,
        krate: &mut Crate,
    ) -> HeaderDeclarations<'a, 'tcx> {

        // Decide which items we should keep in the header. This is currently
        // all functions, static globals, and any uses they reference.
        fn keep_items(items: &[P<Item>]) -> HashSet<NodeId> {
            let mut keep_items = HashSet::new();
            let mut used_idents = HashSet::new();
            for item in &items[..] {
                match &item.kind {
                    ItemKind::Fn(box Fn { body: Some(ref body), .. }) => {
                        keep_items.insert(item.id);
                        visit_nodes(&**body, |path: &Path| {
                            if let [segment] = &path.segments[..] {
                                used_idents.insert(segment.ident);
                            }
                        });
                    }

                    ItemKind::Static(_, _, init) if !is_exported(item) => {
                        keep_items.insert(item.id);

                        if let Some(init) = init {
                            visit_nodes(&**init, |path: &Path| {
                                if let [segment] = &path.segments[..] {
                                    used_idents.insert(segment.ident);
                                }
                            });
                        }
                    }

                    _ => {}
                }
            }

            // This assume the complex uses have been split apart already
            for item in &items[..] {
                if let ItemKind::Use(tree) = &item.kind {
                    if used_idents.contains(&tree.ident()) {
                        keep_items.insert(item.id);
                        continue;
                    }
                }
            }
            keep_items
        }

        let mut declarations = HeaderDeclarations::new(self.cx);
        FlatMapNodes::visit(krate, |mut item: P<Item>| {
            if let Some((path, _)) = parse_source_header(&item.attrs) {
                let header_item = item.clone();
                // TODO: handle use's at the top of the crate
                if let ItemKind::Mod(_, ModKind::Loaded(ref mut mod_items, _, _)) = &mut item.kind {
                    // Split complex uses before iterating over the items
                    mod_items.flat_map_in_place(|item| {
                        match &item.kind {
                            ItemKind::Use(tree) if is_nested(tree) => split_uses(item),
                            _ => smallvec![item],
                        }
                    });

                    let needed_items = keep_items(&mod_items);

                    mod_items.retain(|item| {
                        if needed_items.contains(&item.id) {
                            return true;
                        }

                        if let ItemKind::Use(_) = &item.kind {
                            // Don't add unused uses of non-exported parent
                            // items. These won't get merged with anything and
                            // will violate visibility if we move them.
                            if let Some(def_id) = self.cx
                                .try_resolve_use_id(item.id)
                                .and_then(|def| def.res.opt_def_id())
                            {
                                if !self.cx.is_exported_def(def_id) {
                                    return false;
                                }
                            }
                        }

                        let header_info = HeaderInfo::new(
                            header_item.ident,
                            path.clone(),
                        );
                        let inserted = declarations.insert_item(item.clone(), header_info);
                        // Keep the item if we are not collapsing it
                        !inserted
                    });

                    if mod_items.is_empty() {
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
        });
        declarations
    }

    /// Iterate over krate, matching up declarations to their definitions if
    /// available
    fn match_defs(&mut self, declarations: &mut HeaderDeclarations, krate: &Crate) {
        visit_nodes(krate, |item: &Item| {
            if !is_exported(item) {
                return;
            }
            let ns = match item.kind {
                // Can't match these items
                ItemKind::Use(..) | ItemKind::Impl(..) | ItemKind::ForeignMod(..)
                    | ItemKind::Mod(..) => return,

                // Values
                ItemKind::Static(..) | ItemKind::Const(..) | ItemKind::Fn(..) => Namespace::ValueNS,

                // Types
                _ => Namespace::TypeNS,
            };

            let decl_ids = declarations.remove_matching_defs(ns, item.ident, |decl| {
                match decl {
                    DeclKind::Item(decl) => self.cx.compatible_types(&decl, item),
                    DeclKind::ForeignItem(foreign, _) => foreign_equiv(&foreign, item),
                }
            });
            if !decl_ids.is_empty() {
                let def_id = self.cx.node_def_id(item.id);
                let dest_path = self.cx.def_path(def_id);
                let ldid = def_id.expect_local();
                let mod_hir_id = self.cx.ty_ctxt().parent_module_from_def_id(ldid);
                let mod_id = self.cx.hir_map().local_def_id_to_node_id(mod_hir_id);
                decl_ids.into_iter()
                    .for_each(|decl_id| {
                        self.path_mapping.insert(
                            decl_id,
                            Replacement {
                                path: dest_path.clone(),
                                parent: mod_id,
                                def: Some(def_id),
                            }
                        );
                    });
            }
        });

        for crate_def in &self.cx.crate_defs() {
            if crate_def.is_local() {
                continue;
            }
            match self.cx.ty_ctxt().extern_crate(*crate_def) {
                Some(extern_crate) if extern_crate.is_direct() => {}
                _ => continue,
            }
            for item in self.cx.ty_ctxt().module_children(*crate_def).iter() {
                let crate_name = self.cx.ty_ctxt().crate_name(crate_def.krate);
                let path = Path {
                    span: DUMMY_SP,
                    segments: vec![
                        PathSegment::path_root(DUMMY_SP),
                        PathSegment::from_ident(Ident::with_dummy_span(crate_name)),
                    ],
                    tokens: None,
                };
                self.match_exports(declarations, path, item);
            }
        }
    }

    fn match_exports(
        &mut self,
        declarations: &mut HeaderDeclarations,
        mut path: Path,
        item: &ModChild,
    ) {
        path.segments.push(PathSegment::from_ident(item.ident));
        if item.vis != ty::Visibility::Public {
            return;
        }
        let possible_match = match item.res {
            Res::Def(DefKind::ForeignTy, _) => false,
            Res::Def(DefKind::Fn, def_id) => {
                matches!(self.cx.ty_ctxt().fn_sig(def_id).abi(), Abi::C { .. })
            }
            Res::Def(DefKind::Static(_), def_id) => {
                if let ty::TyKind::Adt(def, _) = self.cx.ty_ctxt().type_of(def_id).kind() {
                    def.repr().c()
                } else {
                    false
                }
            }
            Res::Def(DefKind::Mod, def_id) => {
                for item in self.cx.ty_ctxt().module_children(def_id).iter() {
                    self.match_exports(declarations, path.clone(), item);
                }
                false
            }
            Res::Def(DefKind::Struct, _) | Res::Def(DefKind::Union, _) |
            Res::Def(DefKind::Enum, _) | Res::Def(DefKind::Const, _) |
            Res::Def(DefKind::TyAlias, _) => true,
            _ => false,
        };
        if !possible_match {
            return;
        }
        if let Some(def_id) = item.res.opt_def_id() {
            let ns = namespace(&item.res).unwrap();
            let decl_ids = declarations.remove_matching_defs(ns, item.ident, |decl| {
                match decl {
                    DeclKind::Item(decl) => match decl.kind {
                        // We don't want to replace type aliases, as they are
                        // easy to replace later if desired.
                        ItemKind::TyAlias(..) => false,

                        _ => {
                            if let Some(decl_ty) = self.cx.opt_node_type(decl.id) {
                                self.cx.structural_eq_tys_with_vis(decl_ty, self.cx.ty_ctxt().type_of(def_id))
                            } else {
                                false
                            }
                        }
                    }

                    // If we find a matching name from another crate, we
                    // should resolve to it.
                    // TODO: Refactor foreign_equiv to be able to use it
                    // without an Item
                    DeclKind::ForeignItem(foreign, _abi) => {
                        match &foreign.kind {
                            ForeignItemKind::Fn { .. } => {
                                if let Res::Def(DefKind::Fn, def_id) = item.res {
                                    let export_fn_sig = self.cx.ty_ctxt().fn_sig(def_id);
                                    let export_fn_sig = match export_fn_sig.no_bound_vars() {
                                        Some(sig) => sig,
                                        None => return false,
                                    };
                                    let foreign_def_id = self.cx.node_def_id(foreign.id);
                                    let foreign_fn_sig = self.cx.ty_ctxt().fn_sig(foreign_def_id);
                                    let foreign_fn_sig = match foreign_fn_sig.no_bound_vars() {
                                        Some(sig) => sig,
                                        None => return false,
                                    };
                                    self.cx.compatible_fn_sigs(&export_fn_sig, &foreign_fn_sig)
                                } else {
                                    false
                                }
                            }
                            ForeignItemKind::Static(..) => {
                                if let Res::Def(DefKind::Static(_), def_id) = item.res {
                                    let export_static_ty = self.cx.ty_ctxt().type_of(def_id);
                                    let foreign_def_id = self.cx.node_def_id(foreign.id);
                                    let foreign_static_ty = self.cx.ty_ctxt().type_of(foreign_def_id);
                                    self.cx.structural_eq_tys_with_vis(export_static_ty, foreign_static_ty)
                                } else {
                                    false
                                }
                            }
                            ForeignItemKind::TyAlias(..) => true,
                            ForeignItemKind::MacCall(..) => false,
                        }
                    }
                }
            });
            decl_ids.into_iter()
                .for_each(|decl_id| {
                    self.path_mapping.insert(
                        decl_id,
                        Replacement {
                            path: path.clone(),
                            parent: DUMMY_NODE_ID,
                            def: Some(def_id),
                        }
                    );
                });
        }
    }

    /// Update items set in ModuleInfos with current remaining items in that
    /// module so that we don't override an existing item
    fn update_module_info_items(&mut self, krate: &Crate) {
        visit_nodes(krate, |item: &Item| {
            if let ItemKind::Mod(_, ModKind::Loaded(mod_items, _, _)) = &item.kind {
                if let Some(info) = self.modules.get_mut(&item.id) {
                    for item in &mod_items[..] {
                        if let ItemKind::ForeignMod(m) = &item.kind {
                            for item in &m.items {
                                let ns = match &item.kind {
                                    ForeignItemKind::Fn { .. } | ForeignItemKind::Static(..) => Namespace::ValueNS,
                                    ForeignItemKind::TyAlias(..) => Namespace::TypeNS,
                                    ForeignItemKind::MacCall(..) => unimplemented!(),
                                };
                                info.items[ns].insert(item.ident);
                            }
                        } else {
                            if let Some(namespace) = self.cx.item_namespace(item) {
                                info.items[namespace].insert(item.ident);
                            }
                        }
                    }
                }
            }
        });
    }

    /// Choose destinations for items remaining in `declarations`, add these
    /// items to their destination module, and create any new modules.
    fn move_items(&mut self, declarations: HeaderDeclarations, krate: &mut Crate) {
        let HeaderDeclarations {idents, unnamed_items, matching_defs, ..} = declarations;

        // TODO: this probably needs to be PerNS
        let mut module_items: IndexMap<NodeId, Vec<MovedDecl>> = IndexMap::new();
        // Move named items into module_items
        idents.map(|idents| {
            for items in idents.into_values() {
                for (idx, mut item) in items.into_iter().enumerate() {
                    if idx > 0 {
                        let ident = item.ident();
                        // Append a number suffix to this item if
                        // there are multiple items with the same name
                        let old_name = ident.name.as_str();
                        let new_name = format!("{old_name}_{idx}");
                        warn!("Renaming identifier {old_name} to {new_name} due to collision");
                        item.ident_mut().name = Symbol::intern(&new_name);
                    }

                    let dest_module_id = self.find_destination_id(&item);

                    let ident = item.ident();
                    let dest_module_info = self.modules.get_mut(&dest_module_id).unwrap();
                    dest_module_info.items[item.namespace].insert(ident);
                    let mut path_segments = dest_module_info.path.clone();
                    path_segments.push(mk().path_segment(ident.name));
                    let dest_path = mk().path(path_segments);
                    self.path_mapping.insert(
                        item.def_id,
                        Replacement {
                            path: dest_path,
                            parent: dest_module_id,
                            def: None, // hasn't changed
                        },
                    );

                    // Move the item to the `module_items` mapping.
                    module_items.entry(dest_module_id).or_default().push(item);
                }
            }
        });

        // Move unnamed items into module_items
        unnamed_items.map(|items| {
            for item in items.into_iter() {
                let ident = item.ident();
                let parent = self.find_destination_id(&item);

                let dest_module_info = &self.modules[&parent];
                let mut path_segments = dest_module_info.path.clone();
                path_segments.push(mk().path_segment(ident.name));
                let path = mk().path(path_segments);
                self.path_mapping.insert(
                    item.def_id,
                    Replacement {
                        path,
                        parent,
                        def: None,
                    },
                );

                // Move the item to the `module_items` mapping.
                module_items.entry(parent).or_default().push(item);
            }
        });

        // Add path mappings for all defs in matching_defs
        for (old_def, mut new_def) in &matching_defs {
            while let Some(other) = matching_defs.get(&new_def) {
                new_def = other;
            }
            if let Some(mapping) = self.path_mapping.get(&new_def).cloned() {
                self.path_mapping.insert(*old_def, mapping);
            }
        }

        // Convert the module_items vector into a HeaderDeclarations struct for
        // each module
        let mut module_item_decls: IndexMap<NodeId, HeaderDeclarations> = module_items
            .into_iter()
            .map(|(module_id, items)| {
                let mut decls = HeaderDeclarations::new(self.cx);
                decls.extend(items);
                (module_id, decls)
            }).collect();

        // We should have handled merging of idents in match_defs
        // above. Therefore this new decl won't conflict with a decl in the
        // destination module, although it may need to replace an import or
        // foreign item.
        FlatMapNodes::visit(krate, |mut item: P<Item>| {
            let id = item.id;
            if let ItemKind::Mod(_, ModKind::Loaded(mod_items, _, _)) = &mut item.kind {
                if let Some(mut declarations) = module_item_decls.remove(&id) {
                    let module_info = &self.modules[&id];

                    // Remove extern declarations or imports of new items we are
                    // injecting
                    mod_items
                        .drain_filter(|item| {
                            if let ItemKind::ForeignMod(m) = &mut item.kind {
                                let abi = m
                                    .abi
                                    .and_then(|abi| abi::lookup(&abi.symbol.as_str()))
                                    .unwrap_or(Abi::Rust);
                                m.items.retain(|item| {
                                    match declarations.find_foreign_item(item, abi) {
                                        ContainsDecl::NotContained => true,
                                        ContainsDecl::Equivalent(_) => false,
                                        ContainsDecl::Definition(_) => false,
                                        ContainsDecl::Use(_) => true,
                                    }
                                });
                                m.items.is_empty()
                            } else {
                                let namespace = self.cx.item_namespace(&item);
                                if let Some(namespace) = namespace {
                                    match declarations.find_item(item, namespace) {
                                        ContainsDecl::NotContained => false,
                                        ContainsDecl::Equivalent(_) => true,
                                        ContainsDecl::Definition(_) => true,
                                        ContainsDecl::Use(_) => false,
                                    }
                                } else {
                                    false
                                }
                            }
                        });

                    let new_items: Vec<P<Item>> = declarations.into_items(self.st, module_info);
                    let old_items = mem::replace(mod_items, new_items);
                    mod_items.extend(old_items);
                }

            }
            smallvec![item]
        });

        // Put new modules for executables inline, because we can't really put
        // them into the source tree where the library sources are since they
        // will conflict.
        let _inline = self.cx.is_executable();
        for mod_info in self.modules.values() {
            if let Some(declarations) = module_item_decls.remove(&mod_info.id) {
                let new_items = declarations.into_items(self.st, mod_info);
                if !new_items.is_empty() {
                    #[inline]
                    fn match_mod_item(item: &mut P<Item>, ident: Ident) -> Option<&mut ModKind> {
                        if item.ident == ident {
                            match item.kind {
                                ItemKind::Mod(_, ref mut m) => Some(m),
                                _ => None
                            }
                        } else {
                            None
                        }
                    }

                    if let Some(ModKind::Loaded(ref mut mod_items, _, _)) = krate
                        .items
                        .iter_mut()
                        .find_map(|item| match_mod_item(item, mod_info.unique_ident))
                    {
                        // FIXME: we should also check if items overlap
                        mod_items.extend(new_items.into_iter());
                    } else {
                        // TODO: we should outline the modules, but there
                        // is currently an issue with the pretty-printer
                        // where it both writes an output file, and the inline
                        // module in the parent
                        //
                        //let modb = if inline {
                        //    mk.inline()
                        //} else {
                        //    mk()
                        //};
                        let modb = mk().inline();
                        let new_mod = modb.mod_(new_items);
                        let new_mod_item = mk()
                            .pub_()
                            .id(mod_info.id)
                            .mod_item(mod_info.unique_ident, new_mod);

                        krate.items.insert(0, new_mod_item);
                    }
                }
            }
        }

        // Remove src_loc attributes
        FlatMapNodes::visit(krate, |mut item: P<Item>| {
            item.attrs
                .retain(|attr| !is_c2rust_attr(attr, "src_loc"));
            smallvec![item]
        });
        FlatMapNodes::visit(krate, |mut item: P<ForeignItem>| {
            item.attrs
                .retain(|attr| !is_c2rust_attr(attr, "src_loc"));
            smallvec![item]
        });

        // Remove header_src attributes
        FlatMapNodes::visit(krate, |mut item: P<Item>| {
            item.attrs
                .retain(|attr| !is_c2rust_attr(attr, "header_src"));
            smallvec![item]
        });
    }

    /// Update paths to moved items and remove redundant imports.
    fn update_paths(&self, krate: &mut Crate) {
        let tcx = self.cx.ty_ctxt();

        // Maps NodeId of an AST element with an updated path to the NodeId of
        // the module its target is now located in and the DefId of the original
        // target.
        let mut remapped_paths = HashMap::new();

        // Mapping from use node id to resolutions in other namespaces. We need
        // this so we can add extra uses if we move a type from a header but
        // don't move a value with the same ident.
        let mut multi_namespace_uses = HashMap::new();

        fold_resolved_paths_with_id(krate, self.cx, |id, qself, path, defs| {
            debug!("Folding path {:?} (def: {:?})", path, defs);
            if defs.len() > 1 {
                let def_ids: Vec<_> = defs[1..].iter().flat_map(|def| def.opt_def_id()).collect();
                multi_namespace_uses.insert(id, (path.clone(), def_ids));
            }
            if let Some(def_id) = defs[0].opt_def_id() {
                if let Some(replacement) = self.path_mapping.get(&def_id) {
                    let inserted = remapped_paths.insert(id, (replacement.parent, def_id)).is_none();
                    assert!(inserted);
                    debug!("  -> {:?}", replacement.path);
                    return (qself, replacement.path.clone());
                } else if is_relative_path(&path) {
                    // Canonicalize a new path from the crate root. Will rewrite
                    // any relative paths that we may have moved into absolute
                    // paths.
                    if let Some(ldid) = def_id.as_local() {
                        let mod_hir_id = self.cx.ty_ctxt().parent_module_from_def_id(ldid);
                        let mod_id = self.cx.hir_map().local_def_id_to_node_id(mod_hir_id);
                        let inserted = remapped_paths.insert(id, (mod_id, def_id)).is_none();
                        assert!(inserted);
                    }
                    return self.cx.def_qpath(def_id);
                }
            }
            (qself, path)
        });

        // Fix casts of the following format:
        // &global as *const [T; 0]
        // where global is now [T; n] where n > 0, not [T; 0]
        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let cast_id = e.id;
            let (val, ty) = match_or!([&mut e.kind] ExprKind::Cast(val, ty) => (val, ty); return);
            let val = match_or!([&val.kind] ExprKind::AddrOf(_, _mutbl, val) => val; return);
            let old_def_id = match_or!([self.cx.try_resolve_expr(&val)] Some(id) => id; return);
            let replacement = match_or!([self.path_mapping.get(&old_def_id)] Some(x) => x; return);
            let new_def_id = match_or!([replacement.def] Some(id) => id; return);
            let val_ty = self.cx.def_type(new_def_id);
            let val_len = match_or!([val_ty.kind()] ty::TyKind::Array(_ty, n) => n; return);
            let cast_ty = match_or!([self.cx.opt_node_type(cast_id)] Some(ty) => ty; return);
            let cast_ty = match_or!([cast_ty.kind()] ty::TyKind::RawPtr(ty) => ty; return);
            let cast_ty = cast_ty.ty;
            let cast_len = match_or!([cast_ty.kind()] ty::TyKind::Array(_ty, n) => n; return);
            if let Some(0) = cast_len.try_eval_usize(tcx, ParamEnv::empty()) {
                if let Some(val_len) = val_len.try_eval_usize(tcx, ParamEnv::empty()) {
                    let ty = match_or!([&mut ty.kind] TyKind::Ptr(ty) => ty; return);
                    let cast_len = match_or!([&mut ty.ty.kind] TyKind::Array(_ty, n) => n; return);
                    let lit = mk().lit_expr(mk().int_lit(val_len as u128, LitIntType::Unsuffixed));
                    *cast_len = mk().anon_const(lit);
                }
            }
        });

        // Mapping from old DefId to new DefId
        let replacement_map = self.path_mapping.iter().filter_map(|(old_def, replacement)| {
            match self.cx.hir_map().get_if_local(*old_def) {
                Some(Node::ForeignItem(_)) => {}
                Some(Node::Item(item)) => match item.kind {
                    hir::ItemKind::Static(..) | hir::ItemKind::Const(..) | hir::ItemKind::Fn(..)
                    | hir::ItemKind::Union(..) | hir::ItemKind::Enum(..)
                    | hir::ItemKind::TyAlias(..) => {}
                    _ => return None,
                },
                _ => return None,
            }
            replacement.def.map(|new_def| (*old_def, new_def))
        }).collect();
        // Mapping from user path expr NodeId to old DefId
        let path_ids = remapped_paths.iter().map(|(user, (_parent, def))| (*user, *def)).collect();
        // Mapping from old DefId to new path
        let new_paths = self.path_mapping.iter().map(|(old_did, replacement)| {
            (*old_did, (None, replacement.path.clone()))
        }).collect::<HashMap<_, _>>();

        // Cast updated values back to their original type if needed
        externs::fix_users(krate, &replacement_map, &path_ids, &new_paths, self.cx);

        // Remove use statements that now refer to their self module.
        FlatMapNodes::visit(krate, |mut item: P<Item>| {
            let mod_id = item.id;
            if let ItemKind::Mod(_, ModKind::Loaded(mod_items, _, _)) = &mut item.kind {
                // Add use statements for split namespace imports
                mod_items.flat_map_in_place(|item: P<Item>| -> SmallVec<[P<Item>; 1]> {
                    let mut items = smallvec![];
                    if let ItemKind::Use(_) = &item.kind {
                        if let Some((path, def_ids)) = multi_namespace_uses.get(&item.id) {
                            for def_id in def_ids {
                                let (other_mod_id, _) = remapped_paths[&item.id];
                                if let Some(Replacement {path, parent, ..}) = self.path_mapping.get(&def_id) {
                                    if other_mod_id != *parent {
                                        items.push(mk().use_simple_item(
                                            path,
                                            None::<String>,
                                        ));
                                    }
                                } else if is_relative_path(&path) {
                                    // Canonicalize a new path from the crate root. Will rewrite
                                    // any relative paths that we may have moved into absolute
                                    // paths.
                                    if let Some(ldid) = def_id.as_local() {
                                        let mod_hir_id = self.cx.ty_ctxt().parent_module_from_def_id(ldid);
                                        let mod_id = self.cx.hir_map().local_def_id_to_node_id(mod_hir_id);
                                        if other_mod_id != mod_id {
                                            let new_node_id = self.st.next_node_id();
                                            let inserted = remapped_paths.insert(new_node_id, (mod_id, *def_id)).is_none();
                                            assert!(inserted);
                                            items.push(mk().id(new_node_id).use_simple_item(
                                                self.cx.def_path(*def_id),
                                                None::<String>,
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    items.push(item);
                    items
                });

                // Mapping from ident to the module we are importing that ident from
                let mut uses: PerNS<HashMap<Ident, NodeId>> = PerNS::default();
                mod_items.retain(|item| {
                    if let ItemKind::Use(u) = &item.kind {
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
                                if let Some((mod_def_id, _)) = remapped_paths.get(&item.id) {
                                    if *mod_def_id == mod_id {
                                        return false;
                                    }
                                }
                            }
                        }

                        if let Some(namespace) = self.cx.item_namespace(&item) {
                            // Uses import from all available namespaces. If any
                            // namespace contains a use of this ident pointing from
                            // the same parent module, we only need to keep one.
                            if let Some(def_id) = self.cx
                                .try_resolve_use_id(item.id)
                                .and_then(|def| def.res.opt_def_id())
                            {
                                if let Some(Replacement {parent, ..}) = self.path_mapping.get(&def_id) {
                                    for ns in &[Namespace::ValueNS, Namespace::TypeNS] {
                                        if let Some(target_mod) = uses[*ns].get(&u.ident()) {
                                            if target_mod == parent {
                                                return false;
                                            } else if *ns == namespace {
                                                panic!(
                                                    "Conflicting imports of {:?} from {:?} and {:?}",
                                                    u.ident(),
                                                    target_mod,
                                                    *parent,
                                                );
                                            }
                                        }
                                    }
                                }
                            }

                            if uses[namespace].contains_key(&u.ident()) {
                                return false;
                            } else {
                                if let Some(def_id) = self.cx
                                    .try_resolve_use_id(item.id)
                                    .and_then(|def| def.res.opt_def_id())
                                {
                                    let mod_id = if let Some(Replacement {parent, ..}) = self.path_mapping.get(&def_id) {
                                        *parent
                                    } else {
                                        if let Some(ldid) = def_id.as_local() {
                                            let mod_hir_id = self.cx.ty_ctxt().parent_module_from_def_id(ldid);
                                            self.cx.hir_map().local_def_id_to_node_id(mod_hir_id)
                                        } else {
                                            DUMMY_NODE_ID
                                        }
                                    };
                                    uses[namespace].insert(u.ident(), mod_id);
                                }
                            }
                        }
                    }
                    true
                });
            }
            smallvec![item]
        });
    }
}

#[derive(Clone, Debug)]
struct HeaderInfo {
    ident: Ident,
    path: String,
}

impl HeaderInfo {
    fn new(ident: Ident, path: String) -> Self {
        Self {
            ident,
            path,
        }
    }

    /// A complementary check to `has_source_header`. Checks if the header source
    /// path contains `/usr/include`
    // TODO: In macOS mojave the system headers aren't in `/usr/include` anymore,
    // so this needs to be updated.
    fn is_std(&self) -> bool {
        self.path.contains("/usr/include")
    }
}

impl ModuleInfo {
    fn new(orig_ident: Ident, unique_ident: Ident, id: NodeId) -> Self {
        Self {
            orig_ident,
            unique_ident,
            id,
            path: vec![mk().path_segment(kw::Crate), mk().path_segment(unique_ident.name)],
            has_main: false,
            header_lines: HashMap::new(),
            headers: HashSet::new(),
            items: PerNS::default(),
        }
    }

    /// Create a ModuleInfo from a module `Item`
    fn from_item(item: &Item, cx: &RefactorCtxt) -> Self {
        let module = expect!([&item.kind] ItemKind::Mod(_, m) => m);
        let mod_items = expect!([&module] ModKind::Loaded(ref items, _, _) => items);
        let mut has_main = false;
        let mut header_lines: HashMap<Ident, usize> = HashMap::new();
        let mut headers = HashSet::new();
        let def_id = cx.node_def_id(item.id);
        let path = cx.def_path(def_id);
        for i in &mod_items[..] {
            match &i.kind {
                ItemKind::Fn(..) => {
                    if i.ident.as_str() == "main" {
                        has_main = true;
                    }
                }
                ItemKind::Mod(..) => {
                    let (path, line) = parse_source_header(&i.attrs)
                        .unwrap_or_else(|| panic!("Could not parse source header for item: {:?}", i));
                    headers.insert(path);
                    if header_lines.insert(i.ident, line).is_some() {
                        panic!(
                            "Conflicting headers in the same module with name: {}",
                            i.ident
                        );
                    }
                }
                _ => {}
            }
        }
        Self {
            orig_ident: item.ident,
            unique_ident: item.ident,
            id: item.id,
            path: path.segments,
            has_main,
            header_lines,
            headers,
            items: PerNS::default(),
        }
    }
}

#[derive(Debug)]
struct MovedDecl {
    kind: DeclKind,
    def_id: DefId,
    namespace: Namespace,
    loc: Option<SrcLoc>,
    parent_header: HeaderInfo,
}

impl MovedDecl {
    fn new<T>(decl: T, def_id: DefId, namespace: Namespace, parent_header: HeaderInfo) -> Self
    where
        T: Into<DeclKind>,
    {
        let kind: DeclKind = decl.into();
        let loc: Option<SrcLoc> = kind
            .attrs()
            .iter()
            .find(|attr| is_c2rust_attr(attr, "src_loc"))
            .map(|l| l.into());

        Self {
            kind,
            def_id,
            namespace,
            loc,
            parent_header,
        }
    }

    fn is_foreign(&self) -> bool {
        match &self.kind {
            DeclKind::ForeignItem(..) => true,
            _ => false,
        }
    }

    fn visibility(&self) -> &Visibility {
        match &self.kind {
            DeclKind::ForeignItem(item, _) => &item.vis,
            DeclKind::Item(item) => &item.vis,
        }
    }

    fn join_visibility(&mut self, vis: &VisibilityKind) {
        match &mut self.kind {
            DeclKind::ForeignItem(item, _) => item.vis.kind = join_visibility(&item.vis.kind, vis),
            DeclKind::Item(item) => item.vis.kind = join_visibility(&item.vis.kind, vis),
        }
    }

    fn ident(&self) -> Ident {
        match &self.kind {
            DeclKind::ForeignItem(item, _) => item.ident,
            DeclKind::Item(item) => if let ItemKind::Use(tree) = &item.kind {
                tree.ident()
            } else {
                item.ident
            },
        }
    }

    fn ident_mut(&mut self) -> &mut Ident {
        match &mut self.kind {
            DeclKind::ForeignItem(item, _) => &mut item.ident,
            DeclKind::Item(item) => {
                // Reborrow the item inside the P<Item> so we can
                // sub-borrow different fields from it
                let item = &mut **item;
                if let ItemKind::Use(UseTree { kind: UseTreeKind::Simple(Some(rename), _, _), .. }) = &mut item.kind {
                    rename
                } else {
                    &mut item.ident
                }
            }
        }
    }
}

impl ToString for MovedDecl {
    fn to_string(&self) -> String {
        match &self.kind {
            DeclKind::ForeignItem(item, _) => pprust::to_string(|s| {
                s.foreign_item_to_string(item);
            }),
            DeclKind::Item(item) => item_to_string(item),
        }
    }
}

#[derive(Clone, Debug, From)]
enum DeclKind {
    Item(P<Item>),
    ForeignItem(P<ForeignItem>, Abi),
}

impl HasAttrs for DeclKind {
    const SUPPORTS_CUSTOM_INNER_ATTRS: bool = true;

    fn attrs(&self) -> &[Attribute] {
        match self {
            DeclKind::Item(i) => i.attrs(),
            DeclKind::ForeignItem(i, _) => i.attrs(),
        }
    }

    fn visit_attrs(&mut self, f: impl FnOnce(&mut Vec<Attribute>)) {
        match self {
            DeclKind::Item(i) => i.visit_attrs(f),
            DeclKind::ForeignItem(i, _) => i.visit_attrs(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct SrcLoc {
    line: usize,
    col: usize,
}

impl From<&Attribute> for SrcLoc {
    fn from(attr: &Attribute) -> Self {
        let value_sym = attr
            .value_str()
            .expect("Expected a value for src_loc attribute");
        let mut iter = value_sym.as_str().split(':');
        let line: usize = iter
            .next()
            .and_then(|x| x.parse().ok())
            .expect("Expected a line number in src_loc attribute");
        let col: usize = iter
            .next()
            .and_then(|x| x.parse().ok())
            .expect("Expected an column number in src_loc attribute");
        Self { line, col }
    }
}

/// Store and de-duplicate header-declared items
struct HeaderDeclarations<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    idents: PerNS<IndexMap<Ident, Vec<MovedDecl>>>,
    unnamed_items: PerNS<Vec<MovedDecl>>,
    matching_defs: HashMap<DefId, DefId>
    // // Set of imported definition NodeIds that must be made pub(crate) at least
    // imports: HashSet<HirId>,

}
impl<'a, 'tcx> Extend<MovedDecl> for HeaderDeclarations<'a, 'tcx> {
    fn extend<T: IntoIterator<Item = MovedDecl>>(&mut self, iter: T) {
        for item in iter {
            let ident = item.ident();
            if ident.as_str().contains("C2RustUnnamed") {
                self.unnamed_items[item.namespace].push(item);
            } else {
                self.idents[item.namespace].entry(ident).or_default().push(item);
            }
        }    
    }
}

impl<'a, 'tcx> HeaderDeclarations<'a, 'tcx> {
    pub fn new(cx: &'a RefactorCtxt<'a, 'tcx>) -> Self {
        Self {
            cx,
            idents: PerNS::default(),
            unnamed_items: PerNS::default(),
            matching_defs: HashMap::new(),
            // imports: HashSet::new(),
        }
    }

    /// Remove and return declarations matching the specified item definition
    fn remove_matching_defs<P>(
        &mut self,
        namespace: Namespace,
        ident: Ident,
        mut predicate: P,
    ) -> Vec<DefId>
        where P: FnMut(&DeclKind) -> bool
    {
        assert!(ident.name != kw::Empty);
        let mut matches = vec![];
        if let Some(items) = self.idents[namespace].get_mut(&ident) {
            items.retain(|decl| {
                match &decl.kind {
                    DeclKind::Item(decl) => {
                        // Don't match use statements
                        if let ItemKind::Use(_) = decl.kind {
                            return true;
                        }
                    }
                    _ => {}
                }
                if predicate(&decl.kind) {
                    matches.push(decl.def_id);
                    false
                } else {
                    true
                }
            });
        }
        matches
    }

    /// Add an item into the module. If it has a name conflict with an existing
    /// item, choose the definition item over any declarations.
    pub fn insert_item(
        &mut self,
        mut item: P<Item>,
        parent_header: HeaderInfo,
    ) -> bool {
        let namespace = self.cx.item_namespace(&item);
        let new_def_id = self.cx.node_def_id(item.id);
        let ident = if let ItemKind::Use(tree) = &item.kind {
            tree.ident()
        } else {
            item.ident
        };
        match &item.kind {
            // We have to disambiguate anonymous items by contents,
            // since we don't have a proper Ident.

            // Uses are split into simple uses (no brackets) and added into
            // ident_map.
            ItemKind::Use(tree) if is_nested(tree) => {
                for u in split_uses(item).into_iter() {
                    self.insert_item(u, parent_header.clone());
                }
                true
            }

            // Keep function definitions, if any
            ItemKind::Fn(..) => false,

            // Don't keep impl blocks, these are expanded from macros anyway
            ItemKind::Impl(..) => true,

            // We collect all ForeignItems and later filter out any idents
            // defined in ident_map after processing the whole list of items.
            ItemKind::ForeignMod(f) => {
                for item in f.items.iter() {
                    let abi = f
                        .abi
                        .and_then(|abi| abi::lookup(&abi.symbol.as_str()))
                        .unwrap_or(Abi::Rust);
                    self.insert_foreign_item(item.clone(), abi, parent_header.clone());
                }
                true
            }

            // We disambiguate named items by their names and check that
            // we don't have any items with the same name but different
            // contents.

            _ => {
                let unnamed = ident.as_str().contains("C2RustUnnamed");
                let def_id_mapping = match self.find_item(&item, namespace.unwrap()) {
                    ContainsDecl::NotContained => {
                        let new_item = MovedDecl::new(item, new_def_id, namespace.unwrap(), parent_header);
                        if unnamed {
                            self.unnamed_items[namespace.unwrap()].push(new_item);
                        } else {
                            self.idents[namespace.unwrap()]
                                .entry(ident)
                                .or_default()
                                .push(new_item);
                        }
                        None
                    }

                    ContainsDecl::Definition(existing) => {
                        existing.join_visibility(&item.vis.kind);
                        Some((new_def_id, existing.def_id))
                    }

                    ContainsDecl::Use(existing) => {
                        let existing_def_id = existing.def_id;
                        existing.join_visibility(&item.vis.kind);
                        *existing = MovedDecl::new(item, new_def_id, namespace.unwrap(), parent_header);
                        Some((existing_def_id, new_def_id))
                    }

                    ContainsDecl::Equivalent(existing) if existing.is_foreign() => {
                        let existing_def_id = existing.def_id;
                        item.vis.kind = join_visibility(&existing.visibility().kind, &item.vis.kind);
                        *existing = MovedDecl::new(item, new_def_id, namespace.unwrap(), parent_header);
                        Some((existing_def_id, new_def_id))
                    }

                    ContainsDecl::Equivalent(existing) => {
                        Some((new_def_id, existing.def_id))
                    }
                };
                if let Some((old, new)) = def_id_mapping {
                    self.matching_defs.insert(old, new);
                }
                true
            }
        }
    }

    fn insert_foreign_item(
        &mut self,
        item: P<ForeignItem>,
        abi: Abi,
        parent_header: HeaderInfo,
    ) {
        let new_def_id = self.cx.node_def_id(item.id);
        let ident = item.ident;
        let namespace = self.cx.foreign_item_namespace(&item).unwrap();
        let unnamed = ident.as_str().contains("C2RustUnnamed");
        let def_id_mapping = match self.find_foreign_item(&item, abi) {
            ContainsDecl::NotContained => {
                let new_item = MovedDecl::new(
                    (item.clone(), abi),
                    new_def_id,
                    namespace,
                    parent_header.clone(),
                );
                if unnamed {
                    self.unnamed_items[namespace].push(new_item);
                } else {
                    self.idents[namespace]
                        .entry(ident)
                        .or_default()
                        .push(new_item);
                }
                None
            }

            ContainsDecl::Definition(existing) => {
                let existing_def_id = existing.def_id;
                *existing = MovedDecl::new(
                    (item.clone(), abi),
                    new_def_id,
                    namespace,
                    parent_header.clone(),
                );
                Some((existing_def_id, new_def_id))
            }

            ContainsDecl::Equivalent(existing) => {
                existing.join_visibility(&item.vis.kind);
                Some((new_def_id, existing.def_id))
            }

            ContainsDecl::Use(..) => panic!("Foreign items cannot be use statements"),
        };
        if let Some((old, new)) = def_id_mapping {
            self.matching_defs.insert(old, new);
        }
    }

    /// Finalize and return a de-duplicated Vec of items
    fn into_items(self, st: &CommandState, info: &ModuleInfo) -> Vec<P<Item>> {
        fn make_header_comment(last_mod: Option<Ident>, next_mod: Ident) -> Comment {
            let mut lines = vec![];
            if let Some(last_mod) = last_mod {
                lines.push(format!(
                    "// ================ END {} ================",
                    last_mod.as_str(),
                ));
            }
            lines.push(format!(
                "// =============== BEGIN {} ================",
                next_mod.as_str(),
            ));
            Comment {
                style: CommentStyle::Isolated,
                lines,
                pos: BytePos(0),
            }
        }

        let Self {
            idents,
            unnamed_items,
            ..
        } = self;

        let mut all_items = unnamed_items
            .type_ns
            .into_iter()
            .chain(unnamed_items.value_ns.into_iter())
            .chain(idents.type_ns.into_values().flatten())
            .chain(idents.value_ns.into_values().flatten())
            .collect::<Vec<_>>();

        all_items.sort_by(|a, b| {
            if a.parent_header.ident == b.parent_header.ident {
                a.loc.cmp(&b.loc)
            } else {
                let line_a = info.header_lines.get(&a.parent_header.ident).unwrap_or(&0);
                let line_b = info.header_lines.get(&b.parent_header.ident).unwrap_or(&0);
                if line_a != line_b {
                    line_a.cmp(line_b)
                } else {
                    a.parent_header.ident.as_str().cmp(&b.parent_header.ident.as_str())
                }
            }
        });

        let mut items: Vec<P<Item>> = Vec::new();
        let mut foreign_items: HashMap<Abi, Vec<P<ForeignItem>>> = HashMap::new();
        let mut last_item_mod = None;
        let mut last_foreign_item_mod = None;
        for item in all_items {
            let cur_mod_name = item.parent_header.ident;
            match item.kind {
                DeclKind::Item(i) => {
                    if last_item_mod != Some(cur_mod_name) {
                        st.add_comment(i.id, make_header_comment(last_item_mod, cur_mod_name));
                        last_item_mod = Some(cur_mod_name);
                    }
                    items.push(i);
                }
                DeclKind::ForeignItem(fi, abi) => {
                    if last_foreign_item_mod != Some(cur_mod_name) {
                        st.add_comment(
                            fi.id,
                            make_header_comment(last_foreign_item_mod, cur_mod_name),
                        );
                        last_foreign_item_mod = Some(cur_mod_name);
                    }
                    foreign_items.entry(abi).or_default().push(fi);
                }
            }
        }

        let foreign_mods = foreign_items
            .into_iter()
            .map(|(abi, items)| mk().extern_(abi).foreign_items(items));

        foreign_mods
            .chain(items.into_iter())
            .collect()
    }

    fn find_item<'b>(&'b mut self, item: &Item, namespace: Namespace) -> ContainsDecl<'b> {
        let ident = if let ItemKind::Use(tree) = &item.kind {
            tree.ident()
        } else {
            item.ident
        };
        assert!(ident.name != kw::Empty);

        if ident.as_str().contains("C2RustUnnamed") {
            for existing_decl in self.unnamed_items[namespace].iter_mut() {
                match &existing_decl.kind {
                    DeclKind::Item(existing_item) => match &existing_item.kind {
                        ItemKind::TyAlias(..)
                        | ItemKind::Struct(..)
                        | ItemKind::Union(..)
                        | ItemKind::Enum(..) => {
                            // Does the new item match the existing item, except
                            // for unnamed names?
                            if item.kind.unnamed_equiv(&existing_item.kind) {
                                return ContainsDecl::Equivalent(existing_decl);
                            }
                        }

                        // TODO?
                        _ => {}
                    },

                    DeclKind::ForeignItem(existing_foreign, _) => {
                        if let ForeignItemKind::TyAlias(_) = &existing_foreign.kind {
                            if foreign_equiv(&existing_foreign, &item) {
                                // This item is equivalent to an existing foreign item,
                                // modulo visibility.
                                return ContainsDecl::Equivalent(existing_decl);
                            }
                        }
                    }
                }
            }

            return ContainsDecl::NotContained;
        }

        if let Some(existing_decls) = self.idents[namespace].get_mut(&ident) {
            for existing_decl in existing_decls {
                match &existing_decl.kind {
                    DeclKind::Item(existing_item) => match (&existing_item.kind, &item.kind) {
                        // Replace a use with a real definition
                        (ItemKind::Use(..), _) => {
                            return ContainsDecl::Use(existing_decl);
                        }

                        // Make sure we use the widest visibility for this item
                        (_, ItemKind::Use(..)) => {
                            return ContainsDecl::Definition(existing_decl);
                        }

                        // Otherwise make sure these items are structurally
                        // equivalent.
                        _ => {
                            if self.cx.compatible_types(&item, &existing_item) {
                                return ContainsDecl::Equivalent(existing_decl);
                            }
                        }
                    }

                    DeclKind::ForeignItem(existing_foreign, _) => {
                        if let ItemKind::Use(..) = item.kind {
                            // If the import refers to an existing foreign item, do
                            // not replace it.
                            let path = self.cx.resolve_use_id(item.id);
                            if let Some(did) = path.res.opt_def_id() {
                                if let Some(Node::ForeignItem(_)) = self.cx.hir_map().get_if_local(did)
                                {
                                    return ContainsDecl::Definition(existing_decl);
                                }
                            }

                            return ContainsDecl::Equivalent(existing_decl);
                        }
                        if foreign_equiv(&existing_foreign, &item) {
                            return ContainsDecl::Equivalent(existing_decl);
                        }
                    }
                }
                trace!("{:?} and {:?} share idents, but are not compatible", item, existing_decl);
            }
        }

        ContainsDecl::NotContained
    }

    fn find_foreign_item<'b>(&'b mut self, item: &P<ForeignItem>, abi: Abi) -> ContainsDecl<'b> {
        let ns = match &item.kind {
            ForeignItemKind::Fn { .. } | ForeignItemKind::Static(..) => Namespace::ValueNS,
            ForeignItemKind::TyAlias(..) => Namespace::TypeNS,
            ForeignItemKind::MacCall(..) => unimplemented!(),
        };
        let ident = item.ident;
        assert!(ident.name != kw::Empty);

        if let Some(existing_decls) = self.idents[ns].get_mut(&ident) {
            for existing_decl in existing_decls {
                match &existing_decl.kind {
                    DeclKind::Item(existing_item) => {
                        if foreign_equiv(&item, &existing_item) {
                            return ContainsDecl::Equivalent(existing_decl)
                        } else if let ItemKind::Use(_) = existing_item.kind {
                            // A use takes precedence over a foreign declaration
                            // unless the use refers to the foreign declaration are
                            // attempting to insert.
                            let path = self.cx.resolve_use_id(existing_item.id);
                            if let Some(did) = path.res.opt_def_id() {
                                if let Some(Node::ForeignItem(_)) = self.cx.hir_map().get_if_local(did)
                                {
                                    return ContainsDecl::Definition(existing_decl);
                                }
                            }
                            return ContainsDecl::Equivalent(existing_decl);
                        }
                    }

                    DeclKind::ForeignItem(existing_foreign, existing_abi) => {
                        if *existing_abi != abi {
                            continue;
                        }
                        let matches_existing = match (&existing_foreign.kind, &item.kind) {
                            (ForeignItemKind::Fn(box Fn { sig: sig1, .. }),
                             ForeignItemKind::Fn(box Fn { sig: sig2, .. })) => {
                                self.cx.compatible_fn_prototypes(&sig1.decl, &sig2.decl)
                            }

                            _ => existing_foreign.ast_equiv(item),
                        };
                        if matches_existing {
                            return ContainsDecl::Equivalent(existing_decl);
                        }
                    }
                }
            }
        }

        ContainsDecl::NotContained
    }
}

enum ContainsDecl<'a> {
    NotContained,

    /// The module contains a declaration or definition that is equivalent to
    /// the given item.
    Equivalent(&'a mut MovedDecl),

    /// The module contains a definition that can replace the given item.
    Definition(&'a mut MovedDecl),

    /// The module contains a use of the given item.
    Use(&'a mut MovedDecl),
}

/// Returns true if the given ForeignItem can be a declaration for the given
/// Item definition.
fn foreign_equiv(foreign: &ForeignItem, item: &Item) -> bool {
    match (&foreign.kind, &item.kind) {
        // We should never encounter a foreign function with the same name but a
        // different declaration. Nonetheless, the FnDecls in rust might be
        // slightly different (param name, mutability), so we can't do an
        // ast_equiv on the FnDecl. Might be worth writing a custom comparison
        // for a sanity check, but not doing that right now.
        (ForeignItemKind::Fn { .. }, ItemKind::Fn { .. }) => true,

        (ForeignItemKind::Static(frn_ty, _frn_mutbl, _), ItemKind::Static(ty, _mutbl, _)) => {
            if frn_ty.ast_equiv(&ty) {
                return true;
            }

            match (&frn_ty.kind, &ty.kind) {
                // An extern array declaration of any length matches a concrete
                // definition if they have the same element type
                (TyKind::Array(frn_elem_ty, _), TyKind::Array(elem_ty, _)) => {
                    frn_elem_ty.ast_equiv(elem_ty)
                }
                _ => false,
            }
        }

        // If we have a definition for this type name we can assume it is
        // equivalent.
        (ForeignItemKind::TyAlias(..), ItemKind::TyAlias(..))
        | (ForeignItemKind::TyAlias(..), ItemKind::Enum(..))
        | (ForeignItemKind::TyAlias(..), ItemKind::Struct(..))
        | (ForeignItemKind::TyAlias(..), ItemKind::Union(..)) => true,

        _ => false,
    }
}

/// Check if the `Item` has the `#[header_src = "/some/path"]` attribute
fn has_source_header(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| is_c2rust_attr(attr, "header_src"))
}

/// Check if the `Item` has the `#[header_src = "/some/path"]` attribute
fn parse_source_header(attrs: &[Attribute]) -> Option<(String, usize)> {
    attrs.iter().find(|a| is_c2rust_attr(a, "header_src")).map(|attr| {
        let value_sym = attr
            .value_str()
            .expect("Expected a value for header_src attribute");
        let mut iter = value_sym.as_str().split(':');
        let path = iter
            .next()
            .expect("Expected a path in header_src attribute");
        let line: usize = iter
            .next()
            .and_then(|line| line.parse().ok())
            .expect("Expected an include line number in header_src attribute");
        (path.to_string(), line)
    })
}

fn is_nested(tree: &UseTree) -> bool {
    if let UseTreeKind::Nested(..) = &tree.kind {
        true
    } else {
        false
    }
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
