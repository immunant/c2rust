use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use regex::Regex;
use rustc::hir::HirId;
use syntax::attr;
use syntax::ast::*;
use syntax::source_map::DUMMY_SP;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use smallvec::SmallVec;

use c2rust_ast_builder::{mk, Make, IntoSymbol};
use crate::ast_manip::{fold_nodes, Fold, AstEquiv};
use crate::command::{CommandState, Registry};
use crate::driver::{self, Phase};
use crate::path_edit::fold_resolved_paths;
use crate::transform::Transform;
use crate::RefactorCtxt;


/// # `rename_items_regex` Command
/// 
/// Usage: `rename_items_regex PAT REPL [FILTER]`
/// 
/// Marks: reads `FILTER`
/// 
/// Replace `PAT` (a regular expression) with `REPL` in all item names.  If `FILTER` is provided,
/// only items bearing the `FILTER` mark will be renamed.
pub struct RenameRegex {
    pattern: String,
    repl: String,
    filter: Option<Symbol>,
}

impl Transform for RenameRegex {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &RefactorCtxt) -> Crate {
        let re = Regex::new(&self.pattern).unwrap();

        // (1) Fold over items and rewrite their `ident`s.  Records the new paths of modified items
        // into `new_paths`.

        let mut new_idents = HashMap::new();
        let krate = fold_nodes(krate, |i: P<Item>| {
            if let Some(label) = self.filter {
                if !st.marked(i.id, label) {
                    return smallvec![i];
                }
            }

            let name = i.ident.name.as_str();
            let new_name = re.replace(&name, &self.repl as &str);
            if let Cow::Owned(new_name) = new_name {
                new_idents.insert(cx.hir_map().node_to_hir_id(i.id), mk().ident(&new_name));

                smallvec![i.map(|i| {
                    Item {
                        ident: mk().ident(&new_name),
                        .. i
                    }
                })]
            } else {
                smallvec![i]
            }
        });

        // (2) Rewrite paths referring to renamed defs

        let krate = fold_resolved_paths(krate, cx, |qself, mut path, def| {
            if let Some(hir_id) = cx.def_to_hir_id(def) {
                if let Some(new_ident) = new_idents.get(&hir_id) {
                    path.segments.last_mut().unwrap().ident = new_ident.clone();
                }
            }
            (qself, path)
        });

        krate
    }
}

/// # `rename_unnamed` Command
/// 
/// Usage: `rename_unnamed`
/// 
/// Renames all `Ident`s that have `unnamed` throughout the `Crate`, so the `Crate` can
/// have a completely unique naming scheme for Anonymous Types.
/// This command should be ran after transpiling using `c2rust-transpile`, and
/// is also mainly to be used when doing the `reorganize_definition` pass; although
/// this pass can run on any `c2rust-transpile`d project.
/// 
/// Example:
/// ```
/// pub mod foo {
///     pub struct unnamed {
///         a: i32
///     }
/// }
/// 
/// pub mod bar {
///     pub struct unnamed {
///         b: usize
///     }
/// }
/// ```
/// Becomes:
/// ```
/// pub mod foo {
///     pub struct unnamed {
///         a: i32
///     }
/// }
/// 
/// pub mod bar {
///     pub struct unnamed_1 {
///         b: usize
///     }
/// }
/// ```
pub struct RenameUnnamed;

impl Transform for RenameUnnamed {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &RefactorCtxt) -> Crate {
        #[derive(Debug, Default)]
        struct Renamer {
            items_to_change: HashSet<NodeId>,
            new_idents: HashMap<HirId, Ident>,
            new_to_old: HashMap<Ident, Ident>,
            is_source: bool,
        }
        let mut renamer: Renamer = Default::default();
        let mut counter: usize = 0;

        let has_unnamed = |ident: &Ident| { ident.as_str().contains("unnamed") };
        let make_name = |counter| { Ident::from_str(&format!("unnamed_{}", counter)) };

        // 1. Rename Anonymous types to the unique Ident
        let krate = fold_nodes(krate, |i: P<Item>| {
            if attr::contains_name(&i.attrs, "header_src") && !renamer.is_source {
                renamer.is_source = true;
            }

            let is_module = match i.node {
                ItemKind::Mod(..) => true,
                _ => false,
            };

            if !has_unnamed(&i.ident) || is_module {
                return smallvec![i];
            }

            let new_name = make_name(counter);

            renamer
                .new_idents
                .insert(cx.hir_map().node_to_hir_id(i.id), new_name);
            renamer.new_to_old.insert(new_name, i.ident);
            counter += 1;
            smallvec![i.map(|i| Item {
                ident: new_name,
                ..i
            })]
        });

        // 2. Update types to match the new renamed Anonymous Types
        let krate = fold_resolved_paths(krate, cx, |qself, mut path, def| {
            if let Some(hir_id) = cx.def_to_hir_id(def) {
                if let Some(new_ident) = renamer.new_idents.get(&hir_id) {
                    path.segments.last_mut().unwrap().ident = new_ident.clone();
                }
            }
            (qself, path)
        });

        // No need to update paths if the project wasn't transpiled
        // with `--reorganize-definitions` flag
        if !renamer.is_source {
            return krate;
        }

        // 3. Update paths to from the old AnonymousType `Ident` to the new AnonymousType `Ident`
        let krate = fold_nodes(krate, |mut i: P<Item>| {
            // This pass is only intended to be ran when the `--reorganize-definition` flag is used
            // on `c2rust-transpile`, and the reason is due to having use statements importing
            // `Item`s within submodules (also the only time the `c2rust-transpile`r uses use
            // statements).
            match i.node {
                ItemKind::Mod(ref mut outer_mod) => {

                    // What the transpiler does is, separate items into their modules based on
                    // source location, we need to gather the changed ident so paths can be
                    // correctly updated.

                    // This map holds the module name that will be used as a check in the path,
                    // and a mapping between the old ident and the new one
                    // mod_ident -> <old_unnamed_ident -> new_unnamed_ident>
                    let mut mod_to_old_idents = HashMap::new();
                    for outer_item in &outer_mod.items {
                        // populate mod_to_old_idents
                        match outer_item.node {
                            ItemKind::Mod(ref inner_mod) => {
                                let mut old_idents: HashMap<Ident, Ident> = HashMap::new();
                                // iterate through and find all occurences of `unnamed` within this
                                // inner module

                                // TODO: if a module `foo_h` defines an Item with an ident
                                // of `bar_h`, and there is also a module with that same ident.
                                // That may cause some issues
                                for inner_item in &inner_mod.items {
                                    if let Some(old_ident) =
                                        renamer.new_to_old.get(&inner_item.ident)
                                    {
                                        old_idents.insert(*old_ident, inner_item.ident);
                                    }
                                }
                                mod_to_old_idents.insert(outer_item.ident, old_idents);
                            }
                            _ => {}
                        }
                    }

                    // Iterate through the items and locate use statements
                    for outer_item in &mut outer_mod.items {
                        match outer_item.node {
                            ItemKind::Use(ref mut ut) => {
                                let mut old_idents = HashMap::new();
                                for segment in &ut.prefix.segments {
                                    if let Some(map) = mod_to_old_idents.get(&segment.ident) {
                                        old_idents = map.clone();
                                    }
                                }
                                if !old_idents.is_empty() {
                                    match ut.kind {
                                        // Change paths that look like:
                                        // use self::module::{unnamed, unnamed_0};
                                        //
                                        // unnamed -> unnamed_12 && unnamed_0 -> unnamed_13
                                        // use self::module::{unnamed_12, unnamed_13};
                                        UseTreeKind::Nested(ref mut use_trees) => {
                                            for (use_tree, _) in use_trees.iter_mut() {
                                                if let Some(new_ident) =
                                                    old_idents.get(&use_tree.ident())
                                                {
                                                    let new_path =
                                                        mk().path(Path::from_ident(*new_ident));
                                                    use_tree.prefix = new_path;
                                                }
                                            }
                                        }
                                        // Update simple paths:
                                        // use self::module::unnamed_0;
                                        //
                                        // unnamed_0 -> unnamed_17
                                        // use self::module::unnamed_17;
                                        UseTreeKind::Simple(..) => {
                                            // Iterate through each segment until an unchanged unnamed
                                            // is found
                                            for segment in &mut ut.prefix.segments {
                                                if let Some(new_ident) = old_idents.get(&segment.ident)
                                                {
                                                    segment.ident = *new_ident;
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
            smallvec![i]
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `replace_items` Command
/// 
/// Usage: `replace_items`
/// 
/// Marks: `target`, `repl`
/// 
/// Replace all uses of items marked `target` with reference to the item marked
/// `repl`, then remove all `target` items.
pub struct ReplaceItems;

impl Transform for ReplaceItems {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &RefactorCtxt) -> Crate {
        // (1) Scan items for `target` and `repl` marks, collecting the relevant `DefId`s and
        // removing all `target` items.

        let mut target_ids = HashSet::new();
        let mut repl_id = None;

        // (1a) Top-level items
        let krate = fold_nodes(krate, |i: P<Item>| {
            if st.marked(i.id, "repl") {
                if repl_id.is_none() {
                    repl_id = Some(cx.node_def_id(i.id));
                } else {
                    panic!("found multiple `repl` items");
                }
            }

            if st.marked(i.id, "target") {
                target_ids.insert(cx.node_def_id(i.id));
                smallvec![]
            } else {
                smallvec![i]
            }
        });

        // (1b) Impl items
        // TODO: Only inherent impls are supported for now.  May not work on trait impls.
        let krate = fold_nodes(krate, |i: ImplItem| {
            if st.marked(i.id, "repl") {
                if repl_id.is_none() {
                    repl_id = Some(cx.node_def_id(i.id));
                } else {
                    panic!("found multiple `repl` items");
                }
            }

            if st.marked(i.id, "target") {
                target_ids.insert(cx.node_def_id(i.id));
                smallvec![]
            } else {
                smallvec![i]
            }
        });

        let repl_id = repl_id.expect("found no `repl` item");

        // (2) Rewrite references to `target` items to refer to `repl` instead.

        let krate = fold_resolved_paths(krate, cx, |qself, path, def| {
            match def.opt_def_id() {
                Some(def_id) if target_ids.contains(&def_id) =>
                    (None, cx.def_path(repl_id)),
                _ => (qself, path),
            }
        });

        // (3) Find impls for `target` types, and remove them.  This way, if a struct is removed,
        // we also remove the associated `Clone` impl.

        let krate = fold_nodes(krate, |i: P<Item>| {
            let opt_def_id = match i.node {
                ItemKind::Impl(_, _, _, _, _, ref ty, _) => cx.try_resolve_ty(ty),
                _ => None,
            };

            if let Some(def_id) = opt_def_id {
                if target_ids.contains(&def_id) {
                    return smallvec![];
                }
            }
            smallvec![i]
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `set_visibility` Command
/// 
/// Usage: `set_visibility VIS`
/// 
/// Marks: `target`
/// 
/// Set the visibility of all items marked `target` to `VIS`.  `VIS` is a Rust
/// visibility qualifier such as `pub`, `pub(crate)`, or the empty string.
/// 
/// Doesn't handle struct field visibility, for now.
pub struct SetVisibility {
    vis_str: String,
}

impl Transform for SetVisibility {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &RefactorCtxt) -> Crate {
        let vis = driver::run_parser(cx.session(), &self.vis_str,
                                     |p| p.parse_visibility(false));

        struct SetVisFolder<'a> {
            st: &'a CommandState,
            vis: Visibility,

            /// `true` when the closest enclosing item is a trait impl (not an inherent impl).
            /// This matters for the ImplItem case because trait impl items don't have visibility.
            in_trait_impl: bool,
        }

        impl<'a> Folder for SetVisFolder<'a> {
            fn fold_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                if self.st.marked(i.id, "target") && !i.vis.ast_equiv(&self.vis) {
                    i = i.map(|mut i| {
                        i.vis = self.vis.clone();
                        i
                    });
                }

                let was_in_trait_impl = self.in_trait_impl;
                self.in_trait_impl = matches!([i.node]
                        ItemKind::Impl(_, _, _, _, Some(_), _, _));
                let r = fold::noop_fold_item(i, self);
                self.in_trait_impl = was_in_trait_impl;

                r
            }

            fn fold_impl_item(&mut self, mut i: ImplItem) -> SmallVec<[ImplItem; 1]> {
                if self.in_trait_impl {
                    return fold::noop_fold_impl_item(i, self);
                }

                if self.st.marked(i.id, "target") {
                    i.vis = self.vis.clone();
                }
                fold::noop_fold_impl_item(i, self)
            }

            fn fold_foreign_item(&mut self, mut i: ForeignItem) -> SmallVec<[ForeignItem; 1]> {
                if self.st.marked(i.id, "target") {
                    i.vis = self.vis.clone();
                }
                fold::noop_fold_foreign_item(i, self)
            }

            // Trait items have no visibility.
        }

        krate.fold(&mut SetVisFolder { st, vis, in_trait_impl: false })
    }
}


/// # `set_mutability` Command
/// 
/// Usage: `set_mutability MUT`
/// 
/// Marks: `target`
/// 
/// Set the mutability of all items marked `target` to `MUT`.  `MUT` is either
/// `imm` or `mut`.  This command only affects `static` items (including extern statics).
pub struct SetMutability {
    mut_str: String,
}

impl Transform for SetMutability {
    fn transform(&self, krate: Crate, st: &CommandState, _cx: &RefactorCtxt) -> Crate {
        let mutbl = <&str as Make<Mutability>>::make(&self.mut_str, &mk());

        struct SetMutFolder<'a> {
            st: &'a CommandState,
            mutbl: Mutability,
        }

        impl<'a> Folder for SetMutFolder<'a> {
            fn fold_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                if self.st.marked(i.id, "target") {
                    i = i.map(|mut i| {
                        match i.node {
                            ItemKind::Static(_, ref mut mutbl, _) => *mutbl = self.mutbl,
                            _ => {},
                        }
                        i
                    });
                }
                fold::noop_fold_item(i, self)
            }

            fn fold_foreign_item(&mut self, mut i: ForeignItem) -> SmallVec<[ForeignItem; 1]> {
                if self.st.marked(i.id, "target") {
                    match i.node {
                        ForeignItemKind::Static(_, ref mut is_mutbl) =>
                            *is_mutbl = self.mutbl == Mutability::Mutable,
                        _ => {},
                    }
                }
                fold::noop_fold_foreign_item(i, self)
            }
        }

        krate.fold(&mut SetMutFolder { st, mutbl })
    }
}


/// Set unsafety of all marked items.
pub struct SetUnsafety {
    unsafe_str: String,
}

impl Transform for SetUnsafety {
    fn transform(&self, krate: Crate, st: &CommandState, _cx: &RefactorCtxt) -> Crate {
        let unsafety = <&str as Make<Unsafety>>::make(&self.unsafe_str, &mk());

        struct SetUnsafetyFolder<'a> {
            st: &'a CommandState,
            unsafety: Unsafety,
        }

        impl<'a> Folder for SetUnsafetyFolder<'a> {
            fn fold_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                if self.st.marked(i.id, "target") {
                    i = i.map(|mut i| {
                        match i.node {
                            ItemKind::Fn(_, ref mut header, _, _) =>
                                header.unsafety = self.unsafety,
                            ItemKind::Trait(_, ref mut unsafety, _, _, _) =>
                                *unsafety = self.unsafety,
                            ItemKind::Impl(ref mut unsafety, _, _, _, _, _, _) =>
                                *unsafety = self.unsafety,
                            _ => {},
                        }
                        i
                    });
                }
                fold::noop_fold_item(i, self)
            }

            fn fold_trait_item(&mut self, mut i: TraitItem) -> SmallVec<[TraitItem; 1]> {
                if self.st.marked(i.id, "target") {
                    match i.node {
                        TraitItemKind::Method(ref mut sig, _) =>
                            sig.header.unsafety = self.unsafety,
                        _ => {},
                    }
                }
                fold::noop_fold_trait_item(i, self)
            }

            fn fold_impl_item(&mut self, mut i: ImplItem) -> SmallVec<[ImplItem; 1]> {
                if self.st.marked(i.id, "target") {
                    match i.node {
                        ImplItemKind::Method(ref mut sig, _) =>
                            sig.header.unsafety = self.unsafety,
                        _ => {},
                    }
                }
                fold::noop_fold_impl_item(i, self)
            }
        }

        krate.fold(&mut SetUnsafetyFolder { st, unsafety })
    }
}


/// # `create_item` Command
/// 
/// Usage: `create_item ITEMS <inside/after> [MARK]`
/// 
/// Marks: `MARK`/`target`
/// 
/// Parse `ITEMS` as item definitions, and insert the parsed items either `inside` (as the first
/// child) or `after` (as a sibling) of the AST node bearing `MARK` (default: `target`).  Supports
/// adding items to both `mod`s and blocks.
/// 
/// Note that other itemlikes, such as impl and trait items, are not handled by this command.
pub struct CreateItem {
    header: String,
    pos: String,
    mark: Symbol,
}

impl Transform for CreateItem {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &RefactorCtxt) -> Crate {
        let mark = self.mark;

        let inside = match &self.pos as &str {
            "inside" => true,
            "after" => false,
            _ => panic!("expected position to be 'inside' or 'after'"),
        };

        let items = st.parse_items(cx, &format!("{}", self.header));

        for i in &items {
            st.add_mark(i.id, "new");
        }


        struct CreateFolder<'a> {
            st: &'a CommandState,
            mark: Symbol,
            inside: bool,
            items: Vec<P<Item>>,
        }

        impl<'a> CreateFolder<'a> {
            fn handle_mod(&mut self, parent_id: NodeId, m: Mod, skip_dummy: bool) -> Mod {
                let mut items = Vec::with_capacity(m.items.len());

                // When true, insert before the next item that satisfies `skip_dummy`
                let mut insert_inside = self.inside && self.st.marked(parent_id, self.mark);

                for i in m.items {
                    if insert_inside {
                        // Special case for `inside` mode with the Crate marked.  We want to insert
                        // after the injected std and prelude items, because inserting before an
                        // item with `DUMMY_SP` confuses sequence rewriting.
                        if !skip_dummy || i.span != DUMMY_SP {
                            items.extend(self.items.iter().cloned());
                            insert_inside = false;
                        }
                    }

                    let insert = !self.inside && self.st.marked(i.id, self.mark);
                    items.push(i);
                    if insert {
                        items.extend(self.items.iter().cloned());
                    }
                }

                if insert_inside {
                    // There were no acceptable items, so add it at the end.
                    items.extend(self.items.iter().cloned());
                }

                Mod { items, ..m }
            }
        }

        impl<'a> Folder for CreateFolder<'a> {
            fn fold_crate(&mut self, c: Crate) -> Crate {
                let c = Crate {
                    module: self.handle_mod(CRATE_NODE_ID, c.module, true),
                    ..c
                };

                // We do this instead of noop_fold_module, because noop_fold_crate makes up a dummy
                // Item for the crate, causing us to try and insert into c.module a second time.
                // (We don't just omit fold_crate and rely on this dummy item because the dummy
                // item has DUMMY_NODE_ID instead of CRATE_NODE_ID.)
                Crate {
                    module: fold::noop_fold_mod(c.module, self),
                    ..c
                }
            }

            fn fold_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                let i = if !matches!([i.node] ItemKind::Mod(..)) {
                    i
                } else {
                    i.map(|i| {
                        unpack!([i.node] ItemKind::Mod(m));
                        Item {
                            node: ItemKind::Mod(self.handle_mod(i.id, m, false)),
                            .. i
                        }
                    })
                };
                fold::noop_fold_item(i, self)
            }

            fn fold_block(&mut self, b: P<Block>) -> P<Block> {
                let b = b.map(|b| {
                    let mut stmts = Vec::with_capacity(b.stmts.len());

                    if self.inside && self.st.marked(b.id, self.mark) {
                        stmts.extend(self.items.iter().cloned().map(|i| mk().item_stmt(i)));
                    }

                    for s in b.stmts {
                        let insert = !self.inside && self.st.marked(s.id, self.mark);
                        stmts.push(s);
                        if insert {
                            stmts.extend(self.items.iter().cloned().map(|i| mk().item_stmt(i)));
                        }
                    }

                    Block { stmts, .. b }
                });
                fold::noop_fold_block(b, self)
            }

            fn fold_mac(&mut self, mac: Mac) -> Mac {
                fold::noop_fold_mac(mac, self)
            }
        }

        krate.fold(&mut CreateFolder { st, mark, inside, items })
    }
}


/// # `delete_items` Command
/// 
/// Usage: `delete_items`
/// 
/// Marks: `target`
/// 
/// Delete all items marked `target` from the AST.  This handles items in both `mod`s and blocks,
/// but doesn't handle other itemlikes.
pub struct DeleteItems;

impl Transform for DeleteItems {
    fn transform(&self, krate: Crate, st: &CommandState, _cx: &RefactorCtxt) -> Crate {
        let mark = "target".into_symbol();

        struct DeleteFolder<'a> {
            st: &'a CommandState,
            mark: Symbol,
        }

        impl<'a> Folder for DeleteFolder<'a> {
            fn fold_mod(&mut self, mut m: Mod) -> Mod {
                m.items.retain(|i| !self.st.marked(i.id, self.mark));
                fold::noop_fold_mod(m, self)
            }

            fn fold_block(&mut self, b: P<Block>) -> P<Block> {
                let b = b.map(|mut b| {
                    b.stmts.retain(|s| match s.node {
                        StmtKind::Item(ref i) => !self.st.marked(i.id, self.mark),
                        _ => true,
                    });
                    b
                });
                fold::noop_fold_block(b, self)
            }
        }

        krate.fold(&mut DeleteFolder { st, mark })
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("rename_items_regex", |args| mk(RenameRegex {
        pattern: args[0].clone(),
        repl: args[1].clone(),
        filter: args.get(2).map(|x| (x as &str).into_symbol()),
    }));

    reg.register("rename_unnamed", |_args| mk(RenameUnnamed));

    reg.register("replace_items", |_args| mk(ReplaceItems));

    reg.register("set_visibility", |args| mk(SetVisibility {
        vis_str: args[0].clone(),
    }));

    reg.register("set_mutability", |args| mk(SetMutability {
        mut_str: args[0].clone(),
    }));

    reg.register("set_unsafety", |args| mk(SetUnsafety {
        unsafe_str: args[0].clone(),
    }));

    reg.register("create_item", |args| mk(CreateItem {
        header: args[0].clone(),
        pos: args[1].clone(),
        mark: args.get(2).map(|s| (s as &str).into_symbol())
            .unwrap_or_else(|| "target".into_symbol()),
    }));

    reg.register("delete_items", |_args| mk(DeleteItems));
}

