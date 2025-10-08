use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use regex::Regex;
use rustc_hir::HirId;
use rustc_parse::parser::FollowedByType;
use rustc_ast::*;
use rustc_span::source_map::DUMMY_SP;
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_span::symbol::{Ident, Symbol};
use smallvec::{smallvec, SmallVec};

use crate::ast_builder::{mk, Make, IntoSymbol};
use crate::ast_manip::{FlatMapNodes, MutVisit, AstEquiv};
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
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let re = Regex::new(&self.pattern).unwrap();

        // (1) Fold over items and rewrite their `ident`s.  Records the new paths of modified items
        // into `new_paths`.

        let mut new_idents = HashMap::new();
        FlatMapNodes::visit(krate, |i: P<Item>| {
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

        fold_resolved_paths(krate, cx, |qself, mut path, def| {
            if let Some(hir_id) = cx.res_to_hir_id(&def[0]) {
                if let Some(new_ident) = new_idents.get(&hir_id) {
                    path.segments.last_mut().unwrap().ident = *new_ident;
                }
            }
            (qself, path)
        });
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
/// ```ignore
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
/// ```ignore
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
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        #[derive(Debug, Default)]
        struct Renamer {
            new_idents: HashMap<HirId, Ident>,
            new_to_old: HashMap<Ident, Ident>,
        }
        let mut renamer: Renamer = Default::default();
        let mut counter: usize = 0;

        let has_unnamed = |ident: &Ident| { ident.as_str().contains("C2RustUnnamed") };
        let make_name = |counter| { Ident::from_str(&format!("C2RustUnnamed_{}", counter)) };

        // 1. Rename Anonymous types to the unique Ident
        FlatMapNodes::visit(krate, |i: P<Item>| {
            let is_module = match i.kind {
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
        fold_resolved_paths(krate, cx, |qself, mut path, def| {
            if let Some(hir_id) = cx.res_to_hir_id(&def[0]) {
                if let Some(new_ident) = renamer.new_idents.get(&hir_id) {
                    path.segments.last_mut().unwrap().ident = *new_ident;
                }
            }
            (qself, path)
        });
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
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Scan items for `target` and `repl` marks, collecting the relevant `DefId`s and
        // removing all `target` items.

        let mut target_ids = HashSet::new();
        let mut repl_id = None;

        // (1a) Top-level items
        FlatMapNodes::visit(krate, |i: P<Item>| {
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
        // TODO: Re-enable this once MutVisit can distinguish between ItemImpl and TraitImpl.
        //FlatMapNodes::visit(krate, |i: AssocItem| {
        //    if st.marked(i.id, "repl") {
        //        if repl_id.is_none() {
        //            repl_id = Some(cx.node_def_id(i.id));
        //        } else {
        //            panic!("found multiple `repl` items");
        //        }
        //    }

        //    if st.marked(i.id, "target") {
        //        target_ids.insert(cx.node_def_id(i.id));
        //        smallvec![]
        //    } else {
        //        smallvec![i]
        //    }
        //});

        let repl_id = repl_id.expect("found no `repl` item");

        // (2) Rewrite references to `target` items to refer to `repl` instead.

        fold_resolved_paths(krate, cx, |qself, path, def| {
            match def[0].opt_def_id() {
                Some(def_id) if target_ids.contains(&def_id) =>
                    (None, cx.def_path(repl_id)),
                _ => (qself, path),
            }
        });

        // (3) Find impls for `target` types, and remove them.  This way, if a struct is removed,
        // we also remove the associated `Clone` impl.

        FlatMapNodes::visit(krate, |i: P<Item>| {
            let opt_def_id = match i.kind {
                ItemKind::Impl(box Impl { ref self_ty, .. }) => cx.try_resolve_ty(self_ty),
                _ => None,
            };

            if let Some(def_id) = opt_def_id {
                if target_ids.contains(&def_id) {
                    return smallvec![];
                }
            }
            smallvec![i]
        });
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
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let vis = driver::run_parser(cx.session(), &self.vis_str,
                                     |p| p.parse_visibility(FollowedByType::No));

        struct SetVisFolder<'a> {
            st: &'a CommandState,
            vis: Visibility,

            /// `true` when the closest enclosing item is a trait impl (not an inherent impl).
            /// This matters for the AssocItem case because trait impl items don't have visibility.
            in_trait_impl: bool,
        }

        impl<'a> MutVisitor for SetVisFolder<'a> {
            fn flat_map_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                if self.st.marked(i.id, "target") && !i.vis.ast_equiv(&self.vis) {
                    i = i.map(|mut i| {
                        i.vis = self.vis.clone();
                        i
                    });
                }

                let was_in_trait_impl = self.in_trait_impl;
                self.in_trait_impl = crate::matches!([i.kind]
                        ItemKind::Impl(box Impl { of_trait: Some(_), .. }));
                let r = mut_visit::noop_flat_map_item(i, self);
                self.in_trait_impl = was_in_trait_impl;

                r
            }

            fn flat_map_impl_item(&mut self, mut i: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
                if self.in_trait_impl {
                    return mut_visit::noop_flat_map_assoc_item(i, self);
                }

                if self.st.marked(i.id, "target") {
                    i.vis = self.vis.clone();
                }
                mut_visit::noop_flat_map_assoc_item(i, self)
            }

            fn flat_map_foreign_item(&mut self, mut i: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
                if self.st.marked(i.id, "target") {
                    i.vis = self.vis.clone();
                }
                mut_visit::noop_flat_map_foreign_item(i, self)
            }

            // Trait items have no visibility.
        }

        krate.visit(&mut SetVisFolder { st, vis, in_trait_impl: false })
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
    fn transform(&self, krate: &mut Crate, st: &CommandState, _cx: &RefactorCtxt) {
        let mutbl = <&str as Make<Mutability>>::make(&self.mut_str, &mk());

        struct SetMutFolder<'a> {
            st: &'a CommandState,
            mutbl: Mutability,
        }

        impl<'a> MutVisitor for SetMutFolder<'a> {
            fn flat_map_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                if self.st.marked(i.id, "target") {
                    i = i.map(|mut i| {
                        match i.kind {
                            ItemKind::Static(_, ref mut mutbl, _) => *mutbl = self.mutbl,
                            _ => {},
                        }
                        i
                    });
                }
                mut_visit::noop_flat_map_item(i, self)
            }

            fn flat_map_foreign_item(&mut self, mut i: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
                if self.st.marked(i.id, "target") {
                    match i.kind {
                        ForeignItemKind::Static(_, ref mut is_mutbl, _) =>
                            *is_mutbl = self.mutbl,
                        _ => {},
                    }
                }
                mut_visit::noop_flat_map_foreign_item(i, self)
            }
        }

        krate.visit(&mut SetMutFolder { st, mutbl })
    }
}


/// Set unsafety of all marked items.
pub struct SetUnsafety {
    unsafe_str: String,
}

impl Transform for SetUnsafety {
    fn transform(&self, krate: &mut Crate, st: &CommandState, _cx: &RefactorCtxt) {
        let unsafety = <&str as Make<Unsafe>>::make(&self.unsafe_str, &mk());

        struct SetUnsafetyFolder<'a> {
            st: &'a CommandState,
            unsafety: Unsafe,
        }

        impl<'a> MutVisitor for SetUnsafetyFolder<'a> {
            fn flat_map_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                if self.st.marked(i.id, "target") {
                    i = i.map(|mut i| {
                        match i.kind {
                            ItemKind::Fn(box Fn { ref mut sig, .. }) =>
                                sig.header.unsafety = self.unsafety,
                            ItemKind::Trait(box Trait { ref mut unsafety, .. }) =>
                                *unsafety = self.unsafety,
                            ItemKind::Impl(box Impl { ref mut unsafety, .. }) =>
                                *unsafety = self.unsafety,
                            _ => {},
                        }
                        i
                    });
                }
                mut_visit::noop_flat_map_item(i, self)
            }

            fn flat_map_trait_item(&mut self, mut i: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
                if self.st.marked(i.id, "target") {
                    match i.kind {
                        AssocItemKind::Fn(box Fn { ref mut sig, .. }) =>
                            sig.header.unsafety = self.unsafety,
                        _ => {},
                    }
                }
                mut_visit::noop_flat_map_assoc_item(i, self)
            }
        }

        krate.visit(&mut SetUnsafetyFolder { st, unsafety })
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
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
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
            fn handle_mod(&mut self, parent_id: NodeId, m_items: &mut Vec<P<Item>>, skip_dummy: bool) {
                let mut items = Vec::with_capacity(m_items.len());

                // When true, insert before the next item that satisfies `skip_dummy`
                let mut insert_inside = self.inside && self.st.marked(parent_id, self.mark);

                for i in &m_items[..] {
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
                    items.push(i.clone());
                    if insert {
                        items.extend(self.items.iter().cloned());
                    }
                }

                if insert_inside {
                    // There were no acceptable items, so add it at the end.
                    items.extend(self.items.iter().cloned());
                }

                *m_items = items;
            }
        }

        impl<'a> MutVisitor for CreateFolder<'a> {
            fn visit_crate(&mut self, c: &mut Crate) {
                self.handle_mod(CRATE_NODE_ID, &mut c.items, true);
                mut_visit::noop_visit_crate(c, self);
            }

            fn flat_map_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
                let id = i.id;
                if let ItemKind::Mod(_, ModKind::Loaded(items, ..)) = &mut i.kind {
                    self.handle_mod(id, items, false);
                }
                mut_visit::noop_flat_map_item(i, self)
            }

            fn visit_block(&mut self, b: &mut P<Block>) {
                let mut stmts = Vec::with_capacity(b.stmts.len());

                if self.inside && self.st.marked(b.id, self.mark) {
                    stmts.extend(self.items.iter().cloned().map(|i| mk().item_stmt(i)));
                }

                for s in &b.stmts {
                    let insert = !self.inside && self.st.marked(s.id, self.mark);
                    stmts.push(s.clone());
                    if insert {
                        stmts.extend(self.items.iter().cloned().map(|i| mk().item_stmt(i)));
                    }
                }
                b.stmts = stmts;

                mut_visit::noop_visit_block(b, self)
            }

            fn visit_mac_call(&mut self, mac: &mut MacCall) {
                mut_visit::noop_visit_mac(mac, self)
            }
        }

        krate.visit(&mut CreateFolder { st, mark, inside, items })
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
    fn transform(&self, krate: &mut Crate, st: &CommandState, _cx: &RefactorCtxt) {
        let mark = "target".into_symbol();

        struct DeleteFolder<'a> {
            st: &'a CommandState,
            mark: Symbol,
        }

        impl<'a> MutVisitor for DeleteFolder<'a> {
            fn visit_crate(&mut self, c: &mut Crate) {
                c.items.retain(|i| !self.st.marked(i.id, self.mark));
                mut_visit::noop_visit_crate(c, self);
            }

            fn visit_item_kind(&mut self, i: &mut ItemKind) {
                if let ItemKind::Mod(_, ModKind::Loaded(items, ..)) = i {
                    items.retain(|i| !self.st.marked(i.id, self.mark));
                }
                mut_visit::noop_visit_item_kind(i, self)
            }

            fn visit_block(&mut self, b: &mut P<Block>) {
                b.stmts.retain(|s| match s.kind {
                    StmtKind::Item(ref i) => !self.st.marked(i.id, self.mark),
                    _ => true,
                });
                mut_visit::noop_visit_block(b, self)
            }
        }

        krate.visit(&mut DeleteFolder { st, mark })
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

