use rustc::hir::def::{DefKind, Res};
use rustc::hir::def_id::{DefId, CRATE_DEF_INDEX, LOCAL_CRATE};
use rustc::hir::Node;
use rustc::hir::{ForeignMod, Mod};
use rustc::ty::TyCtxt;
use syntax::ast::Ident;
use syntax_pos::symbol::Symbol;

fn push_hir_mod_children(tcx: TyCtxt, m: &Mod, children: &mut Vec<(Symbol, Res)>) {
    use rustc::hir::ItemKind::*;

    for &iid in &m.item_ids {
        let node = tcx.hir().get(iid.id);
        let item = expect!([node] Node::Item(i) => i);
        let item_did = tcx.hir().local_def_id(item.hir_id);

        match item.node {
            ForeignMod(ref fm) => {
                push_hir_foreign_mod_children(tcx, fm, children);
            }

            ExternCrate(..) => {
                let krate = tcx
                    .extern_mod_stmt_cnum(item_did)
                    .expect("no cnum available for `extern crate`");
                let krate_did = DefId {
                    krate,
                    index: CRATE_DEF_INDEX,
                };
                // This is a little bogus (the thing at `krate_did` isn't really a module), but it
                // works well enough.
                let krate_def = Res::Def(DefKind::Mod, krate_did);
                children.push((item.ident.name, krate_def));
            }

            // We could do something clever for `Use`, but it would be a little tricky in the
            // case where the `use` resolves to an extern crate's item.  For now we just use the
            // default logic, which omits the `use` (there is no `Def::Use` variant).
            _ => {
                if let Some(def) = tcx.def_kind(item_did) {
                    children.push((item.ident.name, Res::Def(def, item_did)));
                }
            }
        }
    }
}

fn push_hir_foreign_mod_children(tcx: TyCtxt, fm: &ForeignMod, children: &mut Vec<(Symbol, Res)>) {
    for fi in &fm.items {
        let did = tcx.hir().local_def_id(fi.hir_id);
        if let Some(def) = tcx.def_kind(did) {
            children.push((fi.ident.name, Res::Def(def, did)));
        }
    }
}

/// List the names and `Def`s of all children of the indicated module.
pub fn module_children(tcx: TyCtxt, did: DefId) -> Vec<(Symbol, Res)> {
    use rustc::hir::ItemKind::*;

    if did.krate == LOCAL_CRATE {
        if did.index == CRATE_DEF_INDEX {
            // Crate root needs some special handling.  Looking it up in the HIR map by DefId
            // fails, but we can grab its root `Mod` directly.
            let m = &tcx.hir().krate().module;
            let mut children = Vec::with_capacity(m.item_ids.len());
            push_hir_mod_children(tcx, m, &mut children);
            return children;
        }

        let node = tcx
            .hir()
            .get_if_local(did)
            .expect("definition not present in HIR map");
        let item = expect!([node] Node::Item(i) => i);

        match item.node {
            ExternCrate(..) => {
                let krate = tcx
                    .extern_mod_stmt_cnum(did)
                    .expect("no cnum available for `extern crate`");
                let krate_did = DefId {
                    krate,
                    index: CRATE_DEF_INDEX,
                };
                module_children(tcx, krate_did)
            }

            Use(ref path, _kind) => {
                module_children(tcx, path.res.def_id())
            }

            Mod(ref m) => {
                let mut children = Vec::with_capacity(m.item_ids.len());
                push_hir_mod_children(tcx, m, &mut children);
                children
            }

            ForeignMod(ref fm) => {
                let mut children = Vec::with_capacity(fm.items.len());
                push_hir_foreign_mod_children(tcx, fm, &mut children);
                children
            }

            ref it => panic!("item {:?} does not have resolvable children", it),
        }
    } else {
        let children = tcx.item_children(did);
        children.iter().map(|c| (c.ident.name, c.res)).collect()
    }
}

/// Resolve an absolute path to a `Def`.
pub fn resolve_absolute(tcx: TyCtxt, path: &[Ident]) -> Res {
    let krate_did = DefId {
        krate: LOCAL_CRATE,
        index: CRATE_DEF_INDEX,
    };
    let mut cur_def = Res::Def(DefKind::Mod, krate_did);

    'a: for ident in path {
        for (sym, def) in module_children(tcx, cur_def.def_id()) {
            if sym == ident.name {
                cur_def = def;
                continue 'a;
            }
        }

        panic!("could not find {:?} in {:?}", ident, cur_def);
    }

    cur_def
}
