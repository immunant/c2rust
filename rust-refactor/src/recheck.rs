//! Transformations for putting a post-expansion/resolution ("phase 2") AST into a state that's
//! suitable for running expansion/resolution a second time.
//!
//! There are a few things we have to do here:
//!
//!  1. Delete the injected `std` and `prelude` imports, as rerunning phase_2 will inject them
//!     again.
//!  2. Replace all `NodeId`s with `DUMMY_NODE_ID`.  `NodeId` assignment will panic if it sees that
//!     an ID was already assigned.
//!  3. Replace all `$crate`-equivalents with paths to the appropriate `extern crate`.  Since the
//!     AST is already macro-expanded, running it through phase_2 again will leave `rustc_resolve`
//!     without the macro expansion data it needs to process `$crate`-relative paths.

use std::collections::HashSet;
use rustc::hir::def_id::LOCAL_CRATE;
use rustc::hir::map as hir_map;
use rustc::middle::cstore::CrateStore;
use rustc_metadata::cstore::CStore;
use syntax::ast::*;
use syntax::attr;
use syntax::fold::{self, Folder};
use syntax::symbol::keywords;
use syntax_pos::hygiene::{Mark, MarkKind};

use api::*;
use driver;
use util::IntoSymbol;


/// Rewrite paths that start with `$crate` or `keywords::CrateRoot` so that they work in the
/// absence of macro expansion data.
///
/// Currently we simply replace a `$crate` referring to crate `foo` with the absolute path `::foo`.
/// This should work in most idiomatic Rust code, which places an `extern crate foo;` at the root
/// of the crate.
///
/// Eventually this will all stop working for one reason or another.  We may start needing to
/// handle macros that refer to private items inside their originating crates, or we might start
/// seeing Rust 2018+ crates that omit `extern crate` declarations entirely.  In any case, the
/// right fix is to figure out a way to pass info about macro expansions from the first phase_2
/// (where macro expansion actually happened) to the second phase_2.  Then we won't need this pass
/// at all.
struct ResolveCrateFolder<'a, 'tcx: 'a> {
    cstore: &'a CStore,
    hir_map: &'a hir_map::Map<'tcx>,
}

impl<'a, 'tcx> Folder for ResolveCrateFolder<'a, 'tcx> {
    fn fold_path(&mut self, mut p: Path) -> Path {
        // There are a few ways of referring to the current crate: [CrateRoot], crate,
        // [CrateRoot]::crate, $crate.  We handle all of them here by peeling off as many
        // [CrateRoot]/crate/$crate tokens as we can find at the front of the path.

        let mut prefix = 0;
        let mut legacy = false;
        while prefix < p.segments.len() {
            let name = p.segments[prefix].ident.name;
            if name == keywords::CrateRoot.name() ||
               name == keywords::Crate.name() {
                prefix += 1;
            } else if name == keywords::DollarCrate.name() {
                prefix += 1;
                legacy = true;
            } else {
                break;
            }
        }
        if prefix == 0 {
            return p;
        }

        // Figure out what this `$crate` refers to.  This essentially replicates the logic of
        // `rustc_resolve::Resolver::resolve_crate_root`.
        let ctxt = p.segments[0].ident.span.ctxt();
        let opt_mark = if legacy {
            ctxt.marks().into_iter().find(|m| m.kind() != MarkKind::Modern)
        } else {
            ctxt.modern().adjust(Mark::root())
        };
        let mark = match opt_mark {
            Some(m) => m,
            None => {
                return p;
            },
        };
        let macro_did = self.hir_map.definitions().macro_def_scope(mark);
        let target_cnum = macro_did.krate;

        if target_cnum == LOCAL_CRATE {
            // This path is rooted in the local crate.  We can just scrub off the `SyntaxCtxt`s and
            // resolution should have no problems.
            for seg in &mut p.segments[..prefix] {
                seg.ident = Ident::with_empty_ctxt(seg.ident.name);
            }
            return p;
        }

        // The path points to an external crate.  We replace the prefix with what is hopefully a
        // path to the appropriate `extern crate`.
        let mut new_segs = Vec::with_capacity(p.segments.len() - prefix + 2);
        new_segs.push(mk().path_segment(keywords::CrateRoot));
        new_segs.push(mk().path_segment(self.cstore.crate_name_untracked(target_cnum)));
        new_segs.extend(p.segments.into_iter().skip(prefix));
        p.segments = new_segs;
        p
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}


struct ResetNodeIdFolder;
impl Folder for ResetNodeIdFolder {
    fn new_id(&mut self, _i: NodeId) -> NodeId { DUMMY_NODE_ID }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}


fn remove_injected_std(mut krate: Crate) -> Crate {
    let mut remove_names = HashSet::new();
    // Mirrors the logic in syntax::std_inject
    if attr::contains_name(&krate.attrs, "no_core") {
        // Nothing to add
    } else if attr::contains_name(&krate.attrs, "no_std") {
        if !attr::contains_name(&krate.attrs, "compiler_builtins") {
            remove_names.insert("compiler_builtins".into_symbol());
        }
        remove_names.insert("core".into_symbol());
    } else {
        remove_names.insert("std".into_symbol());
    };

    krate.module.items = krate.module.items.into_iter()
        .filter(|i| {
            let injected = match i.node {
                ItemKind::ExternCrate(_) => remove_names.contains(&i.ident.name),
                ItemKind::Use(_) => i.ident.name == "", // prelude import
                _ => false,
            };
            !injected
        }).collect();
    krate
}


pub fn prepare_recheck(cx: &driver::Ctxt, krate: Crate) -> Crate {
    let krate = remove_injected_std(krate);
    krate.fold(&mut ResetNodeIdFolder)
         .fold(&mut ResolveCrateFolder {
             cstore: cx.cstore(),
             hir_map: cx.hir_map(),
         })
}

