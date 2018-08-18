use std::collections::HashMap;
use rustc::hir::def::Def;
use rustc::hir::def_id::{DefId};
use rustc::ty::{Instance, TyCtxt, TypeVariants, Ty};
use syntax::ast::*;
use syntax::ptr::P;

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};
use reflect;
use resolve;
use transform::Transform;
use util::HirDefExt;



/// For every `extern fn` or `extern static` with the "target" mark, look for an extern with the
/// same symbol in the module at `path`.  If found, delete the old extern and replace all its uses
/// with the one found in `path`.  If the old and new externs have different signatures, also
/// insert casts around arguments and calls to make the types line up.
pub struct CanonicalizeExterns {
    path: String,
}

fn is_foreign_symbol(tcx: TyCtxt, did: DefId) -> bool {
    tcx.is_foreign_item(did) &&
    matches!([tcx.describe_def(did)] Some(Def::Fn(..)), Some(Def::Static(..)))
}

impl Transform for CanonicalizeExterns {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let tcx = cx.ty_ctxt();


        // List all extern fns in the target library module
        let lib_path = self.path.split("::").map(|s| Ident::from_str(s)).collect::<Vec<_>>();
        let lib = resolve::resolve_absolute(tcx, &lib_path);

        let mut symbol_map = HashMap::new();

        for (_sym, def) in resolve::module_children(tcx, lib.def_id()) {
            let did = def.def_id();
            if is_foreign_symbol(tcx, did) {
                // Foreign fns can't have region or type params, so empty substs should be fine.
                let inst = Instance::new(did, tcx.intern_substs(&[]));
                let sym = tcx.symbol_name(inst).name.as_symbol();
                symbol_map.insert(sym, did);
            }
        }

        for (&sym, &def) in &symbol_map {
            info!("  found symbol {} :: {:?} at {:?}", self.path, sym, def);
        }


        // Collect DefIds of marked externs whose symbols match something in `symbol_map`

        // Map from replaced fn DefId to replacement fn DefId
        let mut replace_map = HashMap::new();

        visit_nodes(&krate, |fi: &ForeignItem| {
            if !st.marked(fi.id, "target") {
                return;
            }

            let did = cx.node_def_id(fi.id);
            if !is_foreign_symbol(tcx, did) {
                return;
            }

            let inst = Instance::new(did, tcx.intern_substs(&[]));
            let sym = tcx.symbol_name(inst).name.as_symbol();
            if let Some(&repl_did) = symbol_map.get(&sym) {
                replace_map.insert(did, repl_did);
            }
        });


        // Consult the types of the old and new externs to figure out where we'll need to add casts

        /// In `ty_replace_map`, which types need replacing?
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
        enum TyLoc {
            /// The type of a function argument
            Arg(usize),
            /// The return type of a function
            Ret,
            /// The type of the whole item
            Whole,
        }
        let mut ty_replace_map: HashMap<(DefId, TyLoc), (Ty, Ty)> = HashMap::new();

        for (&old_did, &new_did) in &replace_map {
            let old_ty = cx.def_type(old_did);
            let new_ty = cx.def_type(new_did);

            if !matches!([old_ty.sty] TypeVariants::TyFnDef(..)) {
                // Non-fn items are easy to handle.
                if old_ty != new_ty {
                    ty_replace_map.insert((old_did, TyLoc::Whole), (old_ty, new_ty));
                }
                continue;
            }

            // This is a fn replacement.  Look up sigs and compare arg and return types.
            let old_sig = tcx.fn_sig(old_did);
            let new_sig = tcx.fn_sig(new_did);

            macro_rules! bail {
                ($msg:expr) => {{
                    warn!(concat!("canonicalize_externs: {:?} -> {:?}: ", $msg, " - skipping"),
                          old_did, new_did);
                    continue;
                }};
            }

            let (old_sig, new_sig) = match (old_sig.no_late_bound_regions(),
                                            new_sig.no_late_bound_regions()) {
                (Some(x), Some(y)) => (x, y),
                _ => bail!("old or new sig had late-bound regions"),
            };

            if old_sig.inputs().len() != new_sig.inputs().len() {
                bail!("old and new sig differ in arg count");
            }

            if old_sig.variadic != new_sig.variadic {
                bail!("old and new sig differ in variadicness");
            }

            for (i, (&old_ty, &new_ty)) in old_sig.inputs().iter()
                                                  .zip(new_sig.inputs().iter()).enumerate() {
                if old_ty != new_ty {
                    ty_replace_map.insert((old_did, TyLoc::Arg(i)), (old_ty, new_ty));
                }
            }

            let old_ty = old_sig.output();
            let new_ty = new_sig.output();
            if old_ty != new_ty {
                ty_replace_map.insert((old_did, TyLoc::Ret), (old_ty, new_ty));
            }
        }


        for (&k, &v) in &replace_map {
            info!("REPLACE {:?} ({:?})", k, cx.def_type(k));
            info!("   WITH {:?} ({:?})", v, cx.def_type(v));
        }

        let mut stuff = ty_replace_map.iter().collect::<Vec<_>>();
        stuff.sort_by_key(|&(&a, _)| a);
        for (&(did, loc), &(old, new)) in stuff {
            info!("TYPE CHANGE: {:?}  @{:?}:  {:?} -> {:?}", did, loc, old, new);
        }


        // Replace uses of old externs with new ones

        // Maps the NodeId of each rewritten path expr to the DefId of the old extern that was
        // previously referenced by that path.
        let mut path_ids = HashMap::new();
        let krate = fold_resolved_paths_with_id(krate, cx, |id, qself, path, def| {
            let old_did = match_or!([def.opt_def_id()] Some(x) => x; return (qself, path));
            let new_did = match_or!([replace_map.get(&old_did)] Some(&x) => x;
                                    return (qself, path));
            path_ids.insert(id, old_did);
            cx.def_qpath(new_did)
        });


        // Add casts to rewritten calls and exprs

        let krate = fold_nodes(krate, |mut e: P<Expr>| {
            if let Some(&old_did) = path_ids.get(&e.id) {
                // This whole expr was a reference to the old extern `old_did`.  See if we need a
                // cast around the whole thing.  (This should only be true for statics.)
                if let Some(&(old_ty, _new_ty)) = ty_replace_map.get(&(old_did, TyLoc::Whole)) {
                    // The rewritten expr has type `new_ty`, but its context expects `old_ty`.
                    e = mk().cast_expr(e, reflect::reflect_tcx_ty(tcx, old_ty));
                }
            }

            // TODO: handle assignments to replaced extern statics

            let callee_old_did = match e.node {
                ExprKind::Call(ref f, _) => path_ids.get(&f.id),
                _ => None,
            };
            if let Some(&old_did) = callee_old_did {
                // This expr is a call to a rewritten extern fn.  Add casts around args and around
                // the whole expression, as directed by `ty_replace_map`.
                let arg_count = expect!([e.node] ExprKind::Call(_, ref a) => a.len());
                info!("rewriting call - e = {:?}", e);

                for i in 0 .. arg_count {
                    let k = (old_did, TyLoc::Arg(i));
                    if let Some(&(_old_ty, new_ty)) = ty_replace_map.get(&k) {
                        e = e.map(|mut e| {
                            expect!([e.node] ExprKind::Call(_, ref mut args) => {
                                // The new fn requires `new_ty`, where the old one needed `old_ty`.
                                let ty_ast = reflect::reflect_tcx_ty(tcx, new_ty);
                                let new_arg = mk().cast_expr(&args[i], ty_ast);
                                args[i] = new_arg;
                            });
                            e
                        });
                        info!("  arg {} - rewrote e = {:?}", i, e);
                    }
                }

                if let Some(&(old_ty, _new_ty)) = ty_replace_map.get(&(old_did, TyLoc::Ret)) {
                    // The new fn returns `new_ty`, where the old context requires `old_ty`.
                    e = mk().cast_expr(e, reflect::reflect_tcx_ty(tcx, old_ty));
                    info!("  return - rewrote e = {:?}", e);
                }
            }

            e
        });


        // Remove the old externs

        let krate = fold_nodes(krate, |mut fm: ForeignMod| {
            fm.items.retain(|fi| {
                let did = cx.node_def_id(fi.id);
                !replace_map.contains_key(&did)
            });
            fm
        });


        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;
    reg.register("canonicalize_externs", |args| mk(CanonicalizeExterns {
        path: args[0].clone(),
    }));
}
