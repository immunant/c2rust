use std::collections::{HashMap, HashSet};
use log::{Level, info, log_enabled, warn};
use rustc_hir::def::DefKind;
use rustc_hir::def_id::{DefId};
use rustc_middle::ty::{Instance, TyCtxt, TyKind, Ty};
use rustc_ast::*;
use rustc_ast::ptr::P;
use rustc_span::symbol::Ident;

use crate::ast_builder::mk;
use crate::ast_manip::{MutVisitNodes, visit_nodes};
use crate::command::{CommandState, Registry};
use crate::driver::{Phase};
use crate::expect;
use crate::match_or;
use crate::path_edit::fold_resolved_paths_with_id;
use crate::reflect::Reflector;
use crate::resolve;
use crate::transform::Transform;
use crate::RefactorCtxt;
use crate::context::TypeCompare;


/// Consult the types of the old and new externs to figure out where we'll need
/// to add casts
pub fn fix_users(
    krate: &mut Crate,
    replace_map: &HashMap<DefId, DefId>,
    path_ids: &HashMap<NodeId, DefId>,
    new_paths: &HashMap<DefId, (Option<QSelf>, Path)>,
    cx: &RefactorCtxt,
) {
    let tcx = cx.ty_ctxt();
    let reflector = Reflector::new_with_mapping(tcx, new_paths);
    let ty_compare = TypeCompare::new_with_mapping(cx, replace_map);

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
    let mut changed_fns: HashSet<DefId> = HashSet::new();

    for (&old_did, &new_did) in replace_map {
        let old_ty = cx.def_type(old_did);
        let new_ty = cx.def_type(new_did);

        if !crate::matches!([old_ty.kind()] TyKind::FnDef(..)) {
            // Non-fn items are easy to handle.
            if !ty_compare.structural_eq_tys(old_ty, new_ty) {
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

        let (old_sig, new_sig) = match (old_sig.no_bound_vars(),
                                        new_sig.no_bound_vars()) {
            (Some(x), Some(y)) => (x, y),
            _ => bail!("old or new sig had late-bound regions"),
        };

        if old_sig.inputs().len() != new_sig.inputs().len() {
            bail!("old and new sig differ in arg count");
        }

        if old_sig.c_variadic != new_sig.c_variadic {
            bail!("old and new sig differ in variadicness");
        }

        for (i, (&old_ty, &new_ty)) in old_sig.inputs().iter()
            .zip(new_sig.inputs().iter()).enumerate() {
                if !ty_compare.eq_tys(old_ty, new_ty) {
                    ty_replace_map.insert((old_did, TyLoc::Arg(i)), (old_ty, new_ty));
                    changed_fns.insert(old_did);
                }
            }

        let old_ty = old_sig.output();
        let new_ty = new_sig.output();
        if !ty_compare.eq_tys(old_ty, new_ty) {
            ty_replace_map.insert((old_did, TyLoc::Ret), (old_ty, new_ty));
            changed_fns.insert(old_did);
        }
    }

    if log_enabled!(Level::Info) {
        for (&k, &v) in replace_map {
            info!("REPLACE {:?} ({:?})", k, cx.def_type(k));
            info!("   WITH {:?} ({:?})", v, cx.def_type(v));
        }

        let mut stuff = ty_replace_map.iter().collect::<Vec<_>>();
        stuff.sort_by_key(|&(&a, _)| a);
        for (&(did, loc), &(old, new)) in stuff {
            info!("TYPE CHANGE: {:?}  @{:?}:  {:?} -> {:?}", did, loc, old, new);
        }
    }

    // Add casts to rewritten calls and exprs

    MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
        if let Some(&old_did) = path_ids.get(&e.id) {
            // This whole expr was a reference to the old extern `old_did`.  See if we need a
            // cast around the whole thing.  (This should only be true for statics.)
            if let Some(&(old_ty, _new_ty)) = ty_replace_map.get(&(old_did, TyLoc::Whole)) {
                // The rewritten expr has type `new_ty`, but its context expects `old_ty`.
                *e = make_cast(cx, &reflector, e.clone(), old_ty);
            }
        }

        // TODO: handle assignments to replaced extern statics

        match &e.kind {
            ExprKind::Call(ref f, _) => if let Some(&old_did) = path_ids.get(&f.id) {
                // This expr is a call to a rewritten extern fn.  Add casts around args and around
                // the whole expression, as directed by `ty_replace_map`.
                let arg_count = expect!([e.kind] ExprKind::Call(_, ref a) => a.len());
                info!("rewriting call - e = {:?}", e);

                for i in 0 .. arg_count {
                    let k = (old_did, TyLoc::Arg(i));
                    if let Some(&(_old_ty, new_ty)) = ty_replace_map.get(&k) {
                        expect!([e.kind] ExprKind::Call(_, ref mut args) => {
                            if let Some(ty) = cx.opt_node_type(args[i].id) {
                                if ty_compare.eq_tys(ty, new_ty) {
                                    // We don't need to convert this type, it already matches
                                    return;
                                }
                            }
                            // The new fn requires `new_ty`, where the old one needed `old_ty`.
                            // TODO: fix paths in new_ty based on replacement_map
                            let new_arg = make_cast(cx, &reflector, args[i].clone(), new_ty);
                            args[i] = new_arg;
                        });
                        info!("  arg {} - rewrote e = {:?}", i, e);
                    }
                }

                if let Some(&(old_ty, _new_ty)) = ty_replace_map.get(&(old_did, TyLoc::Ret)) {
                    // The new fn returns `new_ty`, where the old context requires `old_ty`.
                    *e = make_cast(cx, &reflector, e.clone(), old_ty);
                    info!("  return - rewrote e = {:?}", e);
                }
            },

            _ => {}
        }
    });
}

fn make_cast<'a, 'tcx>(
    cx: &RefactorCtxt<'a, 'tcx>,
    reflector: &Reflector<'a, 'tcx>,
    expr: P<Expr>,
    ty: Ty<'tcx>
) -> P<Expr> {
    let ty_ast = reflector.reflect_ty(ty);

    let mut needs_transmute = ty.is_fn_ptr();
    if let Some(info) = cx.opt_callee_info(&expr) {
        if let Some(def_id) = info.def_id {
            if cx.def_path(def_id).segments.last().unwrap().ident.as_str() == "Some"
                && info.fn_sig.inputs()[0].is_fn_ptr()
            {
                needs_transmute = true;
            }
        }
    }

    if needs_transmute {
        mk().call_expr(mk().path_expr(vec!["", "core", "mem", "transmute"]), vec![expr])
    } else {
        let expr = if let ExprKind::AddrOf(_, mutability, _) = expr.kind {
            // We have to cast to *T where &T is the type of expr before casting
            // to something else.
            mk().cast_expr(expr, mk().set_mutbl(mutability).ptr_ty(mk().infer_ty()))
        } else {
            expr
        };
        mk().cast_expr(expr, ty_ast)
    }
}

/// # `canonicalize_externs` Command
/// 
/// Usage: `canonicalize_externs MOD_PATH`
/// 
/// Marks: `target`
/// 
/// Replace foreign items ("externs") with references to externs
/// in a different crate or module.
/// 
/// For each foreign `fn` or `static` marked `target`, if a foreign item with the
/// same symbol exists in the module at `MOD_PATH` (which can be part of an
/// external crate), it deletes the marked foreign item and replaces all its uses
/// with uses of the matching foreign item in `MOD_PATH`.  If a replacement item
/// has a different type than the original, it also inserts the necessary casts at
/// each use of the item.
pub struct CanonicalizeExterns {
    path: String,
}

fn is_foreign_symbol(tcx: TyCtxt, did: DefId) -> bool {
    tcx.is_foreign_item(did) &&
    crate::matches!([tcx.def_kind(did)] DefKind::Fn, DefKind::Static(_))
}

impl Transform for CanonicalizeExterns {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
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
                // Get the actual linker symbol for this extern item, considering both the item's
                // name and its attributes.  This is distinct from the `ast::symbol::Symbol`
                // produced by `module_children`, which is simply the name of the item.
                let sym = tcx.symbol_name(inst).name;
                symbol_map.insert(sym, did);
            }
        }

        for (&sym, &def) in &symbol_map {
            info!("  found symbol {} :: {:?} at {:?}", self.path, sym, def);
        }


        // Collect DefIds of marked externs whose symbols match something in `symbol_map`

        // Map from replaced fn DefId to replacement fn DefId
        let mut replace_map = HashMap::new();

        visit_nodes(krate, |fi: &ForeignItem| {
            if !st.marked(fi.id, "target") {
                return;
            }

            let did = cx.node_def_id(fi.id);
            if !is_foreign_symbol(tcx, did) {
                return;
            }

            let inst = Instance::new(did, tcx.intern_substs(&[]));
            let sym = tcx.symbol_name(inst).name;
            if let Some(&repl_did) = symbol_map.get(&sym) {
                replace_map.insert(did, repl_did);
            }
        });

        // Replace uses of old externs with new ones

        // Maps the NodeId of each rewritten path expr to the DefId of the old extern that was
        // previously referenced by that path.
        let mut path_ids = HashMap::new();
        let new_paths = replace_map.iter().map(|(old_did, new_did)| {
            (*old_did, cx.def_qpath(*new_did))
        }).collect::<HashMap<_, _>>();
        fold_resolved_paths_with_id(krate, cx, |id, qself, path, def| {
            let old_did = match_or!([def[0].opt_def_id()] Some(x) => x; return (qself, path));
            if let Some(new_path) = new_paths.get(&old_did) {
                path_ids.insert(id, old_did);
                new_path.clone()
            } else {
                (qself, path)
            }
        });

        fix_users(krate, &replace_map, &path_ids, &new_paths, cx);

        // Remove the old externs

        MutVisitNodes::visit(krate, |fm: &mut ForeignMod| {
            fm.items.retain(|fi| {
                let did = cx.node_def_id(fi.id);
                !replace_map.contains_key(&did)
            });
        });
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
