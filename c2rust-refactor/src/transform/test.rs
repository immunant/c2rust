//! Transformation passes used for testing parts of the system.

use log::info;
use std::collections::{HashSet, HashMap};
use std::str::FromStr;
use rustc_ast::*;
use rustc_ast::ptr::P;
use rustc_hir as hir;
use rustc_middle::ty::{self, TyCtxt, ParamEnv};
use rustc_middle::ty::subst::InternalSubsts;

use crate::ast_builder::mk;
use crate::ast_manip::{visit_nodes};
use crate::ast_manip::fn_edit::mut_visit_fns;
use crate::command::{RefactorState, CommandState, Command, Registry, TypeckLoopResult};
use crate::driver::{Phase};
use crate::match_or;
use crate::matcher::{replace_expr, replace_stmts};
use crate::transform::Transform;
use crate::RefactorCtxt;


/// # `test_one_plus_one` Command
/// 
/// Test command - not intended for general use.
/// 
/// Usage: `test_one_plus_one`
/// 
/// Replace the expression `2` with `1 + 1` everywhere it appears.
pub struct OnePlusOne;

impl Transform for OnePlusOne {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let krate = replace_expr(st, cx, krate, "2", "1 + 1");
        krate
    }
}


/// # `test_f_plus_one` Command
/// 
/// Test command - not intended for general use.
/// 
/// Usage: `test_f_plus_one`
/// 
/// Replace the expression `f(__x)` with `__x + 1` everywhere it appears.
pub struct FPlusOne;

impl Transform for FPlusOne {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let krate = replace_expr(st, cx, krate, "f(__x)", "__x + 1");
        krate
    }
}


/// # `test_replace_stmts` Command
/// 
/// Test command - not intended for general use.
/// 
/// Usage: `test_replace_stmts OLD NEW`
/// 
/// Replace statement(s) `OLD` with `NEW` everywhere it appears.
pub struct ReplaceStmts(pub String, pub String);

impl Transform for ReplaceStmts {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let krate = replace_stmts(st, cx, krate, &self.0, &self.1);
        krate
    }
}


/// # `test_insert_remove_args` Command
/// 
/// Test command - not intended for general use.
/// 
/// Usage: `test_insert_remove_args INS REM`
/// 
/// In each function marked `target`, insert new arguments at each index listed in
/// `INS` (a comma-separated list of integers), then delete the arguments whose
/// original indices are listed in `REM`.
/// 
/// This is used for testing sequence rewriting of `fn` argument lists.
pub struct InsertRemoveArgs {
    insert_idxs: HashMap<usize, usize>,
    remove_idxs: HashSet<usize>,
}

impl Transform for InsertRemoveArgs {
    fn transform(&self, krate: &mut Crate, st: &CommandState, _cx: &RefactorCtxt) {
        mut_visit_fns(krate, |fl| {
            if !st.marked(fl.id, "target") {
                return;
            }

            let mut counter = 0;
            let mut mk_arg = || {
                let arg = mk().arg(mk().tuple_ty::<P<Ty>>(vec![]),
                                   mk().ident_pat(&format!("new_arg{}", counter)));
                counter += 1;
                arg
            };

            let mut new_args = Vec::new();
            let old_arg_count = fl.decl.inputs.len();
            for (i, arg) in fl.decl.inputs.iter().enumerate() {
                for _ in 0 .. self.insert_idxs.get(&i).cloned().unwrap_or(0) {
                    new_args.push(mk_arg());
                }

                if !self.remove_idxs.contains(&i) {
                    new_args.push(arg.clone());
                }
            }

            for _ in 0 .. self.insert_idxs.get(&old_arg_count).cloned().unwrap_or(0) {
                new_args.push(mk_arg());
            }

            fl.decl.inputs = new_args;
        });
    }
}


/// # `test_typeck_loop` Command
/// 
/// Test command - not intended for general use.
/// 
/// Usage: `test_typeck_loop`
/// 
/// Runs a no-op typechecking loop for three iterations.  Used to test the typechecking loop and
/// AST re-analysis code.
pub struct TestTypeckLoop;

impl Command for TestTypeckLoop {
    fn run(&mut self, state: &mut RefactorState) {
        let mut i = 3;
        state.run_typeck_loop(|_krate, _st, _cx| {
            i -= 1;
            info!("ran typeck loop iteration {}", i);
            if i == 0 {
                TypeckLoopResult::Finished
            } else {
                TypeckLoopResult::Iterate
            }
        }).unwrap();
    }
}


/// # `test_debug_callees` Command
/// 
/// Test command - not intended for general use.
/// 
/// Usage: `test_debug_callees`
/// 
/// Inspect the details of each Call expression.  Used to debug
/// `RefactorCtxt::opt_callee_info`.
pub struct TestDebugCallees;

impl Transform for TestDebugCallees {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        visit_nodes(krate, |e: &Expr| {
            let tcx = cx.ty_ctxt();
            let hir_map = cx.hir_map();

            let parent = hir_map.get_parent_item(hir_map.node_to_hir_id(e.id));
            let parent_body = match_or!([hir_map.maybe_body_owned_by(parent)]
                                        Some(x) => x; return);
            let tables = tcx.typeck_body(parent_body);
            let tdds = tables.type_dependent_defs();

            fn maybe_info<T: ::std::fmt::Debug>(desc: &str, x: Option<T>) {
                if let Some(x) = x {
                    info!("    {}: {:?}", desc, x);
                }
            }

            fn describe_ty<'a, 'tcx: 'a>(tcx: TyCtxt<'tcx>,
                                         desc: &str,
                                         ty: ty::Ty<'tcx>,
                                         substs: Option<&'tcx InternalSubsts<'tcx>>) {
                info!("    {}: {:?}", desc, ty);
                if let Some(substs) = substs {
                    info!("      subst: {:?}",
                          tcx.subst_and_normalize_erasing_regions(
                              substs, ParamEnv::empty(), ty));
                }
                if ty.is_fn() {
                    let sig = ty.fn_sig(tcx);
                    info!("      fn sig: {:?}", sig);
                    info!("      input tys: {:?}", sig.inputs());
                    info!("      input tys (skip): {:?}", sig.skip_binder().inputs());
                    info!("      anonymized: {:?}", tcx.anonymize_late_bound_regions(sig));
                    info!("      erased: {:?}", tcx.erase_late_bound_regions(sig));
                    if let Some(substs) = substs {
                        let sig2 = tcx.subst_and_normalize_erasing_regions(
                            substs, ParamEnv::empty(),
                            tcx.erase_late_bound_regions(sig));
                        info!("      sig + erase + subst: {:?}", sig2);
                        info!("      input tys: {:?}", sig2.inputs());
                    }
                }
            }

            let describe = |e: &Expr| {
                info!("    expr: {:?}", e);
                let hir_id = hir_map.node_to_hir_id(e.id);
                info!("    hir id: {:?}", hir_id);

                if let Some(hir::Node::Expr(hir_expr)) = hir_map.find(e.id) {
                    info!("    hir expr: {:?}", hir_expr);
                    maybe_info("ty", tables.expr_ty_opt(hir_expr));
                    maybe_info("adj ty", tables.expr_ty_adjusted_opt(hir_expr));
                }

                let opt_substs = tables.node_substs_opt(hir_id);
                maybe_info("substs", opt_substs);

                if let Some(did) = cx.try_resolve_expr(e) {
                    info!("    resolution: {:?}", did);
                    describe_ty(tcx, "resolved ty", tcx.type_of(did), opt_substs);
                }

                if let Some(tdd) = tdds.get(hir_id) {
                    info!("    tdd: {:?}", tdd);
                    if let Ok((_, did)) = tdd {
                        info!("    tdd id: {:?}", did);
                        describe_ty(tcx, "tdd ty", tcx.type_of(*did), opt_substs);
                    }
                }
            };

            match e.kind {
                ExprKind::Call(ref func, _) => {
                    info!("at plain call {:?}", e);
                    info!("  call info:");
                    describe(e);
                    info!("  func info:");
                    describe(func);
                },

                ExprKind::MethodCall(_, _, _) => {
                    info!("at method call {:?}", e);
                    info!("  call info:");
                    describe(e);
                },

                ExprKind::Binary(_, _, _) => {
                    info!("at binary op {:?}", e);
                    describe(e);
                },

                _ => {},
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("test_one_plus_one", |_args| mk(OnePlusOne));
    reg.register("test_f_plus_one", |_args| mk(FPlusOne));
    reg.register("test_replace_stmts", |args| mk(
            ReplaceStmts(args[0].clone(), args[1].clone())));

    reg.register("test_insert_remove_args", |args| {
        let mut insert_idxs = HashMap::new();
        let mut remove_idxs = HashSet::new();

        for part in args[0].split(",") {
            if part == "" {
                continue;
            }
            let idx = usize::from_str(part).unwrap();
            *insert_idxs.entry(idx).or_insert(0) += 1;
        }

        for part in args[1].split(",") {
            if part == "" {
                continue;
            }
            let idx = usize::from_str(part).unwrap();
            remove_idxs.insert(idx);
        }

        mk(InsertRemoveArgs { insert_idxs, remove_idxs })
    });

    reg.register("test_typeck_loop", |_| Box::new(TestTypeckLoop));

    reg.register("test_debug_callees", |_args| mk(TestDebugCallees));
}
