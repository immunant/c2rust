//! This module contains an analysis to infer ownership information for pointers.  It analyzes code
//! using raw pointers and indicates, for each pointer, whether it appears to be owned, mutably
//! borrowed, or immutably borrowed.  It can also infer ownership-polymorphic function signatures,
//! which handles cases where the original C code used a single accessor for both mutable and
//! immutable access to a field.
//!
//! The analysis operates on constraint sets over "permission variables", which can be take on the
//! concrete permissions "READ", "WRITE", and "MOVE".  The analysis runs in two phases.  First, for
//! each function, it analyzes the function and produces a set of constraints relating variables in
//! the function's signature, variables appearing in static locations (such as struct field types).
//! Since interprocedural information is not available yet, this phase leaves holes where
//! constraints for callee functions can be plugged in.  The second phase fills in holes in
//! function summaries to produce complete summaries that are useful to analysis consumers.  It
//! runs interprocedurally to a fixed point, on each function plugging in the complete summaries of
//! its callees and simplifying to produce a complete summary for the current function.

use std::cmp;
use std::collections::Bound;
use std::collections::BTreeSet;
use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt;
use std::u32;

use arena::DroplessArena;
use rustc::hir;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::mir::*;
use rustc::mir::tcx::LvalueTy;
use rustc::mir::traversal::{Postorder, ReversePostorder};
use rustc::ty::{Ty, TyS, TyCtxt, FnSig, Instance, TypeVariants, AdtDef};
use rustc::ty::subst::Substs;
use rustc::ty::fold::{TypeVisitor, TypeFoldable};
use rustc_data_structures::bitvec::BitVector;
use rustc_data_structures::indexed_vec::{IndexVec, Idx};
use syntax::ast;

use analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};
use command::CommandState;
use driver;
use type_map::{self, TypeSource};


mod constraint;
mod context;
mod intra;
mod inter;
mod annot;
mod debug;

use self::constraint::*;
use self::context::Ctxt;
use self::intra::IntraCtxt;
use self::inter::InterCtxt;
use self::annot::handle_marks;
use self::debug::*;


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Var(u32);

impl Idx for Var {
    fn new(idx: usize) -> Var {
        assert!(idx as u32 as usize == idx);
        Var(idx as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

type LTy<'tcx> = LabeledTy<'tcx, Option<Perm<'tcx>>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct LFnSig<'tcx> {
    inputs: &'tcx [LTy<'tcx>],
    output: LTy<'tcx>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum ConcretePerm {
    Read,
    Write,
    Move,
}


pub struct FnSummary<'tcx> {
    sig: LFnSig<'tcx>,
    num_sig_vars: u32,
    cset: ConstraintSet<'tcx>,
    insts: Vec<Instantiation>,
}

struct Instantiation {
    callee: DefId,
    first_inst_var: u32,
}






fn is_fn(hir_map: &hir::map::Map, def_id: DefId) -> bool {
    use rustc::hir::map::Node::*;

    let n = match hir_map.get_if_local(def_id) {
        None => return false,
        Some(n) => n,
    };

    match n {
        NodeItem(i) => match i.node {
            hir::ItemFn(..) => true,
            _ => false,
        },
        NodeForeignItem(i) => match i.node {
            hir::ForeignItemFn(..) => true,
            _ => false,
        },
        NodeTraitItem(i) => match i.node {
            hir::TraitItemKind::Method(..) => true,
            _ => false,
        },
        NodeImplItem(i) => match i.node {
            hir::ImplItemKind::Method(..) => true,
            _ => false,
        },
        _ => false,
    }
}

fn analyze_intra<'a, 'gcx, 'tcx>(cx: &mut Ctxt<'tcx>,
                                 hir_map: &hir::map::Map,
                                 tcx: TyCtxt<'a, 'gcx, 'tcx>) {
    for &def_id in tcx.mir_keys(LOCAL_CRATE).iter() {
        // We currently don't process `static` bodies, even though they do have MIR.
        if !is_fn(hir_map, def_id) {
            continue;
        }

        let mir = tcx.optimized_mir(def_id);

        let mut local_cx = IntraCtxt::new(cx, tcx, def_id, mir);
        local_cx.init();

        for (bbid, bb) in mir.basic_blocks().iter_enumerated() {
            local_cx.handle_basic_block(bbid, bb);
        }

        local_cx.finish();
    }
}

fn analyze_inter(cx: &mut Ctxt) {
    let mut inter_cx = InterCtxt::new(cx);
    inter_cx.process();
    inter_cx.finish();
}

pub fn analyze(st: &CommandState, dcx: &driver::Ctxt) {
    let mut cx = Ctxt::new(dcx.ty_arena());

    handle_marks(&mut cx, st, dcx);
    analyze_intra(&mut cx, dcx.hir_map(), dcx.ty_ctxt());
    analyze_inter(&mut cx);

    eprintln!("\n === summary ===");
    /*
    let mut new_lcx = LabeledTyCtxt::new(dcx.ty_arena());

    {
        let mut statics_sorted = cx.static_summ.iter().collect::<Vec<_>>();
        statics_sorted.sort_by_key(|&(k, _)| k);
        for (&def_id, &ty) in statics_sorted {
            let ty = new_lcx.relabel(ty, &mut |p| {
                p.as_ref().map(|&p| (cx.static_cset.lower_bound(p), PrintVar(p)))
            });

            eprintln!("{:?}: {:?}", def_id, Pretty(ty));
        }

        eprintln!("static constraints:");
        for &(a, b) in &cx.static_cset.less {
            eprintln!("    {:?} <= {:?}", a, b);
        }
    }
    */

    let mut new_lcx = LabeledTyCtxt::new(dcx.ty_arena());
    let mut fns_sorted = cx.fn_ids().collect::<Vec<_>>();
    fns_sorted.sort();
    for def_id in fns_sorted {
        let summ = cx.get_fn_summ(def_id).unwrap();
        let mut cset = &summ.cset;
        let mut func2 = |p: &Option<_>| {
            p.map(|p| (cset.lower_bound(p), PrintVar(p)))
        };
        let inputs = new_lcx.relabel_slice(summ.sig.inputs, &mut func2);
        let output = new_lcx.relabel(summ.sig.output, &mut func2);
        eprintln!("{:?}:\n  {:?} -> {:?}", def_id, pretty_slice(inputs), Pretty(output));
        for &(a, b) in cset.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }
    }
}
