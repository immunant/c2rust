use std::cmp;
use std::collections::Bound;
use std::collections::btree_set::{self, BTreeSet};
use std::collections::HashMap;
use std::iter;
use std::mem;
use std::usize;

use rustc::hir::def_id::DefId;
use rustc_data_structures::indexed_vec::IndexVec;

use super::{Var, ConcretePerm, Perm, FnSummary};
use super::constraint::ConstraintSet;
use super::context::Ctxt;


pub struct InstCtxt<'a, 'tcx: 'a> {
    cx: &'a Ctxt<'tcx>,

    /// Available monomorphized signatures of each function
    mono_sigs: &'a HashMap<DefId, Vec<IndexVec<Var, ConcretePerm>>>,

    summ: &'a FnSummary<'tcx>,

    cset: ConstraintSet<'tcx>,

    inst_sel: Vec<Option<usize>>,

    inst_assign: IndexVec<Var, Option<ConcretePerm>>,
}

impl<'a, 'tcx> InstCtxt<'a, 'tcx> {
    pub fn new(cx: &'a Ctxt<'tcx>,
               mono_sigs: &'a HashMap<DefId, Vec<IndexVec<Var, ConcretePerm>>>,
               summ: &'a FnSummary<'tcx>,
               cur_mono_sig: &'a IndexVec<Var, ConcretePerm>)
               -> InstCtxt<'a, 'tcx> {
        let cset = build_inst_cset(cx, summ, cur_mono_sig);

        InstCtxt {
            cx: cx,
            mono_sigs: mono_sigs,
            summ: summ,
            cset: cset,
            inst_sel: Vec::new(),
            inst_assign: IndexVec::new(),
        }
    }

    pub fn solve_instantiations(&mut self) -> Vec<usize> {
        self.inst_sel = iter::repeat(None).take(self.summ.insts.len()).collect::<Vec<_>>();

        let mut num_inst_vars = 0;
        for inst in &self.summ.insts {
            let callee_summ = self.cx.get_fn_summ_imm(inst.callee).unwrap();
            num_inst_vars = cmp::max(num_inst_vars,
                                     inst.first_inst_var + callee_summ.num_sig_vars);
        }
        self.inst_assign = IndexVec::from_elem_n(None, num_inst_vars as usize);

        let ok = self.do_solve(0);
        if !ok {
            panic!("found no solution");
        }

        let inst_sel = mem::replace(&mut self.inst_sel, Vec::new());
        inst_sel.into_iter().map(|i| i.unwrap()).collect::<Vec<_>>()
    }

    /// Simple brute-force search of all combinations of instantiations.  Surprisingly, this works
    /// pretty well, even on pathological cases like `json_tokener_parse_ex` (with its 452 calls).
    fn do_solve(&mut self, inst_idx: usize) -> bool {
        if inst_idx >= self.inst_sel.len() {
            return true;
        }

        // The search is biased toward higher-numbered monomorphizations.  Currently higher indexes
        // correspond to lower output permissions.
        for mono_idx in (0 .. self.num_mono_sigs(inst_idx)).rev() {
            if self.check_mono_compat(inst_idx, mono_idx) {
                self.select(inst_idx, mono_idx);
                if self.do_solve(inst_idx + 1) {
                    return true;
                }
            }
        }

        self.deselect(inst_idx);
        false
    }

    fn num_mono_sigs(&self, inst_idx: usize) -> usize {
        let inst = &self.summ.insts[inst_idx];
        let callee = inst.callee;
        self.mono_sigs[&callee].len()
    }

    fn check_mono_compat(&self, inst_idx: usize, mono_idx: usize) -> bool {
        let inst = &self.summ.insts[inst_idx];
        let callee = inst.callee;
        let mono_sig = &self.mono_sigs[&callee][mono_idx];

        let callee_summ = self.cx.get_fn_summ_imm(callee).unwrap();
        let first_var = Var(inst.first_inst_var);
        let last_var = Var(inst.first_inst_var + callee_summ.num_sig_vars);

        self.cset.check_partial_assignment(|p| {
            let v = match p {
                Perm::InstVar(v) => v,
                _ => return None,
            };

            if first_var <= v && v < last_var {
                let sig_var = Var(v.0 - first_var.0);
                Some(mono_sig[sig_var])
            } else {
                self.inst_assign[v]
            }
        })
    }

    fn select(&mut self, inst_idx: usize, mono_idx: usize) {
        self.inst_sel[inst_idx] = Some(mono_idx);

        let inst = &self.summ.insts[inst_idx];
        let callee = inst.callee;
        let mono_sig = &self.mono_sigs[&callee][mono_idx];
        let callee_summ = self.cx.get_fn_summ_imm(callee).unwrap();
        for i in 0 .. callee_summ.num_sig_vars {
            let src = Var(i);
            let dest = Var(inst.first_inst_var + i);
            self.inst_assign[dest] = Some(mono_sig[src]);
        }
    }

    fn deselect(&mut self, inst_idx: usize) {
        self.inst_sel[inst_idx] = None;

        let inst = &self.summ.insts[inst_idx];
        let callee = inst.callee;
        let callee_summ = self.cx.get_fn_summ_imm(callee).unwrap();
        for i in 0 .. callee_summ.num_sig_vars {
            let dest = Var(inst.first_inst_var + i);
            self.inst_assign[dest] = None;
        }
    }
}


pub fn find_instantiations<'a, 'tcx: 'a>(
    cx: &'a Ctxt<'tcx>,
    mono_sigs: &'a HashMap<DefId, Vec<IndexVec<Var, ConcretePerm>>>)
    -> HashMap<(DefId, usize), Vec<usize>> {

    let mut instantiations = HashMap::new();

    for def_id in cx.fn_ids() {
        let summ = cx.get_fn_summ_imm(def_id).unwrap();
        let fn_mono_sigs = &mono_sigs[&def_id];
        for (idx, fn_mono_sig) in fn_mono_sigs.iter().enumerate() {
            let inst_sel = {
                let mut icx = InstCtxt::new(cx, mono_sigs, summ, fn_mono_sig);
                icx.solve_instantiations()
            };
            instantiations.insert((def_id, idx), inst_sel);
        }
    }

    instantiations
}


pub fn build_inst_cset<'tcx>(cx: &Ctxt<'tcx>,
                             summ: &FnSummary<'tcx>,
                             assign: &IndexVec<Var, ConcretePerm>) -> ConstraintSet<'tcx> {
    let mut cset = summ.inst_cset.clone_substituted(cx.arena, |p| {
        match p {
            Perm::SigVar(v) => Perm::Concrete(assign[v]),
            Perm::StaticVar(v) => Perm::Concrete(cx.static_assign[v]),
            _ => p,
        }
    });

    for inst in &summ.insts {
        let callee_summ = match cx.get_fn_summ_imm(inst.callee) {
            Some(x) => x,
            None => continue,
        };
        cset.import_substituted(&callee_summ.cset, cx.arena, |p| {
            match p {
                Perm::SigVar(v) => Perm::InstVar(Var(v.0 + inst.first_inst_var)),
                p => p,
            }
        });
    }

    cset.retain_perms(cx.arena, |p| {
        match p {
            Perm::StaticVar(_) => false,
            _ => true,
        }
    });

    cset.simplify(cx.arena);

    /*
    eprintln!("  inst constraints:");
    for &(a, b) in cset.iter() {
        eprintln!("    {:?} <= {:?}", a, b);
    }
    */

    cset
}


