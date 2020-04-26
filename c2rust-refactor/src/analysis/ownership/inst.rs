use std::cmp;
use std::iter;
use std::mem;
use std::usize;

use log::Level;

use rustc::hir::def_id::DefId;
use rustc_index::vec::IndexVec;

use super::constraint::ConstraintSet;
use super::context::{Ctxt, Instantiation, VariantSumm};
use super::{ConcretePerm, Perm, Var};

pub struct InstCtxt<'lty, 'tcx> {
    cx: &'lty Ctxt<'lty, 'tcx>,

    insts: &'lty [Instantiation],
    cset: ConstraintSet<'lty>,

    /// Selected mono idx for each instantiation.
    inst_sel: Vec<Option<usize>>,

    /// Assignment to inst vars for the current mono.
    inst_assign: IndexVec<Var, Option<ConcretePerm>>,
}

impl<'lty, 'tcx> InstCtxt<'lty, 'tcx> {
    pub fn new(
        cx: &'lty Ctxt<'lty, 'tcx>,
        func_did: DefId,
        mono_idx: usize,
    ) -> InstCtxt<'lty, 'tcx> {
        let variant = cx.get_mono_variant_summ(func_did, mono_idx);
        let mono = cx.get_mono_summ(func_did, mono_idx);
        let cset = build_inst_cset(cx, variant, &mono.assign);

        InstCtxt {
            cx,
            insts: &variant.insts,
            cset,
            inst_sel: Vec::new(),
            inst_assign: IndexVec::new(),
        }
    }

    pub fn solve_instantiations(&mut self) -> Vec<usize> {
        self.inst_sel = iter::repeat(None)
            .take(self.insts.len())
            .collect::<Vec<_>>();

        let mut num_inst_vars = 0;
        for inst in self.insts {
            let callee_summ = self.cx.get_func_summ(inst.callee);
            num_inst_vars = cmp::max(
                num_inst_vars,
                inst.first_inst_var + callee_summ.num_sig_vars,
            );
        }
        self.inst_assign = IndexVec::from_elem_n(None, num_inst_vars as usize);

        let ok = self.do_solve(0);
        if !ok {
            warn!("found no solution for call to {:?}", self.insts);
            return vec![];
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
        for mono_idx in (0..self.num_mono_sigs(inst_idx)).rev() {
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
        let inst = &self.insts[inst_idx];
        self.cx.get_func_summ(inst.callee).num_monos
    }

    fn check_mono_compat(&self, inst_idx: usize, mono_idx: usize) -> bool {
        let inst = &self.insts[inst_idx];
        let callee = inst.callee;
        let mono_assign = &self.cx.get_mono_summ(callee, mono_idx).assign;
        let callee_summ = self.cx.get_func_summ(callee);
        let first_var = Var(inst.first_inst_var);
        let last_var = Var(inst.first_inst_var + callee_summ.num_sig_vars);

        self.cset.check_partial_assignment(|p| {
            let v = match p {
                Perm::InstVar(v) => v,
                _ => return None,
            };

            if first_var <= v && v < last_var {
                let sig_var = Var(v.0 - first_var.0);
                Some(mono_assign[sig_var])
            } else {
                self.inst_assign[v]
            }
        })
    }

    fn select(&mut self, inst_idx: usize, mono_idx: usize) {
        self.inst_sel[inst_idx] = Some(mono_idx);

        let inst = &self.insts[inst_idx];
        let callee = inst.callee;
        let mono_assign = &self.cx.get_mono_summ(callee, mono_idx).assign;
        let callee_summ = self.cx.get_func_summ(callee);
        for i in 0..callee_summ.num_sig_vars {
            let src = Var(i);
            let dest = Var(inst.first_inst_var + i);
            self.inst_assign[dest] = Some(mono_assign[src]);
        }
    }

    fn deselect(&mut self, inst_idx: usize) {
        self.inst_sel[inst_idx] = None;

        let inst = &self.insts[inst_idx];
        let callee = inst.callee;
        let callee_summ = self.cx.get_func_summ(callee);
        for i in 0..callee_summ.num_sig_vars {
            let dest = Var(inst.first_inst_var + i);
            self.inst_assign[dest] = None;
        }
    }
}

pub fn find_instantiations(cx: &mut Ctxt) {
    let ids = cx.mono_ids().collect::<Vec<_>>();

    for &(func_did, mono_idx) in &ids {
        let inst_sel = {
            let mut icx = InstCtxt::new(cx, func_did, mono_idx);
            icx.solve_instantiations()
        };
        cx.mono_summ(func_did, mono_idx).2.callee_mono_idxs = inst_sel;
    }
}

pub fn build_inst_cset<'lty, 'tcx>(
    cx: &'lty Ctxt<'lty, 'tcx>,
    variant: &VariantSumm<'lty>,
    assign: &IndexVec<Var, ConcretePerm>,
) -> ConstraintSet<'lty> {
    let mut cset = variant.inst_cset.clone_substituted(cx.arena, |p| match p {
        Perm::SigVar(v) => Perm::Concrete(assign[v]),
        Perm::StaticVar(v) => Perm::Concrete(cx.static_assign[v]),
        _ => p,
    });

    for inst in &variant.insts {
        // Import the callee's `sig_cset`.
        let callee_func = cx.get_func_summ(inst.callee);
        cset.import_substituted(&callee_func.sig_cset, cx.arena, |p| match p {
            Perm::SigVar(v) => Perm::InstVar(Var(v.0 + inst.first_inst_var)),
            p => p,
        });
    }

    cset.retain_perms(cx.arena, |p| match p {
        Perm::StaticVar(_) => false,
        _ => true,
    });

    cset.simplify(cx.arena);

    debug!("  inst constraints:");
    if log_enabled!(Level::Debug) {
        for &(a, b) in cset.iter() {
            debug!("    {:?} <= {:?}", a, b);
        }
    }

    cset
}
