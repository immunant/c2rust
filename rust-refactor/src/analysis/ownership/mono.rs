use rustc_data_structures::indexed_vec::IndexVec;

use super::{ConcretePerm, PermVar, Var, LTy};
use super::constraint::{ConstraintSet, Perm};
use super::context::{Ctxt, FuncSumm};


/// Mark all sig variables that occur in output positions.  An "output position" means the return
/// type of a function or the target of an always-mutable reference argument.
pub fn infer_outputs(summ: &FuncSumm) -> IndexVec<Var, bool> {
    let mut is_out = IndexVec::from_elem_n(false, summ.num_sig_vars as usize);

    fn mark_output(ty: LTy, is_out: &mut IndexVec<Var, bool>) {
        if let Some(PermVar::Sig(v)) = ty.label {
            is_out[v] = true;
        }
        for &arg in ty.args {
            mark_output(arg, is_out);
        }
    }

    fn walk_input<'tcx>(ty: LTy<'tcx>,
                        is_out: &mut IndexVec<Var, bool>,
                        cset: &ConstraintSet<'tcx>) {
        let mut target_out = false;
        if let Some(p) = ty.label {
            if cset.lower_bound(Perm::var(p)) >= ConcretePerm::Write {
                target_out = true;
            }
        }

        for &arg in ty.args {
            if target_out {
                mark_output(arg, is_out);
            } else {
                walk_input(arg, is_out, cset);
            }
        }
    }

    for &input in summ.sig.inputs {
        walk_input(input, &mut is_out, &summ.sig_cset);
    }
    mark_output(summ.sig.output, &mut is_out);

    is_out
}

/// Mark all sig variables that are bounded from above.  Note this includes vars that appear in a
/// `min(xs...) <= y` constraint, even though individual `xs` might be able to exceed `y`.
fn upper_bounded_vars(summ: &FuncSumm) -> IndexVec<Var, bool> {
    let mut bounded = IndexVec::from_elem_n(false, summ.num_sig_vars as usize);
    for &(a, _b) in summ.sig_cset.iter() {
        a.for_each_atom(&mut |p| {
            if let Perm::SigVar(v) = p {
                bounded[v] = true;
            }
        });
    }
    bounded
}

fn for_each_output_assignment<F>(summ: &FuncSumm,
                                 is_out: &IndexVec<Var, bool>,
                                 is_bounded: &IndexVec<Var, bool>,
                                 mut callback: F)
        where F: FnMut(&IndexVec<Var, Option<ConcretePerm>>) {

    struct State<'a, 'tcx: 'a, F: 'a> {
        max: Var,
        is_out: &'a IndexVec<Var, bool>,
        is_bounded: &'a IndexVec<Var, bool>,
        cset: &'a ConstraintSet<'tcx>,
        assignment: IndexVec<Var, Option<ConcretePerm>>,
        callback: &'a mut F,
    }

    impl<'a, 'tcx, F> State<'a, 'tcx, F>
            where F: FnMut(&IndexVec<Var, Option<ConcretePerm>>) {
        fn walk_vars(&mut self, cur: Var) {
            if cur >= self.max {
                (self.callback)(&mut self.assignment);
                return;
            }

            let next = Var(cur.0 + 1);
            if !self.is_out[cur] {
                self.walk_vars(next);
                return;
            }

            for &p in &[ConcretePerm::Move, ConcretePerm::Write, ConcretePerm::Read] {
                self.assignment[cur] = Some(p);
                let assign_ok = self.cset.check_partial_assignment(|p| {
                    match p {
                        Perm::SigVar(v) if v <= cur => self.assignment[v],
                        _ => None,
                    }
                });
                if !assign_ok {
                    continue;
                }

                self.walk_vars(next);

                // For unbounded output variables, try only the highest valid assignment.
                if !self.is_bounded[cur] {
                    break;
                }
            }
        }
    }

    State {
        max: Var(is_out.len() as u32),
        is_out: is_out,
        is_bounded: is_bounded,
        cset: &summ.sig_cset,
        assignment: IndexVec::from_elem_n(None, summ.num_sig_vars as usize),
        callback: &mut callback,
    }.walk_vars(Var(0));
}

fn find_input_assignment(summ: &FuncSumm,
                         out_assign: &IndexVec<Var, Option<ConcretePerm>>)
                         -> Option<IndexVec<Var, ConcretePerm>> {
    struct State<'a, 'tcx: 'a> {
        max: Var,
        out_assign: &'a IndexVec<Var, Option<ConcretePerm>>,
        cset: &'a ConstraintSet<'tcx>,
        assignment: IndexVec<Var, ConcretePerm>,
    }

    impl<'a, 'tcx> State<'a, 'tcx> {
        fn walk_vars(&mut self, cur: Var) -> bool {
            if cur >= self.max {
                return true;
            }

            let next = Var(cur.0 + 1);
            if let Some(p) = self.out_assign[cur] {
                let ok = self.try_assign(cur, p);
                if !ok {
                    return false;
                }
                return self.walk_vars(next);
            }

            for &p in &[ConcretePerm::Read, ConcretePerm::Write, ConcretePerm::Move] {
                let ok = self.try_assign(cur, p);
                if !ok {
                    continue;
                }

                if self.walk_vars(next) {
                    return true;
                }
            }

            false
        }

        fn try_assign(&mut self, cur: Var, p: ConcretePerm) -> bool {
            self.assignment[cur] = p;
            let assign_ok = self.cset.check_partial_assignment(|p| {
                match p {
                    Perm::SigVar(v) => {
                        if let Some(c) = self.out_assign[v] {
                            Some(c)
                        } else if v <= cur {
                            Some(self.assignment[v])
                        } else {
                            None
                        }
                    },
                    _ => None,
                }
            });
            assign_ok
        }
    }

    let mut s = State {
        max: Var(out_assign.len() as u32),
        out_assign: out_assign,
        cset: &summ.sig_cset,
        assignment: IndexVec::from_elem_n(ConcretePerm::Read, summ.num_sig_vars as usize),
    };
    let ok = s.walk_vars(Var(0));

    if ok {
        Some(s.assignment)
    } else {
        None
    }
}

pub fn get_mono_sigs(summ: &FuncSumm) -> Vec<IndexVec<Var, ConcretePerm>> {
    let is_out = infer_outputs(&summ);
    let is_bounded = upper_bounded_vars(&summ);

    let mut assigns = Vec::new();

    for_each_output_assignment(summ, &is_out, &is_bounded, |assign| {
        if let Some(assign) = find_input_assignment(summ, assign) {
            assigns.push(assign);
        }
    });

    assigns
}

pub fn compute_all_mono_sigs(cx: &mut Ctxt) {
    let ids = cx.variant_ids().collect::<Vec<_>>();
    for &id in &ids {
        let assigns = {
            let (func, _var) = cx.variant_summ(id);
            if func.monos_provided {
                // No work for us to do in this pass.
                continue;
            }
            get_mono_sigs(func)
        };
        assert!(assigns.len() > 0, "found no mono sigs for {:?}", id);

        for assign in assigns {
            let mono = cx.add_mono(id).2;
            mono.assign = assign;
        }
    }
}
