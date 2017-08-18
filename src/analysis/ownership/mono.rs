
use rustc::hir::def_id::DefId;
use rustc_data_structures::indexed_vec::IndexVec;

use super::{ConcretePerm, Var, Perm, LTy, FnSummary};
use super::constraint::ConstraintSet;


/*
pub type PTy<'tcx> = LabeledTy<'tcx, ConcretePerm>;

pub struct PFnSig<'tcx> {
    inputs: &'tcx [PTy<'tcx>],
    output: PTy<'tcx>,
}
*/


/// Mark all sig variables that occur in output positions.  An "output position" means the return
/// type of a function or the target of an always-mutable reference argument.
fn infer_outputs(summ: &FnSummary) -> IndexVec<Var, bool> {
    let mut is_out = IndexVec::from_elem_n(false, summ.num_sig_vars as usize);

    fn mark_output(ty: LTy, is_out: &mut IndexVec<Var, bool>) {
        if let Some(Perm::SigVar(v)) = ty.label {
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
            if cset.lower_bound(p) >= ConcretePerm::Write {
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
        walk_input(input, &mut is_out, &summ.cset);
    }
    mark_output(summ.sig.output, &mut is_out);

    is_out
}

/// Mark all sig variables that are bounded from above.  Note this includes vars that appear in a
/// `min(xs...) <= y` constraint, even though individual `xs` might be able to exceed `y`.
fn upper_bounded_vars(summ: &FnSummary) -> IndexVec<Var, bool> {
    let mut bounded = IndexVec::from_elem_n(false, summ.num_sig_vars as usize);
    for &(a, b) in summ.cset.iter() {
        a.for_each_atom(&mut |p| {
            if let Perm::SigVar(v) = p {
                bounded[v] = true;
            }
        });
    }
    bounded
}

fn for_each_output_assignment<F>(summ: &FnSummary,
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
                        Perm::SigVar(v) => self.assignment[v],
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
        cset: &summ.cset,
        assignment: IndexVec::from_elem_n(None, summ.num_sig_vars as usize),
        callback: &mut callback,
    }.walk_vars(Var(0));
}

fn find_input_assignment(summ: &FnSummary,
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
        cset: &summ.cset,
        assignment: IndexVec::from_elem_n(ConcretePerm::Read, summ.num_sig_vars as usize),
    };
    let ok = s.walk_vars(Var(0));

    if ok {
        Some(s.assignment)
    } else {
        None
    }
}

pub fn mono_test(summ: &FnSummary, def_id: DefId) {
    use super::debug::*;
    use analysis::labeled_ty::LabeledTyCtxt;
    use arena::DroplessArena;

    let is_out = infer_outputs(&summ);
    let is_bounded = upper_bounded_vars(&summ);

    eprintln!("{:?}:", def_id);
    for_each_output_assignment(summ, &is_out, &is_bounded, |assign| {
        if let Some(assign) = find_input_assignment(summ, assign) {
            let arena = DroplessArena::new();
            let mut new_lcx = LabeledTyCtxt::new(&arena);
            let mut func = |p: &Option<_>| {
                if let Some(Perm::SigVar(v)) = *p {
                    Some(assign[v])
                } else {
                    None
                }
            };

            let inputs = new_lcx.relabel_slice(summ.sig.inputs, &mut func);
            let output = new_lcx.relabel(summ.sig.output, &mut func);
            eprintln!("  {:?} -> {:?}", pretty_slice(inputs), Pretty(output));
        } else {
            eprintln!("  (bad output assignment)");
        }
    });
}
