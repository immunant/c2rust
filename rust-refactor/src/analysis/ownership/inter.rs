//! Interprocedural part of the analysis.

use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use std::collections::VecDeque;
use std::mem;

use rustc::hir::def_id::DefId;

use super::Var;
use super::context::Ctxt;
use super::constraint::{ConstraintSet, Perm};


struct WorkList {
    queue: VecDeque<DefId>,
    in_queue: HashSet<DefId>,
}

impl WorkList {
    fn new() -> WorkList {
        WorkList {
            queue: VecDeque::new(),
            in_queue: HashSet::new(),
        }
    }

    fn push(&mut self, id: DefId) {
        if !self.in_queue.contains(&id) {
            self.queue.push_back(id);
            self.in_queue.insert(id);
        }
    }

    fn pop(&mut self) -> Option<DefId> {
        let r = self.queue.pop_front();
        if let Some(id) = r {
            self.in_queue.remove(&id);
        }
        r
    }
}

pub struct InterCtxt<'a, 'tcx: 'a> {
    cx: &'a mut Ctxt<'tcx>,

    complete_cset: HashMap<DefId, ConstraintSet<'tcx>>,

    work_list: WorkList,
    rev_deps: HashMap<DefId, HashSet<DefId>>,

    static_rev_deps: HashMap<Var, HashSet<DefId>>,
}

impl<'a, 'tcx> InterCtxt<'a, 'tcx> {
    pub fn new(cx: &'a mut Ctxt<'tcx>) -> InterCtxt<'a, 'tcx> {
        InterCtxt {
            cx: cx,
            complete_cset: HashMap::new(),
            work_list: WorkList::new(),
            rev_deps: HashMap::new(),
            static_rev_deps: HashMap::new(),
        }
    }

    fn process_one(&mut self, def_id: DefId) {
        let dummy_cset = ConstraintSet::new();

        let mut cset = self.cx.get_fn_summ(def_id).unwrap().cset.clone();

        // Add constraints for all used static vars.
        let mut used_statics = HashSet::new();
        cset.for_each_perm(|p| {
            match p {
                Perm::StaticVar(v) => {
                    used_statics.insert(v);
                },
                _ => {},
            }
        });
        for &v in &used_statics {
            eprintln!("  import static: {:?} = {:?}", v, self.cx.static_assign[v]);
            cset.add(Perm::Concrete(self.cx.static_assign[v]), Perm::StaticVar(v));
            self.static_rev_deps.entry(v).or_insert_with(HashSet::new).insert(def_id);
        }

        // Copy in complete csets for all instantiations.
        for inst in &self.cx.get_fn_summ(def_id).unwrap().insts {
            let complete = self.complete_cset.get(&inst.callee).unwrap_or(&dummy_cset);
            eprintln!("  instantiate {:?} for vars {}..", inst.callee, inst.first_inst_var);
            cset.import_substituted(complete, self.cx.arena, |p| {
                match p {
                    Perm::SigVar(v) => Perm::InstVar(Var(v.0 + inst.first_inst_var)),
                    p => p,
                }
            });

            self.rev_deps.entry(inst.callee).or_insert_with(HashSet::new).insert(def_id);
        }

        // Simplify away inst vars to produce a new complete cset for this fn.
        eprintln!("  original constraints:");
        for &(a, b) in cset.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        cset.remove_useless();
        cset.simplify_min_lhs(self.cx.arena);

        cset.retain_perms(self.cx.arena, |p| {
            match p {
                Perm::LocalVar(_) | Perm::InstVar(_) => false,
                _ => true,
            }
        });

        eprintln!("  simplified constraints:");
        for &(a, b) in cset.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        // Update `cx.static_assign`
        for &v in &used_statics {
            let old = self.cx.static_assign[v];
            let new = cset.lower_bound(Perm::StaticVar(v));
            eprintln!("  static {:?}: {:?} -> {:?}", v, old, new);
            if new > old {
                self.cx.static_assign[v] = new;
                if let Some(rev_deps) = self.static_rev_deps.get(&v) {
                    for &id in rev_deps {
                        self.work_list.push(id);
                    }
                }
            }
        }

        // Simplify away static vars too.
        cset.retain_perms(self.cx.arena, |p| {
            match p {
                Perm::LocalVar(_) | Perm::InstVar(_) | Perm::StaticVar(_) => false,
                _ => true,
            }
        });

        cset.simplify(self.cx.arena);

        // Update `complete_cset`
        let did_update = match self.complete_cset.entry(def_id) {
            Entry::Vacant(e) => {
                e.insert(cset);
                true
            },
            Entry::Occupied(mut e) => {
                if e.get() != &cset {
                    *e.get_mut() = cset;
                    true
                } else {
                    false
                }
            },
        };

        if did_update {
            if let Some(rev_deps) = self.rev_deps.get(&def_id) {
                for &id in rev_deps {
                    self.work_list.push(id);
                }
            }
        }
    }

    pub fn process(&mut self) {
        let mut idx = 0;

        let ids = self.cx.fn_ids().collect::<Vec<_>>();
        eprintln!("\ninterprocedural analysis: process {} fns", ids.len());
        for id in ids {
            eprintln!("process {} (init): {:?}", idx, id);
            idx += 1;

            self.process_one(id);
        }

        while let Some(id) = self.work_list.pop() {
            eprintln!("process {}: {:?}", idx, id);
            idx += 1;

            self.process_one(id);
        }
    }

    pub fn finish(mut self) {
        for (id, cset) in self.complete_cset {
            let summ = self.cx.get_fn_summ(id).unwrap();
            let inst_cset = mem::replace(&mut summ.cset, cset);
            summ.inst_cset = inst_cset;
        }
    }
}
