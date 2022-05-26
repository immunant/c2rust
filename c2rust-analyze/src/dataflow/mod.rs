use std::mem;

use rustc_middle::mir::Body;
use crate::context::{PermissionSet, PointerId, AnalysisCtxt};

mod type_check;


#[derive(Clone, Debug)]
enum Constraint {
    /// Pointer `.0` must have a subset of the permissions of poniter `.1`.
    Subset(PointerId, PointerId),
    /// Pointer `.0` must have all the permissions in `.1`.
    AllPerms(PointerId, PermissionSet),
    /// Pointer `.0` must not have any of the permissions in `.1`.
    NoPerms(PointerId, PermissionSet),
}

#[derive(Clone, Debug, Default)]
pub struct DataflowConstraints {
    constraints: Vec<Constraint>,
}

impl DataflowConstraints {
    fn add_subset(
        &mut self,
        a: PointerId,
        b: PointerId,
    ) {
        self.constraints.push(Constraint::Subset(a, b));
    }

    fn add_all_perms(
        &mut self,
        ptr: PointerId,
        perms: PermissionSet,
    ) {
        self.constraints.push(Constraint::AllPerms(ptr, perms));
    }

    fn add_no_perms(
        &mut self,
        ptr: PointerId,
        perms: PermissionSet,
    ) {
        self.constraints.push(Constraint::NoPerms(ptr, perms));
    }

    /// Update the pointer permissions in `hypothesis` to satisfy these constraints.
    pub fn propagate(&self, hypothesis: &mut [PermissionSet]) -> bool {
        eprintln!("=== propagating ===");
        eprintln!("constraints:");
        for c in &self.constraints {
            eprintln!("  {:?}", c);
        }
        eprintln!("hypothesis:");
        for (i, p) in hypothesis.iter().enumerate() {
            eprintln!("  {}: {:?}", i, p);
        }
        match self.propagate_inner(hypothesis) {
            Ok(changed) => changed,
            Err(msg) => {
                panic!("{}", msg);
            },
        }
    }

    fn propagate_inner(&self, hypothesis: &mut [PermissionSet]) -> Result<bool, String> {
        let mut hypothesis = TrackedSlice::new(hypothesis);

        let mut changed = false;
        let mut i = 0;
        loop {
            if i > hypothesis.len() + self.constraints.len() {
                return Err(format!("infinite loop in dataflow edges"));
            }
            i += 1;

            for c in &self.constraints {
                match *c {
                    Constraint::Subset(a, b) => {
                        if !hypothesis.dirty(a.index()) && !hypothesis.dirty(b.index()) {
                            continue;
                        }

                        // These should be `const`s, but that produces `error[E0015]: cannot call
                        // non-const operator in constants`.

                        // Permissions that should be propagated "down": if the superset (`b`)
                        // doesn't have it, then the subset (`a`) should have it removed.
                        #[allow(bad_style)]
                        let PROPAGATE_DOWN =
                            PermissionSet::UNIQUE;
                        // Permissions that should be propagated "up": if the subset (`a`) has it,
                        // then the superset (`b`) should be given it.
                        #[allow(bad_style)]
                        let PROPAGATE_UP =
                            PermissionSet::READ |
                            PermissionSet::WRITE |
                            PermissionSet::OFFSET_ADD |
                            PermissionSet::OFFSET_SUB;

                        let old_a = *hypothesis.get(a.index());
                        let old_b = *hypothesis.get(b.index());
                        hypothesis.set(a.index(), old_a & !(!old_b & PROPAGATE_DOWN));
                        hypothesis.set(b.index(), old_b | (old_a & PROPAGATE_UP));
                    },

                    Constraint::AllPerms(ptr, perms) => {
                        if !hypothesis.dirty(ptr.index()) {
                            continue;
                        }
                        let old = *hypothesis.get(ptr.index());
                        hypothesis.set(ptr.index(), old | perms);
                    },

                    Constraint::NoPerms(ptr, perms) => {
                        if !hypothesis.dirty(ptr.index()) {
                            continue;
                        }
                        let old = *hypothesis.get(ptr.index());
                        hypothesis.set(ptr.index(), old & !perms);
                    },
                }
            }

            if !hypothesis.any_new_dirty() {
                break;
            }
            hypothesis.swap_dirty();
            changed = true;
        }

        Ok(changed)
    }
}


struct TrackedSlice<'a, T> {
    xs: &'a mut [T],
    dirty: Vec<bool>,
    new_dirty: Vec<bool>,
    any_new_dirty: bool,
}

impl<'a, T: PartialEq> TrackedSlice<'a, T> {
    pub fn new(xs: &'a mut [T]) -> TrackedSlice<'a, T> {
        let n = xs.len();
        TrackedSlice {
            xs,
            dirty: vec![true; n],
            new_dirty: vec![false; n],
            any_new_dirty: false,
        }
    }

    pub fn len(&self) -> usize {
        self.xs.len()
    }

    pub fn get(&self, i: usize) -> &T {
        &self.xs[i]
    }

    pub fn dirty(&self, i: usize) -> bool {
        self.dirty[i]
    }

    pub fn any_new_dirty(&self) -> bool {
        self.any_new_dirty
    }

    pub fn set(&mut self, i: usize, x: T) {
        if x != self.xs[i] {
            self.xs[i] = x;
            self.new_dirty[i] = true;
            self.any_new_dirty = true;
        }
    }

    pub fn swap_dirty(&mut self) {
        mem::swap(&mut self.dirty, &mut self.new_dirty);
        self.new_dirty.fill(false);
        self.any_new_dirty = false;
    }
}


pub fn generate_constraints<'tcx>(
    acx: &AnalysisCtxt<'tcx>,
    mir: &Body<'tcx>,
) -> DataflowConstraints {
    self::type_check::visit(acx, mir)
}
