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
        let mut changed = false;
        let mut dirty = vec![true; hypothesis.len()];
        let mut i = 0;
        loop {
            if i > hypothesis.len() + self.constraints.len() {
                return Err(format!("infinite loop in dataflow edges"));
            }
            i += 1;

            let mut new_dirty = vec![false; hypothesis.len()];
            let mut any_new_dirty = false;
            for c in &self.constraints {
                match *c {
                    Constraint::Subset(a, b) => {
                        if !dirty[a.index()] && !dirty[b.index()] {
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

                        let old_a = hypothesis[a.index()];
                        let old_b = hypothesis[b.index()];
                        let new_a = old_a & !(!old_b & PROPAGATE_DOWN);
                        let new_b = old_b | (old_a & PROPAGATE_UP);
                        if new_a != old_a {
                            eprintln!("changed {:?}: {:?} => {:?}", a, old_a, new_a);
                            hypothesis[a.index()] = new_a;
                            new_dirty[a.index()] = true;
                            any_new_dirty = true;
                        }
                        if new_b != old_b {
                            eprintln!("changed {:?}: {:?} => {:?}", b, old_b, new_b);
                            hypothesis[b.index()] = new_b;
                            new_dirty[b.index()] = true;
                            any_new_dirty = true;
                        }
                    },

                    Constraint::AllPerms(ptr, perms) => {
                        if !dirty[ptr.index()] {
                            continue;
                        }
                        let old = hypothesis[ptr.index()];
                        let new = old | perms;
                        if new != old {
                            eprintln!("changed {:?}: {:?} => {:?}", ptr, old, new);
                            hypothesis[ptr.index()] = new;
                            new_dirty[ptr.index()] = true;
                            any_new_dirty = true;
                        }
                    },

                    Constraint::NoPerms(ptr, perms) => {
                        if !dirty[ptr.index()] {
                            continue;
                        }
                        let old = hypothesis[ptr.index()];
                        let new = old & !perms;
                        if new != old {
                            hypothesis[ptr.index()] = new;
                            eprintln!("changed {:?}: {:?} => {:?}", ptr, old, new);
                            new_dirty[ptr.index()] = true;
                            any_new_dirty = true;
                        }
                    },
                }
            }

            if !any_new_dirty {
                break;
            }
            dirty = new_dirty;
        }

        Ok(changed)
    }
}


pub fn generate_constraints<'tcx>(
    acx: &AnalysisCtxt<'tcx>,
    mir: &Body<'tcx>,
) -> DataflowConstraints {
    self::type_check::visit(acx, mir)
}
