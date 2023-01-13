use std::mem;

use crate::context::{AnalysisCtxt, Assignment, FlagSet, PermissionSet, PointerId};
use crate::pointer_id::{OwnedPointerTable, PointerTable, PointerTableMut};
use rustc_middle::mir::Body;

mod type_check;

#[derive(Clone, Debug)]
enum Constraint {
    /// Pointer `.0` must have a subset of the permissions of pointer `.1`.
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
    fn add_subset(&mut self, a: PointerId, b: PointerId) {
        self.constraints.push(Constraint::Subset(a, b));
    }

    fn add_all_perms(&mut self, ptr: PointerId, perms: PermissionSet) {
        self.constraints.push(Constraint::AllPerms(ptr, perms));
    }

    #[allow(dead_code)]
    fn _add_no_perms(&mut self, ptr: PointerId, perms: PermissionSet) {
        self.constraints.push(Constraint::NoPerms(ptr, perms));
    }

    /// Update the pointer permissions in `hypothesis` to satisfy these constraints.
    pub fn propagate(&self, hypothesis: &mut PointerTableMut<PermissionSet>) -> bool {
        eprintln!("=== propagating ===");
        eprintln!("constraints:");
        for c in &self.constraints {
            eprintln!("  {:?}", c);
        }
        eprintln!("hypothesis:");
        for (id, p) in hypothesis.iter() {
            eprintln!("  {}: {:?}", id, p);
        }

        struct PropagatePerms;
        impl PropagateRules<PermissionSet> for PropagatePerms {
            fn subset(
                &mut self,
                _a_ptr: PointerId,
                a_val: &PermissionSet,
                _b_ptr: PointerId,
                b_val: &PermissionSet,
            ) -> (PermissionSet, PermissionSet) {
                let old_a = *a_val;
                let old_b = *b_val;

                // These should be `const`s, but that produces `error[E0015]: cannot call
                // non-const operator in constants`.

                // Permissions that should be propagated "down": if the superset (`b`)
                // doesn't have it, then the subset (`a`) should have it removed.
                #[allow(bad_style)]
                let PROPAGATE_DOWN = PermissionSet::UNIQUE;
                // Permissions that should be propagated "up": if the subset (`a`) has it,
                // then the superset (`b`) should be given it.
                #[allow(bad_style)]
                let PROPAGATE_UP = PermissionSet::READ
                    | PermissionSet::WRITE
                    | PermissionSet::OFFSET_ADD
                    | PermissionSet::OFFSET_SUB
                    | PermissionSet::FREE;

                (
                    old_a & !(!old_b & PROPAGATE_DOWN),
                    old_b | (old_a & PROPAGATE_UP),
                )
            }

            fn all_perms(
                &mut self,
                _ptr: PointerId,
                perms: PermissionSet,
                val: &PermissionSet,
            ) -> PermissionSet {
                *val | perms
            }

            fn no_perms(
                &mut self,
                _ptr: PointerId,
                perms: PermissionSet,
                val: &PermissionSet,
            ) -> PermissionSet {
                *val & !perms
            }
        }

        match self.propagate_inner(hypothesis, &mut PropagatePerms) {
            Ok(changed) => changed,
            Err(msg) => {
                panic!("{}", msg);
            }
        }
    }

    fn propagate_inner<T, R>(
        &self,
        xs: &mut PointerTableMut<T>,
        rules: &mut R,
    ) -> Result<bool, String>
    where
        T: PartialEq,
        R: PropagateRules<T>,
    {
        let mut xs = TrackedPointerTable::new(xs.borrow_mut());

        let mut changed = false;
        let mut i = 0;
        loop {
            if i > xs.len() + self.constraints.len() {
                return Err("infinite loop in dataflow edges".to_string());
            }
            i += 1;

            for c in &self.constraints {
                match *c {
                    Constraint::Subset(a, b) => {
                        if !xs.dirty(a) && !xs.dirty(b) {
                            continue;
                        }

                        let old_a = xs.get(a);
                        let old_b = xs.get(b);
                        let (new_a, new_b) = rules.subset(a, old_a, b, old_b);
                        xs.set(a, new_a);
                        xs.set(b, new_b);
                    }

                    Constraint::AllPerms(ptr, perms) => {
                        if !xs.dirty(ptr) {
                            continue;
                        }

                        let old = xs.get(ptr);
                        let new = rules.all_perms(ptr, perms, old);
                        xs.set(ptr, new);
                    }

                    Constraint::NoPerms(ptr, perms) => {
                        if !xs.dirty(ptr) {
                            continue;
                        }

                        let old = xs.get(ptr);
                        let new = rules.no_perms(ptr, perms, old);
                        xs.set(ptr, new);
                    }
                }
            }

            if !xs.any_new_dirty() {
                break;
            }
            xs.swap_dirty();
            changed = true;
        }

        Ok(changed)
    }

    /// Update the pointer permissions in `hypothesis` to satisfy these constraints.
    pub fn propagate_cell(&self, asn: &mut Assignment) {
        let (perms, mut flags) = asn.all_mut();
        let perms = perms.borrow();

        // All pointers that are WRITE and not UNIQUE must have a type like `&Cell<_>`.
        for ((_, p), (_, f)) in perms.iter().zip(flags.iter_mut()) {
            if p.contains(PermissionSet::WRITE) && !p.contains(PermissionSet::UNIQUE) {
                f.insert(FlagSet::CELL);
            }
        }

        struct Rules<'a> {
            perms: PointerTable<'a, PermissionSet>,
        }
        impl PropagateRules<FlagSet> for Rules<'_> {
            fn subset(
                &mut self,
                _a_ptr: PointerId,
                a_val: &FlagSet,
                b_ptr: PointerId,
                b_val: &FlagSet,
            ) -> (FlagSet, FlagSet) {
                // Propagate `CELL` both forward and backward.  On the backward side, if `b` has
                // both `WRITE` and `UNIQUE`, then we remove `CELL`, since `&mut T` can be
                // converted to `&Cell<T>`.
                let mut a_flags = *a_val;
                let mut b_flags = *b_val;
                if a_flags.contains(FlagSet::CELL) {
                    b_flags.insert(FlagSet::CELL);
                }
                if b_flags.contains(FlagSet::CELL) {
                    a_flags.insert(FlagSet::CELL);
                }

                let b_perms = self.perms[b_ptr];
                if b_perms.contains(PermissionSet::WRITE | PermissionSet::UNIQUE) {
                    b_flags.remove(FlagSet::CELL);
                }

                (a_flags, b_flags)
            }

            fn all_perms(
                &mut self,
                _ptr: PointerId,
                _perms: PermissionSet,
                val: &FlagSet,
            ) -> FlagSet {
                *val
            }

            fn no_perms(
                &mut self,
                _ptr: PointerId,
                _perms: PermissionSet,
                val: &FlagSet,
            ) -> FlagSet {
                *val
            }
        }

        match self.propagate_inner(&mut flags, &mut Rules { perms }) {
            Ok(_changed) => {}
            Err(msg) => {
                panic!("{}", msg);
            }
        }
    }
}

impl Constraint {
    pub fn remap_pointers(&mut self, map: PointerTable<PointerId>) {
        *self = match *self {
            Constraint::Subset(a, b) => Constraint::Subset(map[a], map[b]),
            Constraint::AllPerms(ptr, perms) => Constraint::AllPerms(map[ptr], perms),
            Constraint::NoPerms(ptr, perms) => Constraint::NoPerms(map[ptr], perms),
        };
    }
}

impl DataflowConstraints {
    pub fn remap_pointers(&mut self, map: PointerTable<PointerId>) {
        for c in &mut self.constraints {
            c.remap_pointers(map.borrow());
        }
    }
}

struct TrackedPointerTable<'a, T> {
    xs: PointerTableMut<'a, T>,
    dirty: OwnedPointerTable<bool>,
    new_dirty: OwnedPointerTable<bool>,
    any_new_dirty: bool,
}

impl<'a, T: PartialEq> TrackedPointerTable<'a, T> {
    pub fn new(xs: PointerTableMut<'a, T>) -> TrackedPointerTable<'a, T> {
        let mut dirty = OwnedPointerTable::with_len_of(&xs.borrow());
        let mut new_dirty = OwnedPointerTable::with_len_of(&xs.borrow());
        dirty.fill(true);
        new_dirty.fill(false);
        TrackedPointerTable {
            xs,
            dirty,
            new_dirty,
            any_new_dirty: false,
        }
    }

    pub fn len(&self) -> usize {
        self.xs.len()
    }

    pub fn get(&self, id: PointerId) -> &T {
        &self.xs[id]
    }

    pub fn dirty(&self, id: PointerId) -> bool {
        self.dirty[id]
    }

    pub fn any_new_dirty(&self) -> bool {
        self.any_new_dirty
    }

    pub fn set(&mut self, id: PointerId, x: T) {
        if x != self.xs[id] {
            self.xs[id] = x;
            self.new_dirty[id] = true;
            self.any_new_dirty = true;
        }
    }

    pub fn swap_dirty(&mut self) {
        mem::swap(&mut self.dirty, &mut self.new_dirty);
        self.new_dirty.fill(false);
        self.any_new_dirty = false;
    }
}

trait PropagateRules<T> {
    fn subset(&mut self, a_ptr: PointerId, a_val: &T, b_ptr: PointerId, b_val: &T) -> (T, T);
    fn all_perms(&mut self, ptr: PointerId, perms: PermissionSet, val: &T) -> T;
    fn no_perms(&mut self, ptr: PointerId, perms: PermissionSet, val: &T) -> T;
}

pub fn generate_constraints<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
) -> (DataflowConstraints, Vec<(PointerId, PointerId)>) {
    self::type_check::visit(acx, mir)
}
