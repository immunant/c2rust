use std::mem;

use crate::context::{AnalysisCtxt, Assignment, FlagSet, PermissionSet, PointerId};
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::{GlobalPointerTable, PointerTable};
use crate::recent_writes::RecentWrites;
use log::{debug, trace};
use rustc_middle::mir::Body;

mod type_check;

#[derive(Clone, Debug)]
enum Constraint {
    /// Pointer `.0` must have a subset of the permissions of pointer `.1`.
    ///
    /// `Subset` and `SubsetExcept` have a special case involving `FREE` and `OFFSET` permissions.
    /// The rewriter can't produce a cast that converts `Box<[T]>` to `Box<T>`; to avoid needing
    /// such casts, we forbid assignment operations from discarding the `OFFSET` permission while
    /// keeping `FREE`.  We implement this restriction by adding an additional requirement to the
    /// definition of `Subset(L, R)`: if `L` contains `FREE` and `R` contains `OFFSET`, then `L`
    /// must also contain `OFFSET`.  This is sufficient because all assignments and
    /// pseudo-assignments generate `Subset` constraints.
    ///
    /// If `L` does not contain `FREE`, then no additional requirement applies, even if `R` does
    /// contain `OFFSET`.  We allow discarding both `FREE` and `OFFSET` simultaneously during an
    /// assignment.
    Subset(PointerId, PointerId),
    /// Pointer `.0` must have a subset of permissions of pointer `.1`, except
    /// for the provided permission set.
    SubsetExcept(PointerId, PointerId, PermissionSet),
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

    fn add_subset_except(&mut self, a: PointerId, b: PointerId, except: PermissionSet) {
        self.constraints
            .push(Constraint::SubsetExcept(a, b, except));
    }

    fn add_all_perms(&mut self, ptr: PointerId, perms: PermissionSet) {
        self.constraints.push(Constraint::AllPerms(ptr, perms));
    }

    fn add_no_perms(&mut self, ptr: PointerId, perms: PermissionSet) {
        self.constraints.push(Constraint::NoPerms(ptr, perms));
    }

    /// Update the pointer permissions in `hypothesis` to satisfy these constraints.
    ///
    /// If `restrict_updates[ptr]` has some flags set, then those flags will be left unchanged in
    /// `hypothesis[ptr]`.
    pub fn propagate(
        &self,
        hypothesis: &mut GlobalPointerTable<PermissionSet>,
        updates_forbidden: &GlobalPointerTable<PermissionSet>,
    ) -> bool {
        debug!("=== propagating ===");
        debug!("constraints:");
        for c in &self.constraints {
            debug!("  {:?}", c);
        }
        trace!("hypothesis:");
        for (id, p) in hypothesis.iter() {
            trace!("  {}: {:?}", id, p);
        }

        struct PropagatePerms;
        impl PropagateRules<PermissionSet> for PropagatePerms {
            fn subset(
                &mut self,
                a_ptr: PointerId,
                a_val: &PermissionSet,
                b_ptr: PointerId,
                b_val: &PermissionSet,
            ) -> (PermissionSet, PermissionSet) {
                self.subset_except(a_ptr, a_val, b_ptr, b_val, PermissionSet::NONE)
            }

            fn subset_except(
                &mut self,
                _a_ptr: PointerId,
                a_val: &PermissionSet,
                _b_ptr: PointerId,
                b_val: &PermissionSet,
                except: PermissionSet,
            ) -> (PermissionSet, PermissionSet) {
                let old_a = *a_val;
                let old_b = *b_val;

                // These should be `const`s, but that produces `error[E0015]: cannot call
                // non-const operator in constants`.

                // Permissions that should be propagated "down": if the superset (`b`)
                // doesn't have it, then the subset (`a`) should have it removed.
                #[allow(bad_style)]
                let PROPAGATE_DOWN = PermissionSet::UNIQUE
                    | PermissionSet::NON_NULL
                    | PermissionSet::HEAP
                    | PermissionSet::STACK;
                // Permissions that should be propagated "up": if the subset (`a`) has it,
                // then the superset (`b`) should be given it.
                #[allow(bad_style)]
                let PROPAGATE_UP = PermissionSet::READ
                    | PermissionSet::WRITE
                    | PermissionSet::OFFSET_ADD
                    | PermissionSet::OFFSET_SUB
                    | PermissionSet::FREE;

                let remove_a = !old_b & PROPAGATE_DOWN & !except;
                let add_b = old_a & PROPAGATE_UP & !except;

                // Special case: as documented on `Constraint::Subset`, if the subset has `FREE`,
                // we propagate `OFFSET` in the opposite direction.  Specifically, if the superset
                // has `OFFSET`, we add it to the subset, propagating "down".  (Propagating "up"
                // here could allow `OFFSET` and `!OFFSET` to propagated up into the same
                // `PointerId` through two different constraints, creating a conflict.)
                let add_a = if old_a.contains(PermissionSet::FREE) {
                    #[allow(bad_style)]
                    let PROPAGATE_DOWN_WHEN_FREE =
                        PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB;
                    old_b & PROPAGATE_DOWN_WHEN_FREE & !except
                } else {
                    PermissionSet::empty()
                };
                debug_assert_eq!(add_a & remove_a, PermissionSet::empty());

                ((old_a | add_a) & !remove_a, old_b | add_b)
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

            fn restrict_updates(
                &mut self,
                old: &PermissionSet,
                new: &PermissionSet,
                updates_forbidden: &PermissionSet,
            ) -> PermissionSet {
                let (old, new, updates_forbidden) = (*old, *new, *updates_forbidden);
                (new & !updates_forbidden) | (old & updates_forbidden)
            }
        }

        match self.propagate_inner(hypothesis, &mut PropagatePerms, Some(updates_forbidden)) {
            Ok(changed) => changed,
            Err(msg) => {
                panic!("{}", msg);
            }
        }
    }

    /// Update `xs` by propagating dataflow information of type `T` according to the constraints
    /// recorded in `self`.
    ///
    /// If `updates_forbidden` is provided, then the parts of `xs` indicated by `updates_forbidden`
    /// will not be modified.  (Specifically, all updates will be filtered through the method
    /// `PropagateRules::restrict_updates`.)
    fn propagate_inner<T, R>(
        &self,
        xs: &mut GlobalPointerTable<T>,
        rules: &mut R,
        updates_forbidden: Option<&GlobalPointerTable<T>>,
    ) -> Result<bool, String>
    where
        T: PartialEq,
        R: PropagateRules<T>,
    {
        let mut xs = TrackedPointerTable::new(xs);

        let restrict_updates = |rules: &mut R, ptr, old: &T, new: T| {
            if let Some(updates_forbidden) = updates_forbidden {
                rules.restrict_updates(old, &new, &updates_forbidden[ptr])
            } else {
                new
            }
        };

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
                        let new_a = restrict_updates(rules, a, old_a, new_a);
                        let new_b = restrict_updates(rules, b, old_b, new_b);
                        xs.set(a, new_a);
                        xs.set(b, new_b);
                    }

                    Constraint::SubsetExcept(a, b, except) => {
                        if !xs.dirty(a) && !xs.dirty(b) {
                            continue;
                        }

                        let old_a = xs.get(a);
                        let old_b = xs.get(b);
                        let (new_a, new_b) = rules.subset_except(a, old_a, b, old_b, except);
                        let new_a = restrict_updates(rules, a, old_a, new_a);
                        let new_b = restrict_updates(rules, b, old_b, new_b);
                        xs.set(a, new_a);
                        xs.set(b, new_b);
                    }

                    Constraint::AllPerms(ptr, perms) => {
                        if !xs.dirty(ptr) {
                            continue;
                        }

                        let old = xs.get(ptr);
                        let new = rules.all_perms(ptr, perms, old);
                        let new = restrict_updates(rules, ptr, old, new);
                        xs.set(ptr, new);
                    }

                    Constraint::NoPerms(ptr, perms) => {
                        if !xs.dirty(ptr) {
                            continue;
                        }

                        let old = xs.get(ptr);
                        let new = rules.no_perms(ptr, perms, old);
                        let new = restrict_updates(rules, ptr, old, new);
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
        let perms = &asn.perms;
        let flags = &mut asn.flags;

        // All pointers that are WRITE and not UNIQUE must have a type like `&Cell<_>`.
        for ((_, p), (_, f)) in perms.iter().zip(flags.iter_mut()) {
            if p.contains(PermissionSet::WRITE) && !p.contains(PermissionSet::UNIQUE) {
                f.insert(FlagSet::CELL);
            }
        }

        struct Rules<'a> {
            perms: &'a GlobalPointerTable<PermissionSet>,
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

            fn subset_except(
                &mut self,
                a_ptr: PointerId,
                a_val: &FlagSet,
                b_ptr: PointerId,
                b_val: &FlagSet,
                _except: PermissionSet,
            ) -> (FlagSet, FlagSet) {
                // Call original subset function
                self.subset(a_ptr, a_val, b_ptr, b_val)
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

            fn restrict_updates(
                &mut self,
                old: &FlagSet,
                new: &FlagSet,
                updates_forbidden: &FlagSet,
            ) -> FlagSet {
                let (old, new, updates_forbidden) = (*old, *new, *updates_forbidden);
                (new & !updates_forbidden) | (old & updates_forbidden)
            }
        }

        match self.propagate_inner(flags, &mut Rules { perms }, None) {
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
            Constraint::SubsetExcept(a, b, perms) => {
                Constraint::SubsetExcept(map[a], map[b], perms)
            }
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
    xs: &'a mut GlobalPointerTable<T>,
    dirty: GlobalPointerTable<bool>,
    new_dirty: GlobalPointerTable<bool>,
    any_new_dirty: bool,
}

impl<'a, T: PartialEq> TrackedPointerTable<'a, T> {
    pub fn new(xs: &'a mut GlobalPointerTable<T>) -> TrackedPointerTable<'a, T> {
        let mut dirty = GlobalPointerTable::with_len_of(xs);
        let mut new_dirty = GlobalPointerTable::with_len_of(xs);
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
    fn subset_except(
        &mut self,
        a_ptr: PointerId,
        a_val: &T,
        b_ptr: PointerId,
        b_val: &T,
        except: PermissionSet,
    ) -> (T, T);
    fn all_perms(&mut self, ptr: PointerId, perms: PermissionSet, val: &T) -> T;
    fn no_perms(&mut self, ptr: PointerId, perms: PermissionSet, val: &T) -> T;
    /// Apply a filter to restrict updates.  The result is similar to `new`, but all flags marked
    /// in `updates_forbidden` are adjusted to match their `old` values.
    fn restrict_updates(&mut self, old: &T, new: &T, updates_forbidden: &T) -> T;
}

pub fn generate_constraints<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
    recent_writes: &RecentWrites,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
) -> DataflowConstraints {
    self::type_check::visit(acx, mir, recent_writes, pointee_types)
}

pub fn generate_equiv_constraints<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
    recent_writes: &RecentWrites,
) -> Vec<(PointerId, PointerId)> {
    self::type_check::visit_equiv(acx, mir, recent_writes)
}
