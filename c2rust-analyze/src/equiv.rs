//! This module implements a two-level union-find data structure for computing equivalence classes
//! on `PointerId`s.  Any equivalence class that contains at least one global `PointerId` is
//! guaranteed to have a global pointer as its representative.  This means the global layer of the
//! data structure is self-contained, and one global layer can be used with many different local
//! parts.
use crate::pointer_id::{
    GlobalPointerTable, LocalPointerTable, NextGlobalPointerId, NextLocalPointerId, PointerId,
    PointerTableMut,
};
use std::cell::Cell;

#[derive(Clone, Debug)]
pub struct GlobalEquivSet(GlobalPointerTable<Cell<PointerId>>);
#[derive(Clone, Debug)]
pub struct LocalEquivSet(LocalPointerTable<Cell<PointerId>>);
pub struct EquivSet<'a>(PointerTableMut<'a, Cell<PointerId>>);

impl GlobalEquivSet {
    pub fn new(len: usize) -> GlobalEquivSet {
        let raw = (0..len as u32)
            .map(|x| Cell::new(PointerId::global(x)))
            .collect();
        GlobalEquivSet(GlobalPointerTable::from_raw(raw))
    }

    pub fn and_mut<'a>(&'a mut self, local: &'a mut LocalEquivSet) -> EquivSet<'a> {
        EquivSet(self.0.and_mut(&mut local.0))
    }

    fn parent(&self, x: PointerId) -> PointerId {
        self.0[x].get()
    }

    fn set_parent(&self, x: PointerId, parent: PointerId) {
        self.0[x].set(parent);
    }

    fn rep(&self, x: PointerId) -> PointerId {
        let parent = self.parent(x);
        if parent == x || self.parent(parent) == parent {
            return parent;
        }

        let rep = self.rep(parent);
        self.set_parent(x, rep);
        rep
    }

    /// Assign new `PointerId`s for the pointers in this set, with one ID per equivalence class.
    /// Returns the next-ID counter for the new numbering and a map from old `PointerId`s to new
    /// ones.
    pub fn renumber(&self) -> (NextGlobalPointerId, GlobalPointerTable<PointerId>) {
        let mut counter = NextGlobalPointerId::new();
        let mut map = GlobalPointerTable::from_raw(vec![PointerId::NONE; self.0.len()]);

        for old_id in self.0.iter().map(|(x, _)| x) {
            let rep = self.rep(old_id);

            if !map[rep].is_none() {
                map[old_id] = map[rep];
            } else {
                let new_id = counter.next();
                map[old_id] = new_id;
                map[rep] = new_id;
            };
        }

        (counter, map)
    }
}

impl LocalEquivSet {
    pub fn new(base: u32, len: usize) -> LocalEquivSet {
        let raw = (0..len as u32)
            .map(|x| Cell::new(PointerId::local(base + x)))
            .collect();
        LocalEquivSet(LocalPointerTable::from_raw(base, raw))
    }

    fn parent(&self, x: PointerId) -> PointerId {
        self.0[x].get()
    }

    fn set_parent(&self, x: PointerId, parent: PointerId) {
        // `x` must be a local ID; its parent can be either local or global.
        debug_assert!(self.0.contains(x));
        self.0[x].set(parent);
    }

    /// Get the representative of `x`'s equivalence class, considering only the local layer.  If
    /// the representative is a global pointer, this method returns a global pointer from the same
    /// equivalence class, but it is not guaranteed to return the same representative for all
    /// members of the class.  That is, it may return two different global pointers for `rep(x)`
    /// and `rep(y)`, even when `x` and `y` are in the same equivalence class.  This is
    /// unavoidable, as information about the global equivalence classes is not available to this
    /// method.
    fn rep(&self, x: PointerId) -> PointerId {
        let parent = self.parent(x);
        if parent == x || !self.0.contains(parent) || self.parent(parent) == parent {
            return parent;
        }

        let rep = self.rep(parent);
        self.set_parent(x, rep);
        rep
    }

    /// Assign new `PointerId`s for the pointers in this set, with one ID per equivalence class.
    /// Returns the base, the number of pointers after remapping, and a map from old `PointerId`s
    /// to new ones.
    pub fn renumber(
        &self,
        global_map: &GlobalPointerTable<PointerId>,
        counter: &mut NextLocalPointerId,
    ) -> (u32, usize, LocalPointerTable<PointerId>) {
        let base = counter.value();
        let mut map =
            LocalPointerTable::from_raw(self.0.base(), vec![PointerId::NONE; self.0.len()]);

        for old_id in self.0.iter().map(|(x, _)| x) {
            let rep = self.rep(old_id);

            if global_map.contains(rep) {
                map[old_id] = global_map[rep];
            } else if !map[rep].is_none() {
                map[old_id] = map[rep];
            } else {
                let new_id = counter.next();
                map[old_id] = new_id;
                map[rep] = new_id;
            }
        }

        let count = (counter.value() - base) as usize;
        (base, count, map)
    }
}

impl<'g> EquivSet<'g> {
    fn parent(&self, x: PointerId) -> PointerId {
        self.0[x].get()
    }

    fn set_parent(&self, x: PointerId, parent: PointerId) {
        // Local items can point to global ones, but not vice versa.
        if self.0.ptr_is_global(x) {
            debug_assert!(self.0.ptr_is_global(parent));
        }

        self.0[x].set(parent);
    }

    /// Get the representative of `x`'s equivalence class.  Two pointers `x` and `y` are in the
    /// same equivalence class if and only if `rep(x) == rep(y)`.
    pub fn rep(&self, x: PointerId) -> PointerId {
        let parent = self.parent(x);
        if parent == x || self.parent(parent) == parent {
            return parent;
        }

        let rep = self.rep(parent);
        self.set_parent(x, rep);
        rep
    }

    /// Merge the equivalence classes that contain `x` and `y`.  Afterward, all members of both
    /// equivalence classes will have the same `rep`.
    pub fn unify(&mut self, x: PointerId, y: PointerId) {
        let x_rep = self.rep(x);
        let y_rep = self.rep(y);
        if x_rep == y_rep {
            return;
        }

        // Prefer lower-numbered reps over higher-numbered ones.  This ensures that we always pick
        // a global `PointerId` as the rep if the equivalence class contains one.
        if x_rep.index() < y_rep.index() {
            self.set_parent(y_rep, x_rep);
            self.set_parent(y, x_rep);
        } else {
            self.set_parent(x_rep, y_rep);
            self.set_parent(x, y_rep);
        }
    }
}
