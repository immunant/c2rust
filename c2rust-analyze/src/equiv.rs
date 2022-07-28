use crate::pointer_id::{GlobalPointerTable, LocalPointerTable, PointerId, PointerTableMut};
use std::cell::Cell;

pub struct GlobalEquivSet(GlobalPointerTable<Cell<PointerId>>);
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
}

impl LocalEquivSet {
    pub fn new(len: usize) -> LocalEquivSet {
        let raw = (0..len as u32)
            .map(|x| Cell::new(PointerId::local(x)))
            .collect();
        LocalEquivSet(LocalPointerTable::from_raw(raw))
    }
}

impl<'g> EquivSet<'g> {
    fn parent(&self, x: PointerId) -> PointerId {
        self.0[x].get()
    }

    fn set_parent(&self, x: PointerId, parent: PointerId) {
        // Local items can point to global ones, but not vice versa.
        if x.is_global() {
            debug_assert!(parent.is_global());
        }

        self.0[x].set(parent);
    }

    pub fn rep(&self, x: PointerId) -> PointerId {
        let parent = self.parent(x);
        if parent == x || self.parent(parent) == parent {
            return parent;
        }

        let rep = self.rep(parent);
        self.set_parent(x, rep);
        rep
    }

    pub fn unify(&mut self, x: PointerId, y: PointerId) {
        let x_rep = self.rep(x);
        let y_rep = self.rep(y);
        if x_rep == y_rep {
            return;
        }

        if x_rep.is_global() {
            self.set_parent(y_rep, x_rep);
            self.set_parent(y, x_rep);
        } else {
            self.set_parent(x_rep, y_rep);
            self.set_parent(x, y_rep);
        }
    }
}
