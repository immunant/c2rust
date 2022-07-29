use std::cell::Cell;
use std::convert::TryFrom;
use std::fmt;
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PointerId(u32);
const GLOBAL_BIT: u32 = 0x8000_0000;

impl PointerId {
    pub const NONE: PointerId = PointerId(u32::MAX);

    pub fn global(x: u32) -> PointerId {
        assert!(x & GLOBAL_BIT == 0);
        assert!(x | GLOBAL_BIT != PointerId::NONE.0);
        PointerId(x | GLOBAL_BIT)
    }

    pub fn local(x: u32) -> PointerId {
        assert!(x & GLOBAL_BIT == 0);
        PointerId(x)
    }

    pub fn from_raw(x: u32) -> PointerId {
        PointerId(x)
    }

    pub fn index(self) -> u32 {
        self.0 & !GLOBAL_BIT
    }

    pub fn is_none(self) -> bool {
        self == PointerId::NONE
    }

    pub fn is_global(self) -> bool {
        self.0 & GLOBAL_BIT != 0 && !self.is_none()
    }

    pub fn is_local(self) -> bool {
        self.0 & GLOBAL_BIT == 0
    }
}

impl fmt::Display for PointerId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.is_none() {
            write!(fmt, "NONE")
        } else if self.is_local() {
            write!(fmt, "l{}", self.index())
        } else {
            debug_assert!(self.is_global());
            write!(fmt, "g{}", self.index())
        }
    }
}

impl fmt::Debug for PointerId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self)
    }
}

#[derive(Clone, Debug)]
pub struct NextLocalPointerId(Cell<u32>);

impl NextLocalPointerId {
    pub fn new() -> NextLocalPointerId {
        NextLocalPointerId(Cell::new(0))
    }

    pub fn next(&self) -> PointerId {
        let x = self.0.get();
        self.0.set(x + 1);
        PointerId::local(x)
    }

    pub fn num_pointers(&self) -> usize {
        self.0.get() as usize
    }
}

#[derive(Clone, Debug)]
pub struct NextGlobalPointerId(Cell<u32>);

impl NextGlobalPointerId {
    pub fn new() -> NextGlobalPointerId {
        NextGlobalPointerId(Cell::new(0))
    }

    pub fn next(&self) -> PointerId {
        let x = self.0.get();
        self.0.set(x + 1);
        PointerId::global(x)
    }

    pub fn num_pointers(&self) -> usize {
        self.0.get() as usize
    }
}

#[derive(Clone, Debug)]
struct RawPointerTable<T>(Vec<T>);
#[derive(Clone, Debug)]
pub struct LocalPointerTable<T>(RawPointerTable<T>);
#[derive(Clone, Debug)]
pub struct GlobalPointerTable<T>(RawPointerTable<T>);
pub struct PointerTable<'a, T> {
    global: &'a GlobalPointerTable<T>,
    local: &'a LocalPointerTable<T>,
}
pub struct PointerTableMut<'a, T> {
    global: &'a mut GlobalPointerTable<T>,
    local: &'a mut LocalPointerTable<T>,
}
pub struct OwnedPointerTable<T> {
    global: GlobalPointerTable<T>,
    local: LocalPointerTable<T>,
}

impl<T> RawPointerTable<T> {
    pub fn empty() -> RawPointerTable<T> {
        Self::from_raw(Vec::new())
    }

    pub fn new(len: usize) -> RawPointerTable<T>
    where
        T: Default,
    {
        let mut v = Vec::new();
        v.resize_with(len, T::default);
        Self::from_raw(v)
    }

    pub fn from_raw(raw: Vec<T>) -> RawPointerTable<T> {
        RawPointerTable(raw)
    }

    pub fn into_raw(self) -> Vec<T> {
        self.0
    }
}

impl<T> Index<u32> for RawPointerTable<T> {
    type Output = T;
    fn index(&self, index: u32) -> &T {
        &self.0[index as usize]
    }
}

impl<T> IndexMut<u32> for RawPointerTable<T> {
    fn index_mut(&mut self, index: u32) -> &mut T {
        &mut self.0[index as usize]
    }
}

impl<T> LocalPointerTable<T> {
    pub fn empty() -> LocalPointerTable<T> {
        LocalPointerTable(RawPointerTable::empty())
    }

    pub fn new(len: usize) -> LocalPointerTable<T>
    where
        T: Default,
    {
        LocalPointerTable(RawPointerTable::new(len))
    }

    pub fn from_raw(raw: Vec<T>) -> LocalPointerTable<T> {
        LocalPointerTable(RawPointerTable::from_raw(raw))
    }

    pub fn into_raw(self) -> Vec<T> {
        self.0.into_raw()
    }

    pub fn len(&self) -> usize {
        self.0 .0.len()
    }

    pub fn fill(&mut self, x: T)
    where
        T: Clone,
    {
        self.0 .0.fill(x);
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (PointerId, &'a T)> + 'a {
        self.0
             .0
            .iter()
            .enumerate()
            .map(|(i, x)| (PointerId::local(i as u32), x))
    }

    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = (PointerId, &'a mut T)> + 'a {
        self.0
             .0
            .iter_mut()
            .enumerate()
            .map(|(i, x)| (PointerId::local(i as u32), x))
    }
}

impl<T> Index<PointerId> for LocalPointerTable<T> {
    type Output = T;
    fn index(&self, id: PointerId) -> &T {
        assert!(id.is_local());
        &self.0[id.index()]
    }
}

impl<T> IndexMut<PointerId> for LocalPointerTable<T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        assert!(id.is_local());
        &mut self.0[id.index()]
    }
}

impl<T> GlobalPointerTable<T> {
    pub fn empty() -> GlobalPointerTable<T> {
        GlobalPointerTable(RawPointerTable::empty())
    }

    pub fn new(len: usize) -> GlobalPointerTable<T>
    where
        T: Default,
    {
        GlobalPointerTable(RawPointerTable::new(len))
    }

    pub fn from_raw(raw: Vec<T>) -> GlobalPointerTable<T> {
        GlobalPointerTable(RawPointerTable::from_raw(raw))
    }

    pub fn into_raw(self) -> Vec<T> {
        self.0.into_raw()
    }

    pub fn len(&self) -> usize {
        self.0 .0.len()
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (PointerId, &'a T)> + 'a {
        self.0
             .0
            .iter()
            .enumerate()
            .map(|(i, x)| (PointerId::global(i as u32), x))
    }

    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = (PointerId, &'a mut T)> + 'a {
        self.0
             .0
            .iter_mut()
            .enumerate()
            .map(|(i, x)| (PointerId::global(i as u32), x))
    }

    pub fn fill(&mut self, x: T)
    where
        T: Clone,
    {
        self.0 .0.fill(x);
    }

    pub fn and<'a>(&'a self, local: &'a LocalPointerTable<T>) -> PointerTable<'a, T> {
        PointerTable {
            global: self,
            local,
        }
    }

    pub fn and_mut<'a>(
        &'a mut self,
        local: &'a mut LocalPointerTable<T>,
    ) -> PointerTableMut<'a, T> {
        PointerTableMut {
            global: self,
            local,
        }
    }
}

impl<T> Index<PointerId> for GlobalPointerTable<T> {
    type Output = T;
    fn index(&self, id: PointerId) -> &T {
        assert!(id.is_global());
        &self.0[id.index()]
    }
}

impl<T> IndexMut<PointerId> for GlobalPointerTable<T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        assert!(id.is_global());
        &mut self.0[id.index()]
    }
}

impl<'a, T> PointerTable<'a, T> {
    pub fn new(
        global: &'a GlobalPointerTable<T>,
        local: &'a LocalPointerTable<T>,
    ) -> PointerTable<'a, T> {
        PointerTable { global, local }
    }

    pub fn borrow(&self) -> PointerTable<T> {
        PointerTable::new(self.global, self.local)
    }

    pub fn len(&self) -> usize {
        self.global.len() + self.local.len()
    }

    pub fn iter<'b>(&'b self) -> impl Iterator<Item = (PointerId, &'b T)> + 'b {
        self.global.iter().chain(self.local.iter())
    }
}

impl<'a, T> Index<PointerId> for PointerTable<'a, T> {
    type Output = T;
    fn index(&self, id: PointerId) -> &T {
        if id.is_global() {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

impl<'a, T> PointerTableMut<'a, T> {
    pub fn new(
        global: &'a mut GlobalPointerTable<T>,
        local: &'a mut LocalPointerTable<T>,
    ) -> PointerTableMut<'a, T> {
        PointerTableMut { global, local }
    }

    pub fn borrow(&self) -> PointerTable<T> {
        PointerTable::new(self.global, self.local)
    }

    pub fn borrow_mut(&mut self) -> PointerTableMut<T> {
        PointerTableMut::new(self.global, self.local)
    }

    pub fn len(&self) -> usize {
        self.global.len() + self.local.len()
    }

    pub fn iter<'b>(&'b self) -> impl Iterator<Item = (PointerId, &'b T)> + 'b {
        self.global.iter().chain(self.local.iter())
    }

    pub fn iter_mut<'b>(&'b mut self) -> impl Iterator<Item = (PointerId, &'b mut T)> + 'b {
        self.global.iter_mut().chain(self.local.iter_mut())
    }

    pub fn fill(&mut self, x: T)
    where
        T: Clone,
    {
        self.global.fill(x.clone());
        self.local.fill(x);
    }
}

impl<'a, T> Index<PointerId> for PointerTableMut<'a, T> {
    type Output = T;
    fn index(&self, id: PointerId) -> &T {
        if id.is_global() {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

impl<'a, T> IndexMut<PointerId> for PointerTableMut<'a, T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        if id.is_global() {
            &mut self.global[id]
        } else {
            &mut self.local[id]
        }
    }
}

impl<T> OwnedPointerTable<T> {
    pub fn new(global: GlobalPointerTable<T>, local: LocalPointerTable<T>) -> OwnedPointerTable<T> {
        OwnedPointerTable { global, local }
    }

    pub fn with_len_of<U>(table: &PointerTable<U>) -> OwnedPointerTable<T>
    where
        T: Default,
    {
        OwnedPointerTable::new(
            GlobalPointerTable::new(table.global.len()),
            LocalPointerTable::new(table.local.len()),
        )
    }

    pub fn borrow(&self) -> PointerTable<T> {
        PointerTable::new(&self.global, &self.local)
    }

    pub fn borrow_mut(&mut self) -> PointerTableMut<T> {
        PointerTableMut::new(&mut self.global, &mut self.local)
    }

    pub fn len(&self) -> usize {
        self.global.len() + self.local.len()
    }

    pub fn iter<'b>(&'b self) -> impl Iterator<Item = (PointerId, &'b T)> + 'b {
        self.global.iter().chain(self.local.iter())
    }

    pub fn iter_mut<'b>(&'b mut self) -> impl Iterator<Item = (PointerId, &'b mut T)> + 'b {
        self.global.iter_mut().chain(self.local.iter_mut())
    }

    pub fn fill(&mut self, x: T)
    where
        T: Clone,
    {
        self.global.fill(x.clone());
        self.local.fill(x);
    }
}

impl<T> Index<PointerId> for OwnedPointerTable<T> {
    type Output = T;
    fn index(&self, id: PointerId) -> &T {
        if id.is_global() {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

impl<T> IndexMut<PointerId> for OwnedPointerTable<T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        if id.is_global() {
            &mut self.global[id]
        } else {
            &mut self.local[id]
        }
    }
}
