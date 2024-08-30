use std::fmt;
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PointerId(u32);
const GLOBAL_BIT: u32 = 0x8000_0000;

#[allow(dead_code)]
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
pub struct NextLocalPointerId(u32);

impl NextLocalPointerId {
    pub fn new() -> NextLocalPointerId {
        NextLocalPointerId(0)
    }

    pub fn next(&mut self) -> PointerId {
        let x = self.0;
        self.0 += 1;
        PointerId::local(x)
    }

    pub fn num_pointers(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Debug)]
pub struct NextGlobalPointerId(u32);

impl NextGlobalPointerId {
    pub fn new() -> NextGlobalPointerId {
        NextGlobalPointerId(0)
    }

    pub fn next(&mut self) -> PointerId {
        let x = self.0;
        self.0 += 1;
        PointerId::global(x)
    }

    pub fn num_pointers(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct PointerTableInner<T>(Vec<T>);
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LocalPointerTable<T>(PointerTableInner<T>);
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GlobalPointerTable<T>(PointerTableInner<T>);
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

impl<T> PointerTableInner<T> {
    pub fn empty() -> PointerTableInner<T> {
        Self::from_raw(Vec::new())
    }

    pub fn new(len: usize) -> PointerTableInner<T>
    where
        T: Default,
    {
        let mut v = Vec::new();
        v.resize_with(len, T::default);
        Self::from_raw(v)
    }

    pub fn from_raw(raw: Vec<T>) -> PointerTableInner<T> {
        PointerTableInner(raw)
    }

    pub fn into_raw(self) -> Vec<T> {
        self.0
    }

    pub fn push(&mut self, x: T) -> u32 {
        let i = self.0.len();
        self.0.push(x);
        u32::try_from(i).unwrap()
    }
}

impl<T> Index<u32> for PointerTableInner<T> {
    type Output = T;
    fn index(&self, index: u32) -> &T {
        &self.0[index as usize]
    }
}

impl<T> IndexMut<u32> for PointerTableInner<T> {
    fn index_mut(&mut self, index: u32) -> &mut T {
        &mut self.0[index as usize]
    }
}

#[allow(dead_code)]
impl<T> LocalPointerTable<T> {
    pub fn empty() -> LocalPointerTable<T> {
        LocalPointerTable(PointerTableInner::empty())
    }

    pub fn new(len: usize) -> LocalPointerTable<T>
    where
        T: Default,
    {
        LocalPointerTable(PointerTableInner::new(len))
    }

    pub fn from_raw(raw: Vec<T>) -> LocalPointerTable<T> {
        LocalPointerTable(PointerTableInner::from_raw(raw))
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

    pub fn push(&mut self, x: T) -> PointerId {
        let raw = self.0.push(x);
        PointerId::local(raw)
    }

    pub fn iter(&self) -> impl Iterator<Item = (PointerId, &T)> {
        self.0
             .0
            .iter()
            .enumerate()
            .map(|(i, x)| (PointerId::local(i as u32), x))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (PointerId, &mut T)> {
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

#[allow(dead_code)]
impl<T> GlobalPointerTable<T> {
    pub fn empty() -> GlobalPointerTable<T> {
        GlobalPointerTable(PointerTableInner::empty())
    }

    pub fn new(len: usize) -> GlobalPointerTable<T>
    where
        T: Default,
    {
        GlobalPointerTable(PointerTableInner::new(len))
    }

    pub fn from_raw(raw: Vec<T>) -> GlobalPointerTable<T> {
        GlobalPointerTable(PointerTableInner::from_raw(raw))
    }

    pub fn into_raw(self) -> Vec<T> {
        self.0.into_raw()
    }

    pub fn len(&self) -> usize {
        self.0 .0.len()
    }

    pub fn push(&mut self, x: T) -> PointerId {
        let raw = self.0.push(x);
        PointerId::global(raw)
    }

    pub fn iter(&self) -> impl Iterator<Item = (PointerId, &T)> {
        self.0
             .0
            .iter()
            .enumerate()
            .map(|(i, x)| (PointerId::global(i as u32), x))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (PointerId, &mut T)> {
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

#[allow(dead_code)]
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

    pub fn global(&self) -> &'a GlobalPointerTable<T> {
        self.global
    }

    pub fn local(&self) -> &'a LocalPointerTable<T> {
        self.local
    }

    pub fn iter(&self) -> impl Iterator<Item = (PointerId, &T)> {
        self.global.iter().chain(self.local.iter())
    }
}

impl<'a, T> Clone for PointerTable<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for PointerTable<'a, T> {}

impl<'a, T> Index<PointerId> for PointerTable<'a, T> {
    type Output = T;
    fn index(&self, id: PointerId) -> &T {
        debug_assert!(!id.is_none());
        if id.is_global() {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

#[allow(dead_code)]
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

    pub fn global(&self) -> &GlobalPointerTable<T> {
        self.global
    }

    pub fn global_mut(&mut self) -> &mut GlobalPointerTable<T> {
        self.global
    }

    pub fn local(&self) -> &LocalPointerTable<T> {
        self.local
    }

    pub fn local_mut(&mut self) -> &mut LocalPointerTable<T> {
        self.local
    }

    pub fn iter(&self) -> impl Iterator<Item = (PointerId, &T)> {
        self.global.iter().chain(self.local.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (PointerId, &mut T)> {
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
        debug_assert!(!id.is_none());
        if id.is_global() {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

impl<'a, T> IndexMut<PointerId> for PointerTableMut<'a, T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        debug_assert!(!id.is_none());
        if id.is_global() {
            &mut self.global[id]
        } else {
            &mut self.local[id]
        }
    }
}

#[allow(dead_code)]
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

    pub fn iter(&self) -> impl Iterator<Item = (PointerId, &T)> {
        self.global.iter().chain(self.local.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (PointerId, &mut T)> {
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
        debug_assert!(!id.is_none());
        if id.is_global() {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

impl<T> IndexMut<PointerId> for OwnedPointerTable<T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        debug_assert!(!id.is_none());
        if id.is_global() {
            &mut self.global[id]
        } else {
            &mut self.local[id]
        }
    }
}
