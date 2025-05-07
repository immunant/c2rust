use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy)]
pub struct PointerId(u32);

/// Flag used to indicate that a `PointerId` is a global pointer instead of a local one.
///
/// This is only for debug purposes.  The `Display` impl for `PointerId`s prints global pointers
/// like `g99` and local pointers like `l99`.
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
}

impl fmt::Display for PointerId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.is_none() {
            write!(fmt, "NONE")
        } else if self.0 & GLOBAL_BIT == 0 {
            write!(fmt, "l{}", self.index())
        } else {
            debug_assert!(self.0 & GLOBAL_BIT != 0);
            write!(fmt, "g{}", self.index())
        }
    }
}

impl fmt::Debug for PointerId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self)
    }
}

impl PartialEq for PointerId {
    fn eq(&self, other: &PointerId) -> bool {
        self.index() == other.index()
    }
}

impl Eq for PointerId {}

impl PartialOrd for PointerId {
    fn partial_cmp(&self, other: &PointerId) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PointerId {
    fn cmp(&self, other: &PointerId) -> Ordering {
        self.index().cmp(&other.index())
    }
}

impl Hash for PointerId {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.index().hash(hasher);
    }
}

#[derive(Clone, Debug)]
pub struct NextLocalPointerId(u32);

impl NextLocalPointerId {
    pub fn _new() -> NextLocalPointerId {
        NextLocalPointerId(0)
    }

    pub fn next(&mut self) -> PointerId {
        let x = self.0;
        self.0 = self.0.checked_add(1).unwrap();
        PointerId::local(x)
    }

    pub fn value(&self) -> u32 {
        self.0
    }

    pub fn _num_pointers(&self) -> usize {
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
        self.0 = self.0.checked_add(1).unwrap();
        PointerId::global(x)
    }

    pub fn num_pointers(&self) -> usize {
        self.0 as usize
    }

    pub fn into_local(self) -> NextLocalPointerId {
        NextLocalPointerId(self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct PointerTableInner<T>(Vec<T>);
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LocalPointerTable<T> {
    table: PointerTableInner<T>,
    base: u32,
}
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
    pub fn empty(base: u32) -> LocalPointerTable<T> {
        LocalPointerTable {
            table: PointerTableInner::empty(),
            base,
        }
    }

    pub fn new(base: u32, len: usize) -> LocalPointerTable<T>
    where
        T: Default,
    {
        // `base + len` must not exceed `u32::MAX`.
        let len_u32 = u32::try_from(len).unwrap();
        assert!(base.checked_add(len_u32).is_some());

        LocalPointerTable {
            table: PointerTableInner::new(len),
            base,
        }
    }

    pub fn from_raw(base: u32, raw: Vec<T>) -> LocalPointerTable<T> {
        // `base + len` must not exceed `u32::MAX`.
        let len = u32::try_from(raw.len()).unwrap();
        assert!(base.checked_add(len).is_some());

        LocalPointerTable {
            table: PointerTableInner::from_raw(raw),
            base,
        }
    }

    pub fn into_raw(self) -> (u32, Vec<T>) {
        (self.base, self.table.into_raw())
    }

    pub fn base(&self) -> u32 {
        self.base
    }

    pub fn len(&self) -> usize {
        self.table.0.len()
    }

    pub fn next_index(&self) -> u32 {
        self.base + self.len() as u32
    }

    pub fn fill(&mut self, x: T)
    where
        T: Clone,
    {
        self.table.0.fill(x);
    }

    pub fn push(&mut self, x: T) -> PointerId {
        assert!(self.base + (self.len() as u32) < u32::MAX);
        let raw = self.table.push(x);
        PointerId::local(raw + self.base)
    }

    pub fn contains(&self, ptr: PointerId) -> bool {
        // If `ptr.index() < self.base`, the subtraction will wrap to a large number in excess of
        // `self.len()`.
        ptr.index().wrapping_sub(self.base) < self.len() as u32
    }

    pub fn iter(&self) -> impl Iterator<Item = (PointerId, &T)> {
        let base = self.base;
        self.table
            .0
            .iter()
            .enumerate()
            .map(move |(i, x)| (PointerId::local(i as u32 + base), x))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (PointerId, &mut T)> {
        let base = self.base;
        self.table
            .0
            .iter_mut()
            .enumerate()
            .map(move |(i, x)| (PointerId::local(i as u32 + base), x))
    }
}

impl<T> Index<PointerId> for LocalPointerTable<T> {
    type Output = T;
    fn index(&self, id: PointerId) -> &T {
        &self.table[id.index() - self.base]
    }
}

impl<T> IndexMut<PointerId> for LocalPointerTable<T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        &mut self.table[id.index() - self.base]
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

    pub fn with_len_of<U>(table: &GlobalPointerTable<U>) -> GlobalPointerTable<T>
    where
        T: Default,
    {
        GlobalPointerTable::new(table.len())
    }

    pub fn from_raw(raw: Vec<T>) -> GlobalPointerTable<T> {
        GlobalPointerTable(PointerTableInner::from_raw(raw))
    }

    pub fn into_raw(self) -> Vec<T> {
        self.0.into_raw()
    }

    pub fn as_slice(&self) -> &[T] {
        &self.0 .0
    }

    pub fn len(&self) -> usize {
        self.0 .0.len()
    }

    pub fn next_index(&self) -> u32 {
        self.len() as u32
    }

    pub fn push(&mut self, x: T) -> PointerId {
        let raw = self.0.push(x);
        PointerId::global(raw)
    }

    pub fn contains(&self, ptr: PointerId) -> bool {
        ptr.index() < self.len() as u32
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
        &self.0[id.index()]
    }
}

impl<T> IndexMut<PointerId> for GlobalPointerTable<T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
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

    pub fn ptr_is_global(&self, ptr: PointerId) -> bool {
        self.global.contains(ptr)
    }

    pub fn ptr_is_local(&self, ptr: PointerId) -> bool {
        self.local.contains(ptr)
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
        if id.index() < self.local.base {
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

    pub fn ptr_is_global(&self, ptr: PointerId) -> bool {
        self.global.contains(ptr)
    }

    pub fn ptr_is_local(&self, ptr: PointerId) -> bool {
        self.local.contains(ptr)
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
        if id.index() < self.local.base {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

impl<'a, T> IndexMut<PointerId> for PointerTableMut<'a, T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        debug_assert!(!id.is_none());
        if id.index() < self.local.base {
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
            LocalPointerTable::new(table.local.base, table.local.len()),
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

    pub fn ptr_is_global(&self, ptr: PointerId) -> bool {
        self.global.contains(ptr)
    }

    pub fn ptr_is_local(&self, ptr: PointerId) -> bool {
        self.local.contains(ptr)
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
        if id.index() < self.local.base {
            &self.global[id]
        } else {
            &self.local[id]
        }
    }
}

impl<T> IndexMut<PointerId> for OwnedPointerTable<T> {
    fn index_mut(&mut self, id: PointerId) -> &mut T {
        debug_assert!(!id.is_none());
        if id.index() < self.local.base {
            &mut self.global[id]
        } else {
            &mut self.local[id]
        }
    }
}
