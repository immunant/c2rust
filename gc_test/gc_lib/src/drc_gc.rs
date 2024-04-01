use std::any;
use std::cell::Cell;
use std::mem::{self, ManuallyDrop};
use std::ptr::NonNull;
use std::ops::Deref;
use boehm_alloc::Gc;
use crate::cell2::SimpleClone;


pub use crate::drc::BreakCycles;


pub struct Drc<T: ?Sized> {
    ptr: Gc<T>,
}

type Erased = ();

impl<T: ?Sized> Drc<T> {
    pub fn new(x: T) -> Drc<T>
    where T: Sized {
        Drc {
            ptr: Gc::new(x),
        }
    }

    pub fn ptr_eq(self, other: Drc<T>) -> bool {
        self.ptr.as_ptr() == other.ptr.as_ptr()
    }

    pub fn drop_data(self)
    where T: BreakCycles {
        (*self.ptr).break_cycles();
    }

    pub fn project<U, F>(self, f: F) -> SubDrc<U>
    where F: for<'a> FnOnce(&'a T) -> &'a U {
        SubDrc {
            ptr: self.ptr.project(f),
        }
    }
}

impl<T: ?Sized> Clone for Drc<T> {
    fn clone(&self) -> Drc<T> { *self }
}

impl<T: ?Sized> Copy for Drc<T> {}

impl<T: ?Sized> Deref for Drc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &*self.ptr
    }
}


pub struct SubDrc<U: ?Sized> {
    ptr: Gc<U>,
}

impl<U: ?Sized> SubDrc<U> {
    pub fn project<V, F>(self, f: F) -> SubDrc<V>
    where F: for<'a> FnOnce(&'a U) -> &'a V {
        SubDrc {
            ptr: self.ptr.project(f),
        }
    }
}

impl<U: ?Sized> Clone for SubDrc<U> {
    fn clone(&self) -> SubDrc<U> { *self }
}
impl<U: ?Sized> Copy for SubDrc<U> {}

impl<U: ?Sized> Deref for SubDrc<U> {
    type Target = U;
    fn deref(&self) -> &U {
        &*self.ptr
    }
}


pub struct NullableDrc<T: ?Sized>(Option<Drc<T>>);

impl<T: ?Sized> NullableDrc<T> {
    pub fn new(x: T) -> NullableDrc<T>
    where T: Sized {
        NullableDrc(Some(Drc::new(x)))
    }

    pub fn null() -> NullableDrc<T> {
        NullableDrc(None)
    }

    pub fn is_null(&self) -> bool {
        self.0.is_none()
    }

    pub fn ptr_eq(self, other: NullableDrc<T>) -> bool {
        match (self.0, other.0) {
            (Some(a), Some(b)) => a.ptr_eq(b),
            (None, None) => true,
            _ => false,
        }
    }

    pub fn drop_data(&self)
    where T: BreakCycles {
        if let Some(ref x) = self.0 {
            x.drop_data();
        }
    }

    pub fn project<U, F>(self, f: F) -> NullableSubDrc<U>
    where F: for<'a> FnOnce(&'a T) -> &'a U {
        let x = self.0.unwrap();
        NullableSubDrc(Some(x.project(f)))
    }
}

impl<T: ?Sized> Clone for NullableDrc<T> {
    fn clone(&self) -> NullableDrc<T> {
        NullableDrc(self.0.clone())
    }
}
impl<T: ?Sized> Copy for NullableDrc<T> {}

impl<T: ?Sized> Default for NullableDrc<T> {
    fn default() -> NullableDrc<T> {
        NullableDrc::null()
    }
}

impl<T: ?Sized> Deref for NullableDrc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.as_ref().unwrap()
    }
}

impl<T: ?Sized> From<Drc<T>> for NullableDrc<T> {
    fn from(x: Drc<T>) -> NullableDrc<T> {
        NullableDrc(Some(x))
    }
}

impl<T: ?Sized> From<NullableDrc<T>> for Drc<T> {
    fn from(x: NullableDrc<T>) -> Drc<T> {
        x.0.unwrap()
    }
}


pub struct NullableSubDrc<U: ?Sized>(Option<SubDrc<U>>);

impl<U: ?Sized> NullableSubDrc<U> {
    pub fn null() -> NullableSubDrc<U> {
        NullableSubDrc(None)
    }

    pub fn is_null(&self) -> bool {
        self.0.is_none()
    }

    pub fn project<V, F>(self, f: F) -> NullableSubDrc<V>
    where F: for<'a> FnOnce(&'a U) -> &'a V {
        let x = self.0.unwrap();
        NullableSubDrc(Some(x.project(f)))
    }
}

impl<U: ?Sized> Clone for NullableSubDrc<U> {
    fn clone(&self) -> NullableSubDrc<U> {
        NullableSubDrc(self.0.clone())
    }
}
impl<U: ?Sized> Copy for NullableSubDrc<U> {}

impl<U: ?Sized> Default for NullableSubDrc<U> {
    fn default() -> NullableSubDrc<U> {
        NullableSubDrc::null()
    }
}

impl<U: ?Sized> Deref for NullableSubDrc<U> {
    type Target = U;
    fn deref(&self) -> &U {
        self.0.as_ref().unwrap()
    }
}

impl<U: ?Sized> From<SubDrc<U>> for NullableSubDrc<U> {
    fn from(x: SubDrc<U>) -> NullableSubDrc<U> {
        NullableSubDrc(Some(x))
    }
}

impl<U: ?Sized> From<NullableSubDrc<U>> for SubDrc<U> {
    fn from(x: NullableSubDrc<U>) -> SubDrc<U> {
        x.0.unwrap()
    }
}
