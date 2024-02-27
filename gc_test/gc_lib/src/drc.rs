use std::any;
use std::cell::Cell;
use std::mem::{self, ManuallyDrop};
use std::ptr::NonNull;
use std::ops::Deref;
use crate::cell2::SimpleClone;


pub struct Drc<T: ?Sized> {
    ptr: NonNull<DrcInner<T>>,
}

type Erased = ();

#[repr(C)]
struct DrcInner<T: ?Sized> {
    ref_count: Cell<usize>,
    alive: Cell<bool>,
    drop_func: unsafe fn(*mut DrcInner<Erased>),
    data: T,
}

pub trait BreakCycles {
    fn break_cycles(&self);
}

impl<T: ?Sized> DrcInner<T> {
    fn alloc(data: T) -> *mut DrcInner<T>
    where T: Sized {
        let inner = DrcInner {
            ref_count: Cell::new(1),
            alive: Cell::new(true),
            drop_func: unsafe {
                mem::transmute::<
                    unsafe fn(*mut DrcInner<T>),
                    unsafe fn(*mut DrcInner<Erased>),
                >(DrcInner::drop_func)
            },
            data,
        };
        Box::into_raw(Box::new(inner))
    }

    unsafe fn inc_ref(ptr: *mut DrcInner<T>) {
        let ref_count = &(*ptr).ref_count;
        ref_count.set(ref_count.get().checked_add(1).unwrap());
    }

    unsafe fn dec_ref(ptr: *mut DrcInner<T>) {
        let ref_count = &(*ptr).ref_count;
        let rc = ref_count.get();
        if rc > 1 {
            ref_count.set(rc - 1);
        } else {
            let drop_func = (*ptr).drop_func;
            drop_func(ptr as *mut DrcInner<Erased>);
        }
    }

    unsafe fn drop_func(ptr: *mut DrcInner<T>) {
        drop(Box::from_raw(ptr));
    }

    unsafe fn get<'a>(ptr: *mut DrcInner<T>) -> &'a T {
        assert!((*ptr).alive.get());
        &(*ptr).data
    }

    unsafe fn drop_data(ptr: *mut DrcInner<T>)
    where T: BreakCycles {
        (*ptr).alive.set(false);
        (*ptr).data.break_cycles();
    }
}

impl<T: ?Sized> Drc<T> {
    pub fn new(x: T) -> Drc<T>
    where T: Sized {
        unsafe {
            Drc {
                ptr: NonNull::new_unchecked(DrcInner::alloc(x)),
            }
        }
    }

    pub fn ptr_eq(&self, other: &Drc<T>) -> bool {
        self.ptr.as_ptr() == other.ptr.as_ptr()
    }

    pub fn drop_data(&self)
    where T: BreakCycles {
        unsafe {
            DrcInner::drop_data(self.ptr.as_ptr());
        }
    }

    pub fn project<U, F>(self, f: F) -> SubDrc<U>
    where F: for<'a> FnOnce(&'a T) -> &'a U {
        unsafe {
            let this = ManuallyDrop::new(self);
            let child = f(&this) as *const U;
            let child = NonNull::new_unchecked(child as *mut U);
            SubDrc {
                parent: this.ptr.cast(),
                child,
            }
        }
    }
}

impl<T: ?Sized> Clone for Drc<T> {
    fn clone(&self) -> Drc<T> {
        unsafe {
            DrcInner::inc_ref(self.ptr.as_ptr());
            Drc { ptr: self.ptr }
        }
    }
}
unsafe impl<T: ?Sized> SimpleClone for Drc<T> {}

impl<T: ?Sized> Drop for Drc<T> {
    fn drop(&mut self) {
        unsafe {
            DrcInner::dec_ref(self.ptr.as_ptr());
        }
    }
}

impl<T: ?Sized> Deref for Drc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe {
            DrcInner::get(self.ptr.as_ptr())
        }
    }
}


pub struct SubDrc<U: ?Sized> {
    parent: NonNull<DrcInner<Erased>>,
    child: NonNull<U>,
}

impl<U: ?Sized> SubDrc<U> {
    pub fn project<V, F>(self, f: F) -> SubDrc<V>
    where F: for<'a> FnOnce(&'a U) -> &'a V {
        unsafe {
            let this = ManuallyDrop::new(self);
            let child = f(&this) as *const V;
            let child = NonNull::new_unchecked(child as *mut V);
            SubDrc {
                parent: this.parent,
                child,
            }
        }
    }
}

impl<U: ?Sized> Clone for SubDrc<U> {
    fn clone(&self) -> SubDrc<U> {
        unsafe {
            DrcInner::inc_ref(self.parent.as_ptr());
            SubDrc { parent: self.parent, child: self.child }
        }
    }
}
unsafe impl<U: ?Sized> SimpleClone for SubDrc<U> {}

impl<U: ?Sized> Drop for SubDrc<U> {
    fn drop(&mut self) {
        unsafe {
            DrcInner::dec_ref(self.parent.as_ptr());
        }
    }
}

impl<U: ?Sized> Deref for SubDrc<U> {
    type Target = U;
    fn deref(&self) -> &U {
        unsafe {
            &*self.child.as_ptr()
        }
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

    pub fn ptr_eq(&self, other: &NullableDrc<T>) -> bool {
        match (&self.0, &other.0) {
            (&Some(ref a), &Some(ref b)) => a.ptr_eq(b),
            (&None, &None) => true,
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
unsafe impl<T: ?Sized> SimpleClone for NullableDrc<T> {}

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
unsafe impl<U: ?Sized> SimpleClone for NullableSubDrc<U> {}

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
