use std::cell::Cell;
use std::ptr::NonNull;
use std::ops::Deref;
use crate::cell2::SimpleClone;


pub struct Drc<T> {
    ptr: NonNull<DrcInner<T>>,
}

struct DrcInner<T> {
    ref_count: Cell<usize>,
    alive: Cell<bool>,
    data: T,
}

pub trait BreakCycles {
    fn break_cycles(&self);
}

impl<T> DrcInner<T> {
    fn alloc(data: T) -> *mut DrcInner<T> {
        let inner = DrcInner {
            ref_count: Cell::new(1),
            alive: Cell::new(true),
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
            drop(Box::from_raw(ptr));
        }
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

impl<T> Drc<T> {
    pub fn new(x: T) -> Drc<T> {
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
}

impl<T> Clone for Drc<T> {
    fn clone(&self) -> Drc<T> {
        unsafe {
            DrcInner::inc_ref(self.ptr.as_ptr());
            Drc { ptr: self.ptr }
        }
    }
}
unsafe impl<T> SimpleClone for Drc<T> {}

impl<T> Drop for Drc<T> {
    fn drop(&mut self) {
        unsafe {
            DrcInner::dec_ref(self.ptr.as_ptr());
        }
    }
}

impl<T> Deref for Drc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe {
            DrcInner::get(self.ptr.as_ptr())
        }
    }
}


pub struct NullableDrc<T>(Option<Drc<T>>);

impl<T> NullableDrc<T> {
    pub fn new(x: T) -> NullableDrc<T> {
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
}

impl<T> Clone for NullableDrc<T> {
    fn clone(&self) -> NullableDrc<T> {
        NullableDrc(self.0.clone())
    }
}
unsafe impl<T> SimpleClone for NullableDrc<T> {}

impl<T> Default for NullableDrc<T> {
    fn default() -> NullableDrc<T> {
        NullableDrc::null()
    }
}

impl<T> Deref for NullableDrc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.as_ref().unwrap()
    }
}

impl<T> From<Drc<T>> for NullableDrc<T> {
    fn from(x: Drc<T>) -> NullableDrc<T> {
        NullableDrc(Some(x))
    }
}

impl<T> From<NullableDrc<T>> for Drc<T> {
    fn from(x: NullableDrc<T>) -> Drc<T> {
        x.0.unwrap()
    }
}
