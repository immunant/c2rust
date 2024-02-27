use std::any;
use std::cell::Cell;
use std::mem;
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
