use std::cell::Cell;
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::ptr::NonNull;
use std::ops::Deref;


pub struct Drc<T> {
    ptr: NonNull<DrcInner<T>>,
}

struct DrcInner<T> {
    ref_count: Cell<usize>,
    users: Cell<usize>,
    alive: Cell<bool>,
    data: MaybeUninit<T>,
}

impl<T> DrcInner<T> {
    fn alloc(data: T) -> *mut DrcInner<T> {
        let inner = DrcInner {
            ref_count: Cell::new(1),
            users: Cell::new(0),
            alive: Cell::new(true),
            data: MaybeUninit::new(data),
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
            assert_eq!((*ptr).users.get(), 0);
            if (*ptr).alive.get() {
                (*ptr).data.assume_init_drop();
            }
            drop(Box::from_raw(ptr));
        }
    }

    unsafe fn get<'a>(ptr: *mut DrcInner<T>) -> &'a T {
        (*ptr).data.assume_init_ref()
    }

    unsafe fn inc_users(ptr: *mut DrcInner<T>) {
        assert!((*ptr).alive.get(), "data has already been dropped");
        let users = &(*ptr).users;
        users.set(users.get().checked_add(1).unwrap());
    }

    unsafe fn dec_users(ptr: *mut DrcInner<T>) {
        let users = &(*ptr).users;
        let u = users.get();
        users.set(u - 1);
        if u == 1 && !(*ptr).alive.get() {
            (*ptr).data.assume_init_drop();
        }
    }

    unsafe fn drop_data(ptr: *mut DrcInner<T>) {
        assert!((*ptr).alive.get(), "data has already been dropped");
        (*ptr).alive.set(false);
        if (*ptr).users.get() == 0 {
            (*ptr).data.assume_init_drop();
        }
    }

    /*
    fn data_offset() -> usize {
        let header_layout = Layout::new::<DrcHeader>();
        let data_layout = Layout::new::<T>();
        let (layout, data_offset) = header_layout.extend(data_layout).unwrap();
        let layout = layout.pad_to_align();
        assert_eq!(layout, Layout::new::<DrcInner<T>>());
        data_offset
    }
    */
}

impl<T> Drc<T> {
    pub fn new(x: T) -> Drc<T> {
        unsafe {
            Drc {
                ptr: NonNull::new_unchecked(DrcInner::alloc(x)),
            }
        }
    }

    pub fn get<'a>(&'a self) -> Ref<'a, T> {
        unsafe {
            Ref::new(self.ptr)
        }
    }

    pub fn drop_data(&self) {
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
unsafe impl<T> crate::cell2::SimpleClone for Drc<T> {}

impl<T> Drop for Drc<T> {
    fn drop(&mut self) {
        unsafe {
            DrcInner::dec_ref(self.ptr.as_ptr());
        }
    }
}


pub struct Ref<'a, T> {
    ptr: NonNull<DrcInner<T>>,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> Ref<'a, T> {
    unsafe fn new(ptr: NonNull<DrcInner<T>>) -> Ref<'a, T> {
        DrcInner::inc_users(ptr.as_ptr());
        Ref { ptr, _marker: PhantomData }
    }
}

impl<'a, T> Drop for Ref<'a, T> {
    fn drop(&mut self) {
        unsafe {
            DrcInner::dec_users(self.ptr.as_ptr());
        }
    }
}

impl<T> Deref for Ref<'_, T> {
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

    pub fn get<'a>(&'a self) -> Ref<'a, T> {
        self.0.as_ref().unwrap().get()
    }

    pub fn drop_data(&self) {
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
unsafe impl<T> crate::cell2::SimpleClone for NullableDrc<T> {}

impl<T> From<Drc<T>> for NullableDrc<T> {
    fn from(x: Drc<T>) -> NullableDrc<T> {
        NullableDrc(Some(x))
    }
}
