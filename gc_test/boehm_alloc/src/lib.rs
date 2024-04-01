use std::alloc::{self, Layout, GlobalAlloc};
use std::cmp::Ordering;
use std::mem;
use std::ops::Deref;
use std::os::raw::c_void;
use std::ptr::{self, NonNull};

mod ffi;


pub struct BoehmAllocator;

unsafe impl GlobalAlloc for BoehmAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.alloc_zeroed(layout)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        ffi::GC_free(ptr.cast())
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let ptr = ffi::GC_malloc_uncollectable(layout.size());
        ptr.cast()
    }
    unsafe fn realloc(
        &self,
        ptr: *mut u8,
        _layout: Layout,
        new_size: usize
    ) -> *mut u8 {
        let new_ptr = ffi::GC_realloc(ptr.cast(), new_size);
        new_ptr.cast()
    }
}

#[global_allocator]
static GLOBAL_ALLOCATOR: BoehmAllocator = BoehmAllocator;


pub fn allocate_gc(layout: Layout) -> *mut u8 {
    unsafe {
        let ptr = ffi::GC_malloc(layout.size());
        ptr.cast::<u8>()
    }
}


pub struct Gc<T: ?Sized>(NonNull<T>);

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> { *self }
}
impl<T: ?Sized> Copy for Gc<T> {}

impl<A: ?Sized + PartialEq<B>, B: ?Sized> PartialEq<Gc<B>> for Gc<A> {
    fn eq(&self, other: &Gc<B>) -> bool {
        <A as PartialEq<B>>::eq(self, other)
    }
    fn ne(&self, other: &Gc<B>) -> bool {
        <A as PartialEq<B>>::ne(self, other)
    }
}
impl<A: ?Sized + Eq> Eq for Gc<A> {}

impl<A: ?Sized + PartialOrd<B>, B: ?Sized> PartialOrd<Gc<B>> for Gc<A> {
    fn partial_cmp(&self, other: &Gc<B>) -> Option<Ordering> {
        <A as PartialOrd<B>>::partial_cmp(self, other)
    }
}
impl<A: ?Sized + Ord> Ord for Gc<A> {
    fn cmp(&self, other: &Gc<A>) -> Ordering {
        <A as Ord>::cmp(self, other)
    }
}

unsafe extern "C" fn finalizer<T>(obj: *mut c_void, _user_data: *mut c_void) {
    obj.cast::<T>().drop_in_place();
}

impl<T: ?Sized> Gc<T> {
    pub fn new(x: T) -> Gc<T>
    where T: Sized {
        unsafe {
            let layout = Layout::new::<T>();
            let ptr = allocate_gc(layout);
            let ptr = NonNull::new(ptr)
                .unwrap_or_else(|| alloc::handle_alloc_error(layout));
            let ptr = ptr.cast::<T>();
            ptr.as_ptr().write(x);
            if mem::needs_drop::<T>() {
                // FIXME: Think carefully about how to handle `Drop`.  The `no_order` finalizer
                // variant breaks cycles arbitrarily, which can likely be abused to result in
                // `Drop` running after some of its pointees have already been invalidated.
                // However, without this cycle breaking, finalizable objects never get collected.
                ffi::GC_register_finalizer_no_order(
                    ptr.as_ptr().cast::<c_void>(),
                    Some(finalizer::<T>),
                    ptr::null_mut(),
                    ptr::null_mut(),
                    ptr::null_mut(),
                );
            }
            Gc(ptr)
        }
    }

    pub fn as_ptr(self) -> NonNull<T> {
        self.0
    }

    pub fn project<U: ?Sized, F>(self, f: F) -> Gc<U>
    where F: for<'a> FnOnce(&'a T) -> &'a U {
        // Boehm GC supports interior pointers, so we can just wrap up the child pointer as `Gc<U>`
        // and return it.  There's no need to keep the parent pointer alongside it.
        unsafe {
            let ptr = f(&*self) as *const U as *mut U;
            let ptr = NonNull::new_unchecked(ptr);
            Gc(ptr)
        }
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { &*self.0.as_ptr() }
    }
}
