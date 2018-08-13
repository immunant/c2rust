#![feature(libc)]
#![feature(const_fn)]

extern crate libc;
extern crate parking_lot;
extern crate spin;

use libc::{c_void, c_int, size_t};
use std::alloc;
use std::collections::HashMap;
use std::mem;
use std::ptr;
use parking_lot::{Mutex, MutexGuard, MappedMutexGuard};

type MallocFnType        = Option<unsafe extern fn (size: size_t) -> *mut c_void>;
type FreeFnType          = Option<unsafe extern fn (ptr: *mut c_void)>;
type CallocFnType        = Option<unsafe extern fn (nmemb: size_t, size: size_t) -> *mut c_void>;
type ReallocFnType       = Option<unsafe extern fn (ptr: *mut c_void, size: size_t) -> *mut c_void>;
type ReallocArrayFnType  = Option<unsafe extern fn (ptr: *mut c_void, nmemb: size_t, size: size_t) -> *mut c_void>;
type POSIXMemalignFnType = Option<unsafe extern fn (memptr: *mut *mut c_void, alignment: size_t, size: size_t) -> c_int>;

const DLSYM_BUFSIZE: usize = 64;

// Table of wrapped allocation functions, along with internal metadata
// TODO: more functions, e.g., memalign(), valloc(), aligned_alloc()???
struct FunctionTable {
    // Internal bookkeeping
    initialized: spin::Once<()>,
    initializing: bool,
    dlsym_buf: [u8; DLSYM_BUFSIZE],
    dlsym_ofs: usize,

    // Function pointers
    malloc_fn: MallocFnType,
    free_fn: FreeFnType,
    calloc_fn: CallocFnType,
    realloc_fn: ReallocFnType,
    reallocarray_fn: ReallocArrayFnType,
    posix_memalign_fn: POSIXMemalignFnType,
}

#[inline]
unsafe fn load_next_func(fn_name: &str) -> *mut c_void {
    let func = libc::dlsym(libc::RTLD_NEXT, fn_name.as_ptr() as *const i8);
    if func.is_null() { panic!("Function {} not found", fn_name); }
    func
}

impl FunctionTable {
    const fn new() -> FunctionTable {
        FunctionTable {
            initialized: spin::Once::new(),
            initializing: false,
            dlsym_buf: [0; DLSYM_BUFSIZE],
            dlsym_ofs: 0,
            malloc_fn:         None,
            free_fn:           None,
            calloc_fn:         None,
            realloc_fn:        None,
            reallocarray_fn:   None,
            posix_memalign_fn: None,
        }
    }

    fn init(&self) {
        self.initialized.call_once(|| unsafe {
            let mut_self: *mut FunctionTable = mem::transmute(self);
            (*mut_self).initializing = true;
            (*mut_self).malloc_fn         = mem::transmute(load_next_func("malloc\0"));
            (*mut_self).free_fn           = mem::transmute(load_next_func("free\0"));
            (*mut_self).calloc_fn         = mem::transmute(load_next_func("calloc\0"));
            (*mut_self).realloc_fn        = mem::transmute(load_next_func("realloc\0"));
            (*mut_self).reallocarray_fn   = mem::transmute(load_next_func("reallocarray\0"));
            (*mut_self).posix_memalign_fn = mem::transmute(load_next_func("posix_memalign\0"));
            (*mut_self).initializing = false;
        });
    }
}

type SizeMap = HashMap<*mut c_void, usize>;

// We build our own dual-purpose global allocator: we use it both
// for the calls we wrap, and for any Rust allocations made by our code
struct ZeroAllocator {
    fn_table: FunctionTable,
    size_map: Mutex<Option<SizeMap>>,
}

impl ZeroAllocator {
    const fn new() -> ZeroAllocator {
        ZeroAllocator {
            fn_table: FunctionTable::new(),
            size_map: Mutex::new(None),
        }
    }

    fn fn_table(&self) -> &FunctionTable {
        self.fn_table.init();
        &self.fn_table
    }

    fn size_map(&self) -> MappedMutexGuard<SizeMap> {
        MutexGuard::map(self.size_map.lock(), |sm| {
            sm.get_or_insert_with(SizeMap::default)
        })
    }
}

unsafe impl alloc::GlobalAlloc for ZeroAllocator {
    unsafe fn alloc(&self, layout: alloc::Layout) -> *mut u8 {
        let mut out = ptr::null_mut();
        let mfn = self.fn_table().posix_memalign_fn.unwrap();
        let res = mfn(&mut out, layout.align(), layout.size());
        if res != 0 {
            panic!("Failed allocation, code: {}", res);
        };
        out as *mut u8
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: alloc::Layout) {
        let mfn = self.fn_table().free_fn.unwrap();
        mfn(ptr as *mut c_void);
    }
}

// Allocator for our own internal use
#[global_allocator]
static mut ZA: ZeroAllocator = ZeroAllocator::new();

// Functions exported externally
#[no_mangle]
pub unsafe extern fn malloc(size: size_t) -> *mut c_void {
    // Use calloc to allocate zeroed memory
    let mfn = ZA.fn_table().calloc_fn.unwrap();
    let ptr = mfn(1, size);
    #[cfg(feature = "debug-print")] eprintln!("Malloc:{}@{:p}", size, ptr);
    if !ptr.is_null() {
        let ir = ZA.size_map().insert(ptr, size);
        assert!(ir.is_none(), "Address already allocated: {:p}", ptr);
    };
    ptr
}

#[no_mangle]
pub unsafe extern fn free(ptr: *mut c_void) {
    let mfn = ZA.fn_table().free_fn.unwrap();
    mfn(ptr);
    if !ptr.is_null() {
        let rr = ZA.size_map().remove(&ptr);
        assert!(rr.is_some(), "Bad free() address: {:p}", ptr);
        #[cfg(feature = "debug-print")] eprintln!("Free:{:?}@{:p}", rr, ptr);
    }
}

#[no_mangle]
pub unsafe extern fn calloc(nmemb: size_t, size: size_t) -> *mut c_void {
    if ZA.fn_table.initializing {
        // HACK: we create a call cycle between dlsym() and calloc()
        // during initialization, because dlsym() internally calls calloc()
        // To avoid a deadlock, we reserve a special static buffer, i.e.,
        // dlsym_buf in FunctionTable, which we use to hand out memory to dlsym
        let total_size = nmemb.checked_mul(size).unwrap();
        assert!(ZA.fn_table.dlsym_ofs + total_size <= DLSYM_BUFSIZE);
        let res = ZA.fn_table.dlsym_buf.as_ptr()
            .offset(ZA.fn_table.dlsym_ofs as isize);
        ZA.fn_table.dlsym_ofs += total_size;
        return res as *mut c_void;
    };

    let mfn = ZA.fn_table().calloc_fn.unwrap();
    let ptr = mfn(nmemb, size);
    // FIXME: crashes if enabled
    //#[cfg(feature = "debug-print")] eprintln!("Calloc:{}@{:p}", size, ptr);
    if !ptr.is_null() {
        let total_size = nmemb.checked_mul(size).unwrap();
        let ir = ZA.size_map().insert(ptr, total_size);
        assert!(ir.is_none(), "Address already allocated: {:p}", ptr);
    };
    ptr
}

unsafe fn realloc_internal<F>(ptr: *mut c_void, size: size_t, f: F) -> *mut c_void
        where F: FnOnce() -> *mut c_void {
    let mut sm = ZA.size_map();
    let old_size = if ptr.is_null() {
        0
    } else {
        *sm.get(&ptr).unwrap()
    };
    if size == old_size {
        return ptr;
    }

    let new_ptr = f();
    if new_ptr == ptr && !ptr.is_null() {
        // Same pointer, the allocation expanded or shrank in place
        // If the pointer isn't NULL, the entry should already exist
        *sm.get_mut(&ptr).unwrap() = size;
    } else {
        // The data moved, update the size map for both pointers
        if !ptr.is_null() {
            let rr = sm.remove(&ptr);
            assert!(rr.is_some(), "Bad realloc() address: {:p}", ptr);
        }
        if !new_ptr.is_null() {
            let ir = sm.insert(new_ptr, size);
            assert!(ir.is_none(), "Address already allocated: {:p}", new_ptr);
        }
    };
    if !new_ptr.is_null() && size > old_size {
        // Zero out the additional space, if any
        ptr::write_bytes(new_ptr.offset(old_size as isize), 0, size - old_size);
    }
    #[cfg(feature = "debug-print")] eprintln!("Realloc:{}@{:p}=>{}@{:p}",
                                              old_size, ptr, size, new_ptr);
    new_ptr
}

#[no_mangle]
pub unsafe extern fn realloc(ptr: *mut c_void, size: size_t) -> *mut c_void {
    realloc_internal(ptr, size, || {
        let mfn = ZA.fn_table().realloc_fn.unwrap();
        mfn(ptr, size)
    })
}

#[no_mangle]
pub unsafe extern fn reallocarray(ptr: *mut c_void,
                                  nmemb: size_t,
                                  size: size_t) -> *mut c_void {
    let total_size = nmemb.checked_mul(size).unwrap();
    realloc_internal(ptr, total_size, || {
        let mfn = ZA.fn_table().reallocarray_fn.unwrap();
        mfn(ptr, nmemb, size)
    })
}

#[no_mangle]
pub unsafe extern fn posix_memalign(memptr: *mut *mut c_void,
                                    alignment: size_t,
                                    size: size_t) -> c_int {
    let mfn = ZA.fn_table().posix_memalign_fn.unwrap();
    let res = mfn(memptr, alignment, size);
    if res == 0 && !memptr.is_null() && !(*memptr).is_null() {
        let ir = ZA.size_map().insert(*memptr, size);
        assert!(ir.is_none(), "Address already allocated: {:p}", *memptr);
    }
    res
}
