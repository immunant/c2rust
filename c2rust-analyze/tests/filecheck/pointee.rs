use std::ptr;

// CHECK-LABEL: fn memcpy1
unsafe fn memcpy1(dest: *mut (), src: *const ()) {
    // CHECK: let dest = (dest);
    let dest = dest as *mut u8;
    // CHECK: let src = (src);
    let src = src as *const u8;
    *dest = *src;
}

// CHECK-LABEL: fn remove_cast
unsafe fn remove_cast() {
    let src: u8 = 1;
    let mut dest: u8 = 2;
    // CHECK: let src_ptr = ((&*(&src)));
    let src_ptr = &src as *const u8 as *const ();
    // CHECK: let dest_ptr = ((&mut *(&mut dest)));
    let dest_ptr = &mut dest as *mut u8 as *mut ();
    memcpy1(dest_ptr, src_ptr);
}
