#[cfg(not(source_header = "/home/miguelsaldivar/workspace/misc/buffer/buffer.h"))]
pub mod foo_h {
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct foo_t {
        pub len: i32,
    }
}

