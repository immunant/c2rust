use libc;

#[cfg(
    not(
        source_header = "/home/miguelsaldivar/workspace/C2Rust/dependencies/llvm-6.0.1/build.donna/lib/clang/6.0.1/include/stddef.h"
    )
)]
pub mod stddef_h {
    pub type size_t = libc::c_ulong;
    use super::libc;
}

