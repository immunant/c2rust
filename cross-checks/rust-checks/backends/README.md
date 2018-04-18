# Cross-check backends for the rustc plugin
This directory contains several cross-check backends which implement or forward
the `rb_xcheck` function used by the `runtime` crate:
* The `libclevrbuf-sys` backend links in `libclevrbuf.so` and uses its implementation
  of `rb_xcheck`. Note that, due to limitations in cargo and rustc, this
backend does not add the full path of `libclevrbuf.so` to RPATH, so the path
must be in `LD_LIBRARY_PATH` at run-time, e.g., when running `cargo run`.
* `libfakechecks-sys` uses the native `fakechecks` library with the same
  goal and limitations.
* `xcheck-dlsym` uses `dlopen` and `dlsym` to locate `rb_xcheck` at run-time,
  by loading the dynamic library specified in the `RB_XCHECK_LIB` environment
variable. This lets us choose at run-time which implementation of `rb_xcheck`
we want.
