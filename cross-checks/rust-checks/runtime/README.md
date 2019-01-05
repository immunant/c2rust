# Runtime library for the Rust cross-checker

This library is used together with the compiler plugin.
For usage information, see the [README](../rustc-plugin/) file for the plugin.

## Cargo features
This library can be built with several Cargo features enabled:
  * `xcheck-with-dlsym` uses the `dlsym` function to dynamically resolve the
    `rb_xcheck` symbol at run-time, instead of having to statically link to the
    `libclevrbuf.so` library. This allows more flexibility when building the
target crate, since `libclevrbuf.so` does not need to be present during
linking. It also makes building for placeholders, e.g., `libfakechecks.so`,
easier.

  * `xcheck-with-weak` is an experimental (not currently working) approach to
    dynamic resolution of `rb_xcheck`, using weak symbols instead of `dlsym`.
However, Rust does not currently implement weak symbols the same way C does, so
this feature does not work.

  * `libc-hash` enables the specialization of `CrossCheckHash` for types in the
    `libc` crate, currently only `libc::c_void`. This feature is recommended
    when cross-checking translated Rust programs against their C equivalents.
