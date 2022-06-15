// TODO(kkysen) `extern crate`s shouldn't be needed, but not sure how to test this
#[cfg(feature="cross-check")]
extern crate c2rust_xcheck_derive;
#[cfg(feature="cross-check")]
extern crate c2rust_xcheck_runtime;
extern crate autolink;
extern crate buffer;
extern crate markdown;
extern crate stack;
