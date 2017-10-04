#[cfg(feature="xcheck-with-dlsym")]
fn add_libclevrbuf() {
}

#[cfg(not(any(feature="xcheck-with-dlsym")))]
fn add_libclevrbuf() {
    // TODO: find a way to get the full path
    println!("cargo:rustc-link-lib=dylib=clevrbuf");
    println!("cargo:rustc-link-search=native=../../ReMon/libclevrbuf");
}

fn main() {
    add_libclevrbuf();
}
