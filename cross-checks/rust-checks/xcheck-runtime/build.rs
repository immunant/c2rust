#[cfg(any(feature="xcheck-with-dlsym", feature="xcheck-with-weak"))]
fn add_libclevrbuf() {
}

#[cfg(not(any(feature="xcheck-with-dlsym", feature="xcheck-with-weak")))]
fn add_libclevrbuf() {
    let here = ::std::path::PathBuf::from(::std::env::current_dir().unwrap());
    let cross_checks_path = here.parent().and_then(|x| x.parent()).unwrap();
    let libclevrbuf_path = cross_checks_path.join("ReMon").join("libclevrbuf");
    println!("cargo:rustc-link-lib=dylib=clevrbuf");
    println!("cargo:rustc-link-search=native={}", libclevrbuf_path.display());
}

fn main() {
    add_libclevrbuf();
}
