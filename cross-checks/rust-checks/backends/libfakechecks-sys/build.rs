fn main() {
    let here = ::std::path::PathBuf::from(::std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let cross_checks_path = here
        .parent()
        .and_then(|x| x.parent())
        .and_then(|x| x.parent())
        .unwrap();
    let libfakechecks_path = cross_checks_path.join("libfakechecks");
    println!("cargo:rustc-link-lib=dylib=fakechecks");
    println!(
        "cargo:rustc-link-search=native={}",
        libfakechecks_path.display()
    );
}
