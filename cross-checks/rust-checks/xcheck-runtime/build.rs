fn main() {
    println!("cargo:rustc-link-lib=dylib=clevrbuf");
    println!("cargo:rustc-link-search=native=../../ReMon/libclevrbuf");
}
