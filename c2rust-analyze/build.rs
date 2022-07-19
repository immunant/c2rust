use rustc_private_link::SysRoot;

fn main() {
    let sysroot = SysRoot::resolve();
    sysroot.link_rustc_private();

    print!("cargo:rustc-env=C2RUST_TARGET_LIB_DIR=");
    print_bytes::println_bytes(&sysroot.rustlib());
}
