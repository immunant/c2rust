use rustc_private_link::SysRoot;

fn main() {
    let sysroot = SysRoot::resolve();
    sysroot.set_env_rust_sysroot();
    // Not strictly needed here since this is not used as this is a library crate (it's needed in `c2rust`),
    // but it doesn't hurt, and in case this ever adds a binary crate, it's useful.
    sysroot.link_rustc_private();
}
