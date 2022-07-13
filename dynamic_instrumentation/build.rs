use rustc_private_link::SysRoot;

fn main() {
    let sysroot = SysRoot::resolve();
    sysroot.link_rustc_private();
    sysroot.set_env();
}
