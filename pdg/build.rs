use build_utils::SysRoot;

fn main() {
    let sysroot = SysRoot::resolve();
    sysroot.link_rustc_private();
}
