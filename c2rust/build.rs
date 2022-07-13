use rustc_private_link::SysRoot;

fn main() {
    let sysroot = SysRoot::resolve();
    sysroot.set_env_rustlib();
}
