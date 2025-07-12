use std::env;

use c2rust_build_paths::SysRoot;

fn main() {
    let sysroot = SysRoot::resolve();
    sysroot.link_rustc_private();

    let profile = env::var("PROFILE").expect("`cargo` should set `$PROFILE`");
    println!("cargo:rustc-env=PROFILE={profile}");
}
