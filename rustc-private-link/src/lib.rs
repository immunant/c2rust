use std::{env, path::PathBuf, process::Command};

pub struct SysRoot {
    path: PathBuf,
}

impl SysRoot {
    pub fn resolve() -> Self {
        let rustc = env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
        let output = Command::new(rustc)
            .args(&["--print", "sysroot"])
            .output()
            .expect("could not invoke `rustc` to find rust sysroot");
        let path = String::from_utf8(output.stdout)
            .expect("`rustc --print sysroot` is not UTF-8")
            .trim_end()
            .into();
        Self { path }
    }

    pub fn set_env(&self) {
        println!("cargo:rustc-env=RUST_SYSROOT={}", self.path.display());
    }

    pub fn link_rustc_private(&self) {
        let lib_dir = self.path.join("lib");
        println!("cargo:rustc-link-search={}", lib_dir.display());
    }
}
