use std::{
    env,
    path::{Path, PathBuf},
    process::Command,
};

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

    pub fn sysroot(&self) -> &Path {
        self.path.as_path()
    }

    pub fn lib(&self) -> PathBuf {
        self.sysroot().join("lib")
    }

    pub fn rustlib(&self) -> PathBuf {
        let target = env::var_os("TARGET").expect("cargo should set $TARGET");
        [
            self.sysroot(),
            Path::new("lib"),
            Path::new("rustlib"),
            Path::new(&target),
            Path::new("lib"),
        ]
        .iter()
        .collect()
    }

    pub fn set_env_rust_sysroot(&self) {
        println!("cargo:rustc-env=RUST_SYSROOT={}", self.sysroot().display());
    }

    pub fn set_env_rustlib(&self) {
        println!("cargo:rustc-env=RUSTLIB={}", self.rustlib().display());
    }

    pub fn link_rustc_private(&self) {
        let lib = self.lib();
        println!("cargo:rustc-link-search={}", lib.display());
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lib.display());
    }
}
