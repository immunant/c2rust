use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::Command,
    str,
};

use print_bytes::println_bytes;

fn print_cargo_path(name: &str, path: &Path) {
    print!("cargo:{name}");
    println_bytes(path);
}

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
        // trim, but `str::trim` doesn't exist on `[u8]`
        let path = output
            .stdout
            .as_slice()
            .split(|c| c.is_ascii_whitespace())
            .next()
            .unwrap_or_default();
        let path = if cfg!(unix) {
            use std::os::unix::ffi::OsStrExt;

            OsStr::from_bytes(path)
        } else {
            // Windows is hard, so just require UTF-8
            let path = str::from_utf8(path).expect("`rustc --print sysroot` is not UTF-8");
            OsStr::new(path)
        };
        let path = Path::new(path).to_owned();
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
        print_cargo_path("rustc-env=RUST_SYSROOT=", self.sysroot());
    }

    pub fn link_rustc_private(&self) {
        let lib = self.lib();
        print_cargo_path("rustc-link-search=native=", &lib);
        print_cargo_path("rustc-link-arg=-Wl,-rpath,", &lib);
    }
}
