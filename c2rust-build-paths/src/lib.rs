use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    str,
};

use print_bytes::println_lossy;

fn print_cargo_path(name: &str, path: &Path) {
    print!("cargo:{name}");
    println_lossy(path);
}

pub struct SysRoot {
    path: PathBuf,
}

impl SysRoot {
    pub fn resolve() -> Self {
        let rustc = env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
        let output = Command::new(rustc)
            .args(["--print", "sysroot"])
            .output()
            .expect("could not invoke `rustc` to find rust sysroot");
        // trim, but `str::trim` doesn't exist on `[u8]`
        let path = output
            .stdout
            .as_slice()
            .split(|c| c.is_ascii_whitespace())
            .next()
            .unwrap_or_default();
        #[cfg(unix)]
        let path = {
            use std::os::unix::ffi::OsStrExt;

            OsStr::from_bytes(path)
        };
        #[cfg(not(unix))]
        let path = {
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

    pub fn link_rustc_private(&self) {
        let lib = self.lib();
        print_cargo_path("rustc-link-search=native=", &lib);
        print_cargo_path("rustc-link-arg=-Wl,-rpath,", &lib);
    }
}

pub fn find_llvm_config() -> Option<PathBuf> {
    // Explicitly provided path in LLVM_CONFIG_PATH
    env::var_os("LLVM_CONFIG_PATH")
        .map(PathBuf::from)
        .or_else(|| {
            // Relative to LLVM_LIB_DIR
            env::var_os("LLVM_LIB_DIR")
                .map(PathBuf::from)
                .map(|mut lib_dir| {
                    lib_dir.push("../bin/llvm-config");
                    lib_dir.canonicalize().unwrap()
                })
        })
        .or_else(|| {
            // In PATH
            [
                "llvm-config-21",
                "llvm-config-20",
                "llvm-config-19",
                "llvm-config-18",
                "llvm-config-17",
                "llvm-config-16",
                "llvm-config-15",
                "llvm-config-14",
                "llvm-config-13",
                "llvm-config-12",
                "llvm-config-11",
                "llvm-config-10",
                "llvm-config-9",
                "llvm-config-8",
                "llvm-config-7",
                "llvm-config-7.0",
                "llvm-config",
                // Homebrew install locations on Intel macOS
                "/usr/local/opt/llvm@21/bin/llvm-config",
                "/usr/local/opt/llvm@20/bin/llvm-config",
                "/usr/local/opt/llvm@19/bin/llvm-config",
                "/usr/local/opt/llvm@18/bin/llvm-config",
                "/usr/local/opt/llvm@17/bin/llvm-config",
                "/usr/local/opt/llvm@16/bin/llvm-config",
                "/usr/local/opt/llvm@15/bin/llvm-config",
                "/usr/local/opt/llvm@14/bin/llvm-config",
                "/usr/local/opt/llvm@13/bin/llvm-config",
                "/usr/local/opt/llvm@12/bin/llvm-config",
                "/usr/local/opt/llvm@11/bin/llvm-config",
                "/usr/local/opt/llvm@10/bin/llvm-config",
                "/usr/local/opt/llvm@9/bin/llvm-config",
                "/usr/local/opt/llvm@8/bin/llvm-config",
                "/usr/local/opt/llvm/bin/llvm-config",
                // Homebrew install locations on Apple Silicon macOS
                "/opt/homebrew/opt/llvm@21/bin/llvm-config",
                "/opt/homebrew/opt/llvm@20/bin/llvm-config",
                "/opt/homebrew/opt/llvm@19/bin/llvm-config",
                "/opt/homebrew/opt/llvm@18/bin/llvm-config",
                "/opt/homebrew/opt/llvm@17/bin/llvm-config",
                "/opt/homebrew/opt/llvm@16/bin/llvm-config",
                "/opt/homebrew/opt/llvm@15/bin/llvm-config",
                "/opt/homebrew/opt/llvm@14/bin/llvm-config",
                "/opt/homebrew/opt/llvm@13/bin/llvm-config",
                "/opt/homebrew/opt/llvm@12/bin/llvm-config",
                "/opt/homebrew/opt/llvm@11/bin/llvm-config",
                "/opt/homebrew/opt/llvm@10/bin/llvm-config",
                "/opt/homebrew/opt/llvm@9/bin/llvm-config",
                "/opt/homebrew/opt/llvm@8/bin/llvm-config",
                "/opt/homebrew/opt/llvm/bin/llvm-config",
            ]
            .iter()
            .map(Path::new)
            .filter(|c| {
                Command::new(c)
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .spawn()
                    .is_ok()
            })
            .map(PathBuf::from)
            .next()
        })
}
