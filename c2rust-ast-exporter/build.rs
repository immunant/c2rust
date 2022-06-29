use clang_sys::CXVersion;
use cmake::Config;
use color_eyre::eyre::{ensure, eyre};
use color_eyre::{eyre, Help, Report};
use std::ffi::OsStr;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::{env, fmt};

// Use `cargo build -vv` to get detailed output on this script's progress.

fn main() -> eyre::Result<()> {
    // Safety: At the top of `main`, only 1 thread.
    #[allow(unused_unsafe)]
    unsafe {
        env::set_var("RUST_BACKTRACE", "full");
    }
    color_eyre::install()?;
    env_logger::init();

    let llvm_info = LLVMInfo::new()?;

    // Build the exporter library and link it (and its dependencies)
    llvm_info.build_native();
    // Generate ast_tags and ExportResult bindings
    generate_bindings().with_warning(|| match check_clang_version() {
        Ok(()) => eyre!("clang version okay"),
        Err(e) => e,
    })?;
    Ok(())
}

struct LibClangVersion((u32, u32));

impl LibClangVersion {
    pub fn as_major_minor(&self) -> (i32, i32) {
        let (major, minor) = self.0;
        (major.try_into().unwrap(), minor.try_into().unwrap())
    }
}

impl Display for LibClangVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (major, minor) = self.0;
        write!(f, "{major}.{minor}")
    }
}

struct ClangVersion(CXVersion);

impl ClangVersion {
    pub fn as_major_minor(&self) -> (i32, i32) {
        let CXVersion {
            Major: major,
            Minor: minor,
            Subminor: _,
        } = self.0;
        (major, minor)
    }
}

impl Display for ClangVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let CXVersion {
            Major: major,
            Minor: minor,
            Subminor: _,
        } = self.0;
        write!(f, "{major}.{minor}")
    }
}

fn check_clang_version() -> eyre::Result<()> {
    // Check that bindgen is using the same version of libclang and the clang
    // invocation that it pulls -isystem from. See Bindings::generate() for the
    // -isystem construction.
    if let Some(clang) = clang_sys::support::Clang::find(None, &[]) {
        let libclang_version = bindgen::clang_version()
            .parsed
            .map(LibClangVersion)
            .ok_or(eyre!("Could not parse version of libclang in bindgen"))?;
        let clang_version = clang.version.map(ClangVersion).ok_or(eyre!(
            "Could not parse version of clang executable in clang-sys"
        ))?;
        ensure!(
            libclang_version.as_major_minor() == clang_version.as_major_minor(),
            "
        Bindgen requires a matching libclang and clang installation. 
        Bindgen is using libclang version ({libclang_version}), 
        which does not match the autodetected clang version ({clang_version}). 
        If you have clang version {libclang_version} installed, 
        please set the `CLANG_PATH` environment variable 
        to the path of this version of the clang binary."
        );
    }

    Ok(())
}

fn generate_bindings() -> eyre::Result<()> {
    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        // Do not generate unstable Rust code that
        // requires a nightly rustc and enabling
        // unstable features.
        // .no_unstable_rust()
        // The input header we would like to generate
        // bindings for.
        .header("src/ast_tags.hpp")
        .generate_comments(true)
        .derive_default(true)
        .rustified_enum("ASTEntryTag")
        .rustified_enum("TypeTag")
        .rustified_enum("StringTypeTag")
        .rustified_enum("BuiltinVaListKind")
        // Tell bindgen we are processing c++
        .clang_arg("-xc++")
        // Finish the builder and generate the bindings.
        .generate()
        .note("Unable to generate AST bin")?;

    let cppbindings = bindgen::Builder::default()
        .header("src/ExportResult.hpp")
        .allowlist_type("ExportResult")
        .generate_comments(true)
        .derive_default(true)
        // Tell bindgen we are processing c++
        .clang_arg("-xc++")
        .clang_arg("-std=c++11")
        // Finish the builder and generate the bindings.
        .generate()
        .note("Unable to generate ExportResult bindings")?;

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_dir = PathBuf::from(env::var("OUT_DIR")?);
    bindings
        .write_to_file(out_dir.join("bindings.rs"))
        .note("Couldn't write bindings!")?;
    cppbindings
        .write_to_file(out_dir.join("cppbindings.rs"))
        .note("Couldn't write cppbindings!")?;

    Ok(())
}

/// Holds information about LLVM paths we have found
struct LLVMInfo {
    /// LLVM lib dir containing libclang* and libLLVM* libraries
    pub lib_dir: PathBuf,
    /// LLVM cmake dir containing cmake modules
    pub cmake_dir: PathBuf,
    /// Clang cmake dir containing cmake modules
    pub clang_cmake_dir: PathBuf,
    /// List of libs we need to link against
    pub libs: Vec<String>,
}

impl LLVMInfo {
    fn new() -> eyre::Result<Self> {
        fn find_llvm_config() -> eyre::Result<PathBuf> {
            // Explicitly provided path in LLVM_CONFIG_PATH
            env::var("LLVM_CONFIG_PATH")
                .map(PathBuf::from)
                .or_else(|e| {
                    // Relative to LLVM_LIB_DIR
                    env::var("LLVM_LIB_DIR")
                        .map_err(Report::new)
                        .and_then(|llvm_lib_dir| {
                            let path = Path::new(&llvm_lib_dir).join("../bin/llvm-config");
                            Ok(fs_err::canonicalize(&path)?)
                        })
                        .error(e)
                })
                .or_else(|e| {
                    // In PATH
                    [
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
                        // Homebrew install locations on MacOS
                        "/usr/local/opt/llvm@13/bin/llvm-config",
                        "/usr/local/opt/llvm@12/bin/llvm-config",
                        "/usr/local/opt/llvm@11/bin/llvm-config",
                        "/usr/local/opt/llvm@10/bin/llvm-config",
                        "/usr/local/opt/llvm@9/bin/llvm-config",
                        "/usr/local/opt/llvm@8/bin/llvm-config",
                        "/usr/local/opt/llvm/bin/llvm-config",
                    ]
                    .iter()
                    .map(Path::new)
                    .find_map(|cmd| {
                        Command::new(cmd)
                            .stdout(Stdio::null())
                            .stderr(Stdio::null())
                            .spawn()
                            .map(|_| cmd.to_path_buf())
                            .ok()
                    })
                    .ok_or_else(|| eyre!("can't find llvm-config"))
                    .warning(e)
                })
        }

        /// Invoke given `command`, if any, with the specified arguments.
        fn invoke_command<I, S>(command: &Path, args: I) -> eyre::Result<String>
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>,
        {
            let output = Command::new(command).args(args).output()?;
            let output = if output.status.success() {
                Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
            } else {
                Err(eyre!("empty output"))
            }?;
            Ok(output)
        }

        // no need to avoid the llvm_config error here since we unconditionally use it later
        let llvm_config = find_llvm_config()?;
        let llvm_config_missing = "
        Couldn't find `llvm-config`. Make sure `llvm-config` is on $PATH then
        re-build.
        ";
        let llvm_config_libdir_missing = "
        Couldn't find LLVM lib dir. Try setting the `LLVM_LIB_DIR` environment
        variable or make sure `llvm-config` is on $PATH then re-build. For example:

          $ export LLVM_LIB_DIR=/usr/local/opt/llvm/lib
        ";
        let llvm_config_cmakedir_missing = "
        Couldn't find LLVM cmake dir. Try setting the `LLVM_CMAKE_DIR` environment
        variable or make sure `llvm-config` is on $PATH then re-build. For example:

          $ export LLVM_CMAKE_DIR=/usr/local/opt/llvm/lib/cmake/llvm
        ";
        let clang_cmakedir_missing = "
        Couldn't find Clang cmake dir. Try setting the `CLANG_CMAKE_DIR` environment
        variable or make sure `llvm-config` is on $PATH then re-build. For example:

          $ export CLANG_CMAKE_DIR=/usr/local/opt/llvm/lib/cmake/clang
        ";
        let lib_dir = {
            let path_str = env::var("LLVM_LIB_DIR")
                .or_else(|e| invoke_command(&llvm_config, &["--libdir"]).error(e))
                .note(llvm_config_libdir_missing)?;
            fs_err::canonicalize(&path_str)?
        };
        let cmake_dir = {
            let path_str = env::var("LLVM_CMAKE_DIR")
                .or_else(|e| invoke_command(&llvm_config, &["--cmakedir"]).error(e))
                .note(llvm_config_cmakedir_missing)?;
            fs_err::canonicalize(&path_str)?
        };
        let clang_cmake_dir = {
            let path_str = env::var("CLANG_CMAKE_DIR")
                .map(PathBuf::from)
                .unwrap_or_else(|_| cmake_dir.join("../clang"));
            fs_err::canonicalize(&path_str).note(clang_cmakedir_missing)?
        };

        let llvm_shared_libs = invoke_command(&llvm_config, &["--libs", "--link-shared"]);

        // `<sysroot>/lib/rustlib/<target>/lib/` contains a libLLVM DSO for the
        // rust compiler. On MacOS, this lib is named libLLVM.dylib, which will
        // always conflict with the dylib we are trying to link against. On
        // Linux we generally will not hit this issue because the prebuilt lib
        // includes the `svn` suffix. This would conflict with a source build
        // from master, however.
        //
        // We check here if the lib we want to link against will conflict with
        // the rustlib version. If so we can't dynamically link against libLLVM.
        let conflicts_with_rustlib_llvm = {
            if let Ok(llvm_shared_libs) = llvm_shared_libs.as_ref() {
                let dylib_suffix = {
                    if cfg!(target_os = "macos") {
                        ".dylib"
                    } else {
                        ".so"
                    } // Windows is not supported
                };
                let mut dylib_file = String::from("lib");
                dylib_file.push_str(llvm_shared_libs.trim_start_matches("-l"));
                dylib_file.push_str(dylib_suffix);
                let sysroot =
                    invoke_command(&env::var("RUSTC").map(PathBuf::from)?, &["--print=sysroot"])?;

                // Does <sysroot>/lib/rustlib/<target>/lib/<dylib_file> exist?
                let mut libllvm_path = PathBuf::new();
                libllvm_path.push(sysroot);
                libllvm_path.push("lib/rustlib");
                libllvm_path.push(env::var("TARGET")?);
                libllvm_path.push("lib");
                libllvm_path.push(dylib_file);

                libllvm_path.as_path().exists()
            } else {
                false
            }
        };

        let link_statically = cfg!(feature = "llvm-static") || {
            let args = if conflicts_with_rustlib_llvm {
                vec!["--shared-mode", "--ignore-libllvm"]
            } else {
                vec!["--shared-mode"]
            };
            invoke_command(&llvm_config, &args).map_or(false, |c| c == "static")
        };

        let link_mode = if link_statically {
            "--link-static"
        } else {
            "--link-shared"
        };

        let llvm_major_version = {
            let version = invoke_command(&llvm_config, &["--version"]).note(llvm_config_missing)?;
            let msg = || eyre!("invalid version string {}", &version);
            version
                .split('.')
                .next()
                .ok_or_else(msg)?
                .parse::<u32>()
                .with_note(msg)?
        };

        // Construct the list of libs we need to link against
        let mut args = vec![
            "--libs",
            link_mode,
            "MC",
            "MCParser",
            "Support",
            "Option",
            "BitReader",
            "ProfileData",
            "BinaryFormat",
            "Core",
        ];
        if llvm_major_version >= 10 {
            args.push("FrontendOpenMP");
        }

        let mut libs = invoke_command(&llvm_config, &args)
            .unwrap_or_else(|_| "-lLLVM".into())
            .split_whitespace()
            .map(|lib| lib.trim_start_matches("-l").into())
            .collect::<Vec<_>>();

        libs.extend(
            env::var("LLVM_SYSTEM_LIBS")
                .or_else(|e| invoke_command(&llvm_config, &["--system-libs", link_mode]).error(e))
                .unwrap_or_default()
                .split_whitespace()
                .map(|lib| lib.trim_start_matches("-l").into()),
        );

        Ok(Self {
            lib_dir,
            cmake_dir,
            clang_cmake_dir,
            libs,
        })
    }

    /// Call out to CMake, build the exporter library, and tell cargo where to look
    /// for it.  Note that `CMAKE_BUILD_TYPE` gets implicitly determined by the
    /// cmake crate according to the following:
    ///
    ///   - if `opt-level=0`                              then `CMAKE_BUILD_TYPE=Debug`
    ///   - if `opt-level={1,2,3}` and not `debug=false`, then `CMAKE_BUILD_TYPE=RelWithDebInfo`
    fn build_native(&self) {
        let LLVMInfo {
            lib_dir: llvm_lib_dir,     // Find where the (already built) LLVM lib dir is
            cmake_dir: llvm_cmake_dir, // Find where the (already built) LLVM cmake module dir is
            clang_cmake_dir,           // Find where the Clang cmake module dir is
            libs: _,
        } = &self;

        match env::var("C2RUST_AST_EXPORTER_LIB_DIR") {
            Ok(libdir) => {
                println!("cargo:rustc-link-search=native={}", libdir);
            }
            Err(_) => {
                // Build libclangAstExporter.a with cmake
                let dst = Config::new("src")
                    // Where to find LLVM/Clang CMake files
                    .define("LLVM_DIR", llvm_cmake_dir)
                    .define("Clang_DIR", clang_cmake_dir)
                    // What to build
                    .build_target("clangAstExporter")
                    .build();

                let out_dir = dst.display();

                // Set up search path for newly built tinycbor.a and libclangAstExporter.a
                println!("cargo:rustc-link-search=native={}/build/lib", out_dir);
                println!("cargo:rustc-link-search=native={}/build", out_dir);
            }
        };

        // Statically link against 'clangAstExporter' which requires 'tinycbor'
        println!("cargo:rustc-link-lib=static=tinycbor");
        println!("cargo:rustc-link-lib=static=clangAstExporter");

        println!("cargo:rustc-link-search=native={}", llvm_lib_dir.display());

        // Some distro's, including arch and Fedora, no longer build with
        // BUILD_SHARED_LIBS=ON; programs linking to clang are required to
        // link to libclang-cpp.so instead of individual libraries.
        let use_libclang = if cfg!(target_os = "macos") {
            false
        } else {
            // target_os = "linux"
            let mut libclang_path = PathBuf::new();
            libclang_path.push(llvm_lib_dir);
            libclang_path.push("libclang-cpp.so");
            libclang_path.exists()
        };

        if use_libclang {
            println!("cargo:rustc-link-lib=clang-cpp");
        } else {
            // Link against these Clang libs. The ordering here is important! Libraries
            // must be listed before their dependencies when statically linking.
            for lib in &[
                "clangTooling",
                "clangFrontend",
                "clangASTMatchers",
                "clangParse",
                "clangSerialization",
                "clangSema",
                "clangEdit",
                "clangAnalysis",
                "clangDriver",
                "clangFormat",
                "clangToolingCore",
                "clangAST",
                "clangRewrite",
                "clangLex",
                "clangBasic",
            ] {
                println!("cargo:rustc-link-lib={}", lib);
            }
        }

        for lib in &self.libs {
            // IMPORTANT: We cannot specify static= or dylib= here because rustc
            // will reorder those libs before the clang libs above which don't have
            // static or dylib.
            println!("cargo:rustc-link-lib={}", lib);
        }

        // Link against the C++ std library.
        if cfg!(target_os = "macos") {
            println!("cargo:rustc-link-lib=c++");
        } else {
            println!("cargo:rustc-link-lib=stdc++");
        }
    }
}
