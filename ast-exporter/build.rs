#![feature(iterator_find_map)]
extern crate bindgen;
extern crate cmake;
extern crate clang_sys;

use std::env;
use std::process::{self, Command, Stdio};
use std::path::{Path, PathBuf};
use cmake::Config;

// Use `cargo build -vv` to get detailed output on this script's progress.

fn main() {
    let llvm_config = find_llvm_config();

    // Build the exporter library and link it (and its dependencies)
    build_native(&llvm_config);

    // Generate ast_tags and ExportResult bindings
    generate_bindings().err().map(|e| {
        eprintln!("{}", e);
        check_clang_version(&llvm_config).err().map(|ver_e| {
            eprintln!("{}", ver_e);
        });
        process::exit(1);
    });
}

/// Search for an available llvm-config binary in PATH
fn find_llvm_config() -> String {
    env::var("LLVM_CONFIG_PATH").ok().or({
        [
            "llvm-config-7.0",
            "llvm-config-6.1",
            "llvm-config-6.0",
        ].iter().find_map(|c| {
            if Command::new(c)
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
                .is_ok() {
                    Some(String::from(*c))
                } else {
                    None
                }
        })
    }).unwrap_or(String::from("llvm-config"))
}

fn find_llvm_libdir(llvm_config: &str) -> String {
    let path_str = env::var("LLVM_LIB_DIR").ok().or_else(|| {
        let output = Command::new(&llvm_config)
                        .arg("--libdir")
                        .output();

        output.ok().map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
    }).expect(
"
Couldn't find LLVM lib dir. Try setting the `LLVM_LIB_DIR` environment
variable or make sure `llvm-config` is on $PATH then re-build. For example:

  $ export LLVM_LIB_DIR=/usr/local/opt/llvm/lib
"
    );
    String::from(Path::new(&path_str).canonicalize().unwrap().to_string_lossy())
}

fn check_clang_version(_llvm_config: &str) -> Result<(), String> {
    // Check that bindgen is using the same version of libclang and the clang
    // invocation that it pulls -isystem from. See Bindings::generate() for the
    // -isystem construction.
    if let Some(clang) = clang_sys::support::Clang::find(None, &[]) {
        let libclang_version = bindgen::clang_version().parsed.ok_or("Could not parse version of libclang in bindgen")?;
        let clang_version = clang.version.ok_or("Could not parse version of clang executable in clang-sys")?;
        let libclang_version_str = format!(
            "{}.{}",
            libclang_version.0,
            libclang_version.1,
        );
        let clang_version_str = format!(
            "{}.{}",
            clang_version.Major,
            clang_version.Minor,
        );
        if libclang_version.0 != clang_version.Major as u32
            || libclang_version.1 != clang_version.Minor as u32 {
                return Err(format!(
                    "
Bindgen requires a matching libclang and clang installation. Bindgen is using
libclang version ({libclang}) which does not match the autodetected clang
version ({clang}). If you have clang version {libclang} installed, please set
the `CLANG_PATH` environment variable to the path of this version of the clang
binary.",
                    libclang=libclang_version_str,
                    clang=clang_version_str,
                ));
            }
    }

    Ok(())
}

fn generate_bindings() -> Result<(), &'static str> {
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

        // Finish the builder and generate the bindings.
        .generate()
        .or(Err("Unable to generate AST bindings"))?;

    let cppbindings = bindgen::Builder::default()
        .header("src/ExportResult.hpp")
        .whitelist_type("ExportResult")
        .generate_comments(true)
        .derive_default(true)
        // Finish the builder and generate the bindings.
        .generate()
        .or(Err("Unable to generate ExportResult bindings"))?;


    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_dir.join("bindings.rs"))
        .expect("Couldn't write bindings!");
    cppbindings
        .write_to_file(out_dir.join("cppbindings.rs"))
        .expect("Couldn't write cppbindings!");

    Ok(())
}

/** Call out to CMake, build the exporter library, and tell cargo where to look for it.
  * Note that `CMAKE_BUILD_TYPE` gets implicitly determined:
  *
  *   - if `opt-level=0`                              then `CMAKE_BUILD_TYPE=Debug`
  *   - if `opt-level={1,2,3}` and not `debug=false`, then `CMAKE_BUILD_TYPE=RelWithDebInfo`
  */
fn build_native(llvm_config: &str) {
    // Find where the (already built) LLVM lib dir is
    let llvm_lib = find_llvm_libdir(llvm_config);

    let dst = Config::new("src")
        // Where to find LLVM/Clang CMake files
        .define("LLVM_DIR",           &format!("{}/cmake/llvm",  llvm_lib))
        .define("Clang_DIR",          &format!("{}/cmake/clang", llvm_lib))

        // What to build
        .build_target("clangAstExporter")
        .build();

    let out_dir = dst.display();


    // Statically link against static TinyCBOR lib
    println!("cargo:rustc-link-search={}/build/tinycbor/lib", out_dir);
    println!("cargo:rustc-link-lib=static=tinycbor");

    // Statically link against 'clangAstExporter'
    println!("cargo:rustc-link-search={}/build", out_dir);
    println!("cargo:rustc-link-lib=static={}", "clangAstExporter");

    // Link against these Clang libs and libLLVM.so. The ordering here is
    // important! Libraries must be listed before their dependencies when
    // statically linking.
    println!("cargo:rustc-link-search={}", llvm_lib);
    for lib in vec![
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
      "LLVM",
    ] {
        println!("cargo:rustc-link-lib={}", lib);
    }

    // Dynamically link against any system libraries required by LLVM.
    let system_libs = env::var("LLVM_SYSTEM_LIBS").or_else(|_| {
        Command::new(llvm_config)
            .arg("--system-libs")
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
    }).unwrap_or(String::new());
    for lib in system_libs.split_whitespace() {
        println!("cargo:rustc-link-lib={}", lib.trim_left_matches("-l"));
    }

    // Dynamically link against the C++ std library.
    if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-lib=c++");
    } else {
        println!("cargo:rustc-link-lib=stdc++");
    }
}
