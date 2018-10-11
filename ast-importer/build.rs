extern crate bindgen;
extern crate cmake;

use std::env;
use std::process::Command;
use std::path::PathBuf;
use cmake::Config;

// Use `cargo build -vv` to get detailed output on what this script's progress.

fn main() {

    // Find where the (already built) LLVM lib dir is
    let llvm_lib_dir: String = env::var("LLVM_LIB_DIR").ok().or_else(|| {
        let output = Command::new("llvm-config")
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

    // Build the exporter library and link it (and its dependencies) in
    build_ast_exporter(&llvm_lib_dir);

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
        .header("../ast-exporter/ast_tags.hpp")
        .generate_comments(true)
        .derive_default(true)
        .rustified_enum("ASTEntryTag")
        .rustified_enum("TypeTag")
        .rustified_enum("StringTypeTag")

        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    let cppbindings = bindgen::Builder::default()
        .header("../ast-exporter/ExportResult.hpp")
        .whitelist_type("ExportResult")
        .generate_comments(true)
        .derive_default(true)
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");


    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
    cppbindings
        .write_to_file(out_path.join("cppbindings.rs"))
        .expect("Couldn't write cppbindings!");

}



/** Call out to CMake, build the exporter library, and tell cargo where to look for it.
  * Note that `CMAKE_BUILD_TYPE` gets implicitly determined:
  *
  *   - if `opt-level=0`                              then `CMAKE_BUILD_TYPE=Debug`
  *   - if `opt-level={1,2,3}` and not `debug=false`, then `CMAKE_BUILD_TYPE=RelWithDebInfo`
  */
fn build_ast_exporter(llvm_lib: &str) {

    // This is where the C++ source lives.
    // Note that `rerun-if-changed` does not recursively check directories!
    println!("cargo:rerun-if-changed=../ast-exporter");
    println!("cargo:rerun-if-changed=../ast-exporter/*");

    let dst = Config::new("../ast-exporter")
        .generator("Ninja")
        .no_build_target(true)
        .out_dir("../dependencies")

        // General CMake variables
        .define("CMAKE_C_COMPILER",   "clang")
        .define("CMAKE_CXX_COMPILER", "clang++")

        // Where to find LLVM/Clang CMake files
        .define("LLVM_DIR",           &format!("{}/cmake/llvm",  llvm_lib))
        .define("CLANG_DIR",          &format!("{}/cmake/clang", llvm_lib))

        // What to build
        .build_arg("clangAstExporter")
        .build();

    let out_dir = dst.display();


    /* When you build the 'ast-exporter' executable with CMake, it internally
     * computes all of the transitive library dependencies and forwards those
     * to the linker invocation.
     *
     * Unfortunately, I haven't found a nice way to get CMake to output those
     * to that 'build.rs' can pick them up list them here. As a stopgap
     * solution, I ran CMake on the 'ast-exporter' exporter and intercepted
     * the 'ld' call to figure out which the libraries we need to link in.
     *
     * TODO: find a better way to do this!!!
     */

    // Link against these system libs
    println!("cargo:rustc-link-lib=z");
    println!("cargo:rustc-link-lib=curses");
    println!("cargo:rustc-link-lib=m");
    println!("cargo:rustc-link-lib=c++");

    // Link against these LLVM/Clang libs
    println!("cargo:rustc-link-search={}", llvm_lib);
    for lib in vec![
      "clangAST",
      "clangFrontend",
      "clangTooling",
      "clangBasic",
      "clangASTMatchers",
      "clangParse",
      "LLVMMCParser",
      "clangSerialization",
      "clangSema",
      "clangEdit",
      "clangAnalysis",
      "LLVMBitReader",
      "LLVMProfileData",
      "clangDriver",
      "LLVMOption",
      "clangFormat",
      "clangToolingCore",
      "clangRewrite",
      "clangLex",
      "LLVMCore",
      "LLVMBinaryFormat",
      "LLVMMC",
      "LLVMSupport",
      "LLVMDemangle",
    ] {
        println!("cargo:rustc-link-lib={}", lib);
    }

    // Link against static TinyCBOR lib
    println!("cargo:rustc-link-search={}/build/tinycbor/lib", out_dir);
    println!("cargo:rustc-link-lib=static=tinycbor");

    // Link against 'clangAstExporter'
    println!("cargo:rustc-link-search={}/build", out_dir);
    println!("cargo:rustc-link-lib=static={}", "clangAstExporter");
}
