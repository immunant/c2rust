extern crate bindgen;
extern crate cmake;

use std::env;
use std::path::PathBuf;
use cmake::Config;

// This build.rs can take a while. Use `cargo build -vv` to more progress
// output (eg. CMake's).

fn main() {

    build_ast_exporter(false, 1);

    // Tell cargo to tell rustc to link the system bzip2
    // shared library.
    // println!("cargo:rustc-link-lib=bz2");

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

 //   println!("cargo:rustc-link-search=/Users/emertens/Source/c2rust/c2rust/dependencies/llvm-6.0.1/build.argolis.galois.com/lib");
}

// Call out to CMake, build the exporter library, and tell cargo where to look for it.
// Note that `CMAKE_BUILD_TYPE` gets implicitly determined:
//
//    * if `opt-level=0`                              then `CMAKE_BUILD_TYPE=Debug`
//    * if `opt-level={1,2,3}` and not `debug=false`, then `CMAKE_BUILD_TYPE=RelWithDebInfo`
//
// TODO: turn LLVM source into a gitsubmodule.
fn build_ast_exporter(assertions: bool, max_link_jobs: i32) {

    println!("cargo:rerun-if-changed=../dependencies/llvm-6.0.1/src");

    // This takes a while and gives no output. Build progress is outputted to
    // the `../dependencies/llvm-6.0.1/build/.ninja_log` file.
    let dst = Config::new("../dependencies/llvm-6.0.1/src")
        .generator("Ninja")
        .no_build_target(true)
        .out_dir("../dependencies/llvm-6.0.1")

        // General CMake variables
        .define("CMAKE_C_COMPILER",   "clang")
        .define("CMAKE_CXX_COMPILER", "clang++")

        // LLVM/Clang variables (see https://llvm.org/docs/CMake.html#llvm-specific-variables)
        .define("LLVM_ENABLE_ASSERTIONS",  if assertions { "1" } else { "0" })
        .define("LLVM_TARGETS_TO_BUILD",   "X86")
        .define("LLVM_INCLUDE_UTILS",      "1")
        .define("BUILD_SHARED_LIBS",       "1")  // fiddle with this to get static libraries built instead
        .define("LLVM_PARALLEL_LINK_JOBS", &max_link_jobs.to_string())

        // What to build
        .build_arg("clangAstExporter")
        .build();

    // Tell Cargo where to find these libraries
    println!("cargo:rustc-link-search={}/build/lib", dst.display());
    println!("cargo:rustc-link-lib={}", "clangAstExporter");
}
