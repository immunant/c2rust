extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
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

    println!("cargo:rustc-link-search=/Users/emertens/Source/c2rust/c2rust/dependencies/llvm-6.0.1/build.argolis.galois.com/lib");
}
