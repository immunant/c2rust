This is the top-level directory for all cross-checking components, and contains the following:

 * A clang plugin that automatically inserts cross-check instrumentation into C code.
 
 * An equivalent rustc compiler plugin for Rust.
 
 * The `libfakechecks` cross-checking backend library that prints out all cross-checks to standard output.
 This library is supported by both the C and Rust compiler plugins.
 
 * Our experimental fork of the `ReMon` MVEE modified for C/Rust side-by-side checking,
 along with the `mvee-configs` directory that contains some MVEE configuration examples.
