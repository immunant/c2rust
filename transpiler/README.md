# transpiler

The transpiler is used in concert with the ast-exporter to translate C code to Rust. This process was split into two steps such that we could use the clang compiler front end to parse C code but write the Rust code generation using Rust itself.

## Building

You must build the `idiomize` tool first following the instructions in this [README](https://github.com/GaloisInc/C2Rust/blob/master/rust-refactor/README.md).

Then run `cargo +c2rust build` in `$C2RUST_ROOT/transpiler`. 
