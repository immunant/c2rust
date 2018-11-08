# transpiler

c2rust-transpile is used in concert with c2rust-ast-exporter to translate C code to Rust. This process was split into two steps such that we could use the clang compiler front end to parse C code but write the Rust code generation using Rust itself.

## Building

Then run `cargo build` in `$C2RUST_ROOT/c2rust-transpile`. 
