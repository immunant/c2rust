# C2Rust CRISP tools

- `merge_rust`
- `split_rust`
- `split_ffi_entry_points`

## Building

These tools rely on rust-analyzer's libraries, so they need a relatively recent version of Rust. Run `cargo build --release` in their respective directories to build them.

## Running

`split_rust` and `merge_rust` expect the root source file of a Rust project as their first argument, e.g. `lib.rs` or `main.rs`.

- `merge_rust` modifies the specified codebase in-place.
- `split_rust` emits JSON on standard output.

`split_ffi_entry_points` expects a Rust project directory (the directory containing a `Cargo.toml` file) as its only argument.

It modifies that Rust project in-place.
