# Rustc cross-checker compiler plugin

This is a simple cross-check inserter for Rust code that is implemented as a Rust compiler plugin.

## Usage

To use the compiler plugin, you need to take several steps.
First, add the plugin as a Cargo dependency to your `Cargo.toml` file:
```
[dependencies]
cross-checks = { path = ".../C2Rust/cross-checks/rust-checks/rustc-plugin" }
```
with `...` as the full path to the C2Rust repository.
Next, add the following preamble to each of your `.rs` files:
```
#![feature(plugin)]
#![plugin(cross_checks)]
#![cross_check]
```
Finally, create a `build.rs` file next to `Cargo.toml` in your crate and add
these lines:
```
fn main() {
    println!("cargo:rustc-link-lib=dylib=clevrbuf");
    println!("cargo:rustc-link-search=native=.../ReMon/libclevrbuf");
}
```
replacing `...` with the path to your ReMon repository.

## Cross-checker options
Cross-checking is enabled and configured using the `#[cross_check]` directive,
which can either be enabled per-crate (as in the preamble above), or separately
per function (the per-function settings override the global ones).

The directive optionally takes the following options:
  * `always`, `yes`, and `enable` enable cross-checking for the current scope (crate
    or function).
  * `never`, `no`, and `disable` disable cross-checking for the current scope.
  * `name="foo"` sets the cross-checking name for the current scope to `foo`.
  * `id=NNN` sets the cross-checking ID for the current scope to `NNN`;
    overrides `name=foo` if both are present.

Example:
```
#[cross_check(yes, name=foo)]
fn bar() { }

#[cross_check(yes, id=0x1234)]
fn baz() { }
```
