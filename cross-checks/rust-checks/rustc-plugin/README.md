# Rustc cross-checker compiler plugin

This is a simple cross-check inserter for Rust code that is implemented as a Rust compiler plugin.

## Usage

To use the compiler plugin, you need to take several steps.
First, add the plugin as a Cargo dependency to your `Cargo.toml` file:
```
[dependencies]
c2rust-xcheck-plugin = { path = ".../C2Rust/cross-checks/rust-checks/rustc-plugin" }
c2rust-xcheck-derive = { path = ".../C2Rust/cross-checks/rust-checks/derive-macros" }
c2rust-xcheck-runtime = { path = ".../C2Rust/cross-checks/rust-checks/runtime" }
```
with `...` as the full path to the C2Rust repository.
Next, add the following preamble to your `main.rs` or `lib.rs` file:
```rust
#![feature(plugin)]
#![plugin(c2rust_xcheck_plugin)]

#[macro_use]
extern crate c2rust_xcheck_derive;
#[macro_use]
extern crate c2rust_xcheck_runtime;
```

## Cross-checker options
Cross-checking is enabled and configured using the `#[cross_check]` directive,
which can either be enabled globally (using `#![cross_check]` at the beginning of `main.rs` or `lib.rs`) or individually
per function (the per-function settings override the global ones).

The directive optionally takes the following options:
  * `yes` and `enabled` enable cross-checking for the current scope (crate
    or function).
  * `none` and `disabled` disable cross-checking for the current scope.
  * `entry(djb2="foo")` sets the cross-checking name for the current function entry point to the DJB2 hash of `foo`.
  * `entry(fixed=NNN)` sets the cross-checking ID for the current function entry point to `NNN`.

Example:
```rust
#[cross_check(yes, entry(djb2="foo"))]
fn bar() { }

#[cross_check(yes, entry(fixed=0x1234))]
fn baz() { }

#[cross_check(no)]
fn foo() { }
```
