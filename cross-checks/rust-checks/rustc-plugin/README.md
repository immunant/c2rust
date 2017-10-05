# Rustc cross-checker compiler plugin

This is a simple cross-check inserter for Rust code that is implemented as a Rust compiler plugin.

## Usage

To use the compiler plugin, you need to take several steps.
First, add the plugin as a Cargo dependency to your `Cargo.toml` file:
```
[dependencies]
xcheck-plugin = { path = ".../C2Rust/cross-checks/rust-checks/rustc-plugin" }
xcheck-runtime = { path = ".../C2Rust/cross-checks/rust-checks/runtime" }
```
with `...` as the full path to the C2Rust repository.
Next, add the following preamble to your `main.rs` or `lib.rs` file:
```rust
#![feature(plugin)]
#![plugin(xcheck_plugin)]
```

## Cross-checker options
Cross-checking is enabled and configured using the `#[cross_check]` directive,
which can either be enabled globally (using `#![cross_check]` at the beginning of `main.rs` or `lib.rs`) or individually
per function (the per-function settings override the global ones).

The directive optionally takes the following options:
  * `always`, `yes` and `enable` enable cross-checking for the current scope (crate
    or function).
  * `never`, `no` and `disable` disable cross-checking for the current scope.
  * `name="foo"` sets the cross-checking name for the current scope to `foo`.
  * `id=NNN` sets the cross-checking ID for the current scope to `NNN`;
    overrides `name=foo` if both are present.

Example:
```rust
#[cross_check(yes, name=foo)]
fn bar() { }

#[cross_check(yes, id=0x1234)]
fn baz() { }

#[cross_check(no)]
fn foo() { }
```
