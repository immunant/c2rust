---
name: cross-checks
description: Use when adding, building, or debugging C2Rust cross-checks — value probes (c2rust_cross_check_value in C, cross_check_value! in Rust), XCHECK log divergence, libfakechecks, the clang xcheck plugin, or MVEE/ReMon lock-step checking.
---

# C2Rust cross-checks

Cross-checks verify that a transpiled Rust program behaves identically to the C
original. A probe hashes a value and emits a `(tag: u8, hash: u64)` pair through
a single C ABI function, `rb_xcheck(uint8_t tag, uint64_t val)`. Run the C and
Rust binaries with the same inputs, then either diff the recorded streams
offline, or let an MVEE (ReMon) compare them in lock-step and halt on the first
divergence.

Docs: `docs/cross-check-tutorial.md` (end-to-end guide),
`docs/cross-check-config.md` (YAML config language),
`docs/cross-check-hash.md` (hash algorithm).

## Tags

Tags identify which probe fired. `0`–`4` are reserved (`Unk`, `Ent`, `Exi`,
`Arg`, `Ret` — see `cross-checks/rust-checks/runtime/src/xcheck.rs`). For
manual value probes pick custom tags `>= 5` (existing tests use `0x80`,
`0xff`). Use the **same tag for the same logical value** on both sides; the
diff is only meaningful if both binaries emit the same probes in the same
order.

## Inserting manual value probes (the currently working path)

### C side

```c
#include <cross_checks.h>   // cross-checks/c-checks/clang-plugin/include/

uint32_t x = compute();
c2rust_cross_check_value(0x80, x);
```

Without the plugin this is a no-op stub, so annotated code still builds with a
plain compiler. When compiled with the clang plugin loaded **and**
`-DC2RUST_CROSS_CHECK_VALUE_REAL`, the plugin rewrites each call into
`rb_xcheck(tag, __c2rust_hash_T(x, depth))`, generating a type-aware hash
function for the argument's type. Example: `cross-checks/c-checks/clang-plugin/test/value1.c`.

To log a raw value without hashing, declare and call `rb_xcheck` directly:
`rb_xcheck(0x80, (uint64_t)x);`.

### Rust side

Add path dependencies (nightly toolchain; these crates are a separate
workspace, not part of the root one):

```toml
[dependencies]
c2rust-xcheck-runtime = { path = "<repo>/cross-checks/rust-checks/runtime" }
c2rust-xcheck-derive = { path = "<repo>/cross-checks/rust-checks/derive-macros" } # only for struct hashing
c2rust-xcheck-backend-libfakechecks-sys = { path = "<repo>/cross-checks/rust-checks/backends/libfakechecks-sys" }
```

```rust
use c2rust_xcheck_runtime::{cross_check_raw, cross_check_value};
use c2rust_xcheck_backend_libfakechecks_sys as _; // force-link the rb_xcheck provider

cross_check_value!(=0x80, x);        // hash x, emit (0x80, hash); `=` marks a literal/expr tag
cross_check_raw!(=0x81, some_u64);   // emit the value itself, unhashed
```

The default hashers (`JodyHasher` for aggregates, `SimpleHasher` for scalars)
match the C runtime's `__c2rust_hash_*` functions, so equal values of
corresponding types produce equal hashes. For structs, derive
`CrossCheckHash`; a 4-argument form of `cross_check_value!` selects custom
hashers.

## Building

1. **Clang plugin** (one-time): `scripts/build_cross_checks.py` — builds the
   YAML-config C API with `cargo +nightly`, then cmake+ninja into
   `build*/clang-xcheck-plugin/` (`plugin/CrossChecks.so`,
   `runtime/libruntime.a`).
2. **libfakechecks**: `make -C cross-checks/libfakechecks` (the wrapper below
   does this automatically).
3. **C binary**: `python3 scripts/cross_check_value_cc_wrapper.py <usual cc
   args>` — wraps clang with the include path, the
   `C2RUST_CROSS_CHECK_VALUE_REAL` define, the plugin-load flags, and
   `--disable-xchecks` (so *only* explicit value probes fire, no automatic
   entry/exit checks), and links `libruntime.a` + `libfakechecks.so`. Needs
   `plumbum` and the plugin already built.
4. **Rust binary**: `cargo +nightly build` with the dependencies above.

## Running and comparing

```sh
FAKECHECKS_OUTPUT_FILE=c.log    ./c_binary    <args>
FAKECHECKS_OUTPUT_FILE=rust.log ./rust_binary <args>   # may need LD_LIBRARY_PATH=<repo>/cross-checks/libfakechecks
diff c.log rust.log
```

Log lines look like `XCHECK(Ent):0x...` for reserved tags and
`XCHECK(0x80):0x...` for custom ones. Without `FAKECHECKS_OUTPUT_FILE` output
goes to stderr; set `FAKECHECKS_APPEND_PID=1` for forking programs (one file
per pid). The first differing line localizes the divergence; add finer-grained
probes (or `cross_check_raw!` to see actual values) between the last matching
and first diverging probe, and bisect.

Alternative backends (`cross-checks/rust-checks/backends/README.md`):
`dynamic-dlsym` resolves `rb_xcheck` at run time from `$RB_XCHECK_LIB`;
`zstd-logging` writes a compressed binary stream to
`$CROSS_CHECKS_OUTPUT_FILE`, decoded with `c2rust-xcheck-zstd-printer`;
`libclevrbuf-sys` feeds ReMon's ring buffer for online MVEE checking (sample
MVEE config: `cross-checks/mvee-configs/fibo_mvee.ini`; the ReMon submodule
was removed, so a ReMon checkout must be supplied externally).

### Taming nondeterminism

If logs diverge on pointer-derived or allocation-dependent values:
`cross-checks/zero-malloc/` is an `LD_PRELOAD` shim that zeroes allocations;
`cross-checks/pointer-tracer/` is a ptrace supervisor that recovers from
segfaults while hashing through invalid pointers. Prefer probing values, not
addresses.

## Automatic function-level checks (C side only)

The clang plugin can also instrument *every* function's entry/exit/args/return
without manual probes, configured by inline annotations
(`CROSS_CHECK(...)` macros in `include/cross_checks.h`) or external YAML
`.c2r` files (`-Xclang -plugin-arg-crosschecks -Xclang -C<file>`); see
`docs/cross-check-config.md` and `examples/{snudown,libxml2,tmux}/repo/xchecks/`
for real configs. There is currently no working Rust-side equivalent (see
below), so for two-sided comparisons use manual value probes, or replicate the
entry/exit checks in Rust by hand with `cross_check_raw!`.

## Known broken — do not attempt

- `c2rust transpile --cross-checks` / `-x`: the emitted crate uses
  `#![plugin(c2rust_xcheck_plugin)]`, a rustc feature removed years ago. The
  output **will not compile**; don't try to fix it inline.
- `cross-checks/rust-checks/rustc-plugin/`: dead code kept for reference,
  excluded from the workspace. Don't add it as a dependency or try to build it.
- Automatic Rust-side instrumentation does not exist right now; the plan is to
  replace the rustc plugin with a proc-macro/instrumentation approach.
- `cargo test` for `rust-checks` is disabled in `scripts/test_cross_checks.py`.
- The runtime's `xcheck-with-dlsym` / `xcheck-with-weak` features are stale;
  use the default strong-symbol linking plus a backend crate instead.
