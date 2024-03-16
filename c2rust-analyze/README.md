# Usage

Build `c2rust-analyze`:

```sh
cargo build --release
```

Then, in the directory of a cargo project you wish to rewrite, run
`c2rust-analyze` on the project:

```sh
.../path/to/c2rust/target/release/c2rust-analyze build |& tee c2rust-analyze.log
```

`c2rust-analyze` is currently at a prototype stage and produces verbose debug
output by default; the use of `tee` to capture the output to a log file allows
inspecting the results even when they exceed the length of the terminal
scrollback buffer.

`c2rust-analyze` does not modify the target project's source code by default;
it only prints the rewritten code to standard output.  Look for `=====
BEGIN/END =====` markers in the output to see the proposed rewritten code for
each file, or rerun with the `--rewrite-in-place` option (that is,
`c2rust-analyze --rewrite-in-place build`) to apply the rewrites directly to
the source files.

`c2rust-analyze` may take a long time to run even on medium-sized codebases.
In particular, running the Polonius analysis on very large functions may take
several minutes (though Polonius results are cached after the first run).  For
testing, it may be useful to comment out some modules from `lib.rs` to speed up
the analysis.


## Known limitations

The automated safety rewrites in `c2rust-analyze` only apply to a small subset
of unsafe Rust code.  When `c2rust-analyze` encounters unsupported code, it
will report an error and skip rewriting the function in question.

Other notable limitations:

* `c2rust-analyze` does not remove the `unsafe` keyword from function
  definitions, even when it succeeds at removing all unsafe operations from the
  function.  The user must remove the `unsafe` keyword manually where it is
  appropriate to do so.

  Note that even if a function contains only safe operations, it might still
  need to be marked `unsafe` if it could break an invariant that other code
  relies on for safety.  For example, `Vec::set_len` only writes to the
  `self.len` field (a safe operation), but it can be used to violate the
  invariant `self.len <= self.cap`, which `Vec::as_slice` relies on for safety.

* In non-amalgamated builds, where cross-module function calls use `extern "C"
  { fn foo(); }` in the calling module and `#[no_mangle] fn foo() { ... }` in
  the callee, `c2rust-analyze` may rewrite the signature of the `#[no_mangle]`
  function definition in a way that's incompatible with the corresponding
  `extern "C"` declaration in another module.  This can lead to segfaults or
  other undefined behavior at run time.  This can be avoided by using an
  amalgamated build of the C code (where all functions are placed in one
  module), or by manually editing the function definition and/or declaration
  after rewriting to ensure that the signatures match up.
