This tool intercepts uses `ptrace` to intercept crashes caused by invalid
pointer dereferences inside C2Rust cross-checking code, and resumes the
crashing program on a secondary control flow path that correctly cross-checks
the invalid pointer. This is mainly useful when cross-checking programs in
logging mode, and should be run as:
```
  $ c2rust-xcheck-pointer-tracer <program> -- <arguments>
```
