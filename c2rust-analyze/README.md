```sh
cargo run --bin c2rust-analyze -- tests/filecheck/insertion_sort.rs -L "$(rustc --print sysroot)/lib/rustlib/x86_64-unknown-linux-gnu/lib" --crate-type rlib
```

This should produce a large amount of debug output, including a table at the
end listing the type and expression rewrites the analysis has inferred for the
`insertion_sort` function.
