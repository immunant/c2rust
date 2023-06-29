```sh
cargo run --bin c2rust-analyze -- tests/filecheck/insertion_sort.rs -L "$(rustc --print target-libdir)" --crate-type rlib
```

This should produce a large amount of debug output, including a table at the
end listing the type and expression rewrites the analysis has inferred for the
`insertion_sort` function.
