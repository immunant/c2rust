```sh
cargo run -- analysis_metadata.bc ../analysis/test/src/main.rs -L ~/.rustup/toolchains/nightly-2022-02-14-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/ -L ../target/debug/deps/ --extern c2rust_analysis_rt=../target/debug/deps/libc2rust_analysis_rt-4c5a3dcb09c8161c.rlib
INSTRUMENT_BACKEND=debug METADATA_FILE=analysis_metadata.bc ./main
```

This instruments the binary built from main.rs with dynamic memory tracing, and
outputs the necessary metadata to match up instrumentation points to source code
into `analysis_metadata.bc`. We then run the binary, printing output to the
debug console and using the aformentioned metadata file.