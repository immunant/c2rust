From the repository root:

```sh
cargo build
${CARGO_TARGET_DIR:-target}/debug/c2rust-instrument --metadata analysis/tests/misc/instrument.target/debug/metadata.bc -- build --manifest-path analysis/tests/misc/Cargo.toml
(cd analysis/tests/misc/instrument.target/debug; INSTRUMENT_BACKEND=debug INSTRUMENT_RUNTIME=bg METADATA_FILE=metadata.bc ./c2rust-analysis-tests-misc)
```

This instruments the binary built from main.rs with dynamic memory tracing, and
outputs the necessary metadata to match up instrumentation points to source code
into `metadata.bc`. We then run the binary, printing output to the
debug console and using the aforementioned metadata file.