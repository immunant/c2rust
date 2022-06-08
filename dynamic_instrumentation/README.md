The dynamic instrumentation is designed to be run in the root directory of the crate to be instrumented.

```sh
$ c2rust instrument metadata.bc <path/to/c2rust/analysis/runtime/>
```

This instruments the binary built from main.rs with dynamic memory tracing, and
outputs the necessary metadata to match up instrumentation points to source code
into `metadata.bc`. We then run the binary, logging output to `log.bc` 
and using the aformentioned metadata file. By default, the instrumented binary 
is `target/debug/c2rust-analysis-test`.

```sh
$ INSTRUMENT_BACKEND=log INSTRUMENT_OUTPUT=log.bc METADATA_FILE=metadata.bc <path/to/instrumented-binary>
```

For example, to instrument and run `analysis/test`:

```sh
$ cd ~/c2rust/
$ cargo build --features dynamic-instrumentation
$ cd analysis/test/
$ cargo clean
$ ~/c2rust/target/debug/c2rust instrument metadata.bc ../runtime/
$ INSTRUMENT_BACKEND=log INSTRUMENT_OUTPUT=log.bc METADATA_FILE=metadata.bc ./target/debug/c2rust-analysis-test
```