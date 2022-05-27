The dynamic instrumentation is designed to be run in the root directory of the crate to be instrumented.

```sh
$ c2rust instrument metadata.bc <path/to/c2rust/analysis/runtime/>
```

This instruments the binary built from main.rs with dynamic memory tracing, and
outputs the necessary metadata to match up instrumentation points to source code
into `analysis_metadata.bc`. We then run the binary, logging output to log.bc 
and using the aformentioned metadata file.

```sh
$ INSTRUMENT_BACKEND=log INSTRUMENT_OUTPUT=log.bc METADATA_FILE=metadata.bc <path/to/instrumented-binary>
```