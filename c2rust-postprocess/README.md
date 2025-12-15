# LLM-based postprocessing of c2rust transpiler output

This is currently a prototype effort to gauge the extent to which LLMs can 
accelerate the types of translation and migration that help move C code to Rust.

# Prerequisites

- Python 3.12 or later
- `uv` in path
- A valid `GEMINI_API_KEY` set
- A transpiled codebase with `*.c_decls.json` for each `*.rs` you want to transfer comments to.
- `../tools/split_rust/target/release/split_rust`
- `../tools/merge_rust/target/release/merge_rust`

# Running

- `c2rust-postprocess path/to/transpiled_rust.rs`, or
- `uv run postprocess path/to/transpiled_rust.rs`

# Testing

## Test prerequisites

- `bear` and `c2rust` in path

```sh
uv run pytest -v
uv run pytest -v tests/test_utils.py # filter tests to run
```

## Misc

- `uv run ruff format` to format
- `uv run ruff check --fix .` to lint

# TODOs

- testable prototype
  - [x] gemini api support
    + using synchronous API, tabled async API for now
  - file-based caching of model responses
    + storage format could be improved to make it easier to create
      golden input/output pairs for testing
  - pluggable support for getting definitions
  - verifying correctness of responses 
  - filtering by file and function name
  - openai model support
  - antropic model support
  - openrouter API support?
  - non-trivial: use async support to speed up postprocessing
    + supported by gemini api, IDK about others

