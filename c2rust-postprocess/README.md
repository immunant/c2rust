# LLM-based postprocessing of c2rust transpiler output

This tool uses LLMs to accelerate the types of translation and migration that
help move C code to Rust. Compared to a modern coding agent, the postprocessor
is more limited in scope. It is intended as a lightweight cleanup pass that 
can be run immediately after c2rust transpile and c2rust refactor. 

This is a good step to run before a human or a heavyweight agentic workflow
attempts to produce fully safe and idiomatic Rust; not a replacement for these efforts. 

# Prerequisites

- Python 3.12 or later
- `uv` in path
- A valid `OPENAI_API_KEY`, `GEMINI_API_KEY`, or `OPENROUTER_API_KEY` in the environment
- A transpiled codebase with `*.c_decls.json` for each `*.rs` you want to transfer comments to.
- `../tools/split_rust/target/release/split_rust`
- `../tools/merge_rust/target/release/merge_rust`

# Running

- `c2rust-postprocess path/to/transpiled_rust.rs`, or
- `uv run postprocess path/to/transpiled_rust.rs`

## Excluding/Filtering

`c2rust-postprocess` has a few ways to filter/exclude the function identifiers that are processed.

`--ident-filter` simply takes a regex.
Anything matching the regex is filtered out of being processed.
This is very useful for on-the-fly filtering that's easy to change quickly.

`--exclude-file` is for more granular, more permanent filtering/exclusion.
It takes a path to an exclude file, which we tend to call `postprocess-exclude.yml`.
This is a YAML file containing the file paths and function identifiers to exclude.
For example,

```yaml
src/lib.rs:
  - foo
  - bar

src/main.rs:
  - main
```

will exclude `fn foo` and `fn bar` in `src/lib.rs`
(resolved relative to the directory of `postprocess-exclude.yml`)
and `fn main` in `src/main.rs`.

We filter based on the Rust file instead of the C file
because the C file may not be available and isn't read by `c2rust-postprocess`
(only the adjacent `*.c_decls.json` file is).

These file paths and function identifiers must match fully.
They are not regexes or globs.

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

