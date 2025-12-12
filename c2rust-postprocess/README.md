# LLM-based postprocessing of c2rust transpiler output

This is currently a prototype effort to gauge the extent to which LLMs can 
accelerate the types of translation and migration that help move C code to Rust.

# Prerequisites

- Python 3.12 or later
- `uv` in path
- A valid `GEMINI_API_KEY` set
- A transpiled codebase with a correct `compile_commands.json`

# Running

- `c2rust-postprocess path/to/compile_commands.json`, or
- `uv run postprocess path/to/compile_commands.json`

# Testing

## Test prerequisites

- `bear` and `c2rust` in path 

```
   uv run pytest -v
   uv run pytest -v tests/test_utils.py # filter tests to run
```

## Misc

- `uv run ruff check --fix .` to format & lint

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

