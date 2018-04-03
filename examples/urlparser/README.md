# url parser

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Required Manual Changes

No manual changes are required.

## Required Extractor Params

Pass the `-Wwrite-strings` flag to ensure global strings are exported correctly.

## Required Importer Params

Pass the `--translate-entry` flag to the importer so that a valid rust main function is generated from `test.c`. The relooper is not required.

## Linking

No linking is required.
