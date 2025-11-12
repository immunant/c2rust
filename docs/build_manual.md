## Building the manual

1. `cargo install --git https://github.com/immunant/mdBook.git --branch installable`
   * may require `--force` if you already have `mdbook` installed
   * requires custom changes to resolve symlinks; hopefully it will be merged into upstream soon
2. `mdbook build` in the root source directory
3. The manual should now be available in the `book` subdirectory.

## Adding to the manual

1. Add a new Markdown file somewhere in the repository.
2. Edit `manual/SUMMARY.md` and add a link to the new file.
   Use a path relative to the repository root.
3. Add the new Markdown file to the git index (`git add ...`)
4. Run `scripts/link_manual.py` from the root directory.
   This will create a symlink for the new file in the `manual/` directory.
   This symlink should be added to git as well.

## Generated docs

The `./manual/generator_dispatch.py` script runs as an `mdbook` preprocessor and
replaces `{{#generate quote GEN ARGS}}` anywhere in the book with
the output of running generator `GEN` on `ARGS`.
The set of available generators is defined in `generator_dispatch.py`.

As one example, this is used in [`manual/c2rust-refactor/commands.md`](../manual/c2rust-refactor/commands.md)
to replace the `{{#generate quote refactor_commands}}` placeholder
with auto-generated docs for refactoring commands,
by running `./c2rust-refactor/doc/gen_command_docs.py`.
