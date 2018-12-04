# Building the manual

1. `cargo install mdbook` (requires version 0.2.\*)
2. `./build.sh`
3. The manual should now be available in the `book` subdirectory.

# Adding to the manual

1. Add a new Markdown file somewhere in the repository.
2. Edit `SUMMARY.md` and add a link to the new file.
   Use a path relative to the repository root.

Actually, the paths in `SUMMARY.md` are relative to the `manual/docs`
subdirectory, but `build.sh` copies all .md files in the repo into that
directory (preserving directory structure) as part of its build process.  So
all repository-relative .md paths work in `SUMMARY.md`, but so do some
additional paths (notably `_generated/*`) which don't exist in the repository.

## Generated docs

To add generated docs, such as the `rust-refactor` command documentation,
update `build.sh` to run the generator command during the manual's build
process.  By convention, generated docs are placed in the `_generated`
subdirectory, so that it's clear when looking at `SUMMARY.md` that they aren't
present in the actual repository.

Note that `build.sh` will not pick up generated files left in the repository,
as it ignores any files that are listed in `.gitignore`.  Your `build.sh`
modifications must explicitly copy any generated files into `docs/_generated`.

