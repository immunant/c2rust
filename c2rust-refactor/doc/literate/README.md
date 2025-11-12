This is a tool for processing Markdown files with refactoring commands embedded
in them.  It runs any refactoring commands it encounters, and inserts a
rendered diff of the results after each refactoring block.

Basic usage:

```sh
uv run python -m literate --project-dir repo/rust render input.md output.md
```

The tool is controlled mainly by specially-tagged code blocks in the Markdown
input.

## Refactoring commands

Most importantly, blocks with their language set to `refactor` are interpreted
as refactoring commands:

    ```refactor
    rewrite_expr '1 + 1' '2'
    ```

For each such block, the block itself is included in the output (with language
changed to `sh`, since refactor commands and arguments are parsed as shell
words), followed by a diff showing the effect it had on the code being
refactored.


## Options

In each code block header, the language can be followed by space-separated
options.  This tool supports the following options:

 * `refactor-target`: Sets the contents of the current block as the current
   crate.  Future `refactor` blocks will rewrite this code instead of the
   contents of the `--project-dir`.

 * `hidden`: The code block is not included in the output.  This allows
   including a `refactor-target` or `refactor-options` block only for its side
   effects, without display it to the reader.

 * `revert`: After processing the block, discard the results of refactoring,
   reverting the crate to its state before the block.  This is only meaningful
   on `refactor` blocks.  This is useful for showing the effects of several
   different refactoring commands on the same initial code.

 * `filename`: Display the filenames of rewritten files.  Default: `true`.
   Disabling this is useful in combination with `refactor-target`, which causes
   refactoring to run on temporary files with generated names.

 * `collapse-diff`: Put the diff inside a `<details>` tag, causing browsers to
   display it collapsed until the reader clicks on it.  Default: `true`.

 * `hide-diff`: Omit the diff entirely, while still including the code block in
   the output.

 * `hide-code`: Omit the code block entirely, while still including the diff in
   the output.

 * `diff-style = full|context|only-new': Sets the diff rendering mode.  This is
   only meaningful on `refactor` blocks.  The default is `context`, which
   renders only changed lines and nearby context.  `full` renders the entire
   contents of both files, which is useful for tutorials, where the files being
   refactored are usually very small.  `only-new` renders only the new file,
   not the old one (and omits insertion/deletion markers).

Boolean options can be prefixed with `no-` to disable them.  For example,
`no-hidden` cancels the effect of a previous `hidden`, causing the block to
once again appear in the output.

Since code block headers are split on whitespace, attributes with values must
not contain space around the `=`.  Write `diff-style=full`, not `diff-style =
full`.

## Global options

Options can be set globally by including a block whose language is
`refactor-options`:

    ```refactor-options hidden
    # This is a comment
    diff-style = full
    revert
    ```

Any options set this way will be applied to all code blocks in the remainder of
the file.  In this example, all later refactoring blocks will produce full
diffs, and will revert changes after processing.  Global options can still be
overridden on specific blocks, so a block whose header reads `refactor
diff-style=context no-revert` would work as before.



# Internals

The central data structure of `literate.py` is the `File` class, defined in
`file.py`.  A `File` initially contains only basic information, like the raw
file contents, but it is incrementally updated and annotated with additional
information.  Part of this process involves constructing a `Diff` from two
files, which provides information needed for additional annotation and eventual
rendering. 

The other essential data structures are lists of `Span`s (`annot.py`) and lists
of `Point`s (`points.py`), which are used in various ways to annotate `File`s
and their `Line`s.

The overall process looks like this (drawn from `__init__.do_render`,
`render.prepare_files`, and `render.make_diff`):

 * Input parsing, which separates code blocks from non-code text (`parse.py`)
 * Execution of the refactoring commands and processing of their outputs
   (`refactor.py`).  This is the point where the `File` objects are created,
   based on the refactoring outputs.
 * Formatting of source files (`format.py`).  `c2rust-refactor` makes no
   attempt to produce nicely-formatted output, so we need to run `rustfmt` on
   the code to produce readable diffs.
 * Syntax highlighting (`highlight.py`).  This mostly amounts to invoking
   `pygments` and annotating the file with the results.
 * Mark processing, to annotate files with the positions and information about
   marked nodes (`marks.py`).
 * `Diff` construction, for each matching pair of old and new files
   (`diff.py`).  This step is partially interleaved with mark processing, as
   some parts of mark processing require access to both files, while some parts
   of diff construction depend on the locations of marks.  See
   `render.make_diff` for specifics.
 * Rendering of the final diff (`render.py`).
