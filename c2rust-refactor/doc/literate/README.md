This is a tool for processing Markdown files with refactoring commands embedded
in them.  It runs any refactoring commands it encounters, and inserts a
rendered diff of the results after each refactoring block.

Basic usage:

    python3 -m literate --project-dir repo/rust  render input.md output.md


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

 * `diff-style = full|context': Sets whether the rendered diff should display
   the full contents of the old and new files, or only changed portions and
   nearby context.  This is only meaningful on `refactor` blocks.  The default
   is `context`.  `full` is useful for tutorials, where the files being
   refactored are usually very small.

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
