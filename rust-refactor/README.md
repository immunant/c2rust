# Idiomize

This is a refactoring tool for Rust programs, aimed at removing unsafety from
automatically-generated Rust code and transforming it into a more idiomatic
style.


## Building

Install cargo and rustup first.

Build `idiomize` with `cargo build`.


## Usage

`idiomize` command line usage is as follows:

    idiomize [idiomize flags] <command> [command args] -- <input file> [rustc flags]

Flags for `idiomize` are described by `idiomize --help`.

Commands are not currently documented in a central location - grep the source
for `register` to see what commands are currently implemented.  The required
arguments (and marks) are usually documented in the comments on the command
implementation.

All arguments after the `--` are passed to `rustc`.  Since `idiomize` runs
`rustc` analysis passes up through typechecking, the provided flags must
include any `-L` or `--extern` options needed to find external libraries.  In
particular, `-L .../rust/build/<TRIPLE>/stage2/lib/rustlib/<TRIPLE>/lib` is
usually required to find a compatible version of `libstd`.

Note that you may also need to add `.../rust/build/<TRIPLE>/stage2/lib` to
`LD_LIBRARY_PATH` in order to run `idiomize`.


## Marks

Some commands require the user to "mark" some AST nodes for it to operate on.
For example, the `rename_struct` command requires that the user mark the
declaration of the struct that should be renamed.

Each mark associates a "label" with a specific AST node (identified by its
`NodeId`).  Labels are used to distinguish different types of marks.  For
example, when running the `func_to_method` command, which turns functions into
methods in an inherent `impl`, the user must mark the functions to move with
the `target` label and must mark the destination `impl` with the `dest` label.
Nodes marked with other labels will be ignored.  The set of labels recognized
by a command is usually documented in the comments for that command; when
unspecified, the default label is `target`.

A single node can be marked with two labels at once, though this is not often
useful.

In command line usage, there are two ways to specify marks.

The more common way to set a mark is to use the `-c` (`--cursor`) flag, which
adds a mark at a specific position in the source code.  The `-c` flag takes
colon-separated file, line, column, label, and node kind arguments, as in `-c
test.rs:10:20:target:item`, and marks the node at the indicated position in the
file.  If multiple nodes of the indicated kind overlap the indicated position,
the deepest one in the AST will be selected.  If the label and/or kind
arguments are omitted, they default to `target` and `any` respectively.  The
other supported node kinds are defined in `src/pick_node.rs`.

The other way to specify marks is to use the `-m` (`--mark`) flag, which takes
colon-separated node ID and label arguments.  This form is more precise than
`--cursor` but more difficult to use because node IDs are hard to predict.
However, some commands print out node IDs, and the `-m` flag allows them to be
passed back into `idiomize`.


