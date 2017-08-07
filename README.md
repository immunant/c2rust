# Idiomize

This is a refactoring tool for Rust programs, aimed at removing unsafety from
automatically-generated Rust code and transforming it into a more idiomatic
style.


## Building

`idiomize` requires a specific `rustc` version with additional patches.  The
current supported revision is `8f1339af2`, which is the 1.20 nightly build from
2017-07-16.  However, correct operation of `idiomize` requires some fixes to
the Rust pretty printer, which have not been upstreamed yet.  As a result, you
must build the indicated revision from source, after applying the patch
`misc/pprust-expr-precedence.patch`.

After building the correct version of `rustc`, you can compile `idiomize` with
`cargo build`.  You must either extend `PATH` and `LD_LIBRARY_PATH` with
`rust/build/$TARGET/stage2/{bin,lib}`, or (if you are using `rustup`) add
`rust/build/$TARGET/stage2` as a custom toolchain link.

Note that building with the wrong `rustc` may appear to succeed, but running
certain transformations will fail with the message "new and reparsed ASTs
differ" if `idiomize` is built against a `rustc` that does not include the
necessary `pprust` fixes.


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
particular, `-L .../rust/build/$TARGET/stage2/lib/rustlib/$TARGET/lib` is
usually required to find a compatible version of `libstd`.


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


