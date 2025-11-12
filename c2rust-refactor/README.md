[![Docs]][docs.rs]

[Docs]: https://docs.rs/c2rust-refactor/badge.svg
[docs.rs]: https://docs.rs/c2rust-refactor

# C2Rust Refactoring Tool

This is a refactoring tool for Rust programs, aimed at removing unsafety from
automatically-generated Rust code.


## Usage

`c2rust refactor` command line usage is as follows:

```sh
c2rust refactor [flags] <command> [command args] -- <input file> [rustc flags]
```

Flags for `c2rust refactor` are described by `c2rust refactor --help`.

See [the command documentation (online manual)](https://c2rust.com/manual/c2rust-refactor/commands.html)
for a list of commands, including complete usage and descriptions. 
Multiple commands can be separated by an argument consisting of a single
semicolon, as in `c2rust refactor cmd1 arg1 \; cmd2 arg2`.
(Note the semicolon needs to be escaped to prevent it from being interpreted by
the shell.)

`c2rust refactor` requires `rustc` command line arguments for the program to be
refactored, so that it can use `rustc` to load and typecheck the source code.
For projects built with `cargo`, pass the `--cargo` flag to `c2rust refactor`
and it will obtain the right arguments from `cargo` automatically.  Otherwise,
you must provide the `rustc` arguments on the `c2rust refactor` command line,
after a `--` separator.


## Marks

Some commands require the user to "mark" some AST nodes for it to operate on.
For example, the `rename_struct` command requires that the user mark the
declaration of the struct that should be renamed.

Each mark associates a "label" with a specific AST node (identified by its
`NodeId`).  Labels are used to distinguish different types of marks, and a
single node can have any number of marks with distinct labels.  For example,
when running the `func_to_method` command, which turns functions into methods
in an inherent `impl`, the user must mark the functions to move with the
`target` label and must mark the destination `impl` with the `dest` label.
Nodes marked with other labels will be ignored.  The set of labels recognized
by a command is described in the command's documentation; by default, most
commands that use marks operate on `target`.

The most flexible way of marking nodes is by using the
[`select`](https://c2rust.com/manual/c2rust-refactor/commands.html#select)
command.  See the command documentation and `src/select/mod.rs` for details.
Note that marks are not preserved across `c2rust refactor` invocations, so you
usually want to run `select` followed by the command of interest using the `;`
separator mentioned above.
