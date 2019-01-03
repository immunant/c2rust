```refactor-options hidden
revert
diff-style = full
no-show-filename
no-collapse-diff
```


Many refactoring commands in `c2rust-refactor` are designed to work only on
selected portions of the crate, rather than affecting the entire crate
uniformly.  To support this, `c2rust-refactor` has a *mark* system, which
allows marking AST nodes (such as functions, expressions, or type annotations)
with simple string labels.  Certain commands add or remove marks, while others
check the existing marks to identify nodes to transform.

For example, in a program containing several byte string literals, you can use
`select` to mark a specific one:

```rust refactor-target hidden
static B1: &'static [u8] = b"123";
static B2: &'static [u8] = b"abc";
static B3: &'static [u8] = b"!!!";
```

```refactor no-revert
select target 'item(B2); desc(expr);'
```

Then, you can use `bytestr_to_str` to change only the marked byte string to an
ordinary string literal, leaving the others unaffected:

```refactor
bytestr_to_str
```

This ability to limit transformations to specific parts of the program is
useful for refactoring a large codebase incrementally, on a module-by-module or
function-by-function basis.

The remainder of this tutorial describes `select` and related mark-manipulation
commands.  For details of how marks affect various transformation commands, see
the [command documentation](commands.html) or read about the
[`marked!` pattern](rewrite.md#marked) for `rewrite_expr` and other
pattern-matching commands.



# Marks

A "mark" is a short string label that is associated with a node in the AST.
Marks can be applied to nodes of most kinds, including items, expressions,
patterns, type annotations, and so on.  The mark string can be any valid Rust
identifier, though most commands that process marks use short words such as
`target`, `dest`, or `new`.  It's possible to apply multiple distinct marks to
the same node, and it's also possible to mark children of marked nodes
separately from their parents (for example, to mark an expression and one of
its subexpressions).

Here are some examples.

```rust refactor-target hidden
fn f() -> Option<i32> {
    Some(2 + 2)
}

fn g() -> i32 {
    match f() {
        Some(x) => x,
        None => 0,
    }
}
```

```refactor
select target 'crate; desc(match_expr(2 + 2));'
```

The <span style='border: 1px solid'>&#x25b6;</span> ...
<span style='border: 1px solid'>&#x25c0;</span> indicators in the diff show
that the expression `2 + 2` has been marked.  Hover over the indicators for
more details, such as the label of the added mark.

As mentioned above, most kinds of nodes can be marked, not only expressions.
Here we mark a function, a pattern, and a type annotation:

```refactor
select a 'item(f);' ;
select b 'item(g); desc(match_ty(i32));' ;
select c 'item(g); desc(match_pat(Some(x)));' ;
```

As mentioned above, it's possible to mark the same node twice with different
labels.  (Marking it twice with the same label is no different from marking it
once.)  Here's an example of marking a function multiple times:

```refactor
select a 'item(f);' ;
select a 'item(f);' ;
select b 'item(f);' ;
```

As you can see by hovering over the indicators, labels `a` and `b` were both
added to the function `f`.

Marks on a node have no connection to marks on its parent or child nodes.  We
can, for example, mark an expression like `2 + 2`, then separately mark its
subexpressions with either the same or different labels:

```refactor
select a 'item(f); desc(match_expr(2 + 2));' ;
select a 'item(f); desc(match_expr(2)); first;' ;
select b 'item(f); desc(match_expr(2)); last;' ;
```

Hovering over the mark indicators shows precisely what has happened: we marked
both `2 + 2` and the first `2` with the label `a`, and marked the second `2`
with the label `b`.



# The `select` command

The `select` command provides a simple scripting language for applying marks to
specific nodes.  The basic syntax of the command is:

```sh
select LABEL SCRIPT
```

`select` runs a `SCRIPT` (written in the language described below) to obtain a
set of AST nodes, then marks every node in the set with `LABEL`, which should
be a single identifier such as `target`.

More concretely, when running the script, `select` maintains a "current
selection", which is a set of AST nodes.  Script operations (described below)
can extend or modify the current selection.  At the end of the script, `select`
marks every node in the current selection with `LABEL`.

We next describe a few common select script patterns, followed by details on
the available operations and filters.


## Common patterns

### Selecting an item by path

For items such as functions, type declarations, or traits, the `item(path)`
operation selects the item by its path:

```rust refactor-target hidden
fn f() {}
trait T {}
struct S {}
mod m {
    fn g() {}
}
```

```refactor
select target 'item(f);' ;
select target 'item(T);' ;
select target 'item(S);' ;
select target 'item(m::g);' ;
```

Note that this only works for the kinds of items that can be imported via
`use`.  It doesn't handle other kinds of item-like nodes, such as impl methods,
which cannot be imported directly.

### Selecting all nodes matching a filter

The operations `crate; desc(filter);` together select all nodes (or,
equivalently, all descendants of the crate) that match a filter.  For example,
we can select all expressions matching the pattern `2 + 2` using a `match_expr`
filter:

```rust refactor-target hidden
fn f() -> i32 {
    2 + 2
}

const FOUR: i32 = 2 + 2;

static ARRAY: [u8; 2 + 2] = [1, 2, 3, 4];
```

```refactor
select target 'crate; desc(match_expr(2 + 2));'
```

Here we see that `crate; desc(filter);` can find matching items anywhere in the
crate: inside function bodies, constant declarations, and even inside the
length expression of an array type annotation.

### Selecting filtered nodes inside a parent node

In the previous example, `crate; desc(filter);` is made up of two separate
script operations.  `crate` selects the entire crate:

```refactor no-revert
select target 'crate;'
```

Then `desc(filter)` looks for descendants of selected nodes that match
`filter`, and replaces the current selection with the nodes it finds:

```refactor
clear_marks ;
select target 'crate; desc(match_expr(2 + 2));'
```

(Note: we use `clear_marks` here only for illustration purposes, to make the
diff clearly show the changes between the old and new versions of our `select`
command.)

Combining `desc` with operations other than `crate` allows selecting
descendants of only specific nodes.  For example, we can find expressions
matching `2 + 2`, but only within the function `f`:

```refactor
select target 'item(f); desc(match_expr(2 + 2));'
```

In a more complex example, we can use multiple `desc` calls to target an
expression inside of a specific method (recall that methods can't be selected
directly with `item(path)`).  We first select the module containing the impl:

```rust refactor-target hidden
fn f() -> i32 {
    2 + 2
}

mod m {
    struct S;
    impl S {
        fn f(&self) -> i32 {
            2 + 2
        }
    }
}
```

```refactor no-revert
select target 'item(m);'
```

Then we select the method of interest, using the `name` filter (described
below):

```refactor no-revert
clear_marks ;
select target 'item(m); desc(name("f"));'
```

And finally, we select the expression inside the method:

```refactor
clear_marks ;
select target 'item(m); desc(name("f")); desc(match_expr(2 + 2));'
```

Combined with some additional filters described below, this approach is quite
effective for marking nodes that can't be named with an ordinary import path,
such as impl methods or items nested inside functions.


## Script operations

A `select` script can consist of any number of operations, which will be run in
order to completion.  (There is no control flow in `select` scripts.)  Each
operation ends with a semicolon, much like Rust statements.

The remainder of this section documents each script operation.

### `crate`

`crate` (which takes no arguments) adds the root node of the entire crate to
the current selection.  All functions, modules, and other declarations are
descendants of this single root node.

Example:

```rust refactor-target hidden
fn f() -> i32 {
    123
}
mod m {
    static S: i32 = 0;
}
```

```refactor
select target 'crate;'
```

### `item`

`item(p)` adds the item identified by the path `p` to the current selection.
The provided path is handled like in Rust's `use` declarations (except that
only plain paths are supported, not wildcards or curly-braced blocks).

```rust refactor-target hidden
fn f() -> i32 {
    123
}
mod m {
    static S: i32 = 0;
}
```

```refactor
select target 'item(m::S);'
```

Because the `item` operation only adds to the current selection (as opposed to
replacing the current selection with a set containing only the identified
item), we can run `item` multiple times to select several different items at
once:

```refactor
select target 'item(f); item(m::S); item(m);'
```

### `child`

`child(f)` checks each child of each currently selected node against the filter
`f`, and replaces the current selection with the set of matching children.

This can be used, for example, to select a `static`'s type annotation without
selecting type annotations that appear inside its initializer:

```rust refactor-target hidden
static S: i32 = 123_u8 as i32;
const C: u32 = 0;
```

```refactor
select target 'item(S); child(kind(ty));'
```

To illustrate how this works, here is the AST for the `static S` item:

* item `static S`
  * identifier `S` (the name of the `static`)
  * type `i32` (the type annotation of the `static`)
  * expression `123_u8 as i32` (the initializer of the `static`)
    * expression `123_u8` (the input of the cast expression)
    * type `i32` (the target type of the cast expression)

The `static`'s type annotation is a direct child of the static (and has
kind `ty`, matching the `kind(ty)` filter), so the type annotation is selected
by the example command above.  The target type for the cast is not a direct
child of the static - rather, it's a child of the initializer expression, which
is a child of the static - so it is ignored.

### `desc`

`desc(f)` ("descendant") checks each descendant of each currently selected node
against the filter `f`, and replaces the current selection with the set of
matching descendants.  This is similar to `child`, but checks for matching
descendants at any depth, not only matching direct children.

Using the same example as for `child`, we see that `desc`
selects more nodes:

```refactor
select target 'item(S); desc(kind(ty));'
```

Specifically, it selects both the type annotation of the `static` and  the
target type of the cast expression, as both are descendants of the `static`
(though at different depths).  Of course, it still does not select the type
annotation of the `const C`, which is not a descendant of `static S` at any
depth.


Note that `desc` only considers the *strict* descendants of marked nodes - that
is, it does not consider a node to be a "depth-zero" descendant of itself.  So,
for example, the following command selects nothing:

```refactor
select target 'item(S); desc(item_kind(static));'
```

`S` itself is a `static`, but contains no additional statics inside of it, and
`desc` does not consider `S` itself when looking for `item_kind(static)`
descendants.

### `filter`

`filter(f)` checks each currently selected node against the filter `f`, and
replaces the current selection with the set of matching nodes.  Equivalently,
`filter(f)` removes from the current selection any nodes that don't match `f`.

Most uses of the `filter` operation can be replaced by passing a more
appropriate filter expression to `desc` or `child`, so the examples in this
section are somewhat contrived.  (`filter` can still be useful in combination
with `marked`, described below, or in more complex select scripts.)

Here is a slightly roundabout way to select all items named `f`.  First, we
select all items:

```rust refactor-target hidden
fn f() {}
fn g() {}

mod m {
    fn f() {}
}
```

```refactor no-revert
select target 'crate; desc(kind(item));'
```

Then, we use `filter` to keep only items named `f`:

```refactor
clear_marks ;
select target 'crate; desc(kind(item)); filter(name("f"));'
```

With this command, only descendants of crate matching both filters `kind(item)`
and `name("f")` are selected.  (This could be written more simply as `crate;
desc(kind(item) && name("f"));`.)

### `first` and `last`

`first` replaces the current selection with a set containing only the first
selected node.  `last` does the same with the last selected node.  "First" and
"last" are determined by a postorder traversal of the AST, so sibling nodes are
ordered as expected, and a parent node come "after" all of its children.

The `first` and `last` operations are most useful for finding places to insert
new nodes (such as with the [`create_item` command](commands.html#create_item))
while ignoring details such as the specific names or kinds of the nodes around
the insertion point.  For example, we can use `last` to easily select the last
item in a module.  First, we select all the module's items:

```rust refactor-target hidden
mod m {
    fn f() {}
    static S: i32 = 0;
    const C: i32 = 1;
}
```

```refactor no-revert
select target 'item(m); child(kind(item));'
```

Then we use `last` to select only the last such child:

```refactor
clear_marks ;
select target 'item(m); child(kind(item)); last;'
```

Now we could use [`create_item`](commands.html#create_item) to
insert a new item after the last existing one.

### `marked`

`marked(l)` adds all nodes marked with label `l` to the current selection.
This is useful for more complex marking operations, since (together with the
`delete_marks` command) it allows using temporary marks to manipulate multiple
sets of nodes simultaneously.

For example, suppose we wish to select both the first and the last item in a
module.  Normally, this would require duplicating the `select` command, since
both `first` and `last` replace the entire current selection with the single
first or last item.  This would be undesirable if the operations for setting up
the initial set of items were fairly complex.  But with `marked`, we can save
the selection before running `first` and restore it afterward.

```rust refactor-target hidden
mod m {
    fn f() {}
    static S: i32 = 0;
    const C: i32 = 1;
}
```

We begin by selecting all items in the module and saving that selection by
marking it with the `tmp_all_items` label:

```refactor no-revert
select tmp_all_items 'item(m); child(kind(item));'
```

Next, we use `marked` to retrieve the `tmp_all_items` set and take the first
item from it.  This reduces the current selection to only a single item, but
the `tmp_all_items` marks remain intact for later use.

```refactor no-revert
select target 'marked(tmp_all_items); first;'
```

We do the same to mark the last item with `target`:

```refactor no-revert
select target 'marked(tmp_all_items); last;'
```

Finally, we clean up, removing the `tmp_all_items` marks using the
`delete_marks` command:

```refactor
delete_marks tmp_all_items
```

Now the only marks remaining are the `target` marks on the first and last items
of the module, as we originally intended.

### `reset`

`reset` clears the set of marked nodes.  This is only useful in combination
with `mark` and `unmark`, as otherwise the operations before a `reset` have no
effect.

### `mark` and `unmark`

These operations allow `select` scripts to manipulate marks directly, rather
than relying solely on the automatic marking of selected nodes at the end of
the script.  `mark(l)` marks all nodes in the current selection with label `l`
(immediately, rather than waiting until the `select` command is finished), and
`unmark(l)` removes label `l` from all selected nodes.

`mark`, `unmark`, and `reset` can be used to effectively combine multiple
`select` commands in a single script.  Here's the "first and last" example from
the `marked` section, using only a single `select` command:

```refactor
select _dummy '
    item(m); child(kind(item)); mark(tmp_all_items); reset;
    marked(tmp_all_items); first; mark(target); reset;
    marked(tmp_all_items); last; mark(target); reset;
    marked(tmp_all_items); unmark(tmp_all_items); reset;
'
```

Note that we pass `_dummy` as the `LABEL` argument of `select`, since the
desired `target` marks are applied using the `mark` operation, rather than
relying on the implicit marking done by `select`.

`unmark` is also useful in combination with `marked` to interface with
non-`select` mark manipulation commands.  For example, suppose we want to mark
all occurrences of `2 + 2` that are passed as arguments to a function `f`.  One
option is to do this using the `mark_arg_uses` command, with additional
processing by `select` before and after.  Here we start by marking the function
`f`:

```rust refactor-target hidden
fn f(x: i32) {
    // ...
}

fn g(x: i32) {
    // ...
}

fn main() {
    f(1);
    f(2 + 2);
    g(2 + 2);
    let x = 2 + 2;
}
```

```refactor no-revert
select target 'item(f);'
```

Next, we run `mark_arg_uses` to replace the mark on `f` with a mark on each
argument expression passed to `f`:

```refactor no-revert
mark_arg_uses 0 target
```

And finally, we use `select` again to mark only those arguments that match `2 +
2`:

```refactor
select target 'marked(target); unmark(target); filter(match_expr(2 + 2));'
```

Beginning the script with `marked(target); unmark(target);` copies the set of
`target`-marked nodes into the current selection, then removes the existing
marks.  The remainder of the script can then operate as usual, manipulating
only the current selection with no need to worry about additional marks being
already present.


## Filters

### Boolean operators

Filter expressions can be combined using the boolean operators `&&`, `||`, and
`!`.  A node matches the filter `f1 && f2` only if it matches `f1` and also
matches `f2`, and so on.

### `kind`

`kind(k)` matches AST nodes whose node kind is `k`.  The supported node kinds
are:

 * `item` - a top-level item, as in `struct Foo { ... }` or `fn foo() { ... }`.
   Includes both items in modules and items defined inside functions or other
   blocks, but does not include "item-like" nodes inside traits, impls, or
   `extern` blocks.
 * `trait_item` - an item inside a trait definition, such as a method or
   associated type declaration
 * `impl_item` - an item inside an impl block, such as a method or associated
   type definition
 * `foreign_item` - an item inside an `extern block` ("foreign module"), such
   as a C function or static declaration
 * `stmt`
 * `expr`
 * `pat` - a pattern, including single-ident patterns like `foo` in `let foo =
   ...;`
 * `ty` - a type annotation, such as `Foo` in `let x: Foo = ...;`
 * `arg` - a function or method argument declaration
 * `field` - a struct, enum variant, or union field declaration
 * `itemlike` - matches nodes whose kind is any of `item`, `trait_item`,
   `impl_item`, or `foreign_item`
 * `any` - matches any node

The node kind `k` can be used alone as shorthand for `kind(k)`.  For example,
the operation `desc(item);` is the same as `desc(kind(item));`.

### `item_kind`

`item_kind(k)` matches itemlike AST nodes whose subkind is `k`.  The itemlike
subkinds are:

 * `extern_crate`
 * `use`
 * `static`
 * `const`
 * `fn`
 * `mod`
 * `foreign_mod`
 * `global_asm`
 * `ty` - type alias definition, as in `type Foo = Bar;`
 * `existential` - existential type definition, as in `existential type Foo:
   Bar;`.  Note that existential types are currently an unstable language
   feature.
 * `enum`
 * `struct`
 * `union`
 * `trait` - ordinary `trait Foo { ... }` definition, including `unsafe trait`
 * `trait_alias` - trait alias definition, as in `trait Foo = Bar;`
   Note that trait aliases are currently an unstable language feature.
 * `impl` - including both trait and inherent impls
 * `mac` - macro invocation.  Note that `select` works on the macro-expanded
   AST, so macro invocations are never present under normal circumstances.
 * `macro_def` - 2.0/`decl_macro`-style macro definition, as in `macro foo(...)
   { ... }`.  Note that 2.0-style macro definitions are currently an unstable
   language feature.

Note that a single `item_kind` filter can match multiple distinct node kinds,
as long as the subkind is correct.  for example, `item_kind(fn)` will match
`fn` `item`s, method `trait_item`s and `impl_item`s, and `fn` declarations
inside `extern` blocks (`foreign_item`s).  similarly, `item_kind(ty)` matches
ordinary `type` alias definitions, associated type declarations (in traits) and
definitions (in impls), and foreign type declarations inside `extern` blocks.

`item_kind` filters match only those nodes that also match `kind(itemlike)`, as
other node kinds have no itemlike subkind.

The itemlike subkind `k` can be used alone as shorthand for `item_kind(k)`.
For example, the operation `desc(fn);` is the same as `desc(item_kind(fn));`.

### `pub` and `mut`

`pub` matches any item, impl item, or foreign item whose visibility is `pub`.
It currently does not support struct fields, even though they can also be
declared `pub`.

`mut` matches `static mut` items, `static mut` foreign item declarations, and
mutable binding patterns such as the `mut foo` in `let mut foo = ...;`.

### `name`

`name(re)` matches itemlikes, arguments, and fields whose name matches the
regular expression `re`.  For example, `name("[fF].*")` matches `fn f() { ... }`
and `struct Foo { ... }`, but not `trait Bar { ... }`.  It currently does not
support general binding patterns, aside from those in function arguments.

### `path` and `path_prefix`

`path(p)` matches itemlikes and enum variants whose absolute path is `p`.

`path_prefix(n, p)` is similar to `path(p)`, but drops the last `n` segments
of the node's path before comparing to `p`.

### `has_attr`

`has_attr(a)` matches itemlikes, exprs, and field declarations that have an
attribute named `a`.

### `match_*`

`match_expr(e)` uses [`rewrite_expr`-style AST matching](rewrite.md) to compare
exprs to `e`, and matches any node where AST matching succeeds.  For example,
`match_expr(__e + 1)` matches the expressions `1 + 1`, `x + 1`, and `f() + 1`,
but not `2 + 2`.

`match_pat`, `match_ty`, and `match_stmt` are similar, but operate on pat, ty,
and stmt nodes respectively.

### `marked`

`marked(l)` matches nodes that are marked with the label `l`.

### `any_child`, `all_child`, `any_desc`, and `all_desc`

`any_child(f)` matches nodes that have a child that matches `f`.
`all_child(f)` matches nodes where all children of the node match `f`.

`any_desc` and `all_desc` are similar, but consider all descendants instead of
only direct children.


## Other commands

In addition to `select`, `c2rust-refactor` contains a number of other
mark-manipulation commands.  A few of these can be replicated with appropriate
`select` scripts (though using the command is typically easier), but some are
more complex.

### `copy_marks`

`copy_marks OLD NEW` adds a mark with label `NEW` to every node currently
marked with `OLD`.

### `delete_marks`

`delete_marks OLD` removes the label `OLD` from every node that is currently
marked with it.

### `rename_marks`

`rename_marks OLD NEW` behaves like `copy_marks OLD NEW` followed by
`delete_marks OLD`: it adds a mark with label `NEW` to every node marked with
`OLD`, then removes `OLD` from each such node.

### `mark_uses`

`mark_uses LABEL` transfers `LABEL` marks from definitions to uses.  That is,
it finds each definition marked with `LABEL`, marks each use of such a
definition with `LABEL`, then removes `LABEL` from the definitions.  For
example, if a `static FOO: ... = ...` is marked with `target`, then `mark_uses
target` will add a `target` mark to every expression `FOO` that references the
marked definition and then remove `target` from `FOO` itself.

For the purposes of this command, a "use" of a definition is a path or
identifier that resolves to that definition.  This includes expressions
(both paths and struct literals), patterns (paths to constants, structs, and
enum variants), and type annotations.  When a function definition is marked,
only the function path itself (the `foo::bar` in `foo::bar(x)`) is considered a
use, not the entire call expression.  Method calls (whether using dotted or
UFCS syntax) normally can't be handled at all, as their resolution is
"type-dependent" (however, the `mark_callers` command can sometimes work when
`mark_uses` does not).

### `mark_callers`

`mark_callers LABEL` transfers `LABEL` marks from function or method
definitions to uses.  That is, it works like `mark_uses`, but is specialized to
functions and methods.  `mark_callers` uses more a more sophisticated means of
name resolution that allows it to detect uses via type-dependent method paths,
which `mark_uses` cannot handle.

For purposes of `mark_callers`, a "use" is a function call (`foo::bar()`) or
method call (`x.foo()`) expression where the function or method being called is
one of the marked definitons.

### `mark_arg_uses`

`mark_arg_uses INDEX LABEL` transfers `LABEL` marks from function or method
definitions to the argument in position `INDEX` at each use.  That is, it works
like `mark_callers`, but marks the expression passed as argument `INDEX`
instead of the entire call site.

`INDEX` is zero-based.  However, the `self`/receiver argument of a method call
counts as the first argument (index 0), with the first argument in parentheses
having index 1 (`arg0.f(arg1, arg2)`).  For ordinary function calls (including
UFCS method calls), the first argument has index 0 (`f(arg0, arg1, arg2)`)
