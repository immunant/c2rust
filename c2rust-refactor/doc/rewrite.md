```refactor-options hidden
revert
diff-style = full
no-filename
no-collapse-diff
```


`c2rust-refactor` provides a versatile general-purpose rewriting command for
transforming expressions.


In its most basic form, the `rewrite_expr` command replaces one expression
with another, everywhere in the crate:

```rust refactor-target hidden
fn main() {
    println!("{}", 1 + 1);
    println!("{}", 1 + /*comment*/ 1);
    println!("{}", 1 + 11);
}
```

```refactor
rewrite_expr '1+1' '2'
```

Here, all instances of the expression `1+1` (the "pattern") are replaced with
`2`.

Unlike textual search-and-replace, `rewrite_expr` looks at the syntactic
structure of both the crate and the pattern, ignoring whitespace, comments,
and even parentheses.  This means the pattern `1+1` matches not only the exact
source code `1+1`, but also the equivalent forms `1 + 1` and `1 + /* comment */
1`.


# Metavariables

In `rewrite_expr`'s pattern expression, any name beginning with double
underscores is a *metavariable*.  Just as a variable in an ordinary Rust
`match` expression will match any value (and bind it for later use), a
metavariable in a `replace_expr` pattern will match any Rust code.  For
example, a pattern of `__x + 1` will match any expression that adds 1 to
something:

```rust refactor-target hidden
fn f() -> i32 { 123 }

fn main() {
    println!("1 + 1 = {}", 1 + 1);
    println!("2 * 3 + 1 = {}", 2 * 3 + 1);
    println!("4 + 5 + 1 = {}", 4 + 5 + 1);
    println!("f() + 1 = {}", f() + 1);
}
```

```refactor
rewrite_expr '__x + 1' '100'
```

In these examples, the `__x` metavariable matches (and binds) the expressions
`1`, `f()`, and `2 * 3`.  On that last one, note that it binds `2 * 3` because
`2 * 3 + 1` parses as `(2 * 3) + 1`, not `2 * (3 + 1)`.

## Using bindings

As mentioned above, when a metavariable matches against some piece of code, the
code it matches is bound to the variable for later use.  Specifically,
`rewrite_expr`'s replacement argument can refer back to those metavariables to
substitute in the matched code:

```refactor
rewrite_expr '__x + 1' '2 * __x'
```

In each case, the expression bound by the `__x` metavariable is substituted
into the right-hand side of the multiplication.

## Multiple occurences

Finally, the same metavariable can appear multiple times in the pattern.  In
that case, the pattern matches only if each occurence of the metavariable
matches the same expression.  For example:

```rust refactor-target hidden
fn f() -> i32 { 123 }

fn main() {
    let a = 2;
    println!("{}", 1 + 1);
    println!("{}", a + a);
    println!("{}", f() + f());
    println!("{}", f() + 1);
}
```

```refactor
rewrite_expr '__x + __x' '2 * __x'
```

Here `a + a` and `f() + f()` are both replaced, but `f() + 1` is not because
`__x` cannot match both `f()` and `1` at the same time.


## Example: adding a function argument

Suppose we wish to add an argument to an existing function.  All current
callers of the function should pass a default value of `0` for this new
argument.  We can update the existing calls like this:

```rust refactor-target hidden
fn my_func(x: i32, y: i32) {
    /* ... */
}

fn main() {
    my_func(1, 2);
    let x = 123;
    my_func(x, x);
    my_func(0, { let y = x; y + y });
}
```

```refactor
rewrite_expr 'my_func(__x, __y)' 'my_func(__x, __y, 0)'
```

Every call to `my_func` now passes a third argument, and we can update the
definition of `my_func` to match.


# Special matching forms

`rewrite_expr` supports several *special matching forms* that can appear in
patterns to add extra restrictions to matching.


## `def!`

A pattern such as `def!(::foo::f)` matches any ident or path expression that
resolves to the function whose absolute path is `::foo::f`.  For example, to
replace all expressions referencing the function `foo::f` with ones referencing
`foo::g`:

```rust refactor-target hidden
mod foo {
    fn f() { /* ... */ }
    fn g() { /* ... */ }
}

fn main() {
    use self::foo::f; 
    // All these calls get rewritten
    f();
    foo::f();
    ::foo::f();
}

mod bar {
    fn f() {}

    fn f_caller() {
        // This call does not...
        f();
        // But this one still does
        super::foo::f();
    }
}
```

```refactor
rewrite_expr 'def!(::foo::f)' '::foo::g'
```

This works for all direct references to `f`, whether by relative path
(`foo::f`), absolute path (`::foo::f`), or imported identifier (just `f`, with
`use foo::f` in scope).  It can even handle imports under a different name
(`f2` with `use foo::f as f2` in scope), since it checks only the path of the
referenced definition, not the syntax used to reference it.

### Under the hood

When `rewrite_expr` attempts to match `def!(path)` against some expression `e`,
it actually completely ignores the content of `e` itself.  Instead, it performs
these steps:

 1. Check `rustc`'s name resolution results to find the definition `d` that `e`
    resolves to.  (If `e` doesn't resolve to a definition, then the matching
    fails.)
 2. Construct an absolute path `dpath` referring to `d`.  For definitions in
    the current crate, this path looks like `mod1::def1`.  For definitions in
    other crates, it looks like `crate1::mod1::def1`.
 3. Match `dpath` against the `path` pattern provided as the argument
    of `def!`.  Then `e` matches `def!(path)` if `dpath` matches `path`, and
    fails to match otherwise.

### Debugging match failures

Matching with `def!` can sometimes fail in surprising ways, since the
user-provided `path` is matched against a generated path that may not appear
explicitly anywhere in the source code.  For example, this attempt to match
`HashMap::new` does not succeed:

```rust refactor-target hidden
use std::collections::hash_map::HashMap;

fn main() {
    let m: HashMap<i32, i32> = HashMap::new();
}
```

```refactor
rewrite_expr
    'def!(::std::collections::hash_map::HashMap::new)()'
    '::std::collections::hash_map::HashMap::with_capacity(10)'
```

The `debug_match_expr` command exists to diagnose such problems.  It takes only
a pattern, and prints information about attempts to match it at various points
in the crate:

```refactor
debug_match_expr 'def!(::std::collections::hash_map::HashMap::new)()'
```

Here, its output includes this line:

```
def!(): trying to match pattern path(::std::collections::hash_map::HashMap::new) against AST path(::std::collections::HashMap::new)
```

Which reveals the problem: the absolute path `def!` generates for
`HashMap::new` uses the reexport at `std::collections::HashMap`, not the
canonical definition at `std::collections::hash_map::HashMap`.  Updating the
previous `rewrite_expr` command allows it to succeed:

```refactor
rewrite_expr
    'def!(::std::collections::HashMap::new)()'
    '::std::collections::HashMap::with_capacity(10)'
```


### Metavariables

Since the argument to `def!` is a path pattern, it can contain metavariables.
For instance, we can rewrite all calls to functions from the `foo` module:

```rust refactor-target hidden
mod foo {
    fn f() { /* ... */ }
    fn g() { /* ... */ }
}

mod bar {
    fn f() { /* ... */ }
    fn g() { /* ... */ }
}

fn main() {
    foo::f();
    foo::g();
}
```

```refactor
rewrite_expr 'def!(::foo::__name)()' '123'
```

Since every definition in the `foo` module has an absolute path of the form
`foo::(something)`, they all match the expression pattern
`def!(::foo::__name)`.

Like any other metavariable, the ones in a `def!` path pattern can be used in
the replacement expression to substitute in the captured name.  For example, we
can replace all references to items in the `foo` module with references to the
same-named items in the `bar` module:

```refactor
rewrite_expr 'def!(::foo::__name)' '::bar::__name'
```

Note, however, that each metavariable in a path pattern can match only a single
ident.  This means `foo::__name` will not match the path to an item in a
submodule, such as `foo::one::two`.  Handling these requires additional rewrite
steps, such as `rewrite_expr 'def!(::foo::__name1::__name2)'
'::bar::__name1::__name2'`.


## `typed!`

A pattern of the form `typed!(e, ty)` matches any expression that matches the
pattern `e`, but only if the type of that expression matches the pattern `ty`.
For example, we can perform a rewrite that only affects `i32`s:

```rust refactor-target hidden
fn main() {
    let x = 100_i32;
    let y: i32 = 100;
    let z = x + y;

    let a = "hello";
    let b = format!("{}, {}", a, "world");
}
```

```refactor
rewrite_expr 'typed!(__e, i32)' '0'
```

Every expression matches the subpattern `__e`, but only the `i32`s (whether
literals or variables of type `i32`) are affected by the rewrite.


### Under the hood

Internally, `typed!` works much like `def!`.  To match an expression `e`
against `typed!(e_pat, ty_pat)`, `rewrite_expr` follows these steps:

 1. Consult `rustc`'s typechecking results to get the type of `e`.  Call
    that type `tcx_ty`.
 2. `tcx_ty` is an internal, abstract representation of the type, which is not
    suitable for matching.  Construct a concrete representation of `tcx_ty`,
    and call it `ty`.
 3. Match `e` against `e_pat` and `ty` against `ty_pat`.  Then `e` matches
    `typed!(e_pat, ty_pat)` if both matches succeed, and fails to match
    otherwise.


### Debugging match failures

When matching fails unexpectedly, `debug_match_expr` is once again useful for
understanding the problem.  For example, this rewriting command has no effect:

```rust refactor-target hidden
fn main() {
    let a = "hello";
    let b = format!("{}, {}", a, "world");
}
```

```refactor
rewrite_expr "typed!(__e, &'static str)" '"hello"'
```

Passing the same pattern to `debug_match_expr` produces output that includes
the following:

```refactor hidden
debug_match_expr "typed!(__e, &'static str)"
```

```
typed!(): trying to match pattern type(&'static str) against AST type(&str)
```

Now the problem is clear: the concrete type representation constructed for
matching omits lifetimes.  Replacing `&'static str` with `&str` in the pattern
causes the rewrite to succeed:

```refactor
rewrite_expr 'typed!(__e, &str)' '"hello"'
```


### Metavariables

Both arguments of `typed!(e, ty)` can include metavariables, and metavariables
from both subpatterns are usable in the replacement expression.  For example:

```rust refactor-target hidden
fn main() {
    let v: Vec<&'static str> = Vec::with_capacity(20);

    let v: Vec<_> = Vec::with_capacity(10);
    // Allow `v`'s element type to be inferred
    let x: i32 = v[0];
}
```

```refactor
rewrite_expr
    'typed!(Vec::with_capacity(__n), ::std::vec::Vec<__ty>)'
    '::std::iter::repeat(<__ty>::default())
        .take(__n)
        .collect::<Vec<__ty>>()'
```

Notice that the rewritten code has the correct element type in the call to
`default`, even in cases where the type is not written explicitly in the
original expression!  The matching of `typed!` obtains the inferred type
information from `rustc`, and those inferred types can be captured using
metavariables in the type pattern.


## Example: `transmute` to `<*const T>::as_ref`

This example demonstrates usage of `def!` and `typed!`.

Suppose we have some unsafe code that uses `transmute` to convert a raw
pointer that may be null (`*const T`) into an optional reference
(`Option<&T>`).  This conversion is better expressed using the `as_ref` method
of `*const T`, and we'd like to apply this transformation automatically.

### Initial attempt

Here is a basic first attempt:

```rust refactor-target hidden
use std::mem;

unsafe fn foo(ptr: *const u32) {
    let r: &u32 = mem::transmute::<*const u32, Option<&u32>>(ptr).unwrap();

    let opt_r2: Option<&u32> = mem::transmute(ptr);
    let r2 = opt_r2.unwrap();
    let ptr2: *const u32 = mem::transmute(r2);

    {
        use std::mem::transmute;
        let opt_r3: Option<&u32> = transmute(ptr);
        let r3 = opt_r2.unwrap();
    }

    /* ... */
}
```

```refactor
rewrite_expr 'transmute(__e)' '__e.as_ref()'
```

This has two major shortcomings, which we will address in order:

 1. It works only on code that calls exactly `transmute(foo)`.  The instances that
    import `std::mem` and call `mem::transmute(foo)` do not get rewritten.
 2. It rewrites transmutes between any types, not just `*const T` to
    `Option<&T>`.  Only transmutes between those types should be replaced with
    `as_ref`.

### Identifying `transmute` calls with `def!`

We want to rewrite calls to `std::mem::transmute`, regardless of how those
calls are written.  This is a perfect use case for `def!`:

```refactor
rewrite_expr 'def!(::core::mem::transmute)(__e)' '__e.as_ref()'
```

Now our rewrite catches all uses of `transmute`, whether they're written as
`transmute(foo)`, `mem::transmute(foo)`, or even `::std::mem::transmute(foo)`.

Notice that we refer to `transmute` as `core::mem::transmute`: this is the
location of its original definition, which is re-exported in `std::mem`.  See
the "`def!`: debugging match failures" section for an explanation of how we can
discover this.

### Filtering `transmute` calls by type

We now have a command for rewriting all `transmute` calls, but we'd like it to
rewrite only transmutes from `*const T` to `Option<&T>`.  We can achieve this
by filtering the input and output types with `typed!`:

```refactor
rewrite_expr '
    typed!(
        def!(::core::mem::transmute)(
            typed!(__e, *const __ty)
        ),
        Option<&__ty>
    )
' '__e.as_ref()'
```

Now only those transmutes that turn `*const T` into `Option<&T>` are affected
by the rewrite.  And because `typed!` has access to the results of type
inference, this works even on `transmute` calls that are not fully annotated
(`transmute(foo)`, not just `transmute::<*const T, Option<&T>>(foo)`).


## `marked!`

The `marked!` form is simple: `marked!(e, label)` matches an expression only if
`e` matches the expression and the expression is marked with the given `label`.
See the [documentation on marks and `select`](select.md) for more information.



# Other commands

Various other refactoring commands use the same pattern-matching engine as
`rewrite_expr`:

 * `rewrite_ty PAT REPL` works just like `rewrite_expr`, except it matches and
   replaces type annotations instead of expressions.
 * `abstract SIG PAT` replaces expressions matching a pattern with calls to a
   newly-created function.
 * `type_fix_rules` uses type patterns to find the appropriate rule to fix each
   type error.
 * `select`'s `match_expr` and similar filters use syntax patterns to identify
   nodes to mark.


[[TODO:
(1) clarify Rust `match` patterns vs syntax patterns
(2) use the word "AST" more often
]]
