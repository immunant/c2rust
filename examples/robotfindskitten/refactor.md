# Collapsing `ncurses` macros

We begin with a little cleanup.  Some `ncurses` functions used in
`robotfindskitten` are actually implemented as macros, which are expanded by
the C preprocessor before `c2rust-transpile` performs its translation.  This
macro-generated code is hard to read and relies on internal implementation
details of the `ncurses` library.  We'd like to clean up this code as soon as
possible, in case later refactoring passes apply changes that would make the
code harder to detect.

Fortunately, the `ncurses` library also provides these operations as ordinary
functions, which we can call from Rust.  So we begin by providing Rust
declarations for these foreign functions:

```refactor
select target 'crate; child(foreign_mod);' ;
create_item
    '
        extern "C" {
            fn wattr_get(win: *mut WINDOW, attrs: *mut attr_t,
                pair: *mut libc::c_short, opts: *mut libc::c_void) -> libc::c_int;
            fn wattrset(win: *mut WINDOW, attrs: libc::c_int) -> libc::c_int;
        }
    '
    after ;
```

This refactoring script runs two commands.  `select` marks the `crate`'s lone
child of type `foreign_mod` (that is, the crate's one existing `extern "C" {
... }` block).  Then `create_item` inserts a new item, the `extern "C"` block
provided as its first argument, `after` the marked item.

Now we can find Rust code that comes from the expansions of the `wattrset`
macro and replace it with calls to the `wattrset` function.  For that, we use
the `rewrite_expr` command, which performs pattern-matching and replacement on
expressions:

```refactor
rewrite_expr
    '
        if !(__win as *const libc::c_void).is_null() {
            (*__win)._attrs = __attrs
        } else {
        }
    '
    'wattrset(__win, __attrs as libc::c_int)' ;
```

The first argument here is a pattern, which can be any Rust expression.
Anywhere the pattern expression appears in the crate, it will be replaced with
the replacement expression, which is the second argument.  However, the pattern
does not need to match exactly: variables in the pattern starting with `__`
(double underscore) are *bindings*, which will match any expression (or other
kinds of AST nodes, depending on context) and remember the captured expression
so it can be used in the replacement.  With this feature, we can clean up all
expansions of the `wattrset` macro, regardless of what arguments were passed to
the macro invocation.

Next, we do the same thing for the more complicated `wattr_get` macro:

```refactor
rewrite_expr
    '
        if !(__win as *const libc::c_void).is_null() {
            if !(&mut __attrs as *mut attr_t as *const libc::c_void).is_null() {
                __attrs = (*__win)._attrs
            } else {
            };
            if !(&mut __pair as *mut libc::c_short as *const libc::c_void).is_null() {
                __pair = (((*__win)._attrs as libc::c_ulong
                    & ((1u32 << 8i32).wrapping_sub(1u32) << 0i32 + 8i32) as libc::c_ulong)
                    >> 8i32) as libc::c_int as libc::c_short
            } else {
            };
        } else {
        }
    '
    'wattr_get(__win, &mut __attrs, &mut __pair, ::std::ptr::null_mut())' ;
```

Finally, we are done with this bit of cleanup, so we write the changes to disk
before continuing on:

```refactor
commit ;
```

Any pending changes are automatically written to disk at the end of the
refactoring script, but committing changes early is useful for two reasons.
First, it limits the number of changes the refactoring tool needs to apply in a
single rewriting step, which helps the rewriter produce better results.  And
second, during development of the refactoring script, each `commit` provides a
checkpoint where results can be cached, so that later refactoring commands can
be tweaked without rerunning all the previous steps.


# String formatting

Our next step is to replace calls to C string-formatting functions (such as
`printf`) with wrappers using Rust's `format_args!` macro.  Since
`format_args!` arguments are typechecked (while `printf` arguments are not),
doing this replacement early lets later passes avoid accidentally changing
program behavior by changing the types of format arguments.

The replacement itself happens in two steps.  First, we convert `printf` calls
from `printf(<C format args...>)` to `printf(format_args!(<Rust format
args...>))`.  This step adds the type safety we want for the format arguments,
but the resulting code does not itself typecheck: C's `printf` function cannot
accept the `std::fmt::Arguments` produced by the `format_args!` macro.  The
second step then replaces the `printf` call with a call to a wrapper that does
accept `std::fmt::Arguments`.

## `printf` format argument conversion

We run a few commands to mark the nodes involved in string formatting, before
finally running the `convert_format_args` command to perform the actual
transformation.

First, we use `select` and `mark_arg_uses` to mark the first argument of every
`printf` call as `target`s:

```refactor
select target 'item(printf);' ;
mark_arg_uses 0 target ;
```

`convert_format_args` will treat the `target` argument at each call site as a
`printf`-style format string, and will treat all later arguments as format
args.

Next, we mark the format string literal with `fmt_str`, which tells
`convert_format_args` the exact string literal it should use as the format
string.  This usually is not the same as the `target` argument, since
`c2rust-transpile` inserts several casts to turn a Rust string literal into a
`*const libc::c_char`.

```refactor
select fmt_str 'marked(target); desc(expr && !match_expr(__e as __t));' ;
```

With both `target` and `fmt_str` marks in place, we can apply the actual
transformation:

```refactor
convert_format_args ;
```

Finally, we clean up from this step by clearing all the marks.

```refactor 
clear_marks ;
```

`commit` would also
clear the marks, but we don't want to `commit` these changes until we've fixed
the type errors introduced in this step.

## Creating a `printf` wrapper

As a reminder, we currently have code that looks like this:

```Rust
printf(format_args!("Hello, {}!\n", "world"))
```

`printf` itself can't accept the `std::fmt::Arguments` returned by
`format_args!`, so we will define a wrapper that does accept
`std::fmt::Arguments` and then rewrite these `printf` calls to call the wrapper
instead.

First, the wrapper.  We insert it after the last `foreign_mod` (`extern "C" {
... }`):

```refactor
select target 'crate; child(foreign_mod); last;' ;
create_item
    '
        fn fmt_printf(args: ::std::fmt::Arguments) -> libc::c_int {
            print!("{}", args);
            0
        }
    '
    after ;
```

Since Rust provides a `print!` macro with similar functionality to `printf`,
our "wrapper" actually just calls `print!` directly, avoiding the string
conversions necessary to call the actual C `printf`.  (See the next subsection
for an example of a "real" wrapper function.)

With the wrapper in place, we can now update the call sites:

```refactor
rewrite_expr 'printf' 'fmt_printf' ;
```

Now that we've finished this step and the crate typechecks again, we can safely
commit the changes:

```refactor
commit ;
```

## Other string formatting functions

Aside from `printf`, `robotfindskitten` also uses the `ncurses` `printw` and
`mvprintw` string-formatting functions.  The refactoring script for `printw` is
similar to the previous two steps combined:

```refactor
select target 'item(printw);' ;
mark_arg_uses 0 target ;
select fmt_str 'marked(target); desc(expr && !match_expr(__e as __t));' ;

convert_format_args ;

clear_marks ;

select target 'crate; child(foreign_mod); last;' ;
create_item
    '
        fn fmt_printw(args: ::std::fmt::Arguments) -> libc::c_int {
            unsafe {
                ::printw(b"%s\0" as *const u8 as *const libc::c_char,
                         ::std::ffi::CString::new(format!("{}", args))
                             .unwrap().as_ptr())
            }
        }
    '
    after ;
rewrite_expr 'printw' 'fmt_printw' ;
commit ;
```

Aside from replacing the name `printf` with `printw`, the other notable
difference from the `printf` script is the body of `fmt_printw`.  There is no
convenient Rust replacement for `printw`, so instead we call the original
`printw` function, passing in the result of Rust string formatting (converted
to a C string) as an argument.

The `mvprintw` replacement is also similar, just with a few extra arguments:

```refactor
select target 'item(mvprintw);' ;
mark_arg_uses 2 target ;
select fmt_str 'marked(target); desc(expr && !match_expr(__e as __t));' ;

convert_format_args ;

clear_marks ;

select target 'crate; child(foreign_mod); last;' ;
create_item
    '
        fn fmt_mvprintw(y: libc::c_int, x: libc::c_int,
                        args: ::std::fmt::Arguments) -> libc::c_int {
            unsafe {
                ::mvprintw(y, x, b"%s\0" as *const u8 as *const libc::c_char,
                         ::std::ffi::CString::new(format!("{}", args))
                             .unwrap().as_ptr())
            }
        }
    '
    after ;
rewrite_expr 'mvprintw' 'fmt_mvprintw' ;
commit ;
```


# Static string constant - `ver`

`robotfindskitten` defines a static string constant, `ver`, to store the game's
version.  Using `ver` is currently unsafe, first because its Rust type is a raw
pointer (`*mut c_char`), and second because it's mutable.  To make `ver` usage
safe, we first change its type to `&'static str` (and fix up the resulting type
errors), and then we change it from a `static mut` to an ordinary immutable
`static`.

We change the type using `rewrite_ty`, which does pattern/replacement rewriting
similar to `rewrite_expr`, but on types.

```refactor
select target 'item(ver); child(ty);' ;
rewrite_ty 'marked!(*mut libc::c_char)' "&'static str" ;
delete_marks target ;
```

Since `rewrite_ty` rewrites all type annotations in the crate by default, we
need to restrict it to ensure it only operates on the `ver` type annotation.
To do this, we mark `ver`'s type annotation with `select`, and provide a
pattern to `rewrite_ty` that selects only `marked!` nodes.  We delete the mark
afterward, since it's no longer needed.

Simply replacing `*mut c_char` with `&str` introduces type errors throughout
the crate.  The initializer for `ver` still has type `*mut c_char`, and all
uses of `ver` are still expecting a `*mut c_char`.

## Fixing `ver`'s initializer

Fixing the `ver` initializer is straightforward: we simply remove all the
casts, then convert the binary string (`&[u8]`) literal to an ordinary string
literal.  For the casts, we mark all cast expressions in `ver`'s definition,
then replace each one with its subexpression:

```refactor
select target 'item(ver); desc(match_expr(__e as __t));' ;
rewrite_expr 'marked!(__e as __t)' '__e' ;
delete_marks target ;
```

Only the binary string literal remains, so we mark it and change it to an
ordinary `str`:

```refactor
select target 'item(ver); child(expr);' ;
bytestr_to_str ;
delete_marks target ;
```

## Fixing `ver`'s uses

`ver`'s initializer is now well-typed, but its uses are still expecting a `*mut
c_char` instead of a `&str`.  To fix these up, we use the `type_fix_rules`
command, which rewrites expressions anywhere a type error occurs:

```refactor
type_fix_rules '*, &str, *const __t => __old.as_ptr()' ;
```

Here we run `type_fix_rules` with only one rule: in any position (`*`), if an
expression has type `&str` but is expected to have a raw pointer type (`*const
__t`), then wrap the original expression in a call to `.as_ptr()`.  This turns
out to be enough to fix all the errors at uses of `ver`.


## Making `ver` immutable

Now that all type errors have been corrected, we can finish our refactoring of
`ver`.  We make it immutable, then commit all the changes to disk.

```refactor
select target 'item(ver);' ;
set_mutability imm ;

commit ;
```


# Static string array - `messages`

Aside from `ver`, `robotfindskitten` contains a static array of strings, called
`messages`.  Like `ver`, accessing `messages` is unsafe because each element is
a raw `*mut c_char` pointer and because `messages` itself is a `static mut`.

We rewrite the type and initializer of `messages` using the same strategy as
for `ver`:

```refactor
select target 'item(messages); child(ty); desc(ty);' ;
rewrite_ty 'marked!(*mut libc::c_char)' "&'static str" ;
delete_marks target ;
select target 'item(messages); child(expr); desc(expr);' ;
rewrite_expr 'marked!(__e as __t)' '__e' ;
bytestr_to_str ;
delete_marks target ;
```

We use `type_fix_rules` to fix up the uses of `messages`, as we did for `ver`:

```refactor
type_fix_rules
    '*, &str, *const __t => __old.as_ptr()'
    '*, &str, *mut __t => __old.as_ptr() as *mut __t' ;
```

Here we needed a second rule for `*mut` pointers, similar to the one for
`*const`.

With all type errors fixed, we can make `messages` immutable and commit the
changes:

```refactor
select target 'item(messages);' ;
set_mutability imm ;

commit ;
```


# Heap-allocated array - `screen`

`robotfindskitten` uses a heap-allocated array to store information about each
object on the screen.  The initial translation gives this array the type `*mut
*mut c_int`, which is unsafe to access.  We replace it with a
`CArray<CArray<c_int>>`.  `CArray` is a memory-safe collection type provided by
the `c2rust_runtime` library, which is memory-safe and supports C-style access
pattern (including pointer arithmetic).  Of course, we also need to fix up all
uses of `screen` to avoid introducing type errors.

As a preliminary step, we need to add an import of the `c2rust_runtime`
library:

```refactor
select target 'crate;' ;
create_item 'extern crate c2rust_runtime;' inside ;
```

Now we can proceed with the actual refactoring.

## Converting to `CBlockPtr`

We actually perform the conversion from `*mut` to `CArray` via an intermediate
step, using `CBlockPtr`.  This is another type provided by `c2rust_runtime`,
which provides some limited bounds checking, but otherwise is still unsafe (for
example, its access methods do not check for simultaneous mutable access to the
same array element).  `CBlockPtr` serves as a convenient intermediate point
for refactoring between the `*mut` and `CArray` types.

We further break down the transition from `*mut *mut c_int` to
`CBlockPtr<CBlockPtr<c_int>>` into two steps, first converting the inner
pointer (leaving the overall type as `*mut CBlockPtr<c_int>`) and then the
outer.  We change the type annotation first, as we did for `var` and
`messages`:

```refactor
select target 'item(screen); child(ty);' ;
rewrite_ty 'marked!(*mut *mut __t)'
    '*mut ::c2rust_runtime::CBlockPtr<__t>' ;
```

As before, this introduces type errors, which we fix with `type_fix_rules`:

```refactor
type_fix_rules
    'rval, *mut __t, ::c2rust_runtime::CBlockPtr<__u> =>
        unsafe { ::c2rust_runtime::CBlockPtr::from_ptr(__old) }'
    'rval, *mut __t, *mut __u => __old as *mut __u'
    ;
```

The first rule provided here handles the later part of `screen`'s
initialization, where the program allocates a `*mut c_int` array (now
`CBlockPtr<c_int>`) for each row of the screen.  The second rule handles the
earlier part, where it allocates the top-level `*mut *mut c_int` (now `*mut
CBlockPtr<c_int>`) - these allocations now need a cast, since the type of the
rows has changed.

One category of type errors remains: the initialization code tries to
dereference the result of `offset`ting the array pointer, which is not possible
directly with the `CBlockPtr` API.  We add the necessary method call using
`rewrite_expr`:

```refactor
rewrite_expr
    '*typed!(__e, ::c2rust_runtime::block_ptr::CBlockOffset<__t>)'
    '*__e.as_mut()' ;
```

Here, the pattern filters for dereferences of `CBlockOffset` expressions, which
are the result of calling `offset` on a `CBlockPtr`, and adds a call to
`as_mut()` before the dereference.

The conversion of `screen` to `*mut CBlockPtr<c_int>` is now complete.  The
conversion to `CBlockPtr<CBlockPtr<c_int>>` uses a similar refactoring script:

```refactor
select target 'crate; item(screen); child(ty);' ;
rewrite_ty 'marked!(*mut __t)'
    '::c2rust_runtime::CBlockPtr<__t>' ;
type_fix_rules
    'rval, *mut __t, ::c2rust_runtime::CBlockPtr<__u> =>
        unsafe { ::c2rust_runtime::CBlockPtr::from_ptr(__old) }'
    'rval, *mut __t, *mut __u => __old as *mut __u'
    ;
rewrite_expr
    '*typed!(__e, ::c2rust_runtime::block_ptr::CBlockOffset<__t>)'
    '*__e.as_mut()' ;
```

The only change is in the `rewrite_ty` step.

There's one last step: now that `screen` has the desired
`CBlockPtr<CBlockPtr<c_int>>` type, we can rewrite the allocations that
initialize it.  At this point the allocations use the unsafe `malloc` function
followed by the unsafe `CBlockPtr::from_ptr`, but we can change that to use the
safe `CBlockPtr::alloc` method instead:

```refactor
rewrite_expr 'malloc(__e) as *mut __t as *mut __u' 'malloc(__e) as *mut __u' ;
rewrite_expr
    '::c2rust_runtime::CBlockPtr::from_ptr(malloc(__e) as *mut __t)'
    '::c2rust_runtime::CBlockPtr::alloc(
        __e as usize / ::std::mem::size_of::<__t>())'
    ;
```

This doesn't remove the `unsafe` blocks wrapping each allocation - we leave
those until the end of our refactoring, when we perform the final cleanup of
unnecessary `unsafe` blocks throughout the entire crate.

At this point, the refactoring of `screen` to  is
done, and we can commit the changes:

```refactor
commit ;
```

## Converting to `CArray`

