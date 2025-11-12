The `analysis::ownership` module implements a pointer analysis for inferring
ownership information in code using raw pointers.  The goal is to take code
that has been automatically translated from C, and thus uses only raw pointers,
and infer which of those raw pointers should be changed to safe `&`, `&mut`, or
`Box` pointers.  Pointers can appear in a number of places in the input
program, but this analysis focuses mainly on function signatures and `struct`
field types.


# Design

The goal of the analysis is to assign to each raw pointer type constructor a
permission value, one of READ, WRITE, and MOVE, corresponding to the Rust
pointer types `&`, `&mut`, and `Box`.  These permissions form a trivial
lattice, where READ < WRITE < MOVE.  The READ permission indicates that the
pointed-to data may be read, the WRITE permission indicates that the pointed-to
data may be modified, and the MOVE permission indicates that the pointed-to
data may be "moved", or consumed in a linear-typed fashion.  The MOVE
permission also includes the ability to free the pointed-to data, which amounts
to "moving to nowhere".

Here is a simple example to illustrate the major features of the analysis:

```rust
struct Array {
    data: *mut i32,
}

unsafe fn new_array(len: usize) -> *mut Array {
    let data = malloc(size_of::<i32>() * len);
    let arr = malloc(size_of::<Array>());
    (*arr).data = data;
    arr
}

unsafe fn delete_array(arr: *mut Array) {
    free((*arr).data);
    free(arr);
}

unsafe fn element_ptr(arr: *mut Array, idx: usize) -> *mut i32 {
    (*arr).data.offset(idx)
}

unsafe fn get(arr: *mut Array, idx: usize) -> i32 {
    let elt: *mut i32 = element_ptr(arr, idx);
    *elt
}

unsafe fn set(arr: *mut Array, idx: usize, val: i32) {
    let elt: *mut i32 = element_ptr(arr, idx);
    *elt = val;
}
```

The analysis infers pointer permissions by observing how pointers are used, and
applying the rules of the Rust reference model.  For instance, the `set`
function's `elt` pointer must have permission WRITE (or higher), because there
is a write to the pointed-to data.  Similarly, `delete_array`'s first call to
`free` requires that the pointer in the `Array::data` field must have
permission MOVE.  Furthermore, the first `free` also requires `arr` to have
permission MOVE, because consuming the pointer `(*arr).data` constitutes a move
out of `*arr`.  (In general, the pointer permission sets an upper bound on the
permissions of all pointers within the pointed-to data.  For example, if `arr`
has permission READ, then `*(*arr).data` can only be read, not written or
moved.)

The `element_ptr` function presents an interesting case for analysis, because
it is used polymorphically: in `get`, we would like `element_ptr` to take a
READ `*mut Array` and return a READ `*mut i32`, whereas in `set` we would like
the same function to take and return WRITE pointers.  In strictly const-correct
C code, `get` and `set` would respectively call separate const and non-const
variants of `element_ptr`, but a great deal of C code is not const-correct.

This analysis handles functions like `element_ptr` by allowing inferred
function signatures to be *permission polymorphic*.  Signatures may include
permission parameters, which can be instantiated separately at each call site,
subject to a set of constraints.  For example, here is the inferred polymorphic
signature of `element_ptr`, with permission annotations written in comments
(since there is no Rust syntax for them):

```rust
fn element_ptr /* <s0, s1> */ (arr: /* s0 */ *mut Array,
                               idx: usize)
                               -> /* s1 */ *mut i32
    /* where s1 <= s0 */;
```

The function has two permission parameters, `s0` and `s1`, which are the
permissions of the argument and return pointers respectively.  The signature
includes the constraint `s1 <= s0`, indicating that the output pointer's
permission is no higher than that of the input pointer.  The function is called
in `get` with permission arguments `s0 = s1 = READ` and in `set` with `s0 = s1
= WRITE`.

Rust does not support any analogue of the permission polymorphism used in this
analysis.  To make the results useful in actual Rust code, the analysis
includes a monomorphization step, which chooses a set of concrete
instantiations for each polymorphic function, and selects an instantiation to
use for each call site.  In the example above, `element_ptr` would have both
`READ, READ` and `WRITE, WRITE` instantiations, with the first being used for
the callsite in `get` and the second at the callsite in `set`.


# Implementation

The analysis first computes a polymorphic signature for each function, then
monomorphizes to produce functions that can be handled by Rust's type system.

Both parts of the analysis operate on *constraint sets*, which contain
constraints of the form `p1 <= p2`.  The permissions `p1`, `p2` can be concrete
permissions (READ, WRITE, MOVE), permission variables, or expressions of the
form `min(p1, p2)` denoting the less-permissive of two permission values.

Permission variables appear on pointer type constructors in the types of static
variables and struct fields ("static" variables), in the types within function
signatures ("sig"), in the types of temporaries and local variables ("local"),
and at callsites for instantiating a permission polymorphic function ("inst").
Variables are marked with their origin, as variable from different locations
are handled in different phases of the analysis.

The overall goal of the analysis is to produce assignments to static and sig
variables that satisfy all the relevant constraints (or multiple assignments,
when monomorphizing polymorphic functions).

## Polymorphic signatures

The permission variables of each function's polymorphic signature are easily
determined: for simplicity, the analysis introduces one variable for each
occurrence of a pointer type constructor in the function signature.  Cases that
might otherwise involve a single variable appearing at multiple locations in
the signature are instead handled by adding constraints between the variables.
The main task of the first part of the analysis is to compute the constraints
over the signature variables of each function.  This part of the analysis must
also build an assignment of permission values to all static vars, which are not
involved in any sort of polymorphism.

Constraints arise mainly at assignments and function call expressions.

At assignments, the main constraint is that, if the assigned value has a
pointer type, the permission on the LHS pointer type must be no greater than
the permission on the RHS pointer type (`lhs <= rhs`).  In other words, an
assignment of a pointer may downgrade the permission value of that pointer, but
may never upgrade it.  In non-pointer types, and in the pointed-to type of an
outermost pointer type, all permission values occurring in the two types must
be equal (`lhs <= rhs` and `rhs <= lhs`).

Assignments also introduce two additional constraints, both relating to *path
permissions*.  The path permission for an expression is the minimum of the
permission values on all pointers dereferenced in the expression.  For example,
in `*(*x).f`, the path permission is the minimum of the permission on the local
variable `x` and the permission on the struct field `f`.  The calculation of
path permissions reflects the transitive nature of access restrictions in Rust:
for example, if a struct field `x.f` has type `&mut T`, but `x` is an immutable
reference (`&S`), then only immutable access is allowed to `*x.f`.

The two additional constraints introduced by assignments are (1) the path
permission of the LHS must be no lower than WRITE, and (2) the path permission
of the RHS must be no lower than the permission of the LHS pointer type.
Constraint (1) prevents writing through a READ pointer, or through any path
containing a READ pointer.  Constraint (2) prevents assigning a WRITE pointer
accessed through a READ path (or a MOVE pointer accessed through a WRITE or
READ path) to a WRITE pointer variable, which would allow bypassing the READ
restriction.

Function calls require additional work.  At each call site, the analysis
copies in the callee's constraints, substituting a fresh "instantiation"
("inst") variable for each variable in the callee's signature.  It then links
the new inst variables to the surrounding locals by processing a
"pseudo-assignment" from each argument expression to the corresponding formal
parameter type in the substituted signature, and from the return type to the
lvalue expression where the result is to be stored.  The effect is to allow the
analysis to "reason through" the function call, relating the (local) return
value to the caller's argument expressions.  Copying the constraints instead of
relying on a concrete instantiation permits precise reasoning about polymorphic
functions that call other polymorphic functions.

The final step for each function is to simplify the constraint set by
eliminating "local", "inst", and "static" permission variables.  Local
variables have no connection to types outside the current function, and can be
simplified away without consequence.  Eliminating static and instantiation
variables requires fixed-point iteration, which is described below.  The result
of the simplification is a set of constraints over only the function's sig
variables, which is suitable for use as the constraint portion of the function
signature.

Since each function's signature depends on the signatures of its callees, and
functions may be recursive, a fixed-point iteration step is required to compute
the final constraint set for each function.  To simplify the implementation,
the polymorphic signature construction part of the analysis is split into two
phases.  The intraprocedural phase visits every function once and generates
constraints for that function, but doesn't copy in constraints from callees,
which may not have been processed yet.  This phase records details of each call
site for later use.  The intraprocedural phase eliminates local variables at
the end of each function, but it does not have enough information to safely
eliminate static and inst variables.  The interprocedural phase updates each
function in turn, substituting in callees' sig constraints and simplifying away
static and inst variables to produce a new, more accurate set of sig
constraints for the current function, and iterates until it reaches a fixed
point.  The interprocedural phase also computes an assignment of concrete
permission values to static variables, during the process of removing static
variables from functions' constraint sets.

## Monomorphization

The first part of the analysis infers a permission polymorphic signature for
each function, but Rust does not support this form of polymorphism.  To make
the analysis results applicable to actual Rust code, the analysis must provide
enough information to allow *monomorphizing* functions - that is, producing
multiple copies of each function with different concrete instantiations of the
permission variables.

Monomorphization begins by collecting all "useful" monomorphic signatures for
each function.  The analysis identifies all signature variables that appear in
output positions (in the return type, or behind a pointer whose permission
value is always at least `WRITE`), then enumerates all assignments to those
output variables that are allowed by the function's constraints.  For each
combination of outputs, it finds the least-restrictive valid assignment of
permissions to the remaining (input) variables.  For example, given this
function:

```rust
fn element_ptr /* <s0, s1> */ (arr: /* s0 */ *mut Array,
                               idx: usize)
                               -> /* s1 */ *mut i32
    /* where s1 <= s0 */;
```

The only output variable is `s1`, which appears in the return type.  The
monomorphization step will try each assignment to `s1` that is allowed by the
constraints.  Since the only constraint is `s1 <= s0`, `READ`, `WRITE`, and
`MOVE` are all valid.  For each of these, it finds the least restrictive
assignment to `s0` that is compatible with the assignment to `s0`.  For
example, when `s1 = MOVE`, only `s0 = MOVE` is valid, so the analysis records
`MOVE, MOVE` as a monomorphization for the `element_ptr` function.  When `s1 =
WRITE`, both `s0 = MOVE` and `s0 = WRITE` satisfy the constraints, but `s0 =
WRITE` is less restrictive - it allows calling the function with both `MOVE`
and `WRITE` pointers, while setting `s0 = MOVE` allows only `MOVE` pointers.
So the analysis records arguments `WRITE, WRITE` as another monomorphization,
and by similar logic records `READ, READ` as the final one.

The next step of monomorphization is to select a monomorphic variant to call at
each callsite of each monomorphized function.  Given a pair of functions:

```rust
fn f /* <s0, s1> */ (arr: /* s0 */ *mut Array) -> /* s1 */ *mut i32
      /* where s1 <= s0 */ {
  g(arr)
}

fn g /* <s0, s1> */ (arr: /* s0 */ *mut Array) -> /* s1 */ *mut i32
      /* where s1 <= s0 */ {
  ...
}
```

For pointer  permissions to line up properly, a monomorphic variant of `f`
specialized to `READ, READ` will need to call a variant of `g` also specialized
to `READ, READ`, and a variant of `f` specialized to `WRITE, WRITE` will need
to call a `WRITE, WRITE` variant of `g`.

To infer this information, the analysis separately considers each monomorphic
signature of each function.  It performs a backtracking search to select, for
each callsite in the function, a monomorphic signature of the callee, such that
all of the calling function's constraints are satisfied, including constraints
setting the caller's sig variables equal to the concrete permissions in the
monomorphic signature.  The table of callee monomorphization selections is
included in the analysis results so that callsites can be updated appropriately
when splitting functions for monomorphization.


# Annotations

The ownership analysis supports annotations to specify the permission types of
functions and struct fields.  These annotations serve two purposes.  First, the
user can annotate functions to provide custom signatures for functions on which
the analysis produces inaccurate results.  Signatures provided this way will be
propagated throughout the analysis, so manually correcting a single
wrongly-inferred function can fix the inference results for its callers as
well.  Second, the ownership system provides an `ownership_annotate` command
that adds annotations to functions reflecting their inferred signatures.  The
user can then read the generated annotations to check the analysis results,
and optionally edit them to improve precision, before proceeding with further
code transformations.

There are four annotation types currently supported by the ownership system.

* `#[ownership_static(<perms>)]` provides concrete permission values for all
  pointer types in a static declaration or struct field.  The `perms` argument
  is a comma-separated sequence of concrete permission tokens (`READ`, `WRITE`,
  `MOVE`).  The given permission values will be applied to the pointers in the
  static or field type, following a preorder traversal of the type.  For
  example:

    ```rust
    struct S {
        #[ownership_static(READ, WRITE, MOVE)]
        f: *mut (*mut u8, *mut u16)
    }
    ```

  Here the outermost pointer will be given permission `READ`, the pointer to
  `u8` will be given permission WRITE, and the pointer to `u16` will be given
  permission `MOVE`.

* `#[ownership_constraints(<constraints>)` provides the signature constraints
  for the annotated function, overriding polymorphic signature inference.  The
  argument `constraints` is a comma-separated sequence of constraints of the
  form `le(<perm1>, <perm2>)`, each representing a single constraint `perm1 <=
  perm2`.  The permissions used in each constraint may be any combination of
  concrete permissions (`READ`, `WRITE`, `MOVE`), permission variables (`_0`,
  `_1`, ...), or expressions of the form `min(p1, p2, ...)`.  (The permission
  syntax is limited by the requirement for compatibility with Rust's attribute
  syntax.)

  The permission variables used in constraints always refer to signature
  variables of the annotated function.  A signature variable is introduced for
  each pointer type constructor in the function's signature, and they are
  numbered according to a preorder traversal of each node in the argument and
  return types of the function.  This example shows location of each variable
  in a simple signature:

    ```rust
    fn get_err(arr: /* _0 */ *mut Array,
               element_out: /* _1 */ *mut /* _2 */ *mut i32)
               -> /* _3 */ *const c_char;
    ```

* `#[ownership_mono(<suffix>, <perms>)]` supplies a monomorphic signature to be
  used for the annotated function.  The `suffix` argument is a quoted string,
  which (if non-empty) will be used when splitting polymorphic functions into
  monomorphic variants to construct a name for the monomorphized copy of the
  function.  The `perms` argument is a comma-separated list of concrete
  permission tokens, giving the permissions to be used in the function
  signature in this monomorphization.

  The `ownership_mono` annotation can appear multiple times on a single
  function to provide multiple monomorphic signatures.  However, if it appears
  at all, monomorphization inference will be completely overridden for the
  annotated function, and only the provided signatures will be used in callee
  argument inference and later transformations.

  Example:

    ```rust
    #[ownership_mono("mut", WRITE, WRITE)]
    #[ownership_mono("", READ, READ)]
    fn first(arr: *mut Array) -> *mut i32;
    ```

  This function will have two monomorphic variants, one where both pointers'
  permission values are `WRITE` and one where both are `READ`.  When the
  `ownership_split_variants` command splits the function into its monomorphic
  variants, the `WRITE` variant will be named `first_mut` and the `READ`
  variant will keep the original name `first`.

* `#[ownership_variant_of(<name>)]` is used to combine source-level functions
  into variant groups.  See the section on variant groups for details.


# Variant Groups

The "variant group" mechanism allows combining several source-level functions
into a single logical function for purposes of the analysis.  This is useful
for combining a function that was previously split into monomorphic variants
back into a single logical function.  This allows for a sort of "modular
refactoring", in which the user focuses on one module at a time, analyzing,
annotating, and splitting variants in only that module before moving on to
another.

As a concrete example of the purpose of this feature, consider the following
code:
  
  ```rust
  fn f(arr: *mut Array) -> *mut i32 { ... g(arr) ... }

  fn g(arr: *mut Array) -> *mut i32 { ... }
  ```

The user works first on (the module containing) `g`, resulting in splitting `g`
into two variants:

  ```rust
  fn f(arr: *mut Array) -> *mut i32 { ... g_mut(arr) ... }

  fn g(arr: *mut Array) -> *mut i32 { ... }
  fn g_mut(arr: *mut Array) -> *mut i32 { ... }
  ``` 

Note that, because there is still only one variant of `f`, the transformation
must choose a single `g` variant for `f` to call.  In this case, it chose the
`g_mut` variant.

Later, the user works on `f`.  If `g` and `g_mut` are treated as separate
functions, then there are two possibilities.  First, if the constraints on
`g_mut` are set up (or inferred) to require `WRITE` permission for `arr`, then
only a `WRITE` variant of `f` will be generated.  Or second, if the constraints
are relaxed, then `f` may get both `READ` and `WRITE` variants, but both will
(wrongly) call `g_mut`.

Treating `g` and `g_mut` as two variants of a single function allows the
analysis to switch between `g` variants in the different variants of `f`,
resulting in correct code like the following:

  ```rust
  fn f(arr: *mut Array) -> *mut i32 { ... g(arr) ... }
  fn f_mut(arr: *mut Array) -> *mut i32 { ... g_mut(arr) ... }

  fn g(arr: *mut Array) -> *mut i32 { ... }
  fn g_mut(arr: *mut Array) -> *mut i32 { ... }
  ```

The `ownership_split_variants` automatically annotates the split functions so
they will be combined into a variant group during further analysis.  Variant
groups can also be constructed manually using the
`#[ownership_variant_of(<name>)]` annotation, where `name` is an arbitrary
quoted string.  All source-level functions bearing an `ownership_variant_of`
annotation with the same `name` will form a single variant group, which will be
treated as a single function throughout the analysis.  However, signature
inference for the variants themselves is not well supported.  Thus, each
variant must have an `ownership_mono` annotation, and exactly one function in
each variant group must also have an `ownership_constraints` annotation.
Together, these provide enough information that inference is not required.
Note that unlike non-variant functions, variants may not have multiple
`ownership_mono` annotations, as each variant is expected to correspond to a
single monomorphization of the original function.


# The "Collection Hack"

The analysis as described so far tries to mimic the Rust ownership model as
implemented in the Rust compiler.  However, collection data structures in Rust
often use unsafe code to bypass parts of the ownership model.  A particularly
common case is in removal methods, such as `Vec::pop`:
  
  ```rust
  impl<T> Vec<T> {
      fn pop(&mut self) -> Option<T> { ... }
  }
  ```

This method moves a `T` out of `self`'s internal storage, but only takes `self`
by mutable reference.  Under the "normal" rules, this is impossible, and the
analysis described above will infer a stricter signature for the raw pointer
equivalent:
  
  ```rust
  fn pop(this: /* MOVE */ *mut Vec) -> /* MOVE */ *mut c_void { ... }
  ```

The analysis as implemented includes a small adjustment (the "collection hack")
to let it infer the correct signature for such methods.

The collection hack is this: when handling a pointer assignment, instead of
constraining the path permission of the RHS to be at least the permission of
the LHS, we constraint it to be at least `min(lhs_perm, WRITE)`.  The result is
that it becomes possible to move a `MOVE` pointer out of a struct when only
`WRITE` permission is available for the pointer to that struct.  Then the
analysis will infer the correct type for `pop`:
  
  ```rust
  fn pop(this: /* WRITE */ *mut Vec) -> /* MOVE */ *mut c_void { ... }
  ```
