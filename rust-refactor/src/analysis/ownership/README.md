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
permission also includes the ability to free the pointed-to data, which amouns
to "moving to nowhere".

Here is a simple example to illustrate the major features of the analysis:

    struct Array {
        data: *mut i32,
    }

    unsafe fn new_array(len: usize) -> *mut Array {
        let data = malloc(size_of::<i32>() * len);
        let arr = malloc(size_of::<Array>());
        (*arr).data = data;
        array
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

    fn element_ptr /* <s0, s1> */ (arr: /* s0 */ *mut Array,
                                   idx: usize)
                                   -> /* s1 */ *mut i32
        /* where s1 <= s0 */;

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

The two additional constraints introduced by assigments are (1) the path
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


# Annotations


# Variant Groups


# The "collection hack"
