# Cross-checking hashing algorithm
For a given value `x` of a type `T`, our cross-checking implementation needs to hash `x` to a hash value `H(x)` of fixed size 
(64 bits in the current implementation), regardless of the size and layout of `T`.
This document describes the design and implementation of the type-aware hashing algorithms used by the cross-checker.

Using an established hash functions over the raw bytes of `x` has a few disadvantages:
 * C/Rust structures contain padding bytes between consecutive fields (due to alignment requirements), and we must not include this padding in the hash.
 * Pointer addresses are non-deterministic due to ASLR and other factors, so we must hash them by dereference instead of address.

For these reasons, we have chosen to design our own type-aware hashing algorithms.
The algorithms hash each value differently depending on its type, and are implemented by functions with the following signature:
```C
uint64_t __c2rust_hash_T(T x, size_t depth);
```
We use recursive hashing algorithms for complex types.
To prevent infinite recursion and long hashing times, we limit the recursion depth to a fixed value.
When recursion reaches this limit, the hash function returns a constant hash instead of going deeper.

We distinguish between the following kinds of types:
  * Simple types, e.g., integers, booleans, characters, floats, are trivial types which can be hashed directly by value.
  In the current implementation, we hash these values by XORing them with a constant that depends on the type
  (see the [C](../cross-checks/c-checks/clang-plugin/runtime/hash.c) and [Rust](../cross-checks/rust-checks/runtime/src/hash/simple.rs) implementations for details).
  Since simple types cannot recurse, we perform no depth checks for this case.
 
  * Aggregate (or non-trivial) types:
    * Structures. We hash the contents of each structure by recursively hashing each field (with depth increased by one), then aggregating all the hashes into one. We currently use the [JodyHash](https://libraries.io/github/jbruchon/jodyhash) function for the latter.

    * Fixed-size arrays are hashed in fundamentally the same way as structures, by recursively hashing each array element then aggregating the resulting hashes.

    * Pointers. We avoid hashing pointers by address for the reasons listed above.
    Instead, we hash each pointer by recursively hashing its dereferenced value (with depth increased by one).
    We have two special cases here that we need to handle:
      * Null pointers, which our hash functions check and return a special hard-coded hash value for.
      * Non-null invalid pointers. Our cross-checking implementation will crash when dereferencing these pointers.
      However, running the crashing program either using `pointer-tracer` tool or under the MVEE will fix the crashes and safely hash these pointers by returning another special hard-coded value.
    
Other data types, e.g., unions and structures containing bitfields, are difficult to hash programatically and require the user to specify a manual hash function.

The [cross-checking configuration settings](cross-check-config.md) can be used to specify different hashing algorithm separately for simple and aggregate types.
