# Cross-checking hashing algorithm
For a given value `x` of a type `T`, our cross-checking implementation needs to hash `x` to a hash value `H(x)` of fixed size 
(64 bits in the current implementation), regardless of the size and layout of `T`.
This document describes the design and implementation of the type-aware hashing algorithms used by the cross-checker.

Using an established hash functions over the raw bytes of `x` has a few disadvantages:
 * C/Rust structures contain padding bytes between consecutive fields (due to alignment requirements), and we must not include
   this padding in the hash.
 * Pointer addresses are non-deterministic due to ASLR and other factors, so we must hash them by dereference, instead of by address.

For these reasons, we have chosen to design our own type-aware hashing algorithms.
The algorithms described herein hash each value differently depending on its type:
  * Simple types, e.g., integers, booleans, characters, floats, are trivial types which can be hashed directly by value.
  In the current implementation, we hash these values by XORing them with a constant that depends on the type
  (see the [C](../cross-checks/c-checks/clang-plugin/runtime/hash.c) and [Rust](../cross-checks/rust-checks/runtime/src/hash/simple.rs) implementations for details).
 
  * Aggregate (or non-trivial) types:
    * Structures (TODO)
    * Pointers (TODO)
    * Arrays (TODO)
    * Unions (TODO)
