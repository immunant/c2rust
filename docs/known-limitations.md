# Known Limitations of Translation
This document is to track things that we know the translator can't handle, as well as things it probably won't ever handle.

## Unimplemented

  * bitfields
  * variadic function definitions (blocking [Rust issue](https://github.com/rust-lang/rust/issues/44930))
  * some static initializers
  * preserving comments (work in progress)
  * `long double` and `_Complex` types (partially blocked by Rust language)

## Unimplemented, _might_ be implementable but very low priority

  * GNU packed structs (Rust has `#[repr(packed)]` compatible with `#[repr(C)]`)
  * `inline` functions (Rust has `#[inline]`)
  * `restrict` pointers (Rust has references)
  * inline assembly
  * macros
  * SIMD/vector types

## Likely won't ever support

  * __`longjmp`/`setjmp`__ Although there are LLVM intrinsics for these, it is unclear how these interact with Rust (esp. idiomatic Rust).
  * __jumps into and out of statement expressions__ We support GNU C statement expressions, but we can not handle jumping into or out of these. Both entry and exit into the expression have to be through the usual fall-through evaluation of the expression.
