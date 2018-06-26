# Known Limitations of Translation
This document is to track things that we know the translator can't handle, as well as things it probably won't ever handle.

## Unimplemented

  * bitfields (see [#10](https://github.com/GaloisInc/C2Rust/issues/10))
  * variadic function definitions (blocked by Rust language)
  * some static initializers (see [#124](https://github.com/GaloisInc/C2Rust/issues/124))
  * preserving comments
  * `long double` and `_Complex` types (partially blocked by Rust language, see [#17](https://github.com/GaloisInc/C2Rust/issues/17))

## Unimplemented, _might_ be implementable but very low priority

  * GNU packed structs (Rust has `#[repr(packed)]` compatible with `#[repr(C)]`)
  * `inline` functions (Rust has `#[inline]`)
  * `restrict` pointers (Rust has references)
  * inline assembly (see [#96](https://github.com/GaloisInc/C2Rust/issues/96))
  * macros (see [#9](https://github.com/GaloisInc/C2Rust/issues/9))
  * SIMD/vector types (see [#94](https://github.com/GaloisInc/C2Rust/issues/94))

## Likely won't ever support

  * __`longjmp`/`setjmp`__ Although there are LLVM intrinsics for these, it is unclear how these interact with Rust (esp. idiomatic Rust).
  * __jumps into and out of statement expressions__ We support GNU C statement expressions, but we can not handle jumping into or out of these. Both entry and exit into the expression have to be through the usual fall-through evaluation of the expression.
