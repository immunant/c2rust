# Known Limitations of Translation
This document tracks things that we know the translator can't handle, as well as things it probably won't ever handle.


## Partially implemented, experimental
  * variadic function definitions and macros that operate on `va_list`s (`va_copy` support blocked on https://github.com/rust-lang/rust/pull/59625)
  * preserving comments
  * GNU inline assembly
  * `long double` type (Linux only)

## Unimplemented

  * `_Complex` type (partially blocked by Rust language)
  * Non x86/64 SIMD function/types and x86/64 SIMD function/types which have no Rust equivalent
  
## Unimplemented, _might_ be implementable

  * GNU packed structs (Rust has `#[repr(packed)]` compatible with `#[repr(C)]`)
  * `restrict` pointers (Rust has references)
  * inline assembly
  * macros

## Likely won't ever support

  * __`longjmp`/`setjmp`__ Although there are LLVM intrinsics for these, it is unclear how these interact with Rust (esp. idiomatic Rust).
  * __jumps into and out of statement expressions__ We support GNU C statement expressions, but we can not handle jumping into or out of these. Both entry and exit into the expression have to be through the usual fall-through evaluation of the expression.
