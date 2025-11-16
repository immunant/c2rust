# Known Limitations of Translation

This document tracks things that we know the translator can't handle,
as well as things it probably won't ever handle.

## Partially implemented, experimental

* variadic function definitions and macros that operate on `va_list`s
* preserving comments
* `long double` type (Linux only)

## Unimplemented

* `_Complex` type (partially blocked by Rust language)
* Using `long double` type in variadic functions (blocked on Rust language; see https://github.com/immunant/c2rust/issues/154)
* Non-x86/64 SIMD function/types and x86/64 SIMD function/types which have no Rust equivalent
* Certain compiler builtins (see e.g. https://github.com/immunant/c2rust/issues/88)
* Exposing functions with different names and linkage types (blocked on Rust language. Example:  https://github.com/ConradIrwin/libxml2/blob/master/elfgcchack.h)
  
## Unimplemented, _might_ be implementable

* `restrict` pointers (Rust has references)
* macros
* GNU labels-as-values (https://github.com/immunant/c2rust/issues/221)

## Likely won't ever support

* __`longjmp`/`setjmp`__: Although there are LLVM intrinsics for these, it is unclear how these interact with Rust (esp. idiomatic Rust).
* __jumps into and out of statement expressions__: We support GNU C statement expressions, but we can not handle jumping into or out of these. Both entry and exit into the expression have to be through the usual fall-through evaluation of the expression.
