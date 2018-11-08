//! Test program for use with `test_debug_callees`.  Contains examples of a variety of callee
//! cases, in order to see how each one looks in the tcx's tables.
use std::ops::Add;

// Some functions to call
fn f() {}

struct S;
impl S { fn g(&self) {} }

trait T { fn h(&self); }
impl T for S { fn h(&self) {} }

static FN_PTR: fn() = f;

// Force 'a to be an early-bound lifetime
impl S { fn g_early<'a, T: 'a>(&'a self, x: T) {} }


impl Add for S {
    type Output = S;
    fn add(self, rhs: S) -> S { S }
}

fn main() {
    let closure = || {};

    f();    // Plain function call
    S.g();  // Inherent ethod call
    S::g(&S);   // Inherent method call using type-dependent path
    S.h();      // Trait method call
    S::h(&S);   // Trait method call using type-depedent path (Self)
    T::h(&S);   // Trait method call using type-depedent path (trait)
    <S as T>::h(&S);    // Trait method call with explicit Self type
    FN_PTR();   // Call via (static) function pointer
    closure();  // Closure call / overloaded `Fn`

    S::g_early(&S, ());     // Call to function with early-bound lifetimes

    let x = S + S;  // Overloaded operator call
}


