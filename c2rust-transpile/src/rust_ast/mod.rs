pub mod comment_store;
pub mod item_store;
pub mod set_span;
pub mod traverse;

pub use c2rust_ast_printer::pprust::BytePos;
use proc_macro2::Span;

use std::sync::atomic::{AtomicU32, Ordering};

static SPAN_LIMIT: AtomicU32 = AtomicU32::new(0);

fn raise_span_limit(_new_limit: u32) {
    let limit = SPAN_LIMIT.load(Ordering::Relaxed);
    let new_limit = 0x2000000;
    if new_limit >= limit {
        let delta = new_limit - limit;
        let s = str::repeat("        ", (delta as usize + 7) / 8);
        use std::str::FromStr;
        /* used only for its side-effect of expanding the source map */
        let _ = proc_macro2::TokenStream::from_str(&s);
        SPAN_LIMIT.store(new_limit, Ordering::Relaxed);
    }
}

/// Make a new span at `pos`
pub fn pos_to_span(pos: BytePos) -> Span {
    SpanExt::new(pos.0, pos.0)
}

pub trait SpanExt: Sized {
    fn is_dummy(&self) -> bool;

    fn dummy() -> Self;

    fn eq(&self, other: &Self) -> bool;

    fn new(lo: u32, hi: u32) -> Self;

    fn inner(&self) -> (u32, u32);

    #[inline(always)]
    fn lo(&self) -> BytePos {
        BytePos(self.inner().0)
    }

    #[inline(always)]
    fn hi(&self) -> BytePos {
        BytePos(self.inner().1)
    }

    #[inline(always)]
    fn with_lo(&self, lo: BytePos) -> Self {
        SpanExt::new(lo.0, self.hi().0)
    }

    #[inline(always)]
    fn with_hi(&self, hi: BytePos) -> Self {
        SpanExt::new(self.lo().0, hi.0)
    }

    #[inline(always)]
    fn shrink_to_lo(&self) -> Self {
        SpanExt::new(self.lo().0, self.lo().0)
    }

    #[inline(always)]
    fn shrink_to_hi(&self) -> Self {
        SpanExt::new(self.hi().0, self.hi().0)
    }

    #[inline(always)]
    fn between(&self, end: Self) -> Self {
        SpanExt::new(self.hi().0, end.lo().0)
    }

    fn substitute_dummy(self, other: Self) -> Self {
        if self.is_dummy() {
            other
        } else {
            self
        }
    }
}

#[repr(C)]
struct SpanRepr {
    compiler_or_fallback: u32,
    lo: u32,
    hi: u32,
}

/// Safety: `proc_macro2::Span` is (unless compiled with `--cfg proc_macro2_semver_exempt`)
/// the below enum:
///
/// ```compile_fail
/// # // Fails b/c this stuff is internal to rustc.
/// enum Span {
///     Compiler(proc_macro::Span),
///     Fallback(fallback::Span),
/// }
/// ```
/// Unfortunately its ABI is not stable because rustc makes no such guarantees on `repr(Rust)` enums,
/// but we need to get at its guts anyway. So the best we can do is a runtime assertion that a value
/// looks like we expect. The only `Span` value we can synthesize is `Span::call_site()`, so we compare
/// that against what its in-memory representation looked like at development time.
///
/// gdb gives us this debug representation:
/// ```compile_fail
/// # // Fails b/c this stuff is private.
/// # let _ =
/// proc_macro2::Span {
///     inner: proc_macro2::imp::Span::Fallback(proc_macro2::fallback::Span {
///         lo: 0,
///         hi: 0
///     }),
///     _marker: core::marker::PhantomData<proc_macro2::marker::ProcMacroAutoTraits>
/// }
/// # ;
/// ```
/// ...which looks like `[1u32, 0u32, 0u32]` in memory.
///
/// We can't do anything useful or meaningful with the `Compiler` side, and if ABI changes enough that
/// `[1u32, 0u32, 0u32]` actually represents an instance of the Compiler variant (and `Span::call_site()`
/// chooses to return such an instance), we're in trouble.
///
/// But hopefully if such circumstances do befall us, we'll at least know what went wrong.
///
/// On the plus side, the `fallback::Span` payload is a POD pair of two u32s, so that case is trivial.
#[cfg_attr(test, test)]
fn validate_repr() {
    let repr: SpanRepr = unsafe { std::mem::transmute(Span::call_site()) };
    assert!(repr.compiler_or_fallback == 1);
    assert!(repr.lo == 0);
    assert!(repr.hi == 0);
}

/** return (s.lo, s.hi) */
fn get_inner(s: &Span) -> (u32, u32) {
    validate_repr();
    /* safety: safe if it is safe to transmute between `Span` and `SpanRepr`;
    we call `validate_repr` to verify this. see doc comment on `validare_repr` */
    let repr: &SpanRepr = unsafe { std::mem::transmute(s) };
    (repr.lo & 0xffffff, repr.hi & 0xffffff)
}

/** return a span with the given bounds */
fn synthesize(lo: u32, hi: u32) -> Span {
    /* we must raise the span limit by creating dummy sourcemap entries when
    synthesizing a span; otherwise we hit assertions on some span methods */
    raise_span_limit(hi);
    validate_repr();
    /* safety: safe if it is safe to transmute between `Span` and `SpanRepr`;
    we call `validate_repr` to verify this. see doc comment on `validare_repr` */
    let repr = SpanRepr {
        compiler_or_fallback: 1,
        lo: lo | 0x1000000,
        hi: hi | 0x1000000,
    };
    unsafe { std::mem::transmute(repr) }
}

impl SpanExt for Span {
    fn is_dummy(&self) -> bool {
        self.eq(&Self::dummy())
    }

    fn dummy() -> Self {
        Span::call_site()
    }

    fn eq(&self, other: &Self) -> bool {
        get_inner(self) == get_inner(other)
    }

    fn new(lo: u32, hi: u32) -> Self {
        synthesize(lo, hi)
    }

    fn inner(&self) -> (u32, u32) {
        get_inner(self)
    }
}
