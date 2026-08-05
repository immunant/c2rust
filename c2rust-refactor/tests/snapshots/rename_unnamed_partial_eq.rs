// Regression test for #1818: the `PartialEq` derive expansion stamps the span
// of the `PartialEq` token on every node it generates (e.g. all three types in
// `fn eq(&self, other: &C2Rust_Unnamed) -> bool`), which used to make the
// span-based NodeId -> HirId lookup pair the `&C2Rust_Unnamed` parameter type
// with the HIR node for `bool` and crash in `alter_ty_path`.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct C2Rust_Unnamed(pub u32);
pub const Foo0: C2Rust_Unnamed = C2Rust_Unnamed(0);

pub fn use_it(x: C2Rust_Unnamed) -> bool {
    x == Foo0
}

fn main() {
    let _ = use_it(Foo0);
}
