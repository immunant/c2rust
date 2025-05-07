// Regression test: previously, type aliases for `Option<*const T>` caused a panic, as
// `deconstruct_hir_ty` would match the HIR alias with the MIR type `Option<T>` and then get
// confused due to the mismatched number of type arguments.
type AliasOption = Option<*const u8>;

// CHECK: struct UseAliasOption<'h0>
struct UseAliasOption {
    // FIXME: should be `Option<&'h0 u8>`
    // CHECK: x: std::option::Option<&u8>
    x: AliasOption,
}
