extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

fn xcheck_hash_derive(s: synstructure::Structure) -> quote::Tokens {
    // Iterate through all fields, inserting the hash computation for each field
    let hash_fields = s.each(|f| {
        quote! {
            extern crate cross_check_runtime;
            use cross_check_runtime::hash::CrossCheckHash;
            h.write_u64(CrossCheckHash::cross_check_hash_depth::<__XCHA, __XCHS>(#f, _depth - 1));
        }
    });
    s.bound_impl("::cross_check_runtime::hash::CrossCheckHash", quote! {
        fn cross_check_hash_depth<__XCHA, __XCHS>(&self, _depth: usize) -> u64
                where __XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
                      __XCHS: ::cross_check_runtime::hash::CrossCheckHasher {
            let mut h = __XCHA::default();
            match *self { #hash_fields }
            h.finish()
        }
    })
}
decl_derive!([CrossCheckHash] => xcheck_hash_derive);
