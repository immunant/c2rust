extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

extern crate cross_check_config as xcfg;

fn get_cross_check_args(attrs: &[syn::Attribute]) -> Option<xcfg::attr::ArgList> {
    attrs.iter()
         .find(|f| f.name() == "cross_check_hash")
         .map(|attr| xcfg::attr::get_syn_item_args(&attr.value))
}

fn xcheck_hash_derive(s: synstructure::Structure) -> quote::Tokens {
    let top_args = get_cross_check_args(&s.ast().attrs[..]).unwrap_or_default();

    // Allow users to override __XCHA and __XCHS
    let ahasher = top_args.get_ident_arg("ahasher", "__XCHA");
    let shasher = top_args.get_ident_arg("shasher", "__XCHS");

    // Iterate through all fields, inserting the hash computation for each field
    let hash_fields = s.each(|f| {
        get_cross_check_args(&f.ast().attrs[..]).and_then(|args| {
            // FIXME: figure out the argument priorities here
            if args.contains_key("none") ||
               args.contains_key("disabled") {
                // Cross-checking is disabled
                Some(quote::Tokens::new())
            } else if let Some(ref sub_arg) = args.get("fixed_hash") {
                // FIXME: should try parsing this as an integer
                let id = sub_arg.get_str_ident();
                Some(quote! { h.write_u64(#id) })
            } else if let Some(ref sub_arg) = args.get("custom_hash") {
                let id = sub_arg.get_str_ident();
                Some(quote! { #id::<#ahasher, #shasher, Self, _>(&mut h, self, #f, _depth) })
            } else {
                None
            }
        }).unwrap_or_else(|| {
            // Default implementation
            quote! {
                use cross_check_runtime::hash::CrossCheckHash;
                h.write_u64(CrossCheckHash::cross_check_hash_depth::<#ahasher, #shasher>(#f, _depth - 1));
            }
        })
    });

    let hash_code = top_args.get("custom_hash").map(|sub_arg| {
        // Hash this value by calling the specified function
        let id = sub_arg.get_str_ident();
        quote! { #id::<#ahasher, #shasher>(&self, _depth) }
    }).unwrap_or_else(|| {
        // Hash this value using the default algorithm
        let hasher = top_args.get_ident_arg("field_hasher", ahasher);
        quote! {
            #[allow(unused_mut)]
            let mut h = #hasher::default();
            match *self { #hash_fields }
            h.finish()
        }
    });
    s.bound_impl("::cross_check_runtime::hash::CrossCheckHash", quote! {
        fn cross_check_hash_depth<__XCHA, __XCHS>(&self, _depth: usize) -> u64
                where __XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
                      __XCHS: ::cross_check_runtime::hash::CrossCheckHasher {
            #[allow(unused_imports)] use std::hash::Hasher;
            #hash_code
        }
    })
}
decl_derive!([CrossCheckHash, attributes(cross_check_hash)] => xcheck_hash_derive);
