extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

fn xcheck_hash_derive(s: synstructure::Structure) -> quote::Tokens {
    // Iterate through all fields, inserting the hash computation for each field
    let hash_fields = s.each(|f| {
        let xcheck_hash_attr = f.ast().attrs.iter().find(
            |f| f.name() == "cross_check_hash");
        if let Some(ref attr) = xcheck_hash_attr {
            if let syn::MetaItem::List(ref ident, ref items) = attr.value {
                assert!(ident == "cross_check_hash");
                for item in items {
                    if let &syn::NestedMetaItem::MetaItem(ref mi) = item {
                        match *mi {
                            syn::MetaItem::Word(ref kw)
                                if kw == "no" ||
                                   kw == "never" ||
                                   kw == "disable" => return quote! {},

                            syn::MetaItem::NameValue(ref kw, ref val)
                                if kw == "custom" => match *val {
                                    syn::Lit::Str(ref s, syn::StrStyle::Cooked) => {
                                        let id = syn::Ident::from(s.clone());
                                        return quote! { #id(&mut h, #f) };
                                    }
                                    _ => panic!("invalid identifier passed to #[cross_check_hash(custom = ...)]")
                            }

                            _ => panic!("unknown parameter for #[cross_check_hash]")
                        }
                    }
                }
            }
        }
        // Default implementation
        quote! {
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
