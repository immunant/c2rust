extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

extern crate c2rust_xcheck_config as xcfg;

fn get_attr_args<'a>(
    attrs: &'a [syn::Attribute],
    attr_name: &'static str,
) -> impl Iterator<Item = xcfg::attr::ArgList<'a>> {
    attrs
        .iter()
        .filter(move |f| f.name() == attr_name)
        .map(|attr| xcfg::attr::syn::get_item_args(&attr.value))
}

fn xcheck_hash_derive(s: synstructure::Structure) -> quote::Tokens {
    let top_args = get_attr_args(&s.ast().attrs[..], "cross_check_hash")
        .flat_map(IntoIterator::into_iter)
        .collect::<xcfg::attr::ArgList>();

    // Allow users to override __XCHA and __XCHS
    let ahasher = top_args.get_ident_arg("ahasher", "__XCHA");
    let shasher = top_args.get_ident_arg("shasher", "__XCHS");
    let hash_field = move |field, args: &xcfg::attr::ArgList| {
        // FIXME: figure out the argument priorities here
        if args.contains_key("none") || args.contains_key("disabled") {
            // Cross-checking is disabled
            quote::Tokens::new()
        } else if let Some(ref sub_arg) = args.get("fixed") {
            // FIXME: should try parsing this as an integer
            let id = sub_arg.get_str_ident();
            quote! {
                h.write_u64(#id)
            }
        } else if let Some(ref sub_arg) = args.get("custom") {
            let id = sub_arg.get_str_ident();
            quote! {
                #id::<#ahasher, #shasher, Self, _>(&mut h, self, #field, _depth - 1)
            }
        } else {
            // Default implementation
            quote! {
                h.write_u64(::c2rust_xcheck_runtime::hash::CrossCheckHash::cross_check_hash_depth::<#ahasher, #shasher>(&#field, _depth - 1));
            }
        }
    };

    // Iterate through all fields, inserting the hash computation for each field
    let hash_fields = s.each(|bi| {
        let args = get_attr_args(&bi.ast().attrs[..], "cross_check_hash")
            .flat_map(IntoIterator::into_iter)
            .collect::<xcfg::attr::ArgList>();

        let bitfields = get_attr_args(&bi.ast().attrs[..], "bitfield").collect::<Vec<_>>();
        if bitfields.iter().any(|al| al.contains_key("padding")) {
            // This is a padding field, ignore it
            return quote::Tokens::new();
        }

        if bitfields.is_empty() {
            let field = quote! { #bi };
            hash_field(field, &args)
        } else {
            let mut all_bf_hashes = quote::Tokens::new();
            for bf in bitfields {
                let bf_name = if let Some(bf_name) = bf.get("name") {
                    bf_name.get_str_ident()
                } else {
                    continue;
                };

                let bf_field = if let Some(bf_ty) = bf.get("ty") {
                    let bf_ty = bf_ty.get_str_ident();
                    quote! { (self.#bf_name() as #bf_ty) }
                } else {
                    quote! { self.#bf_name() }
                };

                let default_args = Default::default();
                let bf_args = if let Some(bf_args) = bf.get("cross_check") {
                    bf_args.get_list().unwrap_or(&default_args)
                } else if let Some(bf_args) = bf.get("cross_check_hash") {
                    bf_args.get_list().unwrap_or(&default_args)
                } else {
                    &default_args
                };
                let bf_hash = hash_field(bf_field, bf_args);
                all_bf_hashes.append(bf_hash);
            }
            all_bf_hashes
        }
    });

    let ahasher = top_args.get_ident_arg("ahasher", "__XCHA");
    let shasher = top_args.get_ident_arg("shasher", "__XCHS");
    let hash_code = top_args
        .get("custom_hash")
        .map(|sub_arg| {
            // Hash this value by calling the specified function
            let format = top_args
                .get("custom_hash_format")
                .and_then(xcfg::attr::ArgValue::get_str);
            match format {
                None | Some("function") => {
                    let id = sub_arg.get_str_ident();
                    quote! { #id::<#ahasher, #shasher>(&self, _depth) }
                }
                Some("expression") => {
                    let expr = sub_arg.get_str_tokens();
                    quote! { #expr }
                }
                Some("extern") => {
                    let id = sub_arg.get_str_ident();
                    let struct_id = &s.ast().ident;
                    quote! {
                        extern {
                            #[no_mangle]
                            fn #id(_: *const #struct_id, _: usize) -> u64;
                        }
                        unsafe { #id(self as *const #struct_id, _depth) }
                    }
                }
                Some(ref f) => panic!("unexpected custom_hash_format: {}", f),
            }
        })
        .unwrap_or_else(|| {
            // Hash this value using the default algorithm
            let hasher = top_args.get_ident_arg("field_hasher", ahasher);
            quote! {
                if _depth == 0 {
                    ::c2rust_xcheck_runtime::hash::LEAF_RECORD_HASH
                } else {
                    #[allow(unused_mut)]
                    let mut h = #hasher::default();
                    match *self { #hash_fields }
                    h.finish()
                }
            }
        });
    s.bound_impl(
        "::c2rust_xcheck_runtime::hash::CrossCheckHash",
        quote! {
            fn cross_check_hash_depth<__XCHA, __XCHS>(&self, _depth: usize) -> u64
                    where __XCHA: ::c2rust_xcheck_runtime::hash::CrossCheckHasher,
                          __XCHS: ::c2rust_xcheck_runtime::hash::CrossCheckHasher {
                #[allow(unused_imports)]
                use core::hash::Hasher;
                #hash_code
            }
        },
    )
}
decl_derive!([CrossCheckHash, attributes(cross_check_hash)] => xcheck_hash_derive);
