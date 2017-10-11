extern crate proc_macro;
extern crate syn;
extern crate synstructure;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use synstructure::{each_field, BindStyle};

#[proc_macro_derive(XCheckHash)]
pub fn derive_xcheck_hash(input: TokenStream) -> TokenStream {
    let struct_def = syn::parse_derive_input(&input.to_string()).unwrap();
    let hash_fields = each_field(&struct_def, &BindStyle::Ref.into(), |f| quote! {
        extern crate cross_check_runtime;
        use cross_check_runtime::hash::XCheckHash;
        h.write_u64(XCheckHash::xcheck_hash_with_depth::<__XCH>(#f, _depth - 1));
    });

    // TODO: we should mark the impl below with #[automatically_derived],
    // but that currently causes an "unused attribute" warning
    //
    // We use the type generics and the where clause from the original definition,
    // but we use our own modified version of the impl generics
    let (impl_generics, ty_generics, where_clause) = struct_def.generics.split_for_impl();
    let ident = &struct_def.ident;
    let hash_impl = quote! {
        #[allow(unused_mut)]
        impl #impl_generics ::cross_check_runtime::hash::XCheckHash
                for #ident #ty_generics #where_clause {
            fn xcheck_hash_with_depth<__XCH>(&self, _depth: usize) -> u64
                    where __XCH: ::cross_check_runtime::hash::XCheckHasher {
                let mut h = __XCH::default();
                match *self { #hash_fields }
                h.finish()
            }
        }
    };
    hash_impl.parse().unwrap()
}
