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
    let ident = &struct_def.ident;
    //let (impl_generics, ty_generics, where_clause) = struct_def.generics.split_for_impl();
    // TODO: we need to integrate the <H> generic parameter into
    // impl_generics&friends above, then use those in the impl below
    let hash_fields = each_field(&struct_def, &BindStyle::Ref.into(), |f| quote! {
        extern crate cross_check_runtime;
        use cross_check_runtime::hash::XCheckHash;
        h.write_u64(XCheckHash::<H>::xcheck_hash_with_depth(#f, _depth - 1));
    });
    // TODO: we should mark the impl below with #[automatically_derived],
    // but that currently causes an "unused attribute" warning
    let hash_impl = quote! {
        #[allow(unused_mut)]
        impl<H> ::cross_check_runtime::hash::XCheckHash<H> for #ident
                where H : ::std::hash::Hasher + Default {
            fn xcheck_hash_with_depth(&self, _depth: usize) -> u64 {
                let mut h = H::default();
                match *self { #hash_fields }
                h.finish()
            }
        }
    };
    hash_impl.parse().unwrap()
}
