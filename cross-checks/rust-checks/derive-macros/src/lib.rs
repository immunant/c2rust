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

    // Extend the structure definition's generics list with an extra
    // type parameter called __XCheckHasherType for the hasher, e.g.,
    // "struct<T> Foo" gets an "impl<T, __XCheckHasherType : ...> for Foo<T>"
    let mut h_all_generics = struct_def.generics.clone();
    let h_generic_str = "impl<__XCheckHasherType : ::std::hash::Hasher + Default> X {}";
    let h_generic_item = syn::parse_item(h_generic_str).unwrap();
    let h_generics = if let syn::ItemKind::Impl(_, _, item_generics, ..) = h_generic_item.node {
        item_generics
    } else {
        panic!("invalid parse result for fake impl item");
    };
    // FIXME: do we want to add __XCheckHasherType to the beginning or the end?
    h_all_generics.ty_params.extend(h_generics.ty_params.into_iter());

    let hash_fields = each_field(&struct_def, &BindStyle::Ref.into(), |f| quote! {
        extern crate cross_check_runtime;
        use cross_check_runtime::hash::XCheckHash;
        h.write_u64(XCheckHash::<__XCheckHasherType>::xcheck_hash_with_depth(#f, _depth - 1));
    });

    // TODO: we should mark the impl below with #[automatically_derived],
    // but that currently causes an "unused attribute" warning
    //
    // We use the type generics and the where clause from the original definition,
    // but we use our own modified version of the impl generics
    let (_, ty_generics, where_clause) = struct_def.generics.split_for_impl();
    let (h_impl_generics, _, _) = h_all_generics.split_for_impl();
    let ident = &struct_def.ident;
    let hash_impl = quote! {
        #[allow(unused_mut)]
        impl #h_impl_generics ::cross_check_runtime::hash::XCheckHash<__XCheckHasherType>
                for #ident #ty_generics #where_clause {
            fn xcheck_hash_with_depth(&self, _depth: usize) -> u64 {
                let mut h = __XCheckHasherType::default();
                match *self { #hash_fields }
                h.finish()
            }
        }
    };
    hash_impl.parse().unwrap()
}
