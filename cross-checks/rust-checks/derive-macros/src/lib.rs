extern crate proc_macro;
extern crate syn;
extern crate synstructure;
#[macro_use]
extern crate quote;

use std::collections::HashSet;

use proc_macro::TokenStream;
use synstructure::{each_field, BindStyle};

struct CrossCheckHashPredicateInserter<'a> {
    skip_visit: bool,
    xcheck_hash_bound: syn::TyParamBound,
    ty_param_names: &'a HashSet<&'a str>,
    seen_tys: HashSet<syn::PathSegment>,
    ty_where_preds: Vec<syn::WherePredicate>,
}

impl<'a> syn::visit::Visitor for CrossCheckHashPredicateInserter<'a> {
    fn visit_ty(&mut self, ty: &syn::Ty) {
        // If this type matches one of the parameter types, e.g., T,
        // add a "T: ::cross_check_runtime::hash::CrossCheckHash" where-predicate
        if let syn::Ty::Path(_, ref path) = *ty {
            if let Some(segment) = path.segments.first() {
                let seg_name = segment.ident.as_ref();
                if self.ty_param_names.contains(seg_name) &&
                   !self.seen_tys.contains(segment) {
                    let bp = syn::WhereBoundPredicate {
                        bound_lifetimes: vec![],
                        bounded_ty: ty.clone(),
                        bounds: vec![self.xcheck_hash_bound.clone()],
                    };
                    let wp = syn::WherePredicate::BoundPredicate(bp);
                    self.ty_where_preds.push(wp);
                    self.seen_tys.insert(segment.clone());
                }
            }
        }
        syn::visit::walk_ty(self, ty)
    }
}

#[proc_macro_derive(CrossCheckHash)]
pub fn derive_cross_check_hash(input: TokenStream) -> TokenStream {
    let struct_def = syn::parse_derive_input(&input.to_string()).unwrap();

    // Build the CrossCheckHashPredicateInserter
    let ty_param_names: HashSet<&str> = struct_def.generics.ty_params.iter()
        .map(|ty_param| ty_param.ident.as_ref())
        .collect();
    let xcheck_hash_bound = {
        const PATH_STR: &'static str = "::cross_check_runtime::hash::CrossCheckHash";
        let path = syn::parse_path(PATH_STR).unwrap();
        let trait_ref = syn::PolyTraitRef {
            bound_lifetimes: vec![],
            trait_ref: path,
        };
        syn::TyParamBound::Trait(trait_ref, syn::TraitBoundModifier::None)
    };
    let mut hash_pred_inserter = CrossCheckHashPredicateInserter {
        skip_visit: ty_param_names.is_empty(),
        xcheck_hash_bound: xcheck_hash_bound,
        ty_param_names: &ty_param_names,
        seen_tys: Default::default(),
        ty_where_preds: vec![],
    };

    // Iterate through all fields, doing 2 things at once:
    // 1) insert the hash computation for each field
    // 2) add the necessary where-predicates for the type of each field
    let hash_fields = each_field(&struct_def, &BindStyle::Ref.into(), |f| {
        if !hash_pred_inserter.skip_visit {
            syn::visit::Visitor::visit_ty(&mut hash_pred_inserter, &f.field.ty);
        }
        quote! {
            extern crate cross_check_runtime;
            use cross_check_runtime::hash::CrossCheckHash;
            h.write_u64(CrossCheckHash::cross_check_hash_depth::<__XCHA, __XCHS>(#f, _depth - 1));
        }
    });

    // Extract the generics components and add our own where predicates
    let (impl_generics, ty_generics, where_clause) = struct_def.generics.split_for_impl();
    let mut full_where_clause = where_clause.clone();
    full_where_clause.predicates.extend(hash_pred_inserter.ty_where_preds.into_iter());

    // TODO: we should mark the impl below with #[automatically_derived],
    // but that currently causes an "unused attribute" warning
    //
    // TODO: also hash in the name of the structure???
    let ident = &struct_def.ident;
    let hash_impl = quote! {
        #[allow(unused_mut)]
        impl #impl_generics ::cross_check_runtime::hash::CrossCheckHash
                for #ident #ty_generics #full_where_clause {
            fn cross_check_hash_depth<__XCHA, __XCHS>(&self, _depth: usize) -> u64
                    where __XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
                          __XCHS: ::cross_check_runtime::hash::CrossCheckHasher {
                let mut h = __XCHA::default();
                match *self { #hash_fields }
                h.finish()
            }
        }
    };
    hash_impl.parse().unwrap()
}
