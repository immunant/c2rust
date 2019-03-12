#![recursion_limit = "128"]
extern crate syn;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;

use proc_macro2::{Span, TokenStream};
use syn::parse_macro_input;
use syn::{ArgCaptured, Block, FnArg, Ident, Pat, TraitItemMethod, Type, TypeReference};
use syn::visit::Visit;
use quote::quote;

#[derive(Default)]
struct VisitorImpls {
    tokens: TokenStream,
    count: usize,
}

impl VisitorImpls {
    fn generate_visit(&mut self, method_name: &Ident, arg_pat: &Pat, ty: &Type, noop: &Option<Block>) {
        self.tokens.extend(quote! {
            impl MutVisit for #ty {
                fn visit<F: MutVisitor>(&mut self, f: &mut F) {
                    f.#method_name(self)
                }
            }
        });

        let folder_name = format!("Folder{}", self.count);
        let folder_ident = Ident::new(&folder_name, Span::call_site());

        let walk = match noop {
            Some(block) => quote! { #block },
            None => {
                let noop_fn_name = format!("noop_{}", method_name);
                let noop_fn = Ident::new(&noop_fn_name, Span::call_site());
                quote! {
                    syntax::mut_visit::#noop_fn(#arg_pat, self);
                }
            }
        };
        self.tokens.extend(quote! {
            struct #folder_ident<F>
                where F: FnMut(&mut #ty)
            {
                callback: F,
            }

            impl<F> MutVisitor for #folder_ident<F>
                where F: FnMut(&mut #ty)
            {
                fn #method_name(&mut self, #arg_pat: &mut #ty) {
                    #walk
                    (self.callback)(#arg_pat)
                }
            }

            impl MutVisitNodes for #ty {
                fn visit<T, F>(target: &mut T, callback: F)
                    where T: MutVisit,
                          F: FnMut(&mut Self)
                {
                    let mut f = #folder_ident { callback };
                    target.visit(&mut f)
                }
            }
        });

        self.count += 1;
    }

    fn generate_flat_map(&mut self, method_name: &Ident, arg_pat: &Pat, ty: &Type, noop: &Option<Block>) {
        self.tokens.extend(quote! {
            impl MutVisit for #ty {
                fn flat_map<F: MutVisitor>(self, f: &mut F) -> SmallVec<[#ty; 1]> {
                    f.#method_name(self)
                }
            }
        });

        let folder_name = format!("Folder{}", self.count);
        let folder_ident = Ident::new(&folder_name, Span::call_site());

        let walk = match noop {
            Some(block) => quote! { #block },
            None => {
                let noop_fn_name = format!("noop_{}", method_name);
                let noop_fn = Ident::new(&noop_fn_name, Span::call_site());
                quote! {
                    syntax::mut_visit::#noop_fn(#arg_pat, self);
                }
            }
        };
        self.tokens.extend(quote! {
            struct #folder_ident<F>
                where F: FnMut(#ty) -> SmallVec<[#ty; 1]>
            {
                callback: F,
            }

            impl<F> MutVisitor for #folder_ident<F>
                where F: FnMut(#ty) -> SmallVec<[#ty; 1]>
            {
                fn #method_name(&mut self, #arg_pat: #ty) -> SmallVec<[#ty; 1]> {
                    let mut v = #walk;
                    v.flat_map_in_place(|x| (self.callback)(x));
                    v
                }
            }

            impl FlatMapNodes for #ty {
                fn visit<T, F>(target: &mut T, callback: F)
                    where T: MutVisit,
                          F: FnMut(#ty) -> SmallVec<[#ty; 1]>
                {
                    let mut f = #folder_ident { callback };
                    target.visit(&mut f)
                }

                fn flat_map<T, F>(target: T, callback: F) -> SmallVec<[T; 1]>
                    where T: MutVisit,
                          F: FnMut(#ty) -> SmallVec<[#ty; 1]>
                {
                    let mut f = #folder_ident { callback };
                    target.flat_map(&mut f)
                }
            }
        });

        self.count += 1;
    }
}

impl<'ast> Visit<'ast> for VisitorImpls {
    fn visit_trait_item_method(&mut self, m: &TraitItemMethod) {
        let method_name = &m.sig.ident;
        let method_noop = &m.default;
        match &m.sig.decl.inputs[1] {
            FnArg::Captured(
                ArgCaptured {
                    pat,
                    ty,
                    ..
                }
            ) => {
                match ty {
                    Type::Reference(
                        TypeReference {
                            mutability: Some(_),
                            elem,
                            ..
                        }
                    ) => self.generate_visit(method_name, &pat, &elem, method_noop),

                    _ => self.generate_flat_map(method_name, &pat, &ty, method_noop),
                }
            }

            _ => {}
        }
    }
}

#[proc_macro]
pub fn gen_visitor_impls(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let visitor_trait: syn::ItemTrait = parse_macro_input!(tokens);
    let mut visitor = VisitorImpls::default();
    visitor.visit_item_trait(&visitor_trait);

    visitor.tokens.into()
}

