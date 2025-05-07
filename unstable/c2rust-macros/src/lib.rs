#![recursion_limit = "128"]

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse_macro_input;
use syn::visit::Visit;
use syn::{Block, FnArg, Ident, Pat, TraitItemMethod, Type, TypeReference};

#[derive(Default)]
struct VisitorImpls {
    tokens: TokenStream,
    count: usize,
}

impl VisitorImpls {
    fn generate_visit(&mut self, method_name: &Ident, arg_pat: &Pat, ty: &Type, walk: &Block) {
        self.tokens.extend(quote! {
            impl MutVisit for #ty {
                fn visit<F: MutVisitor>(&mut self, f: &mut F) {
                    f.#method_name(self)
                }
            }
        });

        let folder_name = format!("Folder{}", self.count);
        let folder_ident = Ident::new(&folder_name, Span::call_site());

        if !walk.stmts.is_empty() {
            let noop_fn_name = format!("noop_{}", method_name);
            let noop_fn = Ident::new(&noop_fn_name, Span::call_site());
            self.tokens.extend(quote! {
                impl WalkAst for #ty {
                    fn walk<T: MutVisitor>(&mut self, v: &mut T) {
                        rustc_ast::mut_visit::#noop_fn(self, v);
                    }
                }
            });
        }
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

    fn generate_flat_map(&mut self, method_name: &Ident, arg_pat: &Pat, ty: &Type, walk: &Block) {
        self.tokens.extend(quote! {
            impl MutVisit for #ty {
                fn visit<F: MutVisitor>(&mut self, f: &mut F) {
                    let new = f.#method_name(self.clone());
                    *self = new.lone();
                }

                fn flat_map<F: MutVisitor>(self, f: &mut F) -> SmallVec<[#ty; 1]> {
                    f.#method_name(self)
                }
            }
        });

        let folder_name = format!("Folder{}", self.count);
        let folder_ident = Ident::new(&folder_name, Span::call_site());

        if !walk.stmts.is_empty() {
            let noop_fn_name = format!("noop_{}", method_name);
            let noop_fn = Ident::new(&noop_fn_name, Span::call_site());
            self.tokens.extend(quote! {
                impl WalkAst for #ty {
                    fn walk<T: MutVisitor>(&mut self, v: &mut T) {
                        *self = rustc_ast::mut_visit::#noop_fn(self.clone(), v).lone();
                    }
                }
            })
        }
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
        let method_noop = m.default.as_ref().unwrap();
        if let FnArg::Typed(pat_ty) = &m.sig.inputs[1] {
            match &*pat_ty.ty {
                Type::Reference(TypeReference {
                    mutability: Some(_),
                    elem,
                    ..
                }) => self.generate_visit(method_name, &pat_ty.pat, elem, method_noop),

                ty => self.generate_flat_map(method_name, &pat_ty.pat, ty, method_noop),
            }
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
