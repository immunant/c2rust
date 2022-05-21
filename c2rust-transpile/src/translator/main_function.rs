//! This module implements the translation of main functions in C.
//! Translating main requires us to wrap the C implementation to
//! a helper that can be called from a generated main function in
//! Rust.

use super::*;
use failure::format_err;
use proc_macro2::{TokenStream, TokenTree};

impl<'c> Translation<'c> {
    pub fn convert_main(&self, main_id: CDeclId) -> Result<Box<Item>, TranslationError> {
        if let CDeclKind::Function {
            ref parameters,
            typ,
            ..
        } = self.ast_context.index(main_id).kind
        {
            let ret: CTypeKind = match self.ast_context.resolve_type(typ).kind {
                CTypeKind::Function(ret, _, _, _, _) => {
                    self.ast_context.resolve_type(ret.ctype).kind.clone()
                }
                ref k => Err(format_err!(
                    "Type of main function {:?} was not a function type, got {:?}",
                    main_id,
                    k
                ))?,
            };

            let main_fn_name = self
                .renamer
                .borrow()
                .get(&main_id)
                .expect("Could not find main function in renamer");

            let main_fn = syn::Ident::new(&main_fn_name, Span::dummy());

            let mut stmts: Vec<Stmt> = vec![];
            let mut main_args: Vec<Box<Expr>> = vec![];

            let get_arg_ty = |idx: CParamId, name: &str| match self.ast_context[idx].kind {
                CDeclKind::Variable { ref typ, .. } => self.convert_type(typ.ctype),
                _ => Err(TranslationError::generic(
                    format!("Cannot find type of '{}' argument in main function", name),
                )),
            };

            let n = parameters.len();

            if n >= 2 {
                // `argv` and `argc`

                stmts.push(syn::parse_quote! {
                    let mut args: Vec<*mut ::libc::c_char> = Vec::new();
                });
                stmts.push(syn::parse_quote! {
                    for arg in ::std::env::args() {
                        args.push(
                            ::std::ffi::CString::new(arg)
                                .expect("Failed to convert argument into CString.")
                                .into_raw()
                        );
                    }
                });
                stmts.push(syn::parse_quote! {
                    args.push(::std::ptr::null_mut());
                });

                let argc_ty: Box<Type> = get_arg_ty(parameters[0], "argc")?;
                let argv_ty: Box<Type> = get_arg_ty(parameters[1], "argv")?;

                main_args.push(syn::parse_quote!((args.len() - 1) as #argc_ty));
                main_args.push(syn::parse_quote!(args.as_mut_ptr() as #argv_ty));
            }

            if n >= 3 {
                // non-standard `envp`

                stmts.push(syn::parse_quote! {
                    let mut vars: Vec<*mut ::libc::c_char> = Vec::new();
                });
                stmts.push(syn::parse_quote! {
                    for (var_name, var_value) in ::std::env::vars() {
                        let var: String = format!("{}={}", var_name, var_value);
                        vars.push(
                            ::std::ffi::CString::new(var)
                                .expect("Failed to convert environment variable into CString.")
                                .into_raw()
                        );
                    }
                });
                stmts.push(syn::parse_quote! {
                    vars.push(::std::ptr::null_mut);
                });

                let envp_ty: Box<Type> = get_arg_ty(parameters[2], "envp")?;

                main_args.push(syn::parse_quote!(vars.as_mut_ptr() as #envp_ty));
            }

            // Check `main` has the right form
            if n != 0 && n != 2 && n != 3 {
                Err(format_err!(
                    "Main function should have 0, 2, or 3 parameters, not {}.",
                    n
                ))?;
            };

            if let CTypeKind::Void = ret {
                stmts.append(&mut syn::parse_quote! {
                    unsafe { #main_fn( #(#main_args),* ) };
                    ::std::process::exit(0_i32);
                });
            } else {
                stmts.append(&mut syn::parse_quote! {
                    let main_result = unsafe { #main_fn( #(#main_args),* ) };
                    ::std::process::exit(main_result as i32);
                });
            };

            Ok(syn::parse_quote! {
                pub fn main() {
                    #(#stmts)*
                }
            })
        } else {
            Err(TranslationError::generic(
                "Cannot translate non-function main entry point",
            ))
        }
    }
}
