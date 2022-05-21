//! This module implements the translation of main functions in C.
//! Translating main requires us to wrap the C implementation to
//! a helper that can be called from a generated main function in
//! Rust.

use failure::format_err;
use proc_macro2::Span;

use crate::translator::{Translation, TranslationError};
use crate::{CDeclId, CDeclKind, CParamId, CTypeKind};

impl<'c> Translation<'c> {
    pub fn convert_main(&self, main_id: CDeclId) -> Result<Box<syn::Item>, TranslationError> {
        let (parameters, typ) = if let CDeclKind::Function {
            parameters, typ, ..
        } = &self.ast_context[main_id].kind
        {
            (parameters, typ)
        } else {
            return Err(TranslationError::generic(
                "Cannot translate non-function main entry point",
            ));
        };

        let ret: CTypeKind = match &self.ast_context.resolve_type(*typ).kind {
            CTypeKind::Function(ret, _, _, _, _) => {
                self.ast_context.resolve_type(ret.ctype).kind.clone()
            }
            k => {
                return Err(format_err!(
                    "Type of main function {:?} was not a function type, got {:?}",
                    main_id,
                    k
                )
                .into())
            }
        };

        let main_fn_name = self
            .renamer
            .borrow()
            .get(&main_id)
            .expect("Could not find main function in renamer");

        let main_fn = syn::Ident::new(&main_fn_name, Span::call_site());

        let get_arg_ty = |idx: &CParamId, name: &str| match &self.ast_context[*idx].kind {
            CDeclKind::Variable { typ, .. } => self.convert_type(typ.ctype),
            _ => {
                Err(format_err!("Cannot find type of '{}' argument in main function", name).into())
            }
        };

        match parameters.as_slice() {
            [] => {
                if let CTypeKind::Void = ret {
                    Ok(syn::parse_quote! {
                        pub fn main() {
                            unsafe { #main_fn() };
                            ::std::process::exit(0_i32);
                        }
                    })
                } else {
                    Ok(syn::parse_quote! {
                        pub fn main() {
                            let main_result = unsafe { #main_fn() };
                            ::std::process::exit(main_result as i32);
                        }
                    })
                }
            }

            [argc_id, argv_id, rest @ ..] if rest.len() <= 1 => {
                let mut stmts: Vec<syn::Stmt> = vec![];
                let mut main_args: Vec<syn::Expr> = vec![];

                // `argv` and `argc`

                stmts.append(&mut syn::parse_quote! {
                    let mut args: Vec<*mut ::libc::c_char> = Vec::new();
                    for arg in ::std::env::args() {
                        args.push(
                            ::std::ffi::CString::new(arg)
                                .expect("Failed to convert argument into CString.")
                                .into_raw()
                        );
                    }
                    args.push(::std::ptr::null_mut());
                });

                let argc_ty: Box<syn::Type> = get_arg_ty(argc_id, "argc")?;
                let argv_ty: Box<syn::Type> = get_arg_ty(argv_id, "argv")?;

                main_args.push(syn::parse_quote! { (args.len() - 1) as #argc_ty });
                main_args.push(syn::parse_quote! { args.as_mut_ptr() as #argv_ty });

                if let [envp_id] = rest {
                    // non-standard `envp`

                    stmts.append(&mut syn::parse_quote! {
                        let mut vars: Vec<*mut ::libc::c_char> = Vec::new();
                        for (var_name, var_value) in ::std::env::vars() {
                            let var: String = format!("{}={}", var_name, var_value);
                            vars.push(
                                ::std::ffi::CString::new(var)
                                    .expect("Failed to convert environment variable into CString.")
                                    .into_raw()
                            );
                        }
                        vars.push(::std::ptr::null_mut);
                    });

                    let envp_ty: Box<syn::Type> = get_arg_ty(envp_id, "envp")?;

                    main_args.push(syn::parse_quote! { vars.as_mut_ptr() as #envp_ty });
                }

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
            }

            _ => Err(format_err!(
                "Main function should have 0, 2, or 3 parameters, not {}.",
                parameters.len(),
            )
            .into()),
        }
    }
}
