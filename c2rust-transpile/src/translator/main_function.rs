//! This module implements the translation of main functions in C.
//! Translating main requires us to wrap the C implementation to
//! a helper that can be called from a generated main function in
//! Rust.

use super::*;
use c2rust_ast_builder::CaptureBy;
use failure::format_err;
use proc_macro2::{TokenStream, TokenTree};

impl<'c> Translation<'c> {
    pub fn convert_main(&self, main_id: CDeclId) -> TranslationResult<Box<Item>> {
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
                ref k => {
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

            let decl = mk().fn_decl("main", vec![], None, ReturnType::Default);

            let main_fn = mk().path_expr(vec![main_fn_name]);

            let exit_fn = mk().abs_path_expr(vec!["std", "process", "exit"]);
            let args_fn = mk().abs_path_expr(vec!["std", "env", "args"]);
            let vars_fn = mk().abs_path_expr(vec!["std", "env", "vars"]);

            let no_args: Vec<Box<Expr>> = vec![];

            let mut stmts: Vec<Stmt> = vec![];
            let mut main_args: Vec<Box<Expr>> = vec![];

            let n = parameters.len();

            if n >= 2 {
                // `argv` and `argc`

                stmts.push(mk().local_stmt(Box::new({
                    // ty = Vec<Vec<u8>>
                    let ty = mk().path_ty(vec![mk().path_segment_with_args(
                        "Vec",
                        mk().angle_bracketed_args(vec![mk().path_ty(vec![mk()
                            .path_segment_with_args(
                                "Vec",
                                mk().angle_bracketed_args(vec![mk().ident_ty("u8")]),
                            )])]),
                    )]);
                    // map_arg = |arg| {
                    //     (::std::ffi::CString::new(arg))
                    //         .expect("Failed to convert argument into CString.")
                    //         .into_bytes_with_nul()
                    // }
                    let cstring_call = mk().call_expr(
                        // TODO(kkysen) change `"std"` to `"alloc"` after `#![feature(alloc_c_string)]` is stabilized in `1.63.0`
                        mk().abs_path_expr(vec!["std", "ffi", "CString", "new"]),
                        vec![mk().path_expr(vec!["arg"])],
                    );
                    let expect_arg = mk().lit_expr("Failed to convert argument into CString.");
                    let map_arg = mk().closure_expr(
                        CaptureBy::Ref,
                        Movability::Movable,
                        vec![mk().ident_pat("arg")],
                        ReturnType::Default,
                        mk().method_chain_expr(
                            cstring_call,
                            vec![
                                (mk().path_segment("expect"), vec![expect_arg]),
                                (mk().path_segment("into_bytes_with_nul"), vec![]),
                            ],
                        ),
                    );
                    // init = args_fn
                    //     .map(map_arg)
                    //     .collect();
                    let init = mk().method_chain_expr(
                        mk().call_expr(args_fn, vec![]),
                        vec![
                            (mk().path_segment("map"), vec![map_arg]),
                            (mk().path_segment("collect"), vec![]),
                        ],
                    );
                    mk().local(mk().mutbl().ident_pat("args_strings"), Some(ty), Some(init))
                })));

                stmts.push(mk().local_stmt(Box::new({
                    // ty = Vec<*mut ::core::ffi::c_char>
                    let ty = mk().path_ty(vec![mk().path_segment_with_args(
                        "Vec",
                        mk().angle_bracketed_args(vec![mk()
                            .mutbl()
                            .ptr_ty(mk().abs_path_ty(vec!["core", "ffi", "c_char"]))]),
                    )]);
                    // map_arg = |arg| arg.as_mut_ptr() as *mut ::core::ffi::c_char
                    let map_arg = mk().closure_expr(
                        CaptureBy::Ref,
                        Movability::Movable,
                        vec![mk().ident_pat("arg")],
                        ReturnType::Default,
                        mk().cast_expr(
                            mk().method_call_expr(mk().ident_expr("arg"), "as_mut_ptr", vec![]),
                            mk().mutbl()
                                .ptr_ty(mk().abs_path_ty(vec!["core", "ffi", "c_char"])),
                        ),
                    );
                    // chain_arg = ::core::iter::once(::core::ptr::null_mut())
                    let chain_arg = mk().call_expr(
                        mk().abs_path_expr(vec!["core", "iter", "once"]),
                        vec![mk().call_expr(
                            mk().abs_path_expr(vec!["core", "ptr", "null_mut"]),
                            vec![],
                        )],
                    );
                    // init = args_strings
                    //     .iter_mut()
                    //     .map(map_arg)
                    //     .chain(chain_arg)
                    //     .collect()
                    let init = mk().method_chain_expr(
                        mk().ident_expr("args_strings"),
                        vec![
                            (mk().path_segment("iter_mut"), vec![]),
                            (mk().path_segment("map"), vec![map_arg]),
                            (mk().path_segment("chain"), vec![chain_arg]),
                            (mk().path_segment("collect"), vec![]),
                        ],
                    );
                    mk().local(mk().mutbl().ident_pat("args_ptrs"), Some(ty), Some(init))
                })));

                let argc_ty: Box<Type> = match self.ast_context.index(parameters[0]).kind {
                    CDeclKind::Variable { ref typ, .. } => self.convert_type(typ.ctype),
                    _ => Err(TranslationError::generic(
                        "Cannot find type of 'argc' argument in main function",
                    )),
                }?;
                let argv_ty: Box<Type> = match self.ast_context.index(parameters[1]).kind {
                    CDeclKind::Variable { ref typ, .. } => self.convert_type(typ.ctype),
                    _ => Err(TranslationError::generic(
                        "Cannot find type of 'argv' argument in main function",
                    )),
                }?;
                let args = mk().ident_expr("args_ptrs");
                let argc = mk().binary_expr(
                    BinOp::Sub(Default::default()),
                    mk().method_call_expr(args.clone(), "len", no_args.clone()),
                    mk().lit_expr(mk().int_lit(1, "")),
                );
                let argv = mk().method_call_expr(args, "as_mut_ptr", no_args.clone());

                main_args.push(mk().cast_expr(argc, argc_ty));
                main_args.push(mk().cast_expr(argv, argv_ty));
            }

            if n >= 3 {
                // non-standard `envp`

                stmts.push(mk().local_stmt(Box::new(mk().local(
                    mk().mutbl().ident_pat("vars"),
                    Some(mk().path_ty(vec![mk().path_segment_with_args(
                        "Vec",
                        mk().angle_bracketed_args(vec![
                            mk().mutbl().ptr_ty(mk().abs_path_ty(vec!["core", "ffi", "c_char"])),
                        ]),
                    )])),
                    Some(mk().call_expr(mk().path_expr(vec!["Vec", "new"]), vec![])),
                ))));
                let var_name_ident = mk().ident("var_name");
                let var_value_ident = mk().ident("var_value");
                stmts.push(mk().semi_stmt(mk().for_expr(
                    mk().tuple_pat(vec![
                        mk().ident_pat("var_name"),
                        mk().ident_pat("var_value"),
                    ]),
                    mk().call_expr(vars_fn, vec![]),
                    mk().block(vec![
                        mk().local_stmt(Box::new(
                            mk().local(
                                mk().ident_pat("var"),
                                Some(mk().path_ty(vec!["String"])),
                                Some(
                                    mk().mac_expr(
                                        mk().mac(
                                            mk().path(vec!["format"]),
                                            vec![
                                                TokenTree::Literal(
                                                    proc_macro2::Literal::string("{}={}"),
                                                ),
                                                TokenTree::Punct(Punct::new(
                                                    ',',
                                                    proc_macro2::Spacing::Alone,
                                                )),
                                                TokenTree::Ident(var_name_ident),
                                                TokenTree::Punct(Punct::new(
                                                    ',',
                                                    proc_macro2::Spacing::Alone,
                                                )),
                                                TokenTree::Ident(var_value_ident),
                                            ]
                                            .into_iter()
                                            .collect::<TokenStream>(),
                                            MacroDelimiter::Paren(Default::default()),
                                        ),
                                    ),
                                ),
                            ),
                        )),
                        mk().semi_stmt(mk().method_call_expr(
                            mk().path_expr(vec!["vars"]),
                            "push",
                            vec![mk().method_call_expr(
                                mk().method_call_expr(
                                    mk().call_expr(
                                        mk().abs_path_expr(vec![
                                            // TODO(kkysen) change `"std"` to `"alloc"` after `#![feature(alloc_c_string)]` is stabilized in `1.63.0`
                                            "std", "ffi", "CString", "new",
                                        ]),
                                        vec![mk().path_expr(vec!["var"])],
                                    ),
                                    "expect",
                                    vec![mk().lit_expr(
                                    "Failed to convert environment variable into CString."
                                )],
                                ),
                                "into_raw",
                                vec![],
                            )],
                        )),
                    ]),
                    None as Option<Ident>,
                )));
                stmts.push(mk().semi_stmt(mk().method_call_expr(
                    mk().path_expr(vec!["vars"]),
                    "push",
                    vec![
                        mk().call_expr(mk().abs_path_expr(vec!["core", "ptr", "null_mut"]), vec![]),
                    ],
                )));

                let envp_ty: Box<Type> = match self.ast_context.index(parameters[2]).kind {
                    CDeclKind::Variable { ref typ, .. } => self.convert_type(typ.ctype),
                    _ => Err(TranslationError::generic(
                        "Cannot find type of 'envp' argument in main function",
                    )),
                }?;

                let envp = mk().method_call_expr(mk().ident_expr("vars"), "as_mut_ptr", no_args);

                main_args.push(mk().cast_expr(envp, envp_ty));
            }

            // Check `main` has the right form
            if n != 0 && n != 2 && n != 3 {
                return Err(format_err!(
                    "Main function should have 0, 2, or 3 parameters, not {}.",
                    n
                )
                .into());
            };

            if let CTypeKind::Void = ret {
                let call_main = mk().call_expr(main_fn, main_args);
                let unsafe_block = mk().unsafe_block(vec![mk().expr_stmt(call_main)]);

                stmts.push(mk().expr_stmt(mk().unsafe_block_expr(unsafe_block)));

                let exit_arg = mk().lit_expr(mk().int_lit(0, "i32"));
                let call_exit = mk().call_expr(exit_fn, vec![exit_arg]);

                stmts.push(mk().semi_stmt(call_exit));
            } else {
                let call_main = mk().cast_expr(
                    mk().call_expr(main_fn, main_args),
                    mk().path_ty(vec!["i32"]),
                );

                let call_exit = mk().call_expr(exit_fn, vec![call_main]);
                let unsafe_block = mk().unsafe_block(vec![mk().expr_stmt(call_exit)]);

                stmts.push(mk().expr_stmt(mk().unsafe_block_expr(unsafe_block)));
            };

            let block = mk().block(stmts);
            let main_attributes = self.mk_cross_check(mk(), vec!["none"]);
            Ok(main_attributes.pub_().fn_item(decl, block))
        } else {
            Err(TranslationError::generic(
                "Cannot translate non-function main entry point",
            ))
        }
    }
}
