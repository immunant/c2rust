use syntax::ast;
use syntax::ast::*;
use syntax::tokenstream::{TokenStream};
use syntax::parse::token::{DelimToken,Token,Nonterminal};
use syntax::abi::Abi;
use std::collections::HashMap;
use renamer::Renamer;
use convert_type::TypeConverter;
use loops::*;
use idiomize::ast_manip::make_ast::*;
use c_ast;
use c_ast::*;
use syntax::ptr::*;
use syntax::print::pprust::*;
use std::ops::Index;
use std::cell::RefCell;
use dtoa;

use cfg;

pub struct Translation {
    pub items: Vec<P<Item>>,
    type_converter: RefCell<TypeConverter>,
    pub ast_context: TypedAstContext,
    renamer: RefCell<Renamer<CDeclId>>,
    loops: LoopContext,
    reloop_cfgs: bool,
    zero_inits: RefCell<HashMap<CDeclId, Result<P<Expr>, String>>>,
    dump_function_cfgs: bool,
    dump_structures: bool,
    debug_relooper_labels: bool,
    cross_checks: bool,
}

#[derive(Debug)]
pub struct WithStmts<T> {
    pub stmts: Vec<Stmt>,
    pub val: T,
}

impl<T> WithStmts<T> {
    pub fn new(val: T) -> Self {
        WithStmts { stmts: vec![], val, }
    }
    pub fn and_then<U,F: FnOnce(T) -> WithStmts<U>>(self, f : F) -> WithStmts<U> {
        let mut next = f(self.val);
        let mut stmts = self.stmts;
        stmts.append(&mut next.stmts);
        WithStmts {
            val: next.val,
            stmts
        }
    }
    pub fn map<U,F: FnOnce(T) -> U>(self, f : F) -> WithStmts<U> {
        WithStmts {
            val: f(self.val),
            stmts: self.stmts,
        }
    }
    pub fn result_map<U,E,F: FnOnce(T) -> Result<U,E>>(self, f : F) -> Result<WithStmts<U>,E> {
        Ok(WithStmts {
            val: f(self.val)?,
            stmts: self.stmts,
        })
    }
}

impl WithStmts<P<Expr>> {

    /// Package a series of statements and an expression into one block expression
    pub fn to_expr(mut self) -> P<Expr> {
        if self.stmts.is_empty() {
            self.val
        } else {
            self.stmts.push(mk().expr_stmt(self.val));
            mk().block_expr(mk().block(self.stmts))
        }
    }

    /// Package a series of statements and an expression into one block
    pub fn to_block(mut self) -> P<Block> {
        self.stmts.push(mk().expr_stmt(self.val));
        mk().block(self.stmts)
    }
}

fn sequence_option<A,E>(x: Option<Result<A,E>>) -> Result<Option<A>, E> {
    match x {
        None => Ok(None),
        Some(Ok(o)) => Ok(Some(o)),
        Some(Err(e)) => Err(e),
    }
}

fn cast_int(val: P<Expr>, name: &str) -> P<Expr> {
    let opt_literal_val = match &val.node {
        &ExprKind::Lit(ref l) => match &l.node {
            &LitKind::Int(i,_) => Some(i),
            _ => None,
        }
        _ => None,
    };
    match opt_literal_val {
        Some(i) => mk().lit_expr(mk().int_lit(i, name)),
        None => mk().cast_expr(val, mk().path_ty(vec![name])),
    }
}

/// Pointer offset that casts its argument to isize
fn pointer_offset(ptr: P<Expr>, offset: P<Expr>) -> P<Expr> {
    pointer_offset_isize(ptr, cast_int(offset, "isize"))
}


/// Pointer offset that requires its argument to have type isize
fn pointer_offset_isize(ptr: P<Expr>, offset: P<Expr>) -> P<Expr> {
    mk().method_call_expr(ptr, "offset", vec![offset])
}

fn pointer_neg_offset(ptr: P<Expr>, offset: P<Expr>) -> P<Expr> {
    let offset = cast_int(offset, "isize");
    pointer_neg_offset_isize(ptr, offset)
}

fn pointer_neg_offset_isize(ptr: P<Expr>, offset: P<Expr>) -> P<Expr> {
    mk().method_call_expr(ptr, "offset", vec![mk().unary_expr(ast::UnOp::Neg, offset)])
}

/// Construct a new constant null pointer expression
fn null_expr(ty: P<Ty>) -> P<Expr>  {
    mk().call_expr(mk().path_expr(vec![
        mk().path_segment(""),
        mk().path_segment("std"),
        mk().path_segment("ptr"),
        mk().path_segment_with_params("null", mk().angle_bracketed_param_types(vec![
            ty
        ])),
    ]), vec![] as Vec<P<Expr>>)
}

/// Construct a new mutable null pointer expression
fn null_mut_expr(ty: P<Ty>) -> P<Expr> {
    mk().call_expr(mk().path_expr(vec![
        mk().path_segment(""),
        mk().path_segment("std"),
        mk().path_segment("ptr"),
        mk().path_segment_with_params("null_mut", mk().angle_bracketed_param_types(vec![
            ty
        ])),
    ]), vec![] as Vec<P<Expr>>)
}

fn neg_expr(arg: P<Expr>) -> P<Expr> {
    mk().unary_expr(ast::UnOp::Neg, arg)
}

fn wrapping_neg_expr(arg: P<Expr>) -> P<Expr> {
    mk().method_call_expr(arg, "wrapping_neg", vec![] as Vec<P<Expr>>)
}

fn is_int(ty: &CTypeKind) -> bool {
    match *ty {
        CTypeKind::SChar |
        CTypeKind::Short |
        CTypeKind::Int |
        CTypeKind::Long |
        CTypeKind::LongLong |
        CTypeKind::UChar |
        CTypeKind::UShort |
        CTypeKind::UInt |
        CTypeKind::ULong |
        CTypeKind::ULongLong |
        CTypeKind::Int128 |
        CTypeKind::UInt128 => true,
        _ => false,
    }
}

fn transmute_expr(source_ty: P<Ty>, target_ty: P<Ty>, expr: P<Expr>) -> P<Expr> {
    let type_args = vec![source_ty, target_ty];
    let path = vec![
        mk().path_segment(""),
        mk().path_segment("std"),
        mk().path_segment("mem"),
        mk().path_segment_with_params("transmute",
                                      mk().angle_bracketed_param_types(type_args)),
    ];
    mk().call_expr(mk().path_expr(path), vec![expr])
}

pub fn stmts_block(mut stmts: Vec<Stmt>) -> P<Block> {
    if stmts.len() == 1 {
        if let StmtKind::Expr(ref e) = stmts[0].node {
            if let ExprKind::Block(ref b) = e.node {
                return b.clone()
            }
        }
    }

    if stmts.len() > 0 {
        let n = stmts.len() - 1;
        let s = stmts.remove(n);
        stmts.push(s.add_trailing_semicolon())
    }

    mk().block(stmts)
}

pub fn with_stmts_opt<T>(opt: Option<WithStmts<T>>) -> WithStmts<Option<T>> {
    match opt {
        None => WithStmts::new(None),
        Some(x) => WithStmts { stmts: x.stmts, val: Some(x.val) },
    }
}

// Generate link attributes needed to ensure that the generated Rust libraries have the right symbol
// values.
fn mk_linkage(in_extern_block: bool, new_name: &str, old_name: &str) -> Builder {
    if new_name == old_name {
        mk().single_attr("no_mangle")          // Don't touch my name Rust!
    } else if in_extern_block {
        mk().str_attr("link_name", old_name)   // Look for this name
    } else {
        mk().str_attr("export_name", old_name) // Make sure you actually name it this
    }
}

pub fn signed_int_expr(value: i64) -> P<Expr> {
    if value < 0 {
        mk().unary_expr(ast::UnOp::Neg, mk().lit_expr(mk().int_lit((-value) as u128, "")))
    } else {
        mk().lit_expr(mk().int_lit(value as u128, ""))
    }
}

// This should only be used for tests
fn prefix_names(translation: &mut Translation, prefix: &str) {
    for (&decl_id, ref mut decl) in &mut translation.ast_context.c_decls {
        match decl.kind {
            CDeclKind::Function { ref mut name, ref body, .. } if body.is_some() => {
                name.insert_str(0, prefix);

                translation.renamer.borrow_mut().insert(decl_id, &name);
            },
            CDeclKind::Variable { ref mut ident, is_static, .. } if is_static => ident.insert_str(0, prefix),
            _ => (),
        }
    }
}

pub fn translate(
    ast_context: &TypedAstContext,
    reloop_cfgs: bool,
    dump_function_cfgs: bool,
    dump_structures: bool,
    debug_relooper_labels: bool,
    cross_checks: bool,
    cross_check_configs: Vec<&str>,
    prefix_function_names: Option<&str>,
    translate_entry: bool,
) -> String {

    let mut t = Translation::new(
        ast_context.clone(),
        reloop_cfgs,
        dump_function_cfgs,
        dump_structures,
        debug_relooper_labels,
        cross_checks,
    );
    if !translate_entry {
        t.ast_context.c_main = None;
    }

    enum Name<'a> {
        VarName(&'a str),
        TypeName(&'a str),
        AnonymousType,
        NoName,
    }

    fn some_type_name(s: Option<&str>) -> Name {
        match s {
            None => Name::AnonymousType,
            Some(r) => Name::TypeName(r),
        }
    }

    // Used for testing; so that we don't overlap with C function names
    if let Some(prefix) = prefix_function_names {
        prefix_names(&mut t, prefix);
    }

    // Populate renamer with top-level names
    for (&decl_id, decl) in &ast_context.c_decls {
        let decl_name = match &decl.kind {
            &CDeclKind::Struct { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
            &CDeclKind::Enum { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
            &CDeclKind::Union { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
            &CDeclKind::Typedef { ref name, .. } => Name::TypeName(name),
            &CDeclKind::Function { ref name, .. } => Name::VarName(name),
            // &CDeclKind::EnumConstant { ref name, .. } => Name::VarName(name),
            &CDeclKind::Variable { ref ident, .. }
              if ast_context.c_decls_top.contains(&decl_id) => Name::VarName(ident),
            _ => Name::NoName,
        };
        match decl_name {
            Name::NoName => (),
            Name::AnonymousType => { t.type_converter.borrow_mut().declare_decl_name(decl_id, "unnamed"); }
            Name::TypeName(name)=> { t.type_converter.borrow_mut().declare_decl_name(decl_id, name); }
            Name::VarName(name) => { t.renamer.borrow_mut().insert(decl_id, &name); }
        }
    }

    // Export all types
    for (&decl_id, decl) in &ast_context.c_decls {
        let needs_export = match &decl.kind {
            &CDeclKind::Struct { .. } => true,
            &CDeclKind::Enum { .. } => true,
            &CDeclKind::Union { .. } => true,
            &CDeclKind::Typedef { .. } => true,
            _ => false,
        };
        if needs_export {
            match t.convert_decl(true, decl_id) {
                Ok(item) => t.items.push(item),
                Err(e) => {
                    let ref k = t.ast_context.c_decls.get(&decl_id).map(|x| &x.kind);
                    eprintln!("Skipping declaration due to error: {}, kind: {:?}", e, k)
                },
            }
        }
    }

    // Export top-level value declarations
    for top_id in &ast_context.c_decls_top {
        let needs_export = match &ast_context.c_decls[top_id].kind {
            &CDeclKind::Function { .. } => true,
            &CDeclKind::Variable { .. } => true,
            _ => false,
        };
        if needs_export {
            match t.convert_decl(true, *top_id) {
                Ok(item) => t.items.push(item),
                Err(e) => {
                    let ref k = t.ast_context.c_decls.get(top_id).map(|x| &x.kind);
                    eprintln!("Skipping declaration due to error: {}, kind: {:?}", e, k)
                },
            }
        }
    }

    // Add the main entry point
    if let Some(main_id) = t.ast_context.c_main {
        match t.convert_main(main_id) {
            Ok(item) => t.items.push(item),
            Err(e) => eprintln!("Skipping main declaration due to error: {}", e)
        }
    };

    to_string(|s| {

        let mut features =
            vec![("feature",vec!["libc","i128_type","const_ptr_null","offset_to", "const_ptr_null_mut"]),
                 ("allow"  ,vec!["non_upper_case_globals", "non_camel_case_types","non_snake_case",
                                 "dead_code", "mutable_transmutes"]),
            ];
        if cross_checks {
            features.push(("feature", vec!["plugin", "custom_attribute"]));
            features.push(("cross_check", vec!["yes"]));
        }

        for (key,values) in features {
            for value in values {
                s.print_attribute(&mk().attribute::<_, TokenStream>(
                    AttrStyle::Inner,
                    vec![key],
                    vec![
                        Token::OpenDelim(DelimToken::Paren),
                        Token::Ident(mk().ident(value)),
                        Token::CloseDelim(DelimToken::Paren),
                    ].into_iter().collect(),
                ))?
            }
        }

        if cross_checks {
            let mut xcheck_attr_args = String::new();
            for ref config_file in &cross_check_configs {
                if !xcheck_attr_args.is_empty() {
                    xcheck_attr_args.push(',');
                }
                xcheck_attr_args.push_str("config_file=\"");
                xcheck_attr_args.push_str(config_file);
                xcheck_attr_args.push('"');
            }
            let xcheck_attr = format!("cross_check_plugin({})", xcheck_attr_args);
            s.print_attribute(&mk().attribute::<_, TokenStream>(
                AttrStyle::Inner,
                vec!["plugin"],
                vec![
                    Token::OpenDelim(DelimToken::Paren),
                    Token::Ident(mk().ident(xcheck_attr)),
                    Token::CloseDelim(DelimToken::Paren),
                ].into_iter().collect(),
            ))?
        }

        // Add `extern crate libc` to the top of the file
        s.print_item(&mk().extern_crate_item("libc", None))?;
        if cross_checks {
            s.print_item(&mk().single_attr("macro_use")
                              .extern_crate_item("cross_check_derive", None))?;
            s.print_item(&mk().single_attr("macro_use")
                              .extern_crate_item("cross_check_runtime", None))?;
        }

        // Add the items accumulated
        for x in t.items.iter() {
            s.print_item(x)?;
        }

        Ok(())
    })
}


/// Convert a boolean expression to a c_int
fn bool_to_int(val: P<Expr>) -> P<Expr> {
    mk().cast_expr(val, mk().path_ty(vec!["libc","c_int"]))
}

/// This represents all of the ways a C expression can be used in a C program. Making this
/// distinction is important for:
///
///   * not generating a bunch of unnecessary code, e.g., the expression `p = 1` evaluates `1`,
///     but when used in a statement like `p = 1;`, we don't care about this, so we can translate
///     to the Rust `p = 1` (even if it evaluates to the unit type). We get this behaviour by
///     translating expression statements using `ExprUse::Unused`.
///
///   * handling `volatile` properly, e.g., suppose `volatile int n, *p;` and `int x;`.
///     Then, `x = n` is a volatile read of `n` but `p = &n` is not. We get this behaviour
///     by translating the argument of `&` using `ExprUse::LValue` and the right hand side of `=`
///
///     using `ExprUse::RValue`.
///
///   * handling `volatile` properly
///
/// See `Translation::convert_expr` for more details.
#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Ord, Eq)]
pub enum ExprUse {
    /// expressions interesting only for their side-effects - we don't care about their values
    Unused,
    /// expressions used as C lvalues
    LValue,
    /// expressions used as C rvalues
    RValue,
}

impl Translation {
    pub fn new(
        ast_context: TypedAstContext,
        reloop_cfgs: bool,
        dump_function_cfgs: bool,
        dump_structures: bool,
        debug_relooper_labels: bool,
        cross_checks: bool,
    ) -> Translation {
        Translation {
            items: vec![],
            type_converter: RefCell::new(TypeConverter::new()),
            ast_context,
            renamer: RefCell::new(Renamer::new(&[
                // Keywords currently in use
                "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
                "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
                "ref", "return", "Self", "self", "static", "struct", "super", "trait", "true",
                "type", "unsafe", "use", "where", "while",

                // Keywords reserved for future use
                "abstract", "alignof", "become", "box", "do", "final", "macro", "offsetof",
                "override", "priv", "proc", "pure", "sizeof", "typeof", "unsized", "virtual",
                "yield",

                // Prevent use for other reasons
                "main",
            ])),
            loops: LoopContext::new(),
            zero_inits: RefCell::new(HashMap::new()),
            reloop_cfgs,
            dump_function_cfgs,
            dump_structures,
            debug_relooper_labels,
            cross_checks,
        }
    }

    // This node should _never_ show up in the final generated code. This is an easy way to notice
    // if it does.
    pub fn panic() -> P<Expr> {
        mk().mac_expr(mk().mac(vec!["compile_error"], vec![]))
    }

    fn mk_cross_check(&self, mk: Builder, args: Vec<&str>) -> Builder {
        if self.cross_checks {
            mk.call_attr("cross_check", args)
        } else { mk }
    }

    fn convert_main(&self, main_id: CDeclId) -> Result<P<Item>, String> {
        if let CDeclKind::Function { ref parameters, typ, .. } = self.ast_context.index(main_id).kind {

            let ret: CTypeKind = match &self.ast_context.resolve_type(typ).kind {
                &CTypeKind::Function(ret, _, _) => self.ast_context.resolve_type(ret.ctype).kind.clone(),
                k => return Err(format!("Type of main function {:?} was not a function type, got {:?}", main_id, k))
            };

            let decl = mk().fn_decl(
                vec![],
                FunctionRetTy::Ty(mk().tuple_ty(vec![] as Vec<P<Ty>>)),
                false
            );

            let main_fn_name = self.renamer.borrow()
                .get(&main_id).expect("Could not find main function in renamer");
            let main_fn = mk().path_expr(vec![main_fn_name]);

            let exit_fn = mk().path_expr(vec!["", "std", "process", "exit"]);
            let args_fn = mk().path_expr(vec!["", "std", "env", "args"]);
            let vars_fn = mk().path_expr(vec!["", "std", "env", "vars"]);

            let no_args: Vec<P<Expr>> = vec![];

            let mut stmts: Vec<Stmt> = vec![];
            let mut main_args: Vec<P<Expr>> = vec![];

            let n = parameters.len();

            if n >= 2 {
                // `argv` and `argc`

                stmts.push(mk().local_stmt(P(mk().local(
                    mk().mutbl().ident_pat("args"),
                    Some(mk().path_ty(vec![mk().path_segment_with_params(
                        "Vec",
                        mk().angle_bracketed_param_types(
                            vec![mk().mutbl().ptr_ty(
                                mk().path_ty(vec!["libc","c_char"])
                            )]
                        ),
                    )])),
                    Some(mk().call_expr(
                        mk().path_expr(vec!["Vec","new"]),
                        vec![] as Vec<P<Expr>>)
                    ),
                ))));
                stmts.push(mk().semi_stmt(mk().for_expr(
                    mk().ident_pat("arg"),
                    mk().call_expr(args_fn, vec![] as Vec<P<Expr>>),
                    mk().block(vec![
                        mk().semi_stmt(mk().method_call_expr(
                            mk().path_expr(vec!["args"]),
                            "push",
                            vec![
                                mk().method_call_expr(
                                    mk().method_call_expr(
                                        mk().call_expr(
                                            mk().path_expr(vec!["","std","ffi","CString","new"]),
                                            vec![mk().path_expr(vec!["arg"])],
                                        ),
                                        "expect",
                                        vec![mk().lit_expr(
                                            mk().str_lit("Failed to convert argument into CString.")
                                        )],
                                    ),
                                    "into_raw",
                                    vec![] as Vec<P<Expr>>,
                                )
                            ],
                        ))
                    ]),
                    None as Option<Ident>,
                )));
                stmts.push(mk().semi_stmt(mk().method_call_expr(
                    mk().path_expr(vec!["args"]),
                    "push",
                    vec![
                        mk().call_expr(
                            mk().path_expr(vec!["","std","ptr","null_mut"]),
                            vec![] as Vec<P<Expr>>,
                        )
                    ],
                )));


                let argc_ty: P<Ty> = match self.ast_context.index(parameters[0]).kind {
                    CDeclKind::Variable { ref typ, .. } => self.convert_type(typ.ctype),
                    _ => Err(format!("Cannot find type of 'argc' argument in main function")),
                }?;
                let argv_ty: P<Ty> = match self.ast_context.index(parameters[1]).kind {
                    CDeclKind::Variable { ref typ, .. } => self.convert_type(typ.ctype),
                    _ => Err(format!("Cannot find type of 'argv' argument in main function")),
                }?;

                let args = mk().ident_expr("args");
                let argc = mk().binary_expr(
                    BinOpKind::Sub,
                    mk().method_call_expr(args.clone(), "len", no_args.clone()),
                    mk().lit_expr(mk().int_lit(1,""))
                );
                let argv = mk().method_call_expr(args, "as_mut_ptr", no_args.clone());

                main_args.push(mk().cast_expr(argc, argc_ty));
                main_args.push(mk().cast_expr(argv, argv_ty));
            }

            if n >= 3 {
                // non-standard `envp`

                stmts.push(mk().local_stmt(P(mk().local(
                    mk().mutbl().ident_pat("vars"),
                    Some(mk().path_ty(vec![mk().path_segment_with_params(
                        "Vec",
                        mk().angle_bracketed_param_types(
                            vec![mk().mutbl().ptr_ty(
                                mk().path_ty(vec!["libc","c_char"])
                            )]
                        ),
                    )])),
                    Some(mk().call_expr(
                        mk().path_expr(vec!["Vec","new"]),
                        vec![] as Vec<P<Expr>>)
                    ),
                ))));
                stmts.push(mk().semi_stmt(mk().for_expr(
                    mk().tuple_pat(vec![mk().ident_pat("var_name"), mk().ident_pat("var_value")]),
                    mk().call_expr(vars_fn, vec![] as Vec<P<Expr>>),
                    mk().block(vec![
                        mk().local_stmt(P(mk().local(
                            mk().ident_pat("var"),
                            Some(mk().path_ty(vec!["String"])),
                            Some(mk().mac_expr(mk().mac(
                                vec!["format"],
                                vec![
                                    Token::interpolated(Nonterminal::NtExpr(mk().lit_expr(mk().str_lit("{}={}")))),
                                    Token::Comma,
                                    Token::Ident(mk().ident("var_name")),
                                    Token::Comma,
                                    Token::Ident(mk().ident("var_value")),
                                ].into_iter().collect::<TokenStream>(),
                            )))
                        ))),
                        mk().semi_stmt(mk().method_call_expr(
                            mk().path_expr(vec!["vars"]),
                            "push",
                            vec![
                                mk().method_call_expr(
                                    mk().method_call_expr(
                                        mk().call_expr(
                                            mk().path_expr(vec!["","std","ffi","CString","new"]),
                                            vec![mk().path_expr(vec!["var"])],
                                        ),
                                        "expect",
                                        vec![mk().lit_expr(
                                            mk().str_lit("Failed to convert environment variable into CString.")
                                        )],
                                    ),
                                    "into_raw",
                                    vec![] as Vec<P<Expr>>,
                                )
                            ],
                        ))
                    ]),
                    None as Option<Ident>,
                )));
                stmts.push(mk().semi_stmt(mk().method_call_expr(
                    mk().path_expr(vec!["vars"]),
                    "push",
                    vec![
                        mk().call_expr(
                            mk().path_expr(vec!["","std","ptr","null_mut"]),
                            vec![] as Vec<P<Expr>>,
                        )
                    ],
                )));

                let envp_ty: P<Ty> = match self.ast_context.index(parameters[2]).kind {
                    CDeclKind::Variable { ref typ, .. } => self.convert_type(typ.ctype),
                    _ => Err(format!("Cannot find type of 'envp' argument in main function")),
                }?;

                let envp = mk().method_call_expr(mk().ident_expr("vars"), "as_mut_ptr", no_args);

                main_args.push(mk().cast_expr(envp, envp_ty));
            }

            // Check `main` has the right form
            if n != 0 && n != 2 && n != 3 {
                Err(format!("Main function should have 0, 2, or 3 parameters, not {}.", n))?;
            };

            if let CTypeKind::Void = ret {
                let call_main = mk().call_expr(main_fn, main_args);
                let unsafe_block = mk().unsafe_().block(vec![mk().expr_stmt(call_main)]);

                stmts.push(mk().expr_stmt(mk().block_expr(unsafe_block)));

                let exit_arg = mk().lit_expr(mk().int_lit(0,"i32"));
                let call_exit = mk().call_expr(exit_fn, vec![exit_arg]);

                stmts.push(mk().semi_stmt(call_exit));
            } else {
                let call_main = mk().cast_expr(
                    mk().call_expr(main_fn, main_args),
                    mk().path_ty(vec!["i32"]),
                );

                let call_exit = mk().call_expr(exit_fn, vec![call_main]);
                let unsafe_block = mk().unsafe_().block(vec![mk().expr_stmt(call_exit)]);

                stmts.push(mk().expr_stmt(mk().block_expr(unsafe_block)));
            };

            let block = mk().block(stmts);
            Ok(mk().fn_item("main", decl, block))
        } else {
            Err(format!("Cannot translate non-function main entry point"))
        }
    }

    fn convert_decl(&self, toplevel: bool, decl_id: CDeclId) -> Result<P<Item>, String> {
        match self.ast_context.c_decls.get(&decl_id)
            .ok_or_else(|| format!("Missing decl {:?}", decl_id))?
            .kind {
            CDeclKind::Struct { ref fields, .. } => {
                let name = self.type_converter.borrow_mut().resolve_decl_name(decl_id).unwrap();

                // Gather up all the field names and field types
                let mut field_entries = vec![];
                for &x in fields {
                    match &self.ast_context.index(x).kind {
                        &CDeclKind::Field { ref name, typ } => {
                            let name = self.type_converter.borrow_mut().declare_field_name(decl_id, x, name);
                            let typ = self.convert_type(typ.ctype)?;
                            field_entries.push(mk().pub_().struct_field(name, typ))
                        }
                        _ => return Err(format!("Found non-field in record field list")),
                    }
                }

                Ok(self.mk_cross_check(mk().pub_(), vec!["none"])
                    .call_attr("derive", vec!["Copy", "Clone"])
                    .call_attr("repr", vec!["C"])
                    .struct_item(name, field_entries))
            }

            CDeclKind::Union { ref fields, .. } => {
                let name = self.type_converter.borrow_mut().resolve_decl_name(decl_id).unwrap();

                let mut field_syns = vec![];
                for &x in fields {
                    let field_decl = self.ast_context.index(x);
                    match &field_decl.kind {
                        &CDeclKind::Field { ref name, typ } => {
                            let name = self.type_converter.borrow_mut().declare_field_name(decl_id, x, name);
                            let typ = self.convert_type(typ.ctype)?;
                            field_syns.push(mk().struct_field(name, typ))
                        }
                        _ => return Err(format!("Found non-field in record field list")),
                    }
                }

                if field_syns.is_empty() {
                    // Empty unions are a GNU extension, but Rust doesn't allow empty unions.
                    Ok(self.mk_cross_check(mk().pub_(), vec!["none"])
                        .call_attr("derive", vec!["Copy", "Clone"])
                        .call_attr("repr", vec!["C"])
                        .struct_item(name, vec![]))
                } else {
                    Ok(self.mk_cross_check(mk().pub_(), vec!["none"])
                        .call_attr("derive", vec!["Copy", "Clone"])
                        .call_attr("repr", vec!["C"])
                        .union_item(name, field_syns))
                }
            }

            CDeclKind::Field { .. } => Err(format!("Field declarations should be handled inside structs/unions")),

            CDeclKind::Enum { ref variants, .. } => {
                let enum_name = &self.type_converter.borrow().resolve_decl_name(decl_id).expect("Enums should already be renamed");

                let mut variant_syns = vec![];

                // C allows multiple variants to share the same representation while Rust does not.
                // We track which values have been used in order to avoid generating invalid Rust.
                let mut value_map = HashMap::<i64, CDeclId>::new();

                for &v in variants {
                    let enum_constant_decl = self.ast_context.index(v);
                    match &enum_constant_decl.kind {
                        &CDeclKind::EnumConstant { ref name, value } => {

                            if value_map.contains_key(&value) {
                                self.renamer.borrow_mut().alias(v, &value_map[&value]);
                            } else {
                                let disc = signed_int_expr(value);
                                let variant = &self.renamer.borrow_mut()
                                    .insert(v, &format!("{}::{}", enum_name, name))
                                    .expect(&format!("Failed to insert enum variant '{}'", name));
                                let variant = variant.trim_left_matches(&format!("{}::", enum_name));
                                value_map.insert(value, v);
                                variant_syns.push(mk().unit_variant(variant, Some(disc)));
                            }
                        }
                        _ => return Err(format!("Found non-variant in enum variant list")),
                    }
                }

                Ok(self.mk_cross_check(mk().pub_(), vec!["none"])
                    .call_attr("derive", vec!["Copy", "Clone"])
                    .call_attr("repr", vec!["C"])
                    .enum_item(enum_name, variant_syns))
            },

            CDeclKind::EnumConstant { .. } => Err(format!("Enum variants should be handled inside enums")),

            CDeclKind::Function { .. } if !toplevel => Err(format!("Function declarations must be top-level")),
            CDeclKind::Function { is_extern, is_inline, typ, ref name, ref parameters, body, .. } => {
                let new_name = &self.renamer.borrow().get(&decl_id).expect("Functions should already be renamed");


                let (ret, is_var): (CQualTypeId, bool) = match &self.ast_context.resolve_type(typ).kind {
                    &CTypeKind::Function(ret, _, is_var) => (ret, is_var),
                    k => return Err(format!("Type of function {:?} was not a function type, got {:?}", decl_id, k))
                };

                let mut args: Vec<(CDeclId, String, CQualTypeId)> = vec![];
                for param_id in parameters {
                    if let CDeclKind::Variable { ref ident, typ, .. } = self.ast_context.index(*param_id).kind {
                        args.push((*param_id, ident.clone(), typ))
                    } else {
                        return Err(format!("Parameter is not variable declaration"))
                    }
                }

                let is_main = self.ast_context.c_main == Some(decl_id);

                self.convert_function(is_extern, is_inline, is_main, is_var, new_name, name, &args, ret, body)
            },

            CDeclKind::Typedef { ref typ, .. } => {
                let new_name = &self.type_converter.borrow_mut().resolve_decl_name(decl_id).unwrap();

                let ty = self.convert_type(typ.ctype)?;
                Ok(mk().pub_().type_item(new_name, ty))
            },

            // Extern variable without intializer (definition elsewhere)
            CDeclKind::Variable { is_extern: true, is_static, is_defn: false, ref ident, initializer, typ } => {
                assert!(is_static, "An extern variable must be static");
                assert!(initializer.is_none(), "An extern variable that isn't a definition can't have an initializer");

                let new_name = &self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");
                let (ty, mutbl, _) = self.convert_variable(None, typ, is_static)?;

                let extern_item = mk_linkage(true, new_name, ident)
                    .set_mutbl(mutbl)
                    .foreign_static(new_name, ty);

                Ok(mk().abi(Abi::C)
                    .foreign_items(vec![extern_item]))
            }

            // Extern variable with initializer (definition here)
            CDeclKind::Variable { is_extern: true, is_static, ref ident, initializer, typ, .. } => {
                assert!(is_static, "An extern variable must be static");

                let new_name = &self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");
                let (ty, _, init) = self.convert_variable(initializer, typ, is_static)?;

                let init = init?.to_expr();

                // Force mutability due to the potential for raw pointers occuring in the type

                Ok(mk_linkage(false, new_name, ident)
                    .vis(Visibility::Public)
                    .abi(Abi::C)
                    .mutbl()
                    .static_item(new_name, ty, init))
            }

            // Static variable (definition here)
            CDeclKind::Variable { is_static: true, initializer, typ, .. } => {
                let new_name = &self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");
                let (ty, _, init) = self.convert_variable(initializer, typ, true)?;

                let init = init?.to_expr();

                // Force mutability due to the potential for raw pointers occurring in the type
                Ok(mk().mutbl()
                    .static_item(new_name, ty, init))
            }

            CDeclKind::Variable { .. } => Err(format!("This should be handled in 'convert_decl_stmt'")),

            //ref k => Err(format!("Translation not implemented for {:?}", k)),
        }
    }

    fn convert_function(
        &self,
        is_extern: bool,
        is_inline: bool,
        is_main: bool,
        is_variadic: bool,
        new_name: &str,
        name: &str,
        arguments: &[(CDeclId, String, CQualTypeId)],
        return_type: CQualTypeId,
        body: Option<CStmtId>,
    ) -> Result<P<Item>, String> {
        self.with_scope(|| {
            let mut args: Vec<Arg> = vec![];

            for &(decl_id, ref var, typ) in arguments {


                let (ty, mutbl, _) = self.convert_variable(None, typ, false)?;

                let pat = if var.is_empty() {
                    mk().wild_pat()
                } else {
                    // extern function declarations don't support/require mut patterns
                    let mutbl = if body.is_none() { Mutability::Immutable } else { mutbl };

                    let new_var = self.renamer.borrow_mut()
                        .insert(decl_id, var.as_str())
                        .expect(&format!("Failed to insert argument '{}' while converting '{}'", var, name));

                    mk().set_mutbl(mutbl).ident_pat(new_var)
                };

                args.push(mk().arg(ty, pat))
            }

            let ret = FunctionRetTy::Ty(self.convert_type(return_type.ctype)?);

            let decl = mk().fn_decl(args, ret, is_variadic);


            if let Some(body) = body {
                // Translating an actual function

                let ret_type_id: CTypeId = self.ast_context.resolve_type_id(return_type.ctype);
                let ret = match self.ast_context.index(ret_type_id).kind {
                    CTypeKind::Void => cfg::ImplicitReturnType::Void,
                    _ if is_main => cfg::ImplicitReturnType::Main,
                    _ => cfg::ImplicitReturnType::NoImplicitReturnType,
                };

                let mut body_stmts = vec![];
                for &(_, _, typ) in arguments {
                    body_stmts.append(&mut self.compute_variable_array_sizes(typ.ctype)?);
                }
                body_stmts.append(&mut self.convert_function_body(name, body, ret)?);
                let block = stmts_block(body_stmts);

                // Only add linkage attributes if the function is `extern`
                let mk_ = if is_main {
                    mk()
                } else if is_extern && !is_inline {
                    mk_linkage(false, new_name, name)
                        .abi(Abi::C)
                        .vis(Visibility::Public)
                } else {
                    mk().abi(Abi::C)
                };

                Ok(mk_.unsafe_().fn_item(new_name, decl, block))
            } else {
                // Translating an extern function declaration

                let function_decl = mk_linkage(true, new_name, name)
                    .foreign_fn(new_name, decl);

                Ok(mk().abi(Abi::C)
                    .foreign_items(vec![function_decl]))
            }
        })
    }

    fn convert_function_body(
        &self,
        name: &str,
        body_id: CStmtId,
        ret: cfg::ImplicitReturnType,
    ) -> Result<Vec<Stmt>, String> {

        // Function body scope
        self.with_scope(|| {
            if self.reloop_cfgs {
                let (graph, store) = cfg::Cfg::from_stmt(self, body_id, ret)?;

                if self.dump_function_cfgs {
                    graph
                        .dump_dot_graph(&self.ast_context, &store, format!("{}_{}.dot", "cfg", name))
                        .expect("Failed to write CFG .dot file");
                }

                let simplify_structures = true;
                let (lifted_stmts, relooped) = cfg::relooper::reloop(graph, store, simplify_structures);

                if self.dump_structures {
                    eprintln!("Relooped structures:");
                    for s in &relooped {
                        eprintln!("  {:#?}", s);
                    }
                }

                let current_block_ident = self.renamer.borrow_mut().pick_name("current_block");
                let current_block = mk().ident_expr(&current_block_ident);
                let mut stmts: Vec<Stmt> = lifted_stmts;
                if cfg::structures::has_multiple(&relooped) {

                    let current_block_ty = if self.debug_relooper_labels {
                        mk().ref_lt_ty("'static", mk().path_ty(vec!["str"]))
                    } else {
                        mk().path_ty(vec!["u64"])
                    };


                    let local = mk().local(mk().mutbl().ident_pat(current_block_ident),
                                           Some(current_block_ty), None as Option<P<Expr>>);
                    stmts.push(mk().local_stmt(P(local)))
                }

                stmts.extend(cfg::structures::structured_cfg(
                    &relooped,
                    current_block,
                    self.debug_relooper_labels
                ));
                Ok(stmts)
            } else {
                match self.ast_context.index(body_id).kind {
                    CStmtKind::Compound(ref stmts) => {
                        let mut res = vec![];
                        for &stmt in stmts {
                            res.append(&mut self.convert_stmt(stmt)?)
                        }
                        Ok(res)
                    }
                    _ => panic!("function body expects to be a compound statement"),
                }
            }
        })
    }

    fn convert_stmt(&self, stmt_id: CStmtId) -> Result<Vec<Stmt>, String> {
        match self.ast_context.index(stmt_id).kind {
            CStmtKind::Empty => Ok(vec![]),

            CStmtKind::Decls(ref decls) => {
                let mut res = vec![];
                for decl in decls {
                    res.append(&mut self.convert_decl_stmt(*decl)?)
                }
                Ok(res)
            },

            CStmtKind::Return(expr) => self.convert_return_stmt(expr),

            CStmtKind::If { scrutinee, true_variant, false_variant } =>
                self.convert_if_stmt(scrutinee, true_variant, false_variant),

            CStmtKind::While { condition, body } => self.convert_while_stmt(condition, body),

            CStmtKind::DoWhile { body, condition } => self.convert_do_stmt(body, condition),

            CStmtKind::ForLoop { init, condition, increment, body } =>
                self.convert_for_stmt(init, condition, increment, body),

            CStmtKind::Compound(ref stmts) => {
                self.with_scope(|| {
                    let mut res = vec![];
                    for stmt in stmts {
                        res.append(&mut self.convert_stmt(*stmt)?)
                    }

                    Ok(vec![mk().expr_stmt(mk().block_expr(stmts_block(res)))])
                })
            },

            CStmtKind::Expr(expr) => Ok(self.convert_expr(ExprUse::Unused, expr, false)?.stmts),

            CStmtKind::Break => {
                let mut loop_ = self.loops.current_loop_mut();
                loop_.has_break = true;
                let loop_label = loop_.get_or_create_label(&self.loops).to_owned();
                Ok(vec![mk().expr_stmt(mk().break_expr(Some(loop_label)))])
            },

            CStmtKind::Continue => {
                let mut loop_ = self.loops.current_loop_mut();
                loop_.has_continue = true;
                match loop_.loop_type {
                    LoopType::While => {
                        // We can translate C continue in a while loop
                        // directly to Rust's continue
                        let loop_label = loop_.get_or_create_label(&self.loops).to_owned();
                        Ok(vec![mk().expr_stmt(mk().continue_expr(Some(loop_label)))])
                    },
                    _ => {
                        // We translate all other C continue statements
                        // to a break from the inner body loop
                        let body_label = loop_.get_or_create_body_label(&self.loops).to_owned();
                        Ok(vec![mk().expr_stmt(mk().break_expr(Some(body_label)))])
                    },
                }
            },

            ref stmt => Err(format!("convert_stmt {:?}", stmt)),
        }
    }

    /// Convert a C expression to a rust boolean expression
    pub fn convert_condition(&self, target: bool, cond_id: CExprId) -> Result<WithStmts<P<Expr>>, String> {
        let ty_id = self.ast_context.index(cond_id).kind.get_type();

        Ok(self.convert_expr(ExprUse::RValue, cond_id, false)?
            .map(|e| self.match_bool(target, ty_id, e)))
    }

    fn convert_while_stmt(&self, cond_id: CExprId, body_id: CStmtId) -> Result<Vec<Stmt>, String> {
        let cond = self.convert_condition(true, cond_id)?;

        self.loops.push_loop(LoopType::While);
        let body_res = self.convert_stmt(body_id);
        let loop_ = self.loops.pop_loop();
        let body = body_res?;

        let rust_cond = cond.to_expr();
        let rust_body = stmts_block(body);

        Ok(vec![mk().expr_stmt(mk().while_expr(rust_cond, rust_body, loop_.label))])
    }

    fn convert_do_stmt(&self, body_id: CStmtId, cond_id: CExprId) -> Result<Vec<Stmt>, String> {
        let cond = self.convert_condition(false, cond_id)?;
        self.loops.push_loop(LoopType::DoWhile);
        let body_res = self.convert_stmt(body_id);
        let mut loop_ = self.loops.pop_loop();
        let mut body = body_res?;

        // Wrap the body in a 'body: loop { ...; break 'body } loop if needed
        let mut body = match loop_.body_label {
            Some(ref l) => {
                assert!(loop_.has_continue, "Expected do/while loop with body label to contain continue statement");
                body.push(mk().semi_stmt(mk().break_expr(Some(l))));
                vec![mk().expr_stmt(mk().loop_expr(stmts_block(body), Some(l)))]
            },
            None => body,
        };

        let rust_cond = cond.to_expr();
        let loop_label = loop_.get_or_create_label(&self.loops).to_owned();
        let break_stmt = mk().semi_stmt(mk().break_expr(Some(loop_label)));

        // if (!cond) { break 'loopN; }
        body.push(mk().expr_stmt(mk().ifte_expr(rust_cond, mk().block(vec![break_stmt]), None as Option<P<Expr>>)));

        let rust_body = stmts_block(body);

        Ok(vec![mk().semi_stmt(mk().loop_expr(rust_body, loop_.label))])
    }

    fn convert_for_stmt(
        &self,
        init_id: Option<CStmtId>,
        cond_id: Option<CExprId>,
        inc_id: Option<CExprId>,
        body_id: CStmtId,
    ) -> Result<Vec<Stmt>, String> {

        // Open new scope for the for loop initializer
        self.with_scope(|| {
            let mut init = match init_id {
                Some(i) => self.convert_stmt(i)?,
                None => vec![],
            };

            let mut inc = match inc_id {
                Some(i) => self.convert_expr(ExprUse::Unused, i, false)?.stmts,
                None => vec![],
            };

            self.loops.push_loop(LoopType::For);
            let body_res = self.convert_stmt(body_id);
            let loop_ = self.loops.pop_loop();
            let mut body = body_res?;

            // Wrap the body in a 'body: loop { ...; break 'body } loop if needed
            let mut body = match loop_.body_label {
                Some(ref l) => {
                    assert!(loop_.has_continue, "Expected for loop with body label to contain continue statement");
                    body.push(mk().semi_stmt(mk().break_expr(Some(l))));
                    vec![mk().expr_stmt(mk().loop_expr(stmts_block(body), Some(l)))]
                },
                None => body,
            };
            body.append(&mut inc);

            let body_block = stmts_block(body);

            let looper = match cond_id {
                None => mk().loop_expr(body_block, loop_.label), // loop
                Some(i) => mk().while_expr(self.convert_condition(true, i)?.to_expr(), body_block, loop_.label), // while
            };

            init.push(mk().expr_stmt(looper));

            Ok(vec![mk().expr_stmt(mk().block_expr(mk().block(init)))])
        })
    }

    fn convert_if_stmt(
        &self,
        cond_id: CExprId,
        then_id: CStmtId,
        else_id: Option<CStmtId>
    ) -> Result<Vec<Stmt>, String> {
        let mut cond = self.convert_condition(true, cond_id)?;
        let then_stmts = stmts_block(self.convert_stmt(then_id)?);
        let else_stmts = else_id
            .map_or(
                Ok(None),
                |x| {
                    let stmt = self.convert_stmt(x);
                    stmt.map(|s| Some(mk().block_expr(stmts_block(s))))
                })?;

        cond.stmts.push(mk().semi_stmt(mk().ifte_expr(cond.val, then_stmts, else_stmts)));
        Ok(cond.stmts)
    }

    fn convert_return_stmt(&self, result_id: Option<CExprId>) -> Result<Vec<Stmt>, String> {
        let val: Option<WithStmts<P<Expr>>> =
            sequence_option(result_id
                .map(|i| self.convert_expr(ExprUse::RValue, i, false))
            )?;
        let mut ws = with_stmts_opt(val);
        let ret = mk().expr_stmt(mk().return_expr(ws.val));

        ws.stmts.push(ret);
        Ok(ws.stmts)
    }

    pub fn convert_decl_stmt(&self, decl_id: CDeclId) -> Result<Vec<Stmt>, String> {

        match self.convert_decl_stmt_info(decl_id)? {
            cfg::DeclStmtInfo { pre_init: Some(i), decl_and_assign: Some(d), .. } => {
                let mut ret: Vec<Stmt> = vec![];
                ret.extend(i);
                ret.extend(d);
                Ok(ret)
            }
            _ => Err(format!("convert_decl_stmt: couldn't get declaration and initialization info"))
        }
    }

    pub fn convert_decl_stmt_info(&self, decl_id: CDeclId) -> Result<cfg::DeclStmtInfo, String> {
        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable { is_static, is_extern, is_defn, ref ident, initializer, typ } if !is_static && !is_extern => {
                assert!(is_defn, "Only local variable definitions should be extracted");

                let mut stmts = self.compute_variable_array_sizes(typ.ctype)?;

                let rust_name = self.renamer.borrow_mut()
                    .insert(decl_id, &ident)
                    .expect(&format!("Failed to insert variable '{}'", ident));
                let (ty, mutbl, init) = self.convert_variable(initializer, typ, is_static)?;
                let mut init = init?;

                stmts.append(&mut init.stmts);

                let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name.clone());
                let zeroed = self.implicit_default_expr(typ.ctype)?;
                let local_mut = mk().local(pat_mut, Some(ty.clone()), Some(zeroed));

                let pat = mk().set_mutbl(mutbl).ident_pat(rust_name.clone());

                let local = mk().local(pat, Some(ty), Some(init.val.clone()));
                let assign = mk().assign_expr(mk().ident_expr(rust_name), init.val);

                Ok(cfg::DeclStmtInfo::new(
                    vec![mk().local_stmt(P(local_mut))],
                    vec![mk().semi_stmt(assign)],
                    vec![mk().local_stmt(P(local))],
                    stmts,
                ))
            }

            ref decl => {
                let inserted = if let Some(ident) = decl.get_name() {
                    self.renamer.borrow_mut()
                        .insert(decl_id, &ident)
                        .is_some()
                } else {
                    false
                };

                // TODO: We need this because we can have multiple 'extern' decls of the same variable.
                //       When we do, we must make sure to insert into the renamer the first time, and
                //       then skip subsequent times.
                let skip = match decl {
                    &CDeclKind::Variable { .. } => !inserted,
                    &CDeclKind::Struct { .. } => true,
                    &CDeclKind::Union { .. } => true,
                    &CDeclKind::Enum { .. } => true,
                    &CDeclKind::Typedef { .. } => true,
                    _ => false,
                };

                if skip {
                    Ok(cfg::DeclStmtInfo::new(
                        vec![],
                        vec![],
                        vec![],
                        vec![],
                    ))
                } else {
                    let item = self.convert_decl(false, decl_id)?;
                    Ok(cfg::DeclStmtInfo::new(
                        vec![mk().item_stmt(item.clone())],
                        vec![],
                        vec![mk().item_stmt(item)],
                        vec![],
                    ))
                }
            },
        }
    }

    fn convert_variable(
        &self,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
        is_static: bool,
    ) -> Result<(P<Ty>, Mutability, Result<WithStmts<P<Expr>>,String>), String> {
        let init = match initializer {
            Some(x) => self.convert_expr(ExprUse::RValue, x, is_static),
            None => self.implicit_default_expr(typ.ctype).map(WithStmts::new),
        };

        let ty = self.convert_type(typ.ctype)?;
        let mutbl = if typ.qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };

        Ok((ty, mutbl, init))
    }

    fn convert_type(&self, type_id: CTypeId) -> Result<P<Ty>, String> {
        self.type_converter.borrow_mut().convert(&self.ast_context, type_id)
    }

    fn null_ptr(&self, type_id: CTypeId) -> Result<P<Expr>, String> {
        let rust_ty = self.convert_type(type_id)?;

        match rust_ty.node {
            TyKind::Ptr(MutTy { ref ty, mutbl }) => match mutbl {
                Mutability::Mutable => Ok(null_mut_expr(ty.clone())),
                Mutability::Immutable => Ok(null_expr(ty.clone())),
            }
            ref k => Err(format!("Cannot make a null expression for a non-pointer type {:?}, {:?}", type_id, k))
        }
    }

    /// Write to a `lhs` that is volatile
    pub fn volatile_write(&self, lhs: &P<Expr>, lhs_type: CQualTypeId, rhs: P<Expr>) -> Result<P<Expr>, String> {
        let addr_lhs = match lhs.node {
            ExprKind::Unary(ast::UnOp::Deref, ref e) => {
                if lhs_type.qualifiers.is_const {
                    let lhs_type = self.convert_type(lhs_type.ctype)?;
                    let ty = mk().mutbl().ptr_ty(lhs_type);

                    mk().cast_expr(e, ty)
                } else {
                    e.clone()
                }
            }
            _ => {
                let addr_lhs = mk().mutbl().addr_of_expr(lhs);

                let lhs_type = self.convert_type(lhs_type.ctype)?;
                let ty = mk().mutbl().ptr_ty(lhs_type);

                mk().cast_expr(addr_lhs, ty)
            },
        };

        Ok(mk().call_expr(mk().path_expr(vec!["", "std", "ptr", "write_volatile"]), vec![addr_lhs, rhs]))
    }

    /// Read from a `lhs` that is volatile
    pub fn volatile_read(&self, lhs: &P<Expr>, lhs_type: CQualTypeId) -> Result<P<Expr>, String> {
        let addr_lhs = match lhs.node {
            ExprKind::Unary(ast::UnOp::Deref, ref e) => {
                if !lhs_type.qualifiers.is_const {
                    let lhs_type = self.convert_type(lhs_type.ctype)?;
                    let ty = mk().ptr_ty(lhs_type);

                    mk().cast_expr(e, ty)
                } else {
                    e.clone()
                }
            }
            _ => {
                let addr_lhs = mk().addr_of_expr(lhs);

                let lhs_type = self.convert_type(lhs_type.ctype)?;
                let ty = mk().ptr_ty(lhs_type);

                mk().cast_expr(addr_lhs, ty)
            }
        };

        Ok(mk().call_expr(mk().path_expr(vec!["", "std", "ptr", "read_volatile"]), vec![addr_lhs]))
    }

    /// If the referenced expression is a DeclRef inside an Unary or ImplicitCast node, return
    /// the type of the referenced declaration. Returns `Err` in all other cases. See
    /// See https://github.com/GaloisInc/C2Rust/issues/32 for more details on this quirk.
    fn get_declref_type(&self, expr_id: CExprId) -> Result<(Option<Qualifiers>, CTypeId), &str> {
        // Using nested function to avoid exposing the level parameter
        fn _get_declref_type(ast_context: &TypedAstContext, expr_id: CExprId, level: u32) -> Result<(Option<Qualifiers>, CTypeId), &str> {
            let expr : &CExpr = ast_context.index(expr_id);
            return match expr.kind {
                // level 0 arms
                CExprKind::Unary(_type_id, c_ast::UnOp::AddressOf, inner_expr_id) if level == 0  => {
                    _get_declref_type(ast_context, inner_expr_id, level + 1)
                }
                CExprKind::ImplicitCast(_type_id, inner_expr_id, CastKind::FunctionToPointerDecay, _field_id) if level == 0 => {
                    _get_declref_type(ast_context, inner_expr_id, level + 1)
                }
                // level 1 arms
                CExprKind::DeclRef(_type_id, decl_id) if level == 1 => {
                    let cdecl : &CDecl = ast_context.index(decl_id);
                    match cdecl.kind {
                        CDeclKind::Function { typ, .. } => Ok((None, typ)),
                        CDeclKind::Variable { typ, ..} => {
                            Ok((Some(typ.qualifiers), typ.ctype))
                        }
                        _ => Err("couldn't get leaf node type")
                    }
                }
                _ => Err("couldn't get leaf node type")
            }
        }

        _get_declref_type(&self.ast_context, expr_id, 0)
    }

    // Compute the offset multiplier for variable length array indexing
    // Rust type: usize
    pub fn compute_size_of_expr(&self, type_id: CTypeId) -> Option<P<Expr>> {
        match &self.ast_context.resolve_type(type_id).kind {
            &CTypeKind::VariableArray(elts, Some(counts)) => {
                let opt_esize = self.compute_size_of_expr(elts);
                let csize_name = self.renamer.borrow().get(&CDeclId(counts.0))
                                .expect("Failed to lookup VLA expression");
                let csize = mk().path_expr(vec![csize_name]);

                let val = match opt_esize {
                    None => csize,
                    Some(esize) => mk().binary_expr(BinOpKind::Mul, csize, esize),
                };
                Some(val)
            }
            _ => None,
        }
    }

    /// This generates variables that store the computed sizes of the variable-length arrays in
    /// the given type.
    pub fn compute_variable_array_sizes(&self, mut type_id: CTypeId) -> Result<Vec<Stmt>, String> {

        let mut stmts = vec![];

        loop {
            match &self.ast_context.resolve_type(type_id).kind {
                &CTypeKind::Pointer(elt) => type_id = elt.ctype,
                &CTypeKind::ConstantArray(elt, _) => type_id = elt,
                &CTypeKind::VariableArray(elt, Some(expr_id)) => {
                    type_id = elt;

                    // Convert this expression
                    let mut expr = self.convert_expr(ExprUse::RValue, expr_id, false)?;
                    stmts.append(&mut expr.stmts);
                    let name = self.renamer.borrow_mut().insert(CDeclId(expr_id.0), "vla").unwrap(); // try using declref name?
                    // TODO: store the name corresponding to expr_id

                    let local = mk().local(mk().ident_pat(name), None as Option<P<Ty>>, Some(mk().cast_expr(expr.val, mk().path_ty(vec!["usize"]))));

                    stmts.push(mk().local_stmt(P(local)));
                }
                _ => break,
            }
        }

        Ok(stmts)
    }

    // Compute the size of a type
    // Rust type: usize
    pub fn compute_size_of_type(&self, type_id: CTypeId) -> Result<WithStmts<P<Expr>>, String> {
        if let &CTypeKind::VariableArray(elts, len) =
            &self.ast_context.resolve_type(type_id).kind {

            let len = len.expect("Sizeof a VLA type with count expression omitted");

            let mut elts = self.compute_size_of_type(elts)?;
            let mut len = self.convert_expr(ExprUse::RValue, len, false)?;

            let mut stmts = elts.stmts;
            stmts.append(&mut len.stmts);

            let lhs = elts.val;
            let rhs = cast_int(len.val, "usize");

            let val = mk().binary_expr(BinOpKind::Mul, lhs, rhs);

            Ok(WithStmts { stmts, val })

        } else {
            let ty = self.convert_type(type_id)?;
            let name = "size_of";
            let params = mk().angle_bracketed_param_types(vec![ty]);
            let path = vec![mk().path_segment(""),
                            mk().path_segment("std"),
                            mk().path_segment("mem"),
                            mk().path_segment_with_params(name, params)];
            let call = mk().call_expr(mk().path_expr(path), vec![] as Vec<P<Expr>>);
            Ok(WithStmts::new(call))
        }
    }

    pub fn compute_align_of_type(&self, mut type_id: CTypeId)
        -> Result<WithStmts<P<Expr>>, String> {

        while let CTypeKind::VariableArray(elts, _) =
            self.ast_context.resolve_type(type_id).kind {
            type_id = elts;
        }

        let ty = self.convert_type(type_id)?;

        let name = "align_of";
        let tys = vec![ty];
        let path = vec![mk().path_segment(""),
                        mk().path_segment("std"),
                        mk().path_segment("mem"),
                        mk().path_segment_with_params(name,
                                                      mk().angle_bracketed_param_types(tys)),
        ];
        let call = mk().call_expr(mk().path_expr(path), vec![] as Vec<P<Expr>>);
        Ok(WithStmts::new(call))
    }

    /// Translate a C expression into a Rust one, possibly collecting side-effecting statements
    /// to run before the expression.
    ///
    /// The `use_` argument informs us how the C expression we are translating is used in the C
    /// program. See `ExprUse` for more information.
    ///
    /// In the case that `use_` is `ExprUse::Unused`, all side-effecting components will be in the
    /// `stmts` field of the output and it is expected that the `val` field of the output will be
    /// ignored.
    pub fn convert_expr(&self, use_: ExprUse, expr_id: CExprId, is_static: bool) -> Result<WithStmts<P<Expr>>, String> {
        match self.ast_context.index(expr_id).kind {
            CExprKind::UnaryType(_ty, kind, opt_expr, arg_ty) => {
                let result = match kind {
                    UnTypeOp::SizeOf =>
                        match opt_expr {
                            None => self.compute_size_of_type(arg_ty.ctype)?,
                            Some(_) =>
                                {
                                    let mut inner = arg_ty.ctype;
                                    while let &CTypeKind::VariableArray(elt_, _)
                                    = &self.ast_context.resolve_type(inner).kind {
                                        inner = elt_;
                                    }

                                    let inner_size = self.compute_size_of_type(inner)?;

                                    if let Some(sz) = self.compute_size_of_expr(arg_ty.ctype) {
                                        inner_size.map(|x| mk().binary_expr(BinOpKind::Mul, sz, x))
                                    } else {
                                        // Otherwise, use the pointer and make a deref of a pointer offset expression
                                        inner_size
                                    }
                                },
                        }
                    UnTypeOp::AlignOf => self.compute_align_of_type(arg_ty.ctype)?,
                };

                Ok(result.map(|x| mk().cast_expr(x, mk().path_ty(vec!["libc","c_ulong"]))))
            }

            CExprKind::DeclRef(qual_ty, decl_id) => {
                let decl =
                    &self.ast_context.c_decls
                        .get(&decl_id)
                        .ok_or_else(|| format!("Missing declref {:?}", decl_id))?
                        .kind;
                let varname = decl.get_name().expect("expected variable name").to_owned();
                let rustname = self.renamer.borrow_mut()
                    .get(&decl_id)
                    .ok_or_else(|| format!("name not declared: '{}'", varname))?;

                let mut val = mk().path_expr(vec![rustname]);

                // If the variable is volatile and used as something that isn't an LValue, this
                // constitutes a volatile read.
                if use_ != ExprUse::LValue && qual_ty.qualifiers.is_volatile {
                    val = self.volatile_read(&val, qual_ty)?;
                }

                // If the variable is actually an `EnumConstant`, we need to add a cast to the
                // expected integral type. When modifying this, look at `Translation::enum_cast` -
                // this function assumes `DeclRef`'s to `EnumConstants`'s will translate to casts.
                if let &CDeclKind::EnumConstant { .. } = decl {
                    let ty = self.convert_type(qual_ty.ctype)?;
                    val = mk().cast_expr(val, ty);
                }

                Ok(WithStmts::new(val))
            }

            CExprKind::Literal(ty, CLiteral::Integer(val)) => {
                let intty = match &self.ast_context.resolve_type(ty.ctype).kind {
                    &CTypeKind::Int => LitIntType::Signed(IntTy::I32),
                    &CTypeKind::Long => LitIntType::Signed(IntTy::I64),
                    &CTypeKind::LongLong => LitIntType::Signed(IntTy::I64),
                    &CTypeKind::UInt => LitIntType::Unsigned(UintTy::U32),
                    &CTypeKind::ULong => LitIntType::Unsigned(UintTy::U64),
                    &CTypeKind::ULongLong => LitIntType::Unsigned(UintTy::U64),
                    _ => LitIntType::Unsuffixed,
                };
                Ok(WithStmts::new(mk().lit_expr(mk().int_lit(val.into(), intty))))
            }

            CExprKind::Literal(_, CLiteral::Character(val)) => {
                Ok(WithStmts::new(mk().lit_expr(mk().int_lit(val.into(), LitIntType::Unsuffixed))))
            }

            CExprKind::Literal(ty, CLiteral::Floating(val)) => {
                let mut bytes: Vec<u8> = vec![];
                dtoa::write(&mut bytes, val).unwrap();
                let str = String::from_utf8(bytes).unwrap();
                let float_ty = match &self.ast_context.resolve_type(ty.ctype).kind {
                    &CTypeKind::LongDouble => FloatTy::F64,
                    &CTypeKind::Double => FloatTy::F64,
                    &CTypeKind::Float => FloatTy::F32,
                    k => panic!("Unsupported floating point literal type {:?}", k),
                };
                Ok(WithStmts::new(mk().lit_expr(mk().float_lit(str, float_ty))))
            }

            CExprKind::Literal(ty, CLiteral::String(ref val, width)) => {
                let mut val = val.to_owned();

                match &self.ast_context.resolve_type(ty.ctype).kind {
                    // Match the literal size to the expected size padding with zeros as needed
                    &CTypeKind::ConstantArray(_, size) => val.resize(size*(width as usize),0),

                    // Add zero terminator
                    _ => for _ in 0..width { val.push(0); },
                };
                if is_static {
                    let mut vals: Vec<P<Expr>> = vec![];
                    for c in val {
                        vals.push(mk().lit_expr(mk().int_lit(c as u128, LitIntType::Unsuffixed)));
                    }
                    let array = mk().array_expr(vals);
                    Ok(WithStmts::new(array))
                } else {
                    let u8_ty = mk().path_ty(vec!["u8"]);
                    let width_lit = mk().lit_expr(mk().int_lit(val.len() as u128, LitIntType::Unsuffixed));
                    let array_ty = mk().array_ty(u8_ty, width_lit);
                    let source_ty = mk().ref_ty(array_ty);
                    let mutbl = if ty.qualifiers.is_const {
                        Mutability::Immutable
                    } else { Mutability::Mutable };
                    let target_ty = mk().set_mutbl(mutbl).ref_ty(self.convert_type(ty.ctype)?);
                    let byte_literal = mk().lit_expr(mk().bytestr_lit(val));
                    let pointer = transmute_expr(source_ty, target_ty, byte_literal);
                    let array = mk().unary_expr(ast::UnOp::Deref, pointer);
                    Ok(WithStmts::new(array))
                }
            }

            CExprKind::ImplicitCast(ty, expr, kind, opt_field_id) =>
                self.convert_cast(use_, ty, expr, kind, opt_field_id, false),

            CExprKind::ExplicitCast(ty, expr, kind, opt_field_id) =>
                self.convert_cast(use_, ty, expr, kind, opt_field_id, true),

            CExprKind::Unary(type_id, op, arg) =>
                self.convert_unary_operator(use_, op, type_id, arg),

            CExprKind::Conditional(_, cond, lhs, rhs) => {
                let cond = self.convert_condition(true, cond)?;

                let lhs = self.convert_expr(use_, lhs, false)?;
                let rhs = self.convert_expr(use_, rhs, false)?;

                if use_ == ExprUse::Unused {
                    let then: P<Block> = mk().block(lhs.stmts);
                    let els: P<Expr> = mk().block_expr(mk().block(rhs.stmts));

                    Ok(cond.and_then(|c| WithStmts {
                        stmts: vec![mk().semi_stmt(mk().ifte_expr(c, then, Some(els)))],
                        val: Translation::panic(),
                    }))
                } else {
                    let then: P<Block> = lhs.to_block();
                    let els: P<Expr> = rhs.to_expr();

                    Ok(cond.map(|c| mk().ifte_expr(c, then, Some(els))))
                }
            },

            CExprKind::BinaryConditional(ty, lhs, rhs) => {
                if use_ == ExprUse::Unused {
                    let mut lhs = self.convert_expr(ExprUse::RValue, lhs, false)?;
                    let cond = self.match_bool(false, ty.ctype, lhs.val);

                    lhs.stmts.push(
                        mk().semi_stmt(
                            mk().ifte_expr(cond, mk().block(self.convert_expr(ExprUse::Unused,
                                                                              rhs, false)?.stmts), None as Option<P<Expr>>)));
                    Ok(WithStmts { stmts: lhs.stmts, val: Translation::panic(), })
                } else {
                    self.name_reference_write_read(lhs)?.result_map(|(_, lhs_val)| {
                        let cond = self.match_bool(true, ty.ctype, lhs_val.clone());
                        let ite = mk().ifte_expr(cond,
                                       mk().block(vec![mk().expr_stmt(lhs_val)]),
                                       Some(self.convert_expr(use_, rhs, false)?.to_expr()));
                        Ok(ite)
                    })
                }
            },

            CExprKind::Binary(type_id, ref op, lhs, rhs, opt_lhs_type_id, opt_res_type_id) => {
                match *op {
                    c_ast::BinOp::Comma => {

                        // The value of the LHS of a comma expression is always discarded
                        let lhs = self.convert_expr(ExprUse::Unused, lhs, false)?;
                        let rhs = self.convert_expr(use_, rhs, false)?;

                        Ok(WithStmts {
                            stmts: lhs.stmts.into_iter().chain(rhs.stmts).collect(),
                            val: rhs.val,
                        })
                    }

                    c_ast::BinOp::And => {
                        // XXX: do we need the RHS to always be used?
                        let lhs_ty = self.ast_context.index(lhs).kind.get_type();
                        let rhs_ty = self.ast_context.index(rhs).kind.get_type();

                        let lhs =
                            self.convert_expr(ExprUse::RValue, lhs, false)?
                                .map(|x| self.match_bool(true, lhs_ty, x));
                        let rhs =
                            self.convert_expr(ExprUse::RValue, rhs, false)?
                                .map(|x| self.match_bool(true, rhs_ty, x));

                        Ok(lhs.map(|x| bool_to_int(mk().binary_expr(BinOpKind::And, x, rhs.to_expr()))))
                    }

                    c_ast::BinOp::Or => {
                        // XXX: do we need the RHS to always be used?
                        let lhs_ty = self.ast_context.index(lhs).kind.get_type();
                        let rhs_ty = self.ast_context.index(rhs).kind.get_type();

                        let lhs =
                            self.convert_expr(ExprUse::RValue, lhs, false)?
                                .map(|x| self.match_bool(true, lhs_ty, x));
                        let rhs =
                            self.convert_expr(ExprUse::RValue, rhs, false)?
                                .map(|x| self.match_bool(true, rhs_ty, x));

                        Ok(lhs.map(|x| bool_to_int(mk().binary_expr(BinOpKind::Or, x, rhs.to_expr()))))
                    }

                    // No sequence-point cases

                    c_ast::BinOp::AssignAdd |
                    c_ast::BinOp::AssignSubtract |
                    c_ast::BinOp::AssignMultiply |
                    c_ast::BinOp::AssignDivide |
                    c_ast::BinOp::AssignModulus |
                    c_ast::BinOp::AssignBitXor |
                    c_ast::BinOp::AssignShiftLeft |
                    c_ast::BinOp::AssignShiftRight |
                    c_ast::BinOp::AssignBitOr |
                    c_ast::BinOp::AssignBitAnd |
                    c_ast::BinOp::Assign => {
                        self.convert_assignment_operator(use_, *op, type_id, lhs, rhs, opt_lhs_type_id, opt_res_type_id)
                    },

                    _ => {
                        let ty = self.convert_type(type_id.ctype)?;

                        let lhs_type = self.ast_context.index(lhs).kind.get_qual_type();
                        let rhs_type = self.ast_context.index(rhs).kind.get_qual_type();

                        let WithStmts { val: lhs, stmts: lhs_stmts } = self.convert_expr(ExprUse::RValue, lhs, false)?;
                        let WithStmts { val: rhs, stmts: rhs_stmts } = self.convert_expr(ExprUse::RValue, rhs, false)?;

                        let mut stmts = vec![];
                        stmts.extend(lhs_stmts);
                        stmts.extend(rhs_stmts);

                        let val = self.convert_binary_operator(*op, ty, type_id.ctype, lhs_type, rhs_type, lhs, rhs);

                        Ok(WithStmts { stmts, val })
                    }
                }
            }

            CExprKind::ArraySubscript(_, ref lhs, ref rhs) => {
                let lhs_node = &self.ast_context.index(*lhs).kind;
                let rhs_node = &self.ast_context.index(*rhs).kind;

                let lhs_is_pointer = self.ast_context.resolve_type(lhs_node.get_type()).kind.is_pointer();

                // From here on in, the LHS is the pointer/array and the RHS the index
                let (lhs, rhs, lhs_node) =
                    if lhs_is_pointer { (lhs, rhs, lhs_node) } else { (rhs, lhs, rhs_node) };

                let mut stmts = vec![];

                let mut rhs = self.convert_expr(ExprUse::RValue, *rhs, false)?;
                stmts.extend(rhs.stmts);

                let simple_index_array =
                match lhs_node {
                    &CExprKind::ImplicitCast(_, arr, CastKind::ArrayToPointerDecay, _) => Some(arr),
                    _ => None,
                };

                let val = if let Some(arr) = simple_index_array {
                    // If the LHS just underwent an implicit cast from array to pointer, bypass that
                    // to make an actual Rust indexing operation

                    let t = self.ast_context[arr].kind.get_type();
                    let var_elt_type_id = match self.ast_context.resolve_type(t).kind {
                        CTypeKind::ConstantArray(..) => None,
                        CTypeKind::IncompleteArray(..) => None,
                        CTypeKind::VariableArray(elt, _) => Some(elt),
                        ref other => panic!("Unexpected array type {:?}", other),
                    };

                    let lhs = self.convert_expr(use_, arr, false)?;
                    stmts.extend(lhs.stmts);

                    // Don't dereference the offset if we're still within the variable portion
                    if let Some(elt_type_id) = var_elt_type_id {
                        match self.compute_size_of_expr(elt_type_id) {
                            None => mk().unary_expr(ast::UnOp::Deref, pointer_offset(lhs.val, rhs.val)),
                            Some(sz) => pointer_offset(lhs.val, mk().binary_expr(BinOpKind::Mul, sz, cast_int(rhs.val, "usize"))),
                        }
                    } else {
                        mk().index_expr(lhs.val, cast_int(rhs.val, "usize"))
                    }
                } else {

                    let lhs = self.convert_expr(ExprUse::RValue, *lhs, false)?;
                    stmts.extend(lhs.stmts);

                    let lhs_type_id = lhs_node.get_type();

                    // Determine the type of element being indexed
                    let pointee_type_id = match &self.ast_context.resolve_type(lhs_type_id).kind {
                        &CTypeKind::Pointer(pointee_id) => pointee_id,
                        _ => panic!("Subscript applied to non-pointer"),
                    };

                    if let Some(sz) = self.compute_size_of_expr(pointee_type_id.ctype) {
                        let offset = mk().binary_expr(BinOpKind::Mul, sz, cast_int(rhs.val, "usize"));
                        pointer_offset(lhs.val, offset)
                    } else {
                        // Otherwise, use the pointer and make a deref of a pointer offset expression
                        mk().unary_expr(ast::UnOp::Deref, pointer_offset(lhs.val, rhs.val))
                    }
                };

                Ok(WithStmts { stmts, val })
            }

            CExprKind::Call(_, func, ref args) => {
                let WithStmts { mut stmts, val: func } = match self.ast_context.index(func).kind {
                    CExprKind::ImplicitCast(_, fexp, CastKind::FunctionToPointerDecay, _) =>
                        self.convert_expr(ExprUse::RValue, fexp, false)?,
                    _ => {
                        self.convert_expr(ExprUse::RValue, func, false)?.map(|x|
                            mk().method_call_expr(x, "unwrap", vec![] as Vec<P<Expr>>))
                    }
                };

                let mut args_new: Vec<P<Expr>> = vec![];
                for arg in args {
                    let WithStmts { stmts: ss, val } = self.convert_expr(ExprUse::RValue, *arg, false)?;
                    stmts.extend(ss);
                    args_new.push(val);
                }

                let call_expr = mk().call_expr(func, args_new);

                if use_ == ExprUse::Unused {
                    // Recall that if `used` is false, the `stmts` field of the output must contain
                    // all side-effects (and a function call can always have side-effects)
                    stmts.push(mk().semi_stmt(call_expr));

                    Ok(WithStmts { stmts, val: Translation::panic() })
                } else {
                    Ok(WithStmts { stmts, val: call_expr })
                }
            }

            CExprKind::Member(_, expr, decl, kind) => {
                let struct_val = self.convert_expr(use_, expr, false)?;
                let field_name = self.type_converter.borrow().resolve_field_name(None, decl).unwrap();

                if use_ == ExprUse::Unused {
                    Ok(struct_val)
                } else {
                    Ok(struct_val.map(|v| {
                        let v = match kind {
                            MemberKind::Arrow => mk().unary_expr(ast::UnOp::Deref, v),
                            MemberKind::Dot => v,
                        };
                        mk().field_expr(v, field_name)
                    }))
                }
            }

            CExprKind::CompoundLiteral(_, val) =>
                self.convert_expr(use_, val, false),

            CExprKind::InitList(ty, ref ids, opt_union_field_id) => {
                let resolved = &self.ast_context.resolve_type(ty.ctype).kind;

                match resolved {
                    &CTypeKind::ConstantArray(ty, n) => {
                        // Convert all of the provided initializer values

                        // Need to check to see if the next item is a string literal,
                        // if it is need to treat it as a declaration, rather than
                        // an init list. https://github.com/GaloisInc/C2Rust/issues/40
                        let mut is_string = false;

                        if ids.len() == 1 {
                            let v = ids.first().unwrap();
                            if let CExprKind::Literal(_, CLiteral::String { .. }) =
                                      self.ast_context.index(*v).kind {
                                is_string = true;
                            }
                        }

                        let mut stmts: Vec<Stmt> = vec![];
                        let val: P<Expr> = if is_string {
                            let v = ids.first().unwrap();
                            let mut x = self.convert_expr(ExprUse::RValue, *v, false)?;
                            stmts.append(&mut x.stmts);
                            x.val
                        } else  {
                            let mut vals: Vec<P<Expr>> = vec![];
                            for v in ids {
                                let mut x = self.convert_expr(ExprUse::RValue, *v, false)?;
                                stmts.append(&mut x.stmts);
                                vals.push(x.val);
                            }
                            // Pad out the array literal with default values to the desired size
                            for _i in ids.len()..n {
                                vals.push(self.implicit_default_expr(ty)?)
                            }
                            mk().array_expr(vals)
                        };

                        Ok(WithStmts {stmts, val })
                    }
                    &CTypeKind::Struct(struct_id) => {
                        self.convert_struct_literal(struct_id, ids.as_ref())
                    }
                    &CTypeKind::Union(union_id) => {
                        self.convert_union_literal(union_id, ids.as_ref(), ty, opt_union_field_id)
                    }
                    &CTypeKind::Pointer(_) => {
                        let id = ids.first().unwrap();
                        let mut x = self.convert_expr(ExprUse::RValue, *id, false);
                        Ok(x.unwrap())
                    }
                    t => {
                        panic!("Init list not implemented for {:?}", t);
                    }
                }
            }
            CExprKind::ImplicitValueInit(ty) =>
                Ok(WithStmts::new(self.implicit_default_expr(ty.ctype)?)),

            CExprKind::Predefined(_, val_id) =>
                self.convert_expr(use_, val_id, false),
        }
    }

    fn convert_cast(
        &self,
        use_: ExprUse,
        ty: CQualTypeId,
        expr: CExprId,
        kind: CastKind,
        opt_field_id: Option<CFieldId>,
        is_explicit: bool)
        -> Result<WithStmts<P<Expr>>, String> {

        let val = if is_explicit {
            let mut stmts = self.compute_variable_array_sizes(ty.ctype)?;
            let mut val = self.convert_expr(use_, expr, false)?;
            stmts.append(&mut val.stmts);
            val.stmts = stmts;
            val
        } else {
            self.convert_expr(use_, expr, false)?
        };

        match kind {
            CastKind::BitCast => {
                val.result_map(|x| {
                    // TODO: Detect cast from mutable to constant pointer to same type

                    // Special cases
                    if let Ok((source_quals, source_ty_id)) = self.get_declref_type(expr) {
                        let source_ty = self.convert_type(source_ty_id)?;
                        let target_ty_kind = &self.ast_context.resolve_type(ty.ctype).kind;
                        if let &CTypeKind::Pointer(qual_type_id) = target_ty_kind {
                            let target_ty = self.convert_type(qual_type_id.ctype)?;

                            // Detect a quirk where the bitcast is superfluous.
                            // See this issue: https://github.com/GaloisInc/C2Rust/issues/32
                            if target_ty == source_ty {
                                // Don't skip the cast if we're going from const to mutable
                                if source_quals.map_or(true, |x| !x.is_const) || ty.qualifiers.is_const {
                                    return Ok(x)
                                }
                            }

                            let quals_agree = if let Some(sq) = source_quals {
                                sq == qual_type_id.qualifiers
                            } else { false };
                            // Detect bitcasts from array-of-T to slice-of-T
                            if let TyKind::Slice(ref tgt_elem_ty) = target_ty.node {
                                if let TyKind::Array(ref src_elem_ty, _) = source_ty.node {
                                    if tgt_elem_ty == src_elem_ty {
                                        if quals_agree {
                                            return Ok(x)
                                        } else {
                                            // FIXME: handle mismatched qualifiers
                                            panic!("Cannot handle mismatched qualifiers yet.")
                                        }
                                    }
                                }
                            }
                        }
                    }

                    let source_ty_id = self.ast_context.index(expr).kind.get_type();

                    if self.is_function_pointer(ty.ctype) || self.is_function_pointer(source_ty_id) {
                        let source_ty = self.convert_type(source_ty_id)?;
                        let target_ty = self.convert_type(ty.ctype)?;
                        Ok(transmute_expr(source_ty, target_ty, x))
                    } else {
                        // Normal case
                        let target_ty = self.convert_type(ty.ctype)?;
                        Ok(mk().cast_expr(x, target_ty))
                    }
                })
            }

            CastKind::IntegralToPointer | CastKind::PointerToIntegral |
            CastKind::IntegralCast | CastKind::FloatingCast | CastKind::FloatingToIntegral |
            CastKind::IntegralToFloating => {
                let target_ty = self.convert_type(ty.ctype)?;
                let target_ty_ctype = &self.ast_context.resolve_type(ty.ctype).kind;

                let source_ty_ctype_id = self.ast_context.index(expr).kind.get_type();

                if let &CTypeKind::Enum(enum_decl_id) = target_ty_ctype {
                    // Casts targeting `enum` types...
                    let source_ty = self.convert_type(source_ty_ctype_id)?;
                    Ok(self.enum_cast(ty.ctype, enum_decl_id, expr, val, source_ty, target_ty))
                } else {
                    // Other numeric casts translate to Rust `as` casts

                    Ok(val.map(|x| mk().cast_expr(x, target_ty)))
                }
            }

            CastKind::LValueToRValue | CastKind::NoOp | CastKind::ToVoid | CastKind::ConstCast => Ok(val),

            CastKind::FunctionToPointerDecay =>
                Ok(val.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x]))),

            CastKind::BuiltinFnToFnPtr =>
                Ok(val.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x]))),

            CastKind::ArrayToPointerDecay => {

                let pointee = match &self.ast_context.resolve_type(ty.ctype).kind {
                    &CTypeKind::Pointer(pointee) => pointee,
                    _ => panic!("Dereferencing a non-pointer"),
                };

                let is_const = pointee.qualifiers.is_const;

                match &self.ast_context.index(expr).kind {
                    &CExprKind::Literal(_,CLiteral::String(ref bytes,1)) if is_const => {
                        let target_ty = self.convert_type(ty.ctype)?;

                        let mut bytes = bytes.to_owned();
                        bytes.push(0);
                        let byte_literal = mk().lit_expr(mk().bytestr_lit(bytes));
                        let val = mk().cast_expr(byte_literal, mk().ptr_ty(mk().path_ty(vec!["u8"])));
                        let val = mk().cast_expr(val, target_ty);
                        Ok(WithStmts { stmts: vec![], val: val, })
                    }
                    _ => {
                        // Variable length arrays are already represented as pointers.
                        let source_ty = self.ast_context[expr].kind.get_type();
                        if let &CTypeKind::VariableArray(..) = &self.ast_context.resolve_type(source_ty).kind {
                            Ok(val)
                        } else {
                            let method = if is_const { "as_ptr" } else { "as_mut_ptr" };
                            Ok(val.map(|x| mk().method_call_expr(x, method, vec![] as Vec<P<Expr>>)))
                        }
                    },
                }

            }

            CastKind::NullToPointer => {
                assert!(val.stmts.is_empty());

                let res = if self.is_function_pointer(ty.ctype) {
                    mk().path_expr(vec!["None"])
                } else {
                    self.null_ptr(ty.ctype)?
                };

                Ok(WithStmts::new(res))
            }

            CastKind::ToUnion => {
                let field_id = opt_field_id.expect("Missing field ID in union cast");
                let union_id = self.ast_context.field_parents[&field_id];

                let union_name = self.type_converter.borrow().resolve_decl_name(union_id).expect("required union name");
                let field_name = self.type_converter.borrow().resolve_field_name(Some(union_id), field_id).expect("field name required");

                Ok(val.map(|x|
                    mk().struct_expr(mk().path(vec![union_name]), vec![mk().field(field_name, x)])
                ))
            },

            CastKind::IntegralToBoolean | CastKind::FloatingToBoolean => {
                let val_ty = self.ast_context.index(expr).kind.get_type();
                Ok(val.map(|x| self.match_bool(true, val_ty, x)))
            }

            // I don't know how to actually cause clang to generate this
            CastKind::BooleanToSignedIntegral =>
                Err(format!("TODO boolean to signed integral not supported")),

            CastKind::FloatingRealToComplex | CastKind::FloatingComplexToIntegralComplex |
            CastKind::FloatingComplexCast | CastKind::FloatingComplexToReal |
            CastKind::IntegralComplexToReal | CastKind::IntegralRealToComplex |
            CastKind::IntegralComplexCast | CastKind::IntegralComplexToFloatingComplex |
            CastKind::IntegralComplexToBoolean =>
                Err(format!("TODO casts with complex numbers not supported")),
        }
    }

    /// Given an integer value this attempts to either generate the corresponding enum
    /// variant directly, otherwise it transmutes a number to the enum type.
    fn enum_for_i64(&self, enum_type_id: CTypeId, value: i64) -> P<Expr> {

        let def_id = match &self.ast_context.resolve_type(enum_type_id).kind {
            &CTypeKind::Enum(def_id) => def_id,
            _ => panic!("{:?} does not point to an `enum` type"),
        };

        let (variants, underlying_type_id) = match &self.ast_context[def_id].kind {
            &CDeclKind::Enum { ref variants, integral_type, .. } => (variants, integral_type),
            _ => panic!("{:?} does not point to an `enum` declaration")
        };

        let underlying_type = self.convert_type(underlying_type_id.ctype).unwrap();

        for &variant_id in variants {
            match &self.ast_context[variant_id].kind {
                &CDeclKind::EnumConstant { value: v, .. } =>
                if value == v {
                    let name = self.renamer.borrow().get(&variant_id).unwrap();
                    return mk().path_expr(vec![name])
                }
                _ => panic!("{:?} does not point to an enum variant", variant_id),
            }
        }

        let value = match self.ast_context.resolve_type(underlying_type_id.ctype).kind {
            CTypeKind::UInt => mk().lit_expr(mk().int_lit((value as u32) as u128, LitIntType::Unsuffixed)),
            CTypeKind::ULong => mk().lit_expr(mk().int_lit((value as u64) as u128, LitIntType::Unsuffixed)),
            _ => signed_int_expr(value),
        };

        let target_ty = self.convert_type(enum_type_id).unwrap();

        transmute_expr(underlying_type, target_ty, value)
    }

    /// This handles translating casts when the target type in an `enum` type.
    ///
    /// When translating variable references to `EnumConstant`'s, we always insert casts to the
    /// expected type. In C, `EnumConstants` have some integral type, _not_ the enum type. However,
    /// if we then immediately have a cast to convert this variable back into an enum type, we would
    /// like to produce Rust with _no_ casts. This function handles this simplification.
    fn enum_cast(
        &self,
        enum_type: CTypeId,
        enum_decl: CEnumId,      // ID of the enum declaration corresponding to the target type
        expr: CExprId,           // ID of initial C argument to cast
        val: WithStmts<P<Expr>>, // translated Rust argument to cast
        source_ty: P<Ty>,        // source type of cast
        target_ty: P<Ty>,        // target type of cast
    ) -> WithStmts<P<Expr>> {

        // Extract the IDs of the `EnumConstant` decls underlying the enum.
        let variants = match &self.ast_context.index(enum_decl).kind {
            &CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration")
        };

        match &self.ast_context.index(expr).kind {
            // This is the case of finding a variable which is an `EnumConstant` of the same enum
            // we are casting to. Here, we can just remove the extraneous cast instead of generating
            // a new one.
            &CExprKind::DeclRef(_, decl_id) if variants.contains(&decl_id) =>
                return val.map(|x| match x.node {
                    ast::ExprKind::Cast(ref e, _) => e.clone(),
                    _ => panic!(format!("DeclRef {:?} of enum {:?} is not cast", expr, enum_decl)),
                }),

            &CExprKind::Literal(_, CLiteral::Integer(i)) => {
                let new_val = self.enum_for_i64(enum_type, i as i64);
                return WithStmts { stmts: val.stmts, val: new_val }
            }

            &CExprKind::Unary(_, c_ast::UnOp::Negate, subexpr_id) => {
                if let &CExprKind::Literal(_, CLiteral::Integer(i)) = &self.ast_context[subexpr_id].kind {
                    let new_val = self.enum_for_i64(enum_type, -(i as i64));
                    return WithStmts { stmts: val.stmts, val: new_val }
                }
            }

            // In all other cases, a cast to an enum requires a `transmute` - Rust enums cannot be
            // converted into integral types as easily as C ones.
            _ => {},
        }

        val.map(|x| transmute_expr(source_ty, target_ty, x))
    }

    fn convert_union_literal(
        &self,
        union_id: CRecordId,
        ids: &[CExprId],
        _ty: CQualTypeId,
        opt_union_field_id: Option<CFieldId>
    ) -> Result<WithStmts<P<Expr>>, String> {
        let union_field_id = opt_union_field_id.expect("union field ID");

        match &self.ast_context.index(union_id).kind {
            &CDeclKind::Union { name: ref opt_union_name, .. } => {
                let union_name = opt_union_name.as_ref().expect("Anonymous unions not implemented");
                match &self.ast_context.index(union_field_id).kind {
                    &CDeclKind::Field { typ: field_ty, .. } => {
                        let val = if ids.is_empty() {
                            WithStmts {
                                stmts: vec![],
                                val: self.implicit_default_expr(field_ty.ctype)?,
                            }
                        } else {
                            self.convert_expr(ExprUse::RValue, ids[0], false)?
                        };

                        Ok(val.map(|v| {
                            let name = vec![mk().path_segment(union_name)];
                            let field_name = self.type_converter.borrow().resolve_field_name(Some(union_id), union_field_id).unwrap();
                            let fields = vec![mk().field(field_name, v)];
                            mk().struct_expr(name, fields)
                        }))
                    }
                    _ => panic!("Union field decl mismatch"),
                }
            }
            _ => panic!("Expected union decl"),
        }
    }

    fn convert_struct_literal(&self, struct_id: CRecordId, ids: &[CExprId])
                              -> Result<WithStmts<P<Expr>>, String> {
        let struct_decl = &self.ast_context.index(struct_id).kind;

        let field_decls = match struct_decl {
            &CDeclKind::Struct { ref fields, .. } => {
                let mut fieldnames = vec![];

                for &x in fields {
                    let name = self.type_converter.borrow().resolve_field_name(Some(struct_id), x).unwrap();
                    if let &CDeclKind::Field { typ, .. } = &self.ast_context.index(x).kind {
                        fieldnames.push((name, typ));
                    } else {
                        panic!("Struct field decl type mismatch")
                    }
                }

                fieldnames
            }
            _ => panic!("Struct literal declaration mismatch"),
        };

        let struct_name = self.type_converter.borrow().resolve_decl_name(struct_id).unwrap();

        let mut stmts: Vec<Stmt> = vec![];
        let mut fields: Vec<Field> = vec![];

        // Add specified record fields
        for i in 0usize..ids.len() {
            let v = ids[i];
            let &(ref field_name, _) = &field_decls[i];

            let mut x = self.convert_expr(ExprUse::RValue, v, false)?;
            stmts.append(&mut x.stmts);
            fields.push(mk().field(field_name, x.val));
        }

        // Pad out remaining omitted record fields
        for i in ids.len()..fields.len() {
            let &(ref field_name, ty) = &field_decls[i];
            fields.push(mk().field(field_name, self.implicit_default_expr(ty.ctype)?));
        }

        Ok(WithStmts {
            stmts,
            val: mk().struct_expr(vec![mk().path_segment(struct_name)], fields)
        })
    }

    pub fn implicit_default_expr(&self, ty_id: CTypeId) -> Result<P<Expr>, String> {
        let resolved_ty_id = self.ast_context.resolve_type_id(ty_id);
        let resolved_ty = &self.ast_context.index(resolved_ty_id).kind;

        if resolved_ty.is_bool() {
            Ok(mk().lit_expr(mk().bool_lit(false)))
        } else if resolved_ty.is_integral_type() {
            Ok(mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed)))
        } else if resolved_ty.is_floating_type() {
            Ok(mk().lit_expr(mk().float_unsuffixed_lit("0.")))
        } else if self.is_function_pointer(ty_id) {
            Ok(mk().path_expr(vec!["None"]))
        } else if let &CTypeKind::Pointer(_) = resolved_ty {
            self.null_ptr(resolved_ty_id)
        } else if let &CTypeKind::ConstantArray(elt, sz) = resolved_ty {
            let sz = mk().lit_expr(mk().int_lit(sz as u128, LitIntType::Unsuffixed));
            Ok(mk().repeat_expr(self.implicit_default_expr(elt)?, sz))
        } else if let Some(decl_id) = resolved_ty.as_underlying_decl() {
            self.zero_initializer(decl_id, ty_id)
        } else if let &CTypeKind::VariableArray(elt, _) = resolved_ty {

            // Variable length arrays unnested and implemented as a flat array of the underlying
            // element type.

            // Find base element type of potentially nested arrays
            let mut inner = elt;
            while let &CTypeKind::VariableArray(elt_, _)
                      = &self.ast_context.resolve_type(inner).kind {
                inner = elt_;
            }

            let count = self.compute_size_of_expr(ty_id).unwrap();
            let val = self.implicit_default_expr(inner)?;
            let from_elem = mk().path_expr(vec!["", "std","vec","from_elem"]);
            let alloc = mk().call_expr(from_elem, vec![val, count]);
            let ptr = mk().method_call_expr(alloc, "as_mut_ptr", vec![] as Vec<P<Expr>>);
            Ok(ptr)
        } else {
            Err(format!("Unsupported default initializer: {:?}", resolved_ty))
        }
    }

    /// Produce zero-initializers for structs/unions/enums, looking them up when possible.
    fn zero_initializer(&self, decl_id: CDeclId, type_id: CTypeId) -> Result<P<Expr>, String> {

        // Look up the decl in the cache and return what we find (if we find anything)
        if let Some(init) = self.zero_inits.borrow().get(&decl_id) {
            return init.clone()
        }

        // Otherwise, construct the initializer
        let init = match &self.ast_context.index(decl_id).kind {

            // Zero initialize all of the fields
            &CDeclKind::Struct { ref fields, .. } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();
                let fields: Result<Vec<Field>, String> = fields
                    .into_iter()
                    .map(|field_id: &CFieldId| -> Result<Field, String> {
                        let name = self.type_converter.borrow_mut().resolve_field_name(Some(decl_id), *field_id).unwrap();

                        match &self.ast_context.index(*field_id).kind {
                            &CDeclKind::Field { typ, .. } => {
                                let field_init = self.implicit_default_expr(typ.ctype)?;
                                Ok(mk().field(name, field_init))
                            }
                            _ => Err(format!("Found non-field in record field list"))
                        }
                    })
                    .collect();

                Ok(mk().struct_expr(vec![name], fields?))
            },

            // Zero initialize the first field
            &CDeclKind::Union { ref fields, .. } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();
                let &field_id = fields.first().ok_or(format!("A union should have a field"))?;

                let field = match &self.ast_context.index(field_id).kind {
                    &CDeclKind::Field { typ, .. } => {
                        let field_init = self.implicit_default_expr(typ.ctype)?;
                        let name = self.type_converter.borrow().resolve_field_name(Some(decl_id), field_id).unwrap();

                        Ok(mk().field(name, field_init))
                    }
                    _ => Err(format!("Found non-field in record field list"))
                }?;

                Ok(mk().struct_expr(vec![name], vec![field]))
            },

            // Transmute the number `0` into the enum type
            &CDeclKind::Enum { .. } => Ok(self.enum_for_i64(type_id, 0)),

            _ => Err(format!("Declaration is not associated with a type"))
        };

        // Insert the initializer into the cache, then return it
        self.zero_inits.borrow_mut().insert(decl_id, init.clone());
        init
    }


    /// Get back a Rust lvalue corresponding to the expression passed in.
    ///
    /// Do not use the output lvalue expression more than once.
    pub fn name_reference_write(
        &self,
        reference: CExprId,
    ) -> Result<WithStmts<P<Expr>>, String> {
        self.name_reference(reference, false)
            .map(|ws| ws.map(|(lvalue, _)| lvalue))
    }

    /// Get back a Rust (lvalue, rvalue) pair corresponding to the expression passed in.
    ///
    /// You may reuse either of these expressions.
    pub fn name_reference_write_read(
        &self,
        reference: CExprId,
    ) -> Result<WithStmts<(P<Expr>, P<Expr>)>, String> {
        let msg: &str = "When called with `uses_read = true`, `name_reference` should always \
                       return an rvalue (something from which to read the memory location)";

        self.name_reference(reference, true)
            .map(|ws| ws.map(|(lvalue, rvalue)| (lvalue, rvalue.expect(msg))))
    }

    /// This function transforms an expression that should refer to a memory location (a C lvalue)
    /// into a Rust lvalue for writing to that location.
    ///
    /// When called with `uses_read`, this function returns an rvalue too. The rvalue can be used to
    /// read multiple times without duplicating side-effects.
    ///
    /// NOTE: Use `name_reference_write` or `name_reference_write_read` instead of calling this
    ///       directly.
    fn name_reference(
        &self,
        reference: CExprId,
        uses_read: bool,
    ) -> Result<WithStmts<(P<Expr>, Option<P<Expr>>)>, String> {
        let reference_ty = self.ast_context.index(reference).kind.get_qual_type();
        let WithStmts {
            val: reference,
            mut stmts,
        } = self.convert_expr(ExprUse::LValue, reference, false)?;

        /// Check if something is a valid Rust lvalue. Inspired by `librustc::ty::expr_is_lval`.
        fn is_lvalue(e: &Expr) -> bool {
            match e.node {
                ExprKind::Path(..) |
                ExprKind::Unary(ast::UnOp::Deref, _) |
                ExprKind::Field(..) |
                ExprKind::TupField(..) |
                ExprKind::Index(..) => true,
                _ => false,
            }
        }

        // Check if something is a side-effect free Rust lvalue.
        fn is_simple_lvalue(e: &Expr) -> bool {
            match e.node {
                ExprKind::Path(..) => true,
                ExprKind::Unary(ast::UnOp::Deref, ref e) |
                ExprKind::Field(ref e, _) |
                ExprKind::TupField(ref e, _) |
                ExprKind::Index(ref e, _) => is_simple_lvalue(e),
                _ => false,
            }
        }

        // Given the LHS access to a variable, produce the RHS one
        let read = |write: P<Expr>| -> Result<P<Expr>, String> {
            if reference_ty.qualifiers.is_volatile {
                self.volatile_read(&write, reference_ty)
            } else {
                Ok(write)
            }
        };

        if !uses_read && is_lvalue(&*reference) {
            Ok(WithStmts { stmts, val: (reference, None) })
        } else if is_simple_lvalue(&*reference) {
            Ok(WithStmts { stmts, val: (reference.clone(), Some(read(reference)?)) })
        } else {
            // This is the case where we explicitly need to factor out possible side-effects.

            let ptr_name = self.renamer.borrow_mut().fresh();

            // let ref mut p = lhs;
            let compute_ref =
                mk().local_stmt(
                    P(mk().local(mk().mutbl().ident_ref_pat(&ptr_name),
                                 None as Option<P<Ty>>,
                                 Some(reference)))
                );
            stmts.push(compute_ref);

            let write = mk().unary_expr(ast::UnOp::Deref, mk().ident_expr(&ptr_name));

            Ok(WithStmts {
                stmts,
                val: (write.clone(), Some(read(write)?)),
            })
        }
    }

    pub fn convert_pre_increment(&self, ty: CQualTypeId, up: bool, arg: CExprId) -> Result<WithStmts<P<Expr>>, String> {

        let op = if up { c_ast::BinOp::AssignAdd } else { c_ast::BinOp::AssignSubtract };
        let one = WithStmts::new(mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed)));
        let arg_type = self.ast_context[arg].kind.get_qual_type();
        self.convert_assignment_operator_with_rhs(ExprUse::RValue, op, arg_type, arg, ty, one, Some(arg_type), Some(arg_type))
    }

    fn convert_post_increment(&self, use_: ExprUse, ty: CQualTypeId, up: bool, arg: CExprId) -> Result<WithStmts<P<Expr>>, String> {

        // If we aren't going to be using the result, may as well do a simple pre-increment
        if use_ == ExprUse::Unused {
            return self.convert_pre_increment(ty, up, arg)
        }

        let ty = self.ast_context.index(arg).kind.get_qual_type();

        let WithStmts { val: (write, read), stmts: mut lhs_stmts } = self.name_reference_write_read(arg)?;

        let val_name = self.renamer.borrow_mut().fresh();
        let save_old_val =
            mk().local_stmt(
                P(mk().local(mk().ident_pat(&val_name),
                             None as Option<P<Ty>>,
                             Some(read.clone())))
            );

        let mut one = mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed));
        // *p + 1
        let val =
            if let &CTypeKind::Pointer(pointee) = &self.ast_context.resolve_type(ty.ctype).kind {

                if let Some(n) = self.compute_size_of_expr(pointee.ctype) {
                    one = n
                }

                let n = if up { one } else { mk().unary_expr(ast::UnOp::Neg, one) };
                mk().method_call_expr(read.clone(), "offset", vec![n])
            } else {
                if self.ast_context.resolve_type(ty.ctype).kind.is_unsigned_integral_type() {
                    let m = if up { "wrapping_add" } else { "wrapping_sub" };
                    mk().method_call_expr(read.clone(), m, vec![one])
                } else {
                    let k = if up { BinOpKind::Add } else { BinOpKind::Sub };
                    mk().binary_expr(k, read.clone(), one)
                }
            };

        // *p = *p + rhs
        let assign_stmt = if ty.qualifiers.is_volatile {
            self.volatile_write(&write, ty, val)?

        } else {
            mk().assign_expr(&write, val)
        };

        lhs_stmts.push(save_old_val);
        lhs_stmts.push(mk().expr_stmt(assign_stmt));

        Ok(WithStmts {
            stmts: lhs_stmts,
            val: mk().ident_expr(val_name),
        })
    }

    fn convert_unary_operator(
        &self,
        use_: ExprUse,
        name: c_ast::UnOp,
        cqual_type: CQualTypeId,
        arg: CExprId,
    ) -> Result<WithStmts<P<Expr>>, String> {
        let CQualTypeId { ctype, .. } = cqual_type;
        let ty = self.convert_type(ctype)?;
        let resolved_ctype = self.ast_context.resolve_type(ctype);

        match name {
            c_ast::UnOp::AddressOf => {

                // In this translation, there are only pointers to functions and
                // & becomes a no-op when applied to a function.

                let arg = self.convert_expr(ExprUse::LValue, arg, false)?;

                if self.is_function_pointer(ctype) {
                    Ok(arg.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x])))
                } else {
                    let mutbl = match resolved_ctype.kind {
                        CTypeKind::Pointer(pointee) if pointee.qualifiers.is_const => Mutability::Immutable,
                        _ => Mutability::Mutable,
                    };

                    Ok(arg.map(|a| {
                        let addr_of_arg = mk().set_mutbl(mutbl).addr_of_expr(a);
                        mk().cast_expr(addr_of_arg, ty)
                    }))
                }
            },
            c_ast::UnOp::PreIncrement => self.convert_pre_increment(cqual_type, true, arg),
            c_ast::UnOp::PreDecrement => self.convert_pre_increment(cqual_type, false, arg),
            c_ast::UnOp::PostIncrement => self.convert_post_increment(use_, cqual_type, true, arg),
            c_ast::UnOp::PostDecrement => self.convert_post_increment(use_, cqual_type, false, arg),
            c_ast::UnOp::Deref => {
                self.convert_expr(ExprUse::RValue, arg, false)?.result_map(|val: P<Expr>| {

                    if let Some(_vla) = self.compute_size_of_expr(ctype) {
                        Ok(val)
                    } else {
                        let mut val = mk().unary_expr(ast::UnOp::Deref, val);

                        // If the type on the other side of the pointer we are dereferencing is volatile and
                        // this whole expression is not an LValue, we should make this a volatile read
                        if use_ != ExprUse::LValue && cqual_type.qualifiers.is_volatile {
                            val = self.volatile_read(&val, cqual_type)?
                        }
                        Ok(val)
                    }
                })
            },
            c_ast::UnOp::Plus => self.convert_expr(ExprUse::RValue, arg, false), // promotion is explicit in the clang AST

            c_ast::UnOp::Negate => {
                let val = self.convert_expr(ExprUse::RValue, arg, false)?;

                if is_int(&resolved_ctype.kind) {
                    Ok(val.map(wrapping_neg_expr))
                } else {
                    Ok(val.map(neg_expr))
                }
            }
            c_ast::UnOp::Complement =>
                Ok(self.convert_expr(ExprUse::RValue, arg, false)?
                    .map(|a| mk().unary_expr(ast::UnOp::Not, a))),

            c_ast::UnOp::Not => {
                let t = self.ast_context.index(arg).kind.get_type();
                let WithStmts { val: arg, stmts } = self.convert_expr(ExprUse::RValue, arg, false)?;
                Ok(WithStmts { val: self.convert_not(t, arg), stmts })
            },
        }
    }

    fn covert_assignment_operator_aux(
        &self,
        bin_op_kind: BinOpKind,
        bin_op: c_ast::BinOp,
        read: P<Expr>,
        write: P<Expr>,
        rhs: P<Expr>,
        compute_lhs_ty: Option<CQualTypeId>,
        compute_res_ty: Option<CQualTypeId>,
        lhs_ty: CQualTypeId,
        rhs_ty: CQualTypeId
    ) -> Result<P<Expr>, String> {
        let compute_lhs_ty = compute_lhs_ty.unwrap();
        let compute_res_ty = compute_res_ty.unwrap();

        if self.ast_context.resolve_type_id(compute_lhs_ty.ctype) == self.ast_context.resolve_type_id(lhs_ty.ctype) {
            Ok(mk().assign_op_expr(bin_op_kind, write, rhs))
        } else {
            let lhs_type = self.convert_type(compute_lhs_ty.ctype)?;
            let lhs = mk().cast_expr(read, lhs_type);
            let ty = self.convert_type(compute_res_ty.ctype)?;
            let val = self.convert_binary_operator(bin_op, ty, compute_res_ty.ctype, compute_lhs_ty, rhs_ty, lhs, rhs);
            let val = mk().cast_expr(val, self.convert_type(lhs_ty.ctype)?);
            Ok(mk().assign_expr(write.clone(), val))
        }
    }

    fn convert_assignment_operator(
        &self,
        use_: ExprUse,
        op: c_ast::BinOp,
        qtype: CQualTypeId,
        lhs: CExprId,
        rhs: CExprId,
        compute_type: Option<CQualTypeId>,
        result_type: Option<CQualTypeId>
    ) -> Result<WithStmts<P<Expr>>, String> {

        let rhs_type_id = self.ast_context.index(rhs).kind.get_qual_type();
        let rhs_translation = self.convert_expr(ExprUse::RValue, rhs, false)?;
        self.convert_assignment_operator_with_rhs(use_, op, qtype, lhs, rhs_type_id, rhs_translation, compute_type, result_type)
    }

    /// Translate an assignment binary operator
    fn convert_assignment_operator_with_rhs(
        &self,
        use_: ExprUse,
        op: c_ast::BinOp,
        qtype: CQualTypeId,
        lhs: CExprId,
        rhs_type_id: CQualTypeId,
        rhs_translation: WithStmts<P<Expr>>,
        compute_type: Option<CQualTypeId>,
        result_type: Option<CQualTypeId>
    ) -> Result<WithStmts<P<Expr>>, String> {
        let ty = self.convert_type(qtype.ctype)?;

        let result_type_id = result_type.unwrap_or(qtype);
        let compute_lhs_type_id = compute_type.unwrap_or(qtype);
        let initial_lhs_type_id = self.ast_context.index(lhs).kind.get_qual_type();

        let is_volatile = initial_lhs_type_id.qualifiers.is_volatile;
        let is_volatile_compound_assign = op.underlying_assignment().is_some() && is_volatile;

        let qtype_kind = &self.ast_context.resolve_type(qtype.ctype).kind;
        let compute_type_kind = &self.ast_context.resolve_type(compute_lhs_type_id.ctype).kind;

        let pointer_lhs = match qtype_kind {
            &CTypeKind::Pointer(pointee) => Some(pointee),
            _ => None,
        };

        let is_unsigned_arith = match op {
            c_ast::BinOp::AssignAdd | c_ast::BinOp::AssignSubtract |
            c_ast::BinOp::AssignMultiply | c_ast::BinOp::AssignDivide |
            c_ast::BinOp::AssignModulus
             => compute_type_kind.is_unsigned_integral_type(),
            _ => false,
        };

        let (write, read, lhs_stmts) =
            if initial_lhs_type_id.ctype != compute_lhs_type_id.ctype ||
                use_ == ExprUse::RValue ||
                pointer_lhs.is_some() ||
                is_volatile_compound_assign ||
                is_unsigned_arith {
            let WithStmts { val: (write, read), stmts: lhs_stmts } = self.name_reference_write_read(lhs)?;
            (write, read, lhs_stmts)
        } else {
            let WithStmts { val: write, stmts: lhs_stmts } = self.name_reference_write(lhs)?;
            (write, Translation::panic(), lhs_stmts)
        };

        let WithStmts { val: rhs, stmts: rhs_stmts } = rhs_translation;

        // Side effects to accumulate
        let mut stmts = vec![];
        stmts.extend(lhs_stmts);
        stmts.extend(rhs_stmts);

        // Assignment expression itself
        let assign_stmt = match op {
            // Regular (possibly volatile) assignment
            c_ast::BinOp::Assign if !is_volatile => mk().assign_expr(&write, rhs),
            c_ast::BinOp::Assign => self.volatile_write(&write, initial_lhs_type_id, rhs)?,

            // Anything volatile needs to be desugared into explicit reads and writes
            op if is_volatile || is_unsigned_arith => {
                let op = op.underlying_assignment().expect("Cannot convert non-assignment operator");

                let val = if compute_lhs_type_id.ctype == initial_lhs_type_id.ctype {
                    self.convert_binary_operator(op, ty, qtype.ctype, initial_lhs_type_id, rhs_type_id, read.clone(), rhs)
                } else {
                    let lhs_type = self.convert_type(compute_type.unwrap().ctype)?;
                    let write_type = self.convert_type(qtype.ctype)?;
                    let lhs = mk().cast_expr(read.clone(), lhs_type);
                    let ty = self.convert_type(result_type_id.ctype)?;
                    let val = self.convert_binary_operator(op, ty, result_type_id.ctype, compute_lhs_type_id, rhs_type_id, lhs, rhs);
                    mk().cast_expr(val, write_type)
                };

                if is_volatile {
                    self.volatile_write(&write, initial_lhs_type_id, val)?
                } else {
                    mk().assign_expr(write, val)
                }
            },

            // Everything else
            c_ast::BinOp::AssignAdd
            if pointer_lhs.is_some() =>
                {
                    let ptr = match self.compute_size_of_expr(pointer_lhs.unwrap().ctype) {
                        Some(sz) => {
                            let offset = mk().binary_expr(BinOpKind::Mul, cast_int(rhs,"isize"), cast_int(sz,"isize"));
                            pointer_offset_isize(write.clone(), offset)
                        },
                        None => pointer_offset(write.clone(), rhs),
                    };
                    mk().assign_expr(&write, ptr)
                },
            c_ast::BinOp::AssignSubtract
            if pointer_lhs.is_some() => {
                {
                    let ptr = match self.compute_size_of_expr(pointer_lhs.unwrap().ctype) {
                        Some(sz) => pointer_neg_offset_isize(write.clone(), mk().binary_expr(BinOpKind::Mul, cast_int(rhs,"isize"), cast_int(sz,"isize"))),
                        None => pointer_neg_offset(write.clone(), rhs),
                    };
                    mk().assign_expr(&write, ptr)
                }
            },

            c_ast::BinOp::AssignAdd => self.covert_assignment_operator_aux(BinOpKind::Add, c_ast::BinOp::Add, read.clone(), write, rhs, compute_type, result_type,qtype, rhs_type_id)?,
            c_ast::BinOp::AssignSubtract => self.covert_assignment_operator_aux(BinOpKind::Sub, c_ast::BinOp::Subtract, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignMultiply => self.covert_assignment_operator_aux(BinOpKind::Mul, c_ast::BinOp::Multiply, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignDivide => self.covert_assignment_operator_aux(BinOpKind::Div, c_ast::BinOp::Divide, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignModulus => self.covert_assignment_operator_aux(BinOpKind::Rem, c_ast::BinOp::Modulus, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignBitXor => self.covert_assignment_operator_aux(BinOpKind::BitXor, c_ast::BinOp::BitXor, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignShiftLeft => self.covert_assignment_operator_aux(BinOpKind::Shl, c_ast::BinOp::ShiftLeft, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignShiftRight => self.covert_assignment_operator_aux(BinOpKind::Shr, c_ast::BinOp::ShiftRight, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignBitOr => self.covert_assignment_operator_aux(BinOpKind::BitOr, c_ast::BinOp::BitOr, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,
            c_ast::BinOp::AssignBitAnd => self.covert_assignment_operator_aux(BinOpKind::BitAnd, c_ast::BinOp::BitAnd, read.clone(), write, rhs, compute_type, result_type, qtype, rhs_type_id)?,

            _ => panic!("Cannot convert non-assignment operator"),
        };

        stmts.push(mk().expr_stmt(assign_stmt));

        Ok(WithStmts { stmts, val: read })
    }

    /// Translate a non-assignment binary operator. It is expected that the `lhs` and `rhs`
    /// arguments be usable as rvalues.
    fn convert_binary_operator(
        &self,
        op: c_ast::BinOp,
        ty: P<Ty>,
        ctype: CTypeId,
        lhs_type: CQualTypeId,
        rhs_type: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>,
    ) -> P<Expr> {
        let is_unsigned_integral_type = self.ast_context.index(ctype).kind.is_unsigned_integral_type();

        match op {
            c_ast::BinOp::Add => self.convert_addition(lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::Subtract => self.convert_subtraction(ty, lhs_type, rhs_type, lhs, rhs),

            c_ast::BinOp::Multiply if is_unsigned_integral_type =>
                mk().method_call_expr(lhs, mk().path_segment("wrapping_mul"), vec![rhs]),
            c_ast::BinOp::Multiply => mk().binary_expr(BinOpKind::Mul, lhs, rhs),

            c_ast::BinOp::Divide if is_unsigned_integral_type =>
                mk().method_call_expr(lhs, mk().path_segment("wrapping_div"), vec![rhs]),
            c_ast::BinOp::Divide => mk().binary_expr(BinOpKind::Div, lhs, rhs),

            c_ast::BinOp::Modulus if is_unsigned_integral_type =>
                mk().method_call_expr(lhs, mk().path_segment("wrapping_rem"), vec![rhs]),
            c_ast::BinOp::Modulus => mk().binary_expr(BinOpKind::Rem, lhs, rhs),

            c_ast::BinOp::BitXor => mk().binary_expr(BinOpKind::BitXor, lhs, rhs),

            c_ast::BinOp::ShiftRight => mk().binary_expr(BinOpKind::Shr, lhs, rhs),
            c_ast::BinOp::ShiftLeft => mk().binary_expr(BinOpKind::Shl, lhs, rhs),

            c_ast::BinOp::EqualEqual => bool_to_int(mk().binary_expr(BinOpKind::Eq, lhs, rhs)),
            c_ast::BinOp::NotEqual => bool_to_int(mk().binary_expr(BinOpKind::Ne, lhs, rhs)),
            c_ast::BinOp::Less => bool_to_int(mk().binary_expr(BinOpKind::Lt, mk().paren_expr(lhs), rhs)),
            c_ast::BinOp::Greater => bool_to_int(mk().binary_expr(BinOpKind::Gt, lhs, rhs)),
            c_ast::BinOp::GreaterEqual => bool_to_int(mk().binary_expr(BinOpKind::Ge, lhs, rhs)),
            c_ast::BinOp::LessEqual => bool_to_int(mk().binary_expr(BinOpKind::Le, lhs, rhs)),

            c_ast::BinOp::BitAnd => mk().binary_expr(BinOpKind::BitAnd, lhs, rhs),
            c_ast::BinOp::BitOr => mk().binary_expr(BinOpKind::BitOr, lhs, rhs),

            op => unimplemented!("Translation of binary operator {:?}", op),
        }
    }

    fn convert_addition(
        &self,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

        if let &CTypeKind::Pointer(pointee) = lhs_type {
            match self.compute_size_of_expr(pointee.ctype) {
                Some(sz) => {
                    let rhs = mk().binary_expr(BinOpKind::Mul, cast_int(rhs, "isize"), cast_int(sz, "isize"));
                    pointer_offset_isize(lhs, rhs)
                }
                None => pointer_offset(lhs, rhs)
            }
        } else if let &CTypeKind::Pointer(pointee) = rhs_type {
            match self.compute_size_of_expr(pointee.ctype) {
                Some(sz) => {
                    let lhs = mk().binary_expr(BinOpKind::Mul, cast_int(lhs, "isize"), cast_int(sz, "isize"));
                    pointer_offset_isize(rhs, lhs)
                }
                None => pointer_offset(rhs, lhs),
            }
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_add"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Add, lhs, rhs)
        }
    }

    fn convert_subtraction(
        &self,
        ty: P<Ty>,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>,
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

        if let &CTypeKind::Pointer(pointee) = rhs_type {
            // offset_to returns None when a pointer
            // offset_opt := rhs.offset_to(lhs)
            let offset_opt = mk().method_call_expr(rhs, "offset_to", vec![lhs]);
            // msg := "bad offset_to"
            let msg = mk().lit_expr(mk().str_lit("bad offset_to"));
            // offset := offset_opt.expect(msg)
            let mut offset = mk().method_call_expr(offset_opt, "expect", vec![msg]);

            if let Some(sz) = self.compute_size_of_expr(pointee.ctype) {
                offset = mk().binary_expr(BinOpKind::Div, offset, cast_int(sz,"isize"))
            }

            mk().cast_expr(offset, ty)
        } else if let &CTypeKind::Pointer(pointee) = lhs_type {
            match self.compute_size_of_expr(pointee.ctype) {
                None => pointer_neg_offset(lhs, rhs),
                Some(sz) => pointer_neg_offset_isize(lhs, mk().binary_expr(BinOpKind::Mul, cast_int(rhs,"isize"), cast_int(sz, "isize"))),
            }
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_sub"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Sub, lhs, rhs)
        }
    }

    /// Convert a boolean expression to a boolean for use in && or || or if
    fn match_bool(&self, target: bool, ty_id: CTypeId, val: P<Expr>) -> P<Expr> {
        let ty = &self.ast_context.resolve_type(ty_id).kind;

        if self.is_function_pointer(ty_id) {
            if target {
                mk().method_call_expr(val, "is_some", vec![] as Vec<P<Expr>>)
            } else {
                mk().method_call_expr(val, "is_none", vec![] as Vec<P<Expr>>)
            }
        } else if ty.is_pointer() {
            let mut res = mk().method_call_expr(val, "is_null", vec![] as Vec<P<Expr>>);
            if target {
                res = mk().unary_expr(ast::UnOp::Not, res)
            }
            res
        } else if ty.is_bool() {
            if target {
                val
            } else {
                mk().unary_expr(ast::UnOp::Not, val)
            }
        } else {
            let zero = if ty.is_floating_type() {
                mk().lit_expr(mk().float_unsuffixed_lit("0."))
            } else {
                mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed))
            };

            // One simplification we can make at the cost of inspecting `val` more closely: if `val`
            // is already in the form `(x <op> y) as <ty>` where `<op>` is a Rust operator
            // that returns a boolean, we can simple output `x <op> y` or `!(x <op> y)`.
            if let ExprKind::Cast(ref arg, _) = val.node {
                if let ExprKind::Binary(op, _, _) = arg.node {
                    match op.node {
                        BinOpKind::Or | BinOpKind::And |
                        BinOpKind::Eq | BinOpKind::Ne |
                        BinOpKind::Lt | BinOpKind::Le |
                        BinOpKind::Gt | BinOpKind::Ge => if target {
                            // If target == true, just return the argument
                            return arg.clone();
                        } else {
                            // If target == false, return !arg
                            return mk().unary_expr(ast::UnOp::Not, arg.clone());
                        },
                        _ => {}
                    }
                }
            }

            let val = if ty.is_enum() { mk().cast_expr(val, mk().path_ty(vec!["u64"])) } else { val };

            // The backup is to just compare against zero
            if target {
                mk().binary_expr(BinOpKind::Ne, zero, val)
            } else {
                mk().binary_expr(BinOpKind::Eq, zero, val)
            }
        }
    }

    /// Convert expression to c_int using '!' behavior
    fn convert_not(&self, ty_id: CTypeId, val: P<Expr>) -> P<Expr> {
        let b = self.match_bool(false, ty_id, val);
        mk().cast_expr(b, mk().path_ty(vec!["libc", "c_int"]))
    }

    fn is_function_pointer(&self, typ: CTypeId) -> bool {
        let resolved_ctype = self.ast_context.resolve_type(typ);
        if let CTypeKind::Pointer(p) = resolved_ctype.kind {
            if let CTypeKind::Function { .. } = self.ast_context.resolve_type(p.ctype).kind {
                true
            } else { false }
        } else { false }
    }

    pub fn with_scope<F, A>(&self, f: F) -> A
        where F: FnOnce() -> A {
        self.renamer.borrow_mut().add_scope();
        let result = f();
        self.renamer.borrow_mut().drop_scope();
        result
    }

    // Visit all the declarations in the stmt (or the compound stmt)
    //
    // FIXME: when this is done, call this every time we open a scope in the CFG
    pub fn visit_decls(&self, stmt_ids: &Vec<CStmtId>) -> () {
        for stmt_id in stmt_ids {
            if let CStmtKind::Decls(ref decl_ids) = self.ast_context.index(*stmt_id).kind {
                for _decl_id in decl_ids {
                    unimplemented!();
                }
            }
        }
    }
}
