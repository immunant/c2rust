use syntax::{with_globals, ast};
use syntax::ast::*;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::tokenstream::{TokenStream};
use syntax::parse::token::{Token,Nonterminal};
use std::collections::{HashMap,HashSet};
use renamer::Renamer;
use convert_type::TypeConverter;
use loops::*;
use c_ast;
use c_ast::*;
use rust_ast::{mk, Builder};
use rust_ast::comment_store::CommentStore;
use c_ast::iterators::{DFExpr, SomeId};
use syntax::ptr::*;
use syntax::print::pprust::*;
use std::ops::Index;
use std::cell::RefCell;
use std::char;
use std::mem;
use dtoa;
use with_stmts::WithStmts;
use rust_ast::traverse::Traversal;
use std::io;

use cfg;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum DecayRef {
    Yes,
    Default,
    No,
}

impl DecayRef {
    // Here we give intrinsic meaning to default to equate to yes/true
    // when actually evaluated
    pub fn is_yes(&self) -> bool {
        match self {
            DecayRef::Yes => true,
            DecayRef::Default => true,
            DecayRef::No => false,
        }
    }

    pub fn is_no(&self) -> bool {
        !self.is_yes()
    }

    pub fn set_default_to_no(&mut self) {
        if *self == DecayRef::Default {
            *self = DecayRef::No;
        }
    }
}

impl From<bool> for DecayRef {
    fn from(b: bool) -> Self {
        match b {
            true => DecayRef::Yes,
            false => DecayRef::No,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ReplaceMode {
    None,
    Extern,
}

/// Configuration settings for the translation process
#[derive(Debug)]
pub struct TranslationConfig {
    pub reloop_cfgs: bool,
    pub fail_on_multiple: bool,
    pub dump_function_cfgs: bool,
    pub json_function_cfgs: bool,
    pub dump_cfg_liveness: bool,
    pub dump_structures: bool,
    pub debug_relooper_labels: bool,
    pub cross_checks: bool,
    pub cross_check_configs: Vec<String>,
    pub prefix_function_names: Option<String>,
    pub translate_asm: bool,
    pub translate_entry: bool,
    pub use_c_loop_info: bool,
    pub use_c_multiple_info: bool,
    pub simplify_structures: bool,
    pub panic_on_translator_failure: bool,
    pub emit_module: bool,
    pub fail_on_error: bool,
    pub replace_unsupported_decls: ReplaceMode,
    pub translate_valist: bool,
    pub reduce_type_annotations: bool,
}

pub struct Translation {

    // Translation environment
    pub ast_context: TypedAstContext,
    pub tcfg: TranslationConfig,

    // Accumulated outputs
    pub features: RefCell<HashSet<&'static str>>,
    pub items: RefCell<Vec<P<Item>>>,
    pub foreign_items: Vec<ForeignItem>,
    sectioned_static_initializers: RefCell<Vec<Stmt>>,

    // Translation state and utilities
    type_converter: RefCell<TypeConverter>,
    renamer: RefCell<Renamer<CDeclId>>,
    loops: LoopContext,
    zero_inits: RefCell<HashMap<CDeclId, Result<P<Expr>, String>>>,

    // Comment support
    pub comment_context: RefCell<CommentContext>, // Incoming comments
    pub comment_store: RefCell<CommentStore>, // Outgoing comments
}


fn sequence_option<A,E>(x: Option<Result<A,E>>) -> Result<Option<A>, E> {
    match x {
        None => Ok(None),
        Some(o) => Ok(Some(o?)),
    }
}

fn cast_int(val: P<Expr>, name: &str) -> P<Expr> {
    let opt_literal_val = match val.node {
        ExprKind::Lit(ref l) => match l.node {
            LitKind::Int(i,_) => Some(i),
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

/// Given an expression with type Option<fn(...)->...>, unwrap
/// the Option and return the function.
fn unwrap_function_pointer(ptr: P<Expr>) -> P<Expr> {
    let err_msg = mk().lit_expr(mk().str_lit("non-null function pointer"));
    mk().method_call_expr(ptr, "expect", vec![err_msg])
}

fn neg_expr(arg: P<Expr>) -> P<Expr> {
    mk().unary_expr(ast::UnOp::Neg, arg)
}

fn wrapping_neg_expr(arg: P<Expr>) -> P<Expr> {
    mk().method_call_expr(arg, "wrapping_neg", vec![] as Vec<P<Expr>>)
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
            if let ExprKind::Block(ref b, _) = e.node {
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
fn prefix_names(translation: &mut Translation, prefix: String) {
    for (&decl_id, ref mut decl) in &mut translation.ast_context.c_decls {
        match decl.kind {
            CDeclKind::Function { ref mut name, ref body, .. } if body.is_some() => {
                name.insert_str(0, &prefix);

                translation.renamer.borrow_mut().insert(decl_id, &name);
            },
            CDeclKind::Variable { ref mut ident, is_static: true, .. } => ident.insert_str(0, &prefix),
            _ => (),
        }
    }
}

pub fn translate_failure(tcfg: &TranslationConfig, msg: &str) {
    if tcfg.fail_on_error {
        panic!("{}", msg)
    } else {
        eprintln!("{}", msg)
    }
}

pub fn translate(ast_context: TypedAstContext, tcfg: TranslationConfig) -> String {

    let mut t = Translation::new(ast_context, tcfg);

    if !t.tcfg.translate_entry {
        t.ast_context.c_main = None;
    }

    // Headers often pull in declarations that are unused;
    // we simplify the translator output by omitting those.
    t.ast_context.prune_unused_decls();

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
    if let Some(prefix) = t.tcfg.prefix_function_names.clone() {
        prefix_names(&mut t, prefix);
    }

    // `with_globals` sets up a thread-local variable required by the syntax crate.
    with_globals(|| {
        // Identify typedefs that name unnamed types and collapse the two declarations
        // into a single name and declaration, eliminating the typedef altogether.
        let mut prenamed_decls: HashSet<CDeclId> = HashSet::new();
        for (&decl_id, decl) in &t.ast_context.c_decls {
            if let CDeclKind::Typedef { ref name, typ, .. } = decl.kind {
                if let Some(subdecl_id) = t.ast_context.resolve_type(typ.ctype).kind.as_underlying_decl() {
                    let is_unnamed = match t.ast_context[subdecl_id].kind {
                        CDeclKind::Struct { name: None, .. } => true,
                        CDeclKind::Union { name: None, .. } => true,
                        CDeclKind::Enum { name: None, .. } => true,
                        _ => false,
                    };

                    if is_unnamed && !prenamed_decls.contains(&subdecl_id) {
                        prenamed_decls.insert(decl_id);
                        prenamed_decls.insert(subdecl_id);

                        t.type_converter.borrow_mut().declare_decl_name(decl_id, name);
                        t.type_converter.borrow_mut().alias_decl_name(subdecl_id, decl_id);
                    }
                }
            }
        }

        // Populate renamer with top-level names
        for (&decl_id, decl) in &t.ast_context.c_decls {
            let decl_name = match decl.kind {
                _ if prenamed_decls.contains(&decl_id) => Name::NoName,
                CDeclKind::Struct { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
                CDeclKind::Enum { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
                CDeclKind::Union { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
                CDeclKind::Typedef { ref name, .. } => Name::TypeName(name),
                CDeclKind::Function { ref name, .. } => Name::VarName(name),
                CDeclKind::EnumConstant { ref name, .. } => Name::VarName(name),
                CDeclKind::Variable { ref ident, .. }
                if t.ast_context.c_decls_top.contains(&decl_id) => Name::VarName(ident),
                _ => Name::NoName,
            };
            match decl_name {
                Name::NoName => (),
                Name::AnonymousType => { t.type_converter.borrow_mut().declare_decl_name(decl_id, "unnamed"); }
                Name::TypeName(name) => { t.type_converter.borrow_mut().declare_decl_name(decl_id, name); }
                Name::VarName(name) => { t.renamer.borrow_mut().insert(decl_id, &name); }
            }
        }

        // Export all types
        for (&decl_id, decl) in &t.ast_context.c_decls {
            let needs_export = match decl.kind {
                CDeclKind::Struct { .. } => true,
                CDeclKind::Enum { .. } => true,
                CDeclKind::EnumConstant { .. } => true,
                CDeclKind::Union { .. } => true,
                CDeclKind::Typedef { .. } =>
                    !prenamed_decls.contains(&decl_id),
                _ => false,
            };
            if needs_export {
                match t.convert_decl(true, decl_id) {
                    Ok(ConvertedDecl::Item(item)) => t.items.borrow_mut().push(item),
                    Ok(ConvertedDecl::ForeignItem(mut item)) => t.foreign_items.push(item),
                    Err(e) => {
                        let ref k = t.ast_context.c_decls.get(&decl_id).map(|x| &x.kind);
                        let msg = format!("Skipping declaration due to error: {}, kind: {:?}", e, k);
                        translate_failure(&t.tcfg, &msg)
                    },
                }
            }
        }

        // Export top-level value declarations
        for top_id in &t.ast_context.c_decls_top {
            let needs_export = match t.ast_context.c_decls[top_id].kind {
                CDeclKind::Function { is_implicit, .. } => !is_implicit,
                CDeclKind::Variable { .. } => true,
                _ => false,
            };
            if needs_export {
                match t.convert_decl(true, *top_id) {
                    Ok(ConvertedDecl::Item(mut item)) => t.items.borrow_mut().push(item),
                    Ok(ConvertedDecl::ForeignItem(mut item)) => t.foreign_items.push(item),
                    Err(e) => {
                        let ref k = t.ast_context.c_decls.get(top_id).map(|x| &x.kind);
                        let msg = format!("Failed translating declaration due to error: {}, kind: {:?}", e, k);
                        translate_failure(&t.tcfg, &msg)
                    },
                }
            }
        }

        // Add the main entry point
        if let Some(main_id) = t.ast_context.c_main {
            match t.convert_main(main_id) {
                Ok(item) => t.items.borrow_mut().push(item),
                Err(e) => {
                    let msg = format!("Failed translating main declaration due to error: {}", e);
                    translate_failure(&t.tcfg, &msg)
                }
            }
        };

        // Initialize global statics when necessary
        if !t.sectioned_static_initializers.borrow().is_empty() {
            let (initializer_fn, initializer_static) = t.generate_global_static_init();
            let mut items = t.items.borrow_mut();


            items.push(initializer_fn);
            items.push(initializer_static);
            t.use_feature("used");
        }

        // pass all converted items to the Rust pretty printer
        to_string(|s| {

            print_header(s,&t)?;

            // Re-order comments
            let mut traverser = t.comment_store.into_inner().into_comment_traverser();

            let foreign_items: Vec<ForeignItem> = t.foreign_items
                .into_iter()
                .map(|fi| traverser.traverse_foreign_item(fi))
                .collect();
            let items: Vec<P<Item>> = t.items.into_inner()
                .into_iter()
                .map(|p_i| p_i.map(|i| traverser.traverse_item(i)))
                .collect();

            s.comments().get_or_insert(vec![]).extend(traverser.into_comment_store().into_comments());

            if !foreign_items.is_empty() {
                s.print_item(&mk().abi("C").foreign_items(foreign_items))?
            }

            // Add the items accumulated
            for x in items {
                s.print_item(&*x)?;
            }

            Ok(())
        })
    })
}

/// Pretty-print the leading pragmas and extern crate declarations
fn print_header(s: &mut State, t: &Translation) -> io::Result<()> {
    if t.tcfg.emit_module {
        s.print_item(&mk().use_item(vec!["libc"], None as Option<Ident>))?;
    } else {
        let mut features = vec!["libc"];
        features.extend(t.features.borrow().iter());
        features.extend(t.type_converter.borrow().features_used());
        let mut pragmas: Vec<(&str, Vec<&str>)> =
            vec![("allow", vec!["non_upper_case_globals", "non_camel_case_types", "non_snake_case",
                                "dead_code", "mutable_transmutes", "unused_mut"])];
        if t.tcfg.cross_checks {
            features.append(&mut vec!["plugin", "custom_attribute"]);
            pragmas.push(("cross_check", vec!["yes"]));
        }

        pragmas.push(("feature", features));
        for (key, mut values) in pragmas {
            values.sort();
            let value_attr_vec = values.into_iter()
                .map(|value|
                    mk().nested_meta_item(
                        mk().meta_item(vec![value], MetaItemKind::Word)))
                .collect::<Vec<_>>();
            let item = mk().meta_item(
                vec![key],
                MetaItemKind::List(value_attr_vec),
            );
            for attr in mk().meta_item_attr(AttrStyle::Inner, item).as_inner_attrs() {
                s.print_attribute(&attr)?;
            }
        }

        if t.tcfg.cross_checks {
            let mut xcheck_plugin_args: Vec<NestedMetaItem> = vec![];
            for config_file in &t.tcfg.cross_check_configs {
                let file_lit = mk().str_lit(config_file);
                let file_item = mk().meta_item(vec!["config_file"],file_lit.into_inner());
                xcheck_plugin_args.push(mk().nested_meta_item(file_item));
            }
            let xcheck_plugin_item = mk().meta_item(
                vec!["cross_check_plugin"],
                MetaItemKind::List(xcheck_plugin_args),
            );
            let plugin_args = vec![mk().nested_meta_item(xcheck_plugin_item)];
            let plugin_item = mk().meta_item(
                vec!["plugin"],
                MetaItemKind::List(plugin_args),
            );
            for attr in mk().meta_item_attr(AttrStyle::Inner, plugin_item).as_inner_attrs() {
                s.print_attribute(&attr)?;
            }
        }

        // Add `extern crate libc` to the top of the file
        s.print_item(&mk().extern_crate_item("libc", None))?;
        if t.tcfg.cross_checks {
            s.print_item(&mk().single_attr("macro_use")
                .extern_crate_item("cross_check_derive", None))?;
            s.print_item(&mk().single_attr("macro_use")
                .extern_crate_item("cross_check_runtime", None))?;
        }
    }
    Ok(())
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

/// Declarations can be converted into a normal item, or into a foreign item.
/// Foreign items are called out specially because we'll combine all of them
/// into a single extern block at the end of translation.
enum ConvertedDecl {
    ForeignItem(ForeignItem),
    Item(P<Item>),
}

impl Translation {
    pub fn new(mut ast_context: TypedAstContext, tcfg: TranslationConfig) -> Translation {
        let comment_context = RefCell::new(CommentContext::new(&mut ast_context));
        let mut type_converter = TypeConverter::new();

        if tcfg.translate_valist { type_converter.translate_valist = true }

        Translation {
            features: RefCell::new(HashSet::new()),
            items: RefCell::new(vec![]),
            foreign_items: vec![],
            type_converter: RefCell::new(type_converter),
            ast_context,
            tcfg,
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

                // prelude names
                "drop", "Some", "None", "Ok", "Err",
            ])),
            loops: LoopContext::new(),
            zero_inits: RefCell::new(HashMap::new()),
            comment_context,
            comment_store: RefCell::new(CommentStore::new()),
            sectioned_static_initializers: RefCell::new(Vec::new()),
        }
    }

    /// Called when translation makes use of a language feature that will require a feature-gate.
    fn use_feature(&self, feature: &'static str) {
        self.features.borrow_mut().insert(feature);
    }

    // This node should _never_ show up in the final generated code. This is an easy way to notice
    // if it does.
    pub fn panic(&self, msg: &str) -> P<Expr> {
        let macro_name = if self.tcfg.panic_on_translator_failure { "panic" } else { "compile_error" };
        let macro_msg = vec![
            Token::interpolated(Nonterminal::NtExpr(mk().lit_expr(mk().str_lit(msg)))),
        ].into_iter().collect::<TokenStream>();
        mk().mac_expr(mk().mac(vec![macro_name], macro_msg))
    }

    fn mk_cross_check(&self, mk: Builder, args: Vec<&str>) -> Builder {
        if self.tcfg.cross_checks {
            mk.call_attr("cross_check", args)
        } else { mk }
    }

    fn static_initializer_is_uncompilable(&self, expr_id: Option<CExprId>) -> bool {
        use c_ast::UnOp::{AddressOf, Negate};
        use c_ast::CastKind::PointerToIntegral;
        use c_ast::BinOp::{Add, Subtract, Multiply, Divide, Modulus};

        let expr_id = match expr_id {
            Some(expr_id) => expr_id,
            None => return false,
        };

        let iter = DFExpr::new(&self.ast_context, expr_id.into());

        for i in iter {
            let expr_id = match i {
                SomeId::Expr(expr_id) => expr_id,
                _ => unreachable!("Found static initializer type other than expr"),
            };

            match self.ast_context[expr_id].kind {
                // Technically we're being conservative here, but it's only the most
                // contrived array indexing initializers that would be accepted
                CExprKind::ArraySubscript(..) => return true,

                CExprKind::Conditional(..) => return true,
                CExprKind::Unary(typ, Negate, _, _) => {
                    if self.ast_context.resolve_type(typ.ctype).kind.is_unsigned_integral_type() {
                        return true;
                    }
                },
                CExprKind::ImplicitCast(_, _, PointerToIntegral, _, _) => return true,
                CExprKind::Binary(typ, op, _, _, _, _) => {
                    let problematic_op = match op {
                        Add | Subtract | Multiply | Divide | Modulus => true,
                        _ => false,
                    };

                    if problematic_op {
                        let k = &self.ast_context.resolve_type(typ.ctype).kind;
                        if k.is_unsigned_integral_type() || k.is_pointer() {
                            return true;
                        }
                    }
                },
                CExprKind::Unary(_, AddressOf, expr_id, _) => {
                    if let CExprKind::Member(_, expr_id, _, _, _) = self.ast_context[expr_id].kind {
                        if let CExprKind::DeclRef(..) = self.ast_context[expr_id].kind {
                            return true;
                        }
                    }
                },
                _ => {},
            }
        }

        false
    }

    fn add_static_initializer_to_section(&self, name: &str, typ: CQualTypeId, init: &mut P<Expr>) -> Result<(), String> {
        let root_lhs_expr = mk().path_expr(vec![name]);
        let assign_expr = {
            let block = match &init.node {
                ExprKind::Block(block, _) => block,
                _ => unreachable!("Found static initializer type other than block"),
            };

            let expr = match &block.stmts[0].node {
                StmtKind::Expr(ref expr) => expr,
                _ => unreachable!("Found static initializer type other than Expr"),
            };

            mk().assign_expr(root_lhs_expr, expr)
        };

        let stmt = mk().expr_stmt(assign_expr);

        self.sectioned_static_initializers.borrow_mut().push(stmt);

        *init = self.implicit_default_expr(typ.ctype, true)?;

        Ok(())
    }

    fn generate_global_static_init(&mut self) -> (P<Item>, P<Item>) {
        // If we don't want to consume self.sectioned_static_initializers for some reason, we could clone the vec
        let sectioned_static_initializers = self.sectioned_static_initializers.replace(Vec::new());

        let fn_name = self.renamer.borrow_mut().pick_name("run_static_initializers");
        let fn_ty = FunctionRetTy::Ty(mk().tuple_ty(vec![] as Vec<P<Ty>>));
        let fn_decl = mk().fn_decl(vec![], fn_ty, false);
        let fn_block = mk().block(sectioned_static_initializers);
        let fn_attributes = self.mk_cross_check(mk(), vec!["none"]);
        let fn_item = fn_attributes.unsafe_().abi("C").fn_item(&fn_name, &fn_decl, fn_block);

        let static_attributes = mk()
            .single_attr("used")
            .call_attr("cfg_attr", vec!["target_os = \"linux\"", "link_section = \".init_array\""])
            .call_attr("cfg_attr", vec!["target_os = \"windows\"", "link_section = \".CRT$XIB\""])
            .call_attr("cfg_attr", vec!["target_os = \"macos\"", "link_section = \"__DATA,__mod_init_func\""]);
        let static_array_size = mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed));
        let static_ty = mk().array_ty(mk().unsafe_().abi("C").barefn_ty(fn_decl), static_array_size);
        let static_val = mk().array_expr(vec![mk().path_expr(vec![fn_name])]);
        let static_item = static_attributes.static_item("INIT_ARRAY", static_ty, static_val);

        (fn_item, static_item)
    }

    fn convert_main(&self, main_id: CDeclId) -> Result<P<Item>, String> {
        if let CDeclKind::Function { ref parameters, typ, .. } = self.ast_context.index(main_id).kind {

            let ret: CTypeKind = match self.ast_context.resolve_type(typ).kind {
                CTypeKind::Function(ret, _, _, _) => self.ast_context.resolve_type(ret.ctype).kind.clone(),
                ref k => return Err(format!("Type of main function {:?} was not a function type, got {:?}", main_id, k))
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
                                    Token::from_ast_ident(mk().ident("var_name")),
                                    Token::Comma,
                                    Token::from_ast_ident(mk().ident("var_value")),
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
            let main_attributes = self.mk_cross_check(mk(), vec!["none"]);
            Ok(main_attributes.pub_().fn_item("main", decl, block))
        } else {
            Err(format!("Cannot translate non-function main entry point"))
        }
    }

    fn convert_decl(&self, toplevel: bool, decl_id: CDeclId) -> Result<ConvertedDecl, String> {
        let mut s = {
            let decl_cmt = self.comment_context.borrow_mut().remove_decl_comment(decl_id);
            self.comment_store.borrow_mut().add_comment_lines(decl_cmt)
        };

        match self.ast_context.c_decls.get(&decl_id)
            .ok_or_else(|| format!("Missing decl {:?}", decl_id))?
            .kind {
            CDeclKind::Struct { fields: None, .. } |
            CDeclKind::Union { fields: None, .. } |
            CDeclKind::Enum { integral_type: None, .. } => {
                self.use_feature("extern_types");
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();
                let extern_item = mk().span(s).pub_().foreign_ty(name);
                Ok(ConvertedDecl::ForeignItem(extern_item))
            }

            CDeclKind::Struct { fields: Some(ref fields), is_packed, manual_alignment, .. } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                // Gather up all the field names and field types
                let mut field_entries = vec![];
                for &x in fields {
                    match self.ast_context.index(x).kind {
                        CDeclKind::Field { ref name, typ } => {
                            let name = self.type_converter.borrow_mut().declare_field_name(decl_id, x, name);
                            let typ = self.convert_type(typ.ctype)?;
                            field_entries.push(mk().span(s).pub_().struct_field(name, typ))
                        }
                        _ => return Err(format!("Found non-field in record field list")),
                    }
                }

                fn simple_metaitem(name: &str) -> ast::NestedMetaItem {
                    mk().nested_meta_item(
                        NestedMetaItemKind::MetaItem(
                            mk().meta_item(vec![name], MetaItemKind::Word)))
                }

                let mut reprs = vec![simple_metaitem("C")];

                if is_packed { reprs.push(simple_metaitem("packed")); };
                // https://github.com/rust-lang/rust/issues/33626
                if let Some(alignment) = manual_alignment {
                    self.use_feature("repr_align");
                    self.use_feature("attr_literals");

                    let lit = mk().int_lit(alignment as u128, LitIntType::Unsuffixed);
                    let inner = mk().meta_item(
                        vec!["align"],
                        MetaItemKind::List(
                            vec![mk().nested_meta_item(
                                NestedMetaItemKind::Literal(lit.into_inner()))]));
                    reprs.push(mk().nested_meta_item(NestedMetaItemKind::MetaItem(inner)));
                };

                let repr_attr = mk().meta_item(vec!["repr"], MetaItemKind::List(reprs));

                Ok(ConvertedDecl::Item(mk().span(s).pub_()
                    .call_attr("derive", vec!["Copy", "Clone"])
                    .meta_item_attr(AttrStyle::Outer, repr_attr)
                    .struct_item(name, field_entries)))
            }

            CDeclKind::Union { fields: Some(ref fields), .. } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                let mut field_syns = vec![];
                for &x in fields {
                    let field_decl = self.ast_context.index(x);
                    match field_decl.kind {
                        CDeclKind::Field { ref name, typ } => {
                            let name = self.type_converter.borrow_mut().declare_field_name(decl_id, x, name);
                            let typ = self.convert_type(typ.ctype)?;
                            field_syns.push(mk().span(s).struct_field(name, typ))
                        }
                        _ => return Err(format!("Found non-field in record field list")),
                    }
                }

                Ok(if field_syns.is_empty() {
                    // Empty unions are a GNU extension, but Rust doesn't allow empty unions.
                    ConvertedDecl::Item(mk().span(s).pub_()
                        .call_attr("derive", vec!["Copy", "Clone"])
                        .call_attr("repr", vec!["C"])
                        .struct_item(name, vec![]))
                } else {
                    ConvertedDecl::Item(mk().span(s).pub_()
                        .call_attr("derive", vec!["Copy", "Clone"])
                        .call_attr("repr", vec!["C"])
                        .union_item(name, field_syns))
                })
            }

            CDeclKind::Field { .. } => Err(format!("Field declarations should be handled inside structs/unions")),

            CDeclKind::Enum { integral_type: Some(integral_type), .. } => {
                let enum_name = &self.type_converter.borrow().resolve_decl_name(decl_id).expect("Enums should already be renamed");
                let ty = self.convert_type(integral_type.ctype)?;
                Ok(ConvertedDecl::Item(mk().span(s).pub_()
                    .type_item(enum_name, ty)))
            },

            CDeclKind::EnumConstant { value, .. } => {
                let name = self.renamer.borrow_mut().get(&decl_id).expect("Enum constant not named");
                let enum_id = self.ast_context.parents[&decl_id];
                let enum_name = self.type_converter.borrow().resolve_decl_name(enum_id).expect("Enums should already be renamed");
                let ty = mk().path_ty(mk().path(vec![enum_name]));
                let val = match value {
                    ConstIntExpr::I(value) => signed_int_expr(value),
                    ConstIntExpr::U(value) => mk().lit_expr(mk().int_lit(value as u128, LitIntType::Unsuffixed)),
                };

                Ok(ConvertedDecl::Item(mk().span(s).pub_().const_item(name, ty, val)))
            }

            CDeclKind::Function { .. } if !toplevel => Err(format!("Function declarations must be top-level")),
            CDeclKind::Function { is_extern, is_inline, typ, ref name, ref parameters, body, .. } => {
                let new_name = &self.renamer.borrow().get(&decl_id).expect("Functions should already be renamed");


                let (ret, is_var): (Option<CQualTypeId>, bool) = match self.ast_context.resolve_type(typ).kind {
                    CTypeKind::Function(ret, _, is_var, is_noreturn) => (if is_noreturn { None } else { Some(ret) }, is_var),
                    ref k => return Err(format!("Type of function {:?} was not a function type, got {:?}", decl_id, k))
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

                let converted_function =
                    self.convert_function(s, is_extern, is_inline, is_main, is_var,
                                          new_name, name, &args, ret, body);

                converted_function.or_else(|e|
                    match self.tcfg.replace_unsupported_decls {
                        ReplaceMode::Extern if body.is_none() =>
                            self.convert_function(s, is_extern, false, is_main, is_var,
                                                  new_name, name, &args, ret, None),
                        _ => Err(e),
                    })
            },

            CDeclKind::Typedef { ref typ, .. } => {
                let new_name = &self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                let ty = self.convert_type(typ.ctype)?;
                Ok(ConvertedDecl::Item(mk().span(s).pub_().type_item(new_name, ty)))
            },

            // Extern variable without intializer (definition elsewhere)
            CDeclKind::Variable { is_extern: true, is_static, is_defn: false, ref ident, initializer, typ } => {
                assert!(is_static, "An extern variable must be static");
                assert!(initializer.is_none(), "An extern variable that isn't a definition can't have an initializer");

                let new_name = self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");
                let (ty, mutbl, _) = self.convert_variable(None, typ, is_static)?;

                let extern_item = mk_linkage(true, &new_name, ident)
                    .span(s)
                    .set_mutbl(mutbl)
                    .foreign_static(&new_name, ty);

                Ok(ConvertedDecl::ForeignItem(extern_item))
            }

            // Extern variable with initializer (definition here)
            CDeclKind::Variable { is_extern: true, is_static, ref ident, initializer, typ, .. } => {
                assert!(is_static, "An extern variable must be static");

                let new_name = &self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");

                // Collect problematic static initializers and offload them to sections for the linker
                // to initialize for us
                if is_static && self.static_initializer_is_uncompilable(initializer) {
                    // Note: We don't pass is_static through here. Extracted initializers
                    // are run outside of the static initializer.
                    let (ty, _, init) = self.convert_variable(initializer, typ, false)?;

                    let mut init = init?;
                    init.stmts.push(mk().expr_stmt(init.val));
                    let init = mk().block(init.stmts);
                    let mut init = mk().block_expr(init);

                    let comment = String::from("// Initialized in run_static_initializers");
                    // REVIEW: We might want to add the comment to the original span comments
                    s = self.comment_store.borrow_mut().add_comment_lines(vec![comment]);

                    self.add_static_initializer_to_section(new_name, typ, &mut init)?;

                    Ok(ConvertedDecl::Item(mk_linkage(false, new_name, ident)
                    .span(s)
                    .pub_()
                    .abi("C")
                    .mutbl()
                    .static_item(new_name, ty, init)))
                } else {
                    let (ty, _, init) = self.convert_variable(initializer, typ, is_static)?;

                    let mut init = init?;
                    init.stmts.push(mk().expr_stmt(init.val));
                    let init = mk().unsafe_().block(init.stmts);
                    let mut init = mk().block_expr(init);

                    // Force mutability due to the potential for raw pointers occuring in the type
                    // and because we're assigning to these variables in the external initializer
                    Ok(ConvertedDecl::Item(mk_linkage(false, new_name, ident)
                        .span(s)
                        .pub_()
                        .abi("C")
                        .mutbl()
                        .static_item(new_name, ty, init)))
                }
            }

            // Static variable (definition here)
            CDeclKind::Variable { is_static: true, initializer, typ, .. } => {
                let new_name = &self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");
                let (ty, _, init) = self.convert_variable(initializer, typ, true)?;

                // Conservatively assume that some aspect of the initializer is unsafe
                let mut init = init?;
                init.stmts.push(mk().expr_stmt(init.val));
                let init = mk().unsafe_().block(init.stmts);
                let mut init = mk().block_expr(init);

                // Collect problematic static initializers and offload them to sections for the linker
                // to initialize for us
                if self.static_initializer_is_uncompilable(initializer) {
                    let comment = String::from("// Initialized in run_static_initializers");
                    // REVIEW: We might want to add the comment to the original span comments
                    s = self.comment_store.borrow_mut().add_comment_lines(vec![comment]);

                    self.add_static_initializer_to_section(new_name, typ, &mut init)?;
                }

                // Force mutability due to the potential for raw pointers occurring in the type
                // and because we're assigning to these variables in the external initializer
                Ok(ConvertedDecl::Item(mk().span(s).mutbl().static_item(new_name, ty, init)))
            }

            CDeclKind::Variable { .. } => Err(format!("This should be handled in 'convert_decl_stmt'")),

            //ref k => Err(format!("Translation not implemented for {:?}", k)),
        }
    }

    fn convert_function(
        &self,
        span: Span,
        is_extern: bool,
        is_inline: bool,
        is_main: bool,
        is_variadic: bool,
        new_name: &str,
        name: &str,
        arguments: &[(CDeclId, String, CQualTypeId)],
        return_type: Option<CQualTypeId>,
        body: Option<CStmtId>,
    ) -> Result<ConvertedDecl, String> {

        if is_variadic && body.is_some() {
            let message = format!(
                "Failed to translate {}; variadic function implementations not supported",
                name);
            return Err(message);
        }

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

            let ret = match return_type {
                Some(return_type) => self.convert_type(return_type.ctype)?,
                None => mk().never_ty(),
            };
            let ret = FunctionRetTy::Ty(ret);

            let decl = mk().fn_decl(args, ret, is_variadic);


            if let Some(body) = body {
                // Translating an actual function

                let ret = match return_type {
                    Some(return_type) => {
                        let ret_type_id: CTypeId = self.ast_context.resolve_type_id(return_type.ctype);
                        if let CTypeKind::Void = self.ast_context.index(ret_type_id).kind {
                            cfg::ImplicitReturnType::Void
                        } else if is_main {
                            cfg::ImplicitReturnType::Main
                        } else {
                            cfg::ImplicitReturnType::NoImplicitReturnType
                        }
                    }
                    _ => cfg::ImplicitReturnType::Void,
                };

                let mut body_stmts = vec![];
                for &(_, _, typ) in arguments {
                    body_stmts.append(&mut self.compute_variable_array_sizes(typ.ctype)?);
                }

                let body_ids = match self.ast_context.index(body).kind {
                    CStmtKind::Compound(ref stmts) => stmts,
                    _ => panic!("function body expects to be a compound statement"),
                };
                body_stmts.append(&mut self.convert_function_body(name, body_ids, ret)?);
                let block = stmts_block(body_stmts);

                // Only add linkage attributes if the function is `extern`
                let mk_ = if is_main {
                    // Cross-check this function as if it was called `main`
                    // FIXME: pass in a vector of NestedMetaItem elements,
                    // but strings have to do for now
                    self.mk_cross_check(mk(), vec!["entry(djb2=\"main\")",
                                                   "exit(djb2=\"main\")"])
                } else if is_extern && !is_inline {
                    mk_linkage(false, new_name, name)
                        .abi("C")
                        .pub_()
                } else {
                    mk().abi("C")
                };

                Ok(ConvertedDecl::Item(mk_.span(span).unsafe_().fn_item(new_name, decl, block)))
            } else {
                // Translating an extern function declaration

                let function_decl = mk_linkage(true, new_name, name)
                    .span(span)
                    .foreign_fn(new_name, decl);

                Ok(ConvertedDecl::ForeignItem(function_decl))
            }
        })
    }

    fn convert_function_body(
        &self,
        name: &str,
        body_ids: &[CStmtId],
        ret: cfg::ImplicitReturnType,
    ) -> Result<Vec<Stmt>, String> {

        // Function body scope
        self.with_scope(|| {
            if self.tcfg.reloop_cfgs || self.function_requires_relooper(body_ids) {
                let (graph, store) = cfg::Cfg::from_stmts(self, body_ids, ret)?;

                if self.tcfg.dump_function_cfgs {
                    graph
                        .dump_dot_graph(
                            &self.ast_context, &store,
                            self.tcfg.dump_cfg_liveness,
                            self.tcfg.use_c_loop_info,
                            format!("{}_{}.dot", "cfg", name)
                        )
                        .expect("Failed to write CFG .dot file");
                }
                if self.tcfg.json_function_cfgs {
                    graph.dump_json_graph(&store, format!("{}_{}.json", "cfg", name))
                        .expect("Failed to write CFG .json file");
                }

                let (lifted_stmts, relooped) = cfg::relooper::reloop(
                    graph,
                    store,
                    self.tcfg.simplify_structures,
                    self.tcfg.use_c_loop_info,
                    self.tcfg.use_c_multiple_info,
                );

                if self.tcfg.dump_structures {
                    eprintln!("Relooped structures:");
                    for s in &relooped {
                        eprintln!("  {:#?}", s);
                    }
                }

                let current_block_ident = self.renamer.borrow_mut().pick_name("current_block");
                let current_block = mk().ident_expr(&current_block_ident);
                let mut stmts: Vec<Stmt> = lifted_stmts;
                if cfg::structures::has_multiple(&relooped) {

                    if self.tcfg.fail_on_multiple {
                        panic!("Uses of `current_block' are illegal with `--fail-on-multiple'.");
                    }

                    let current_block_ty = if self.tcfg.debug_relooper_labels {
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
                    &mut self.comment_store.borrow_mut(),
                    current_block,
                    self.tcfg.debug_relooper_labels
                )?);
                Ok(stmts)
            } else {
                let mut res = vec![];
                for &stmt in body_ids {
                    res.append(&mut self.convert_stmt(stmt)?)
                }
                Ok(res)
            }
        })
    }

    fn convert_stmt(&self, stmt_id: CStmtId) -> Result<Vec<Stmt>, String> {
        let s = {
            let stmt_cmt = self.comment_context.borrow_mut().remove_stmt_comment(stmt_id);
            self.comment_store.borrow_mut().add_comment_lines(stmt_cmt)
        };

        match self.ast_context.index(stmt_id).kind {
            CStmtKind::Empty => Ok(vec![]),

            CStmtKind::Decls(ref decls) => {
                let mut res = vec![];
                for decl in decls {
                    res.append(&mut self.convert_decl_stmt(*decl)?)
                }
                Ok(res)
            },

            CStmtKind::Return(expr) =>
                self.convert_return_stmt(s, expr),

            CStmtKind::If { scrutinee, true_variant, false_variant } =>
                self.convert_if_stmt(s, scrutinee, true_variant, false_variant),

            CStmtKind::While { condition, body } =>
                self.convert_while_stmt(s, condition, body),

            CStmtKind::DoWhile { body, condition } =>
                self.convert_do_stmt(s, body, condition),

            CStmtKind::ForLoop { init, condition, increment, body } =>
                self.convert_for_stmt(s, init, condition, increment, body),

            CStmtKind::Compound(ref stmts) => {
                self.with_scope(|| {
                    let mut res = vec![];
                    for stmt in stmts {
                        res.append(&mut self.convert_stmt(*stmt)?)
                    }

                    Ok(vec![mk().expr_stmt(mk().block_expr(stmts_block(res)))])
                })
            },

            CStmtKind::Expr(expr) =>
                Ok(self.convert_expr(ExprUse::Unused, expr, false, DecayRef::Default)?.stmts),

            CStmtKind::Break => {
                let mut loop_ = self.loops.current_loop_mut();
                loop_.has_break = true;
                let loop_label = loop_.get_or_create_label(&self.loops).to_owned();
                Ok(vec![mk().span(s).expr_stmt(mk().break_expr(Some(loop_label)))])
            },

            CStmtKind::Continue => {
                let mut loop_ = self.loops.current_loop_mut();
                loop_.has_continue = true;
                match loop_.loop_type {
                    LoopType::While => {
                        // We can translate C continue in a while loop
                        // directly to Rust's continue
                        let loop_label = loop_.get_or_create_label(&self.loops).to_owned();
                        Ok(vec![mk().span(s).expr_stmt(mk().continue_expr(Some(loop_label)))])
                    },
                    _ => {
                        // We translate all other C continue statements
                        // to a break from the inner body loop
                        let body_label = loop_.get_or_create_body_label(&self.loops).to_owned();
                        Ok(vec![mk().span(s).expr_stmt(mk().break_expr(Some(body_label)))])
                    },
                }
            },

            CStmtKind::Asm{is_volatile, ref asm, ref inputs, ref outputs, ref clobbers} => {
                self.convert_asm(s, is_volatile, asm, inputs, outputs, clobbers)
            }

            ref stmt => Err(format!("convert_stmt {:?}", stmt)),
        }
    }

    pub fn convert_asm
        (&self,
         span: Span,
         is_volatile: bool,
         asm: &str,
         inputs: &[AsmOperand],
         outputs: &[AsmOperand],
         clobbers: &[String])
        -> Result<Vec<Stmt>, String> {

        if !self.tcfg.translate_asm {
            return Err(format!("Inline assembly not enabled, to enable use --translate-asm"))
        }

        self.use_feature("asm");

        fn push_expr(tokens: &mut Vec<Token>, expr: P<Expr>) {
            tokens.push(Token::interpolated(Nonterminal::NtExpr(expr)));
        }

        let mut stmts: Vec<Stmt> = vec![];
        let mut tokens: Vec<Token> = vec![];
        let mut first;

        // Assembly template
        push_expr(&mut tokens, mk().lit_expr(mk().str_lit(asm)));

        // Outputs and Inputs
        for list in vec![outputs, inputs] {

            first = true;
            tokens.push(Token::Colon); // Always emitted, even if list is empty

            for &AsmOperand { ref constraints, expression } in list {
                if first { first = false } else { tokens.push(Token::Comma) }

                let mut result = self.convert_expr(ExprUse::LValue, expression, false, DecayRef::Default)?;
                stmts.append(&mut result.stmts);

                push_expr(&mut tokens, mk().lit_expr(mk().str_lit(constraints)));
                push_expr(&mut tokens, mk().paren_expr(result.val));
            }
        }

        // Clobbers
        first = true;
        tokens.push(Token::Colon);
        for clobber in clobbers {
            if first { first = false } else { tokens.push(Token::Comma) }
            push_expr(&mut tokens, mk().lit_expr(mk().str_lit(clobber)));
        }

        // Options
        if is_volatile {
            tokens.push(Token::Colon);
            push_expr(&mut tokens, mk().lit_expr(mk().str_lit("volatile")));
        }

        let mac = mk().mac(vec!["asm"], tokens.into_iter().collect::<TokenStream>());
        let mac = mk().mac_expr(mac);
        let mac = mk().span(span).expr_stmt(mac);
        stmts.push(mac);

        Ok(stmts)
    }

    /// Convert a C expression to a rust boolean expression
    pub fn convert_condition(&self, target: bool, cond_id: CExprId, is_static: bool) -> Result<WithStmts<P<Expr>>, String> {
        let ty_id = self.ast_context[cond_id].kind.get_type().ok_or_else(|| format!("bad condition type"))?;

        let null_pointer_case =
            |negated: bool, ptr: CExprId| -> Result<WithStmts<P<Expr>>, String> {
                    let val = self.convert_expr(ExprUse::RValue, ptr, is_static, DecayRef::Yes)?;
                    let ptr_type = self.ast_context[ptr].kind.get_type().ok_or_else(|| format!("bad pointer type for condition"))?;
                    Ok(val.map(|e| {
                        if self.ast_context.is_function_pointer(ptr_type) {
                            if negated {
                                mk().method_call_expr(e, "is_some", vec![] as Vec<P<Expr>>)
                            } else {
                                mk().method_call_expr(e, "is_none", vec![] as Vec<P<Expr>>)
                            }
                        } else {
                            let is_null = mk().method_call_expr(e, "is_null", vec![] as Vec<P<Expr>>);
                            if negated {
                                mk().unary_expr(ast::UnOp::Not, is_null)
                            } else {
                                is_null
                            }
                        }
                    }))
                };

        match self.ast_context[cond_id].kind {
            CExprKind::Binary(_, c_ast::BinOp::EqualEqual, null_expr, ptr, _, _)
            if self.ast_context.is_null_expr(null_expr) => {
                null_pointer_case(!target, ptr)
            }

            CExprKind::Binary(_, c_ast::BinOp::EqualEqual, ptr, null_expr, _, _)
            if self.ast_context.is_null_expr(null_expr) => {
                null_pointer_case(!target, ptr)
            }

            CExprKind::Binary(_, c_ast::BinOp::NotEqual, null_expr, ptr, _, _)
            if self.ast_context.is_null_expr(null_expr) => {
                null_pointer_case(target, ptr)
            }

            CExprKind::Binary(_, c_ast::BinOp::NotEqual, ptr, null_expr, _, _)
            if self.ast_context.is_null_expr(null_expr) => {
                null_pointer_case(target, ptr)
            }

            CExprKind::Unary(_, c_ast::UnOp::Not, subexpr_id, _) => {
                self.convert_condition(!target, subexpr_id, is_static)
            }

            _ => {
                // DecayRef could (and probably should) be Default instead of Yes here; however, as noted
                // in https://github.com/rust-lang/rust/issues/53772, you cant compare a reference (lhs) to
                // a ptr (rhs) (even though the reverse works!). We could also be smarter here and just
                // specify Yes for that particular case, given enough analysis.
                let val = self.convert_expr(ExprUse::RValue, cond_id, is_static, DecayRef::Yes)?;
                Ok(val.map(|e| self.match_bool(target, ty_id, e)))
            }
        }
    }

    fn convert_while_stmt(&self, span: Span, cond_id: CExprId, body_id: CStmtId) -> Result<Vec<Stmt>, String> {
        let cond = self.convert_condition(true, cond_id, false)?;

        self.loops.push_loop(LoopType::While);
        let body_res = self.convert_stmt(body_id);
        let loop_ = self.loops.pop_loop();
        let body = body_res?;

        let rust_cond = cond.to_expr();
        let rust_body = stmts_block(body);

        Ok(vec![mk().span(span).expr_stmt(mk().while_expr(rust_cond, rust_body, loop_.label))])
    }

    fn convert_do_stmt(&self, span: Span, body_id: CStmtId, cond_id: CExprId) -> Result<Vec<Stmt>, String> {
        let cond = self.convert_condition(false, cond_id, false)?;
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

        Ok(vec![mk().span(span).semi_stmt(mk().loop_expr(rust_body, loop_.label))])
    }

    fn convert_for_stmt(
        &self,
        span: Span,
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
                Some(i) => self.convert_expr(ExprUse::Unused, i, false, DecayRef::Default)?.stmts,
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
                Some(i) => mk().while_expr(self.convert_condition(true, i, false)?.to_expr(), body_block, loop_.label), // while
            };

            init.push(mk().expr_stmt(looper));

            Ok(vec![mk().span(span).expr_stmt(mk().block_expr(mk().block(init)))])
        })
    }

    fn convert_if_stmt(
        &self,
        span: Span,
        cond_id: CExprId,
        then_id: CStmtId,
        else_id: Option<CStmtId>
    ) -> Result<Vec<Stmt>, String> {
        let mut cond = self.convert_condition(true, cond_id, false)?;
        let then_stmts = stmts_block(self.convert_stmt(then_id)?);
        let else_stmts = match else_id {
            None => None,
            Some(x) => {
                let stmt = self.convert_stmt(x)?;
                Some(mk().block_expr(stmts_block(stmt)))
            }
        };

        let ifte = mk().ifte_expr(cond.val, then_stmts, else_stmts);
        cond.stmts.push(mk().span(span).semi_stmt(ifte));
        Ok(cond.stmts)
    }

    fn convert_return_stmt(&self, span: Span, result_id: Option<CExprId>) -> Result<Vec<Stmt>, String> {
        let val: Option<WithStmts<P<Expr>>> =
            sequence_option(result_id
                .map(|i| self.convert_expr(ExprUse::RValue, i, false, DecayRef::Default))
            )?;
        let mut ws = with_stmts_opt(val);
        let ret = mk().span(span).expr_stmt(mk().return_expr(ws.val));

        ws.stmts.push(ret);
        Ok(ws.stmts)
    }

    pub fn convert_decl_stmt(&self, decl_id: CDeclId) -> Result<Vec<Stmt>, String> {

        match self.convert_decl_stmt_info(decl_id)? {
            cfg::DeclStmtInfo { decl_and_assign: Some(d), .. } => Ok(d),
            _ => Err(format!("convert_decl_stmt: couldn't get declaration and initialization info"))
        }
    }

    /// Search for references to the given declaration in a value position
    /// inside the given expression. Uses of the declaration inside typeof
    /// operations are ignored because our translation will ignore them
    /// and use the computed types instead.
    fn has_decl_reference(&self, decl_id: CDeclId, expr_id: CExprId) -> bool {
        let mut iter = DFExpr::new(&self.ast_context, expr_id.into());
        while let Some(x) = iter.next() {
            match x {
                SomeId::Expr(e) => {
                    match self.ast_context[e].kind {
                        CExprKind::DeclRef(_, d, _) if d == decl_id => return true,
                        CExprKind::UnaryType(_, _, Some(_), _) => iter.prune(1),
                        _ => {}
                    }
                }
                SomeId::Type(t) => {
                    if let CTypeKind::TypeOfExpr(_) = self.ast_context[t].kind {
                        iter.prune(1);
                    }
                }
                _ => {}
            }
        }
        false
    }

    pub fn convert_decl_stmt_info(&self, decl_id: CDeclId) -> Result<cfg::DeclStmtInfo, String> {
        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable { ref ident, is_static: true, is_extern: false, is_defn: true, initializer, typ, .. } => {
                if self.static_initializer_is_uncompilable(initializer) {
                    let err_msg = || String::from("Unable to rename function scoped static initializer");
                    let ident2 = self.renamer.borrow_mut().insert_root(decl_id, ident).ok_or_else(err_msg)?;
                    let (ty, _, init) = self.convert_variable(initializer, typ, true)?;
                    let default_init = self.implicit_default_expr(typ.ctype, true)?;
                    let comment = String::from("// Initialized in run_static_initializers");
                    let span = self.comment_store.borrow_mut().add_comment_lines(vec![comment]);
                    let static_item = mk().span(span).mutbl().static_item(&ident2, ty, default_init);
                    let mut init = init?;

                    init.stmts.push(mk().expr_stmt(init.val));

                    let init = mk().unsafe_().block(init.stmts);
                    let mut init = mk().block_expr(init);

                    self.add_static_initializer_to_section(&ident2, typ, &mut init)?;
                    self.items.borrow_mut().push(static_item);

                    return Ok(cfg::DeclStmtInfo::new(Vec::new(), Vec::new(), Vec::new()));
                }
            },
            _ => {},
        };

        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable { is_static, is_extern, is_defn, ref ident, initializer, typ } if !is_static && !is_extern => {
                assert!(is_defn, "Only local variable definitions should be extracted");

                let has_self_reference =
                    if let Some(expr_id) = initializer {
                        self.has_decl_reference(decl_id, expr_id)
                    } else {
                        false
                    };

                let mut stmts = self.compute_variable_array_sizes(typ.ctype)?;

                let rust_name = self.renamer.borrow_mut()
                    .insert(decl_id, &ident)
                    .expect(&format!("Failed to insert variable '{}'", ident));
                let (ty, mutbl, init) = self.convert_variable(initializer, typ, is_static)?;
                let mut init = init?;

                stmts.append(&mut init.stmts);

                if has_self_reference {
                    let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name.clone());
                    let zeroed = self.implicit_default_expr(typ.ctype, is_static)?;
                    let local_mut = mk().local(pat_mut, Some(ty), Some(zeroed));

                    let assign = mk().assign_expr(mk().ident_expr(rust_name), init.val);

                    let mut assign_stmts = stmts.clone();
                    assign_stmts.push(mk().semi_stmt(assign.clone()));

                    let mut decl_and_assign = vec![mk().local_stmt(P(local_mut.clone()))];
                    decl_and_assign.append(&mut stmts);
                    decl_and_assign.push(mk().expr_stmt(assign));

                    Ok(cfg::DeclStmtInfo::new(
                        vec![mk().local_stmt(P(local_mut))],
                        assign_stmts,
                        decl_and_assign,
                    ))
                } else {
                    let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name.clone());
                    let zeroed = self.implicit_default_expr(typ.ctype, is_static)?;
                    let local_mut = mk().local(pat_mut, Some(ty.clone()), Some(zeroed));

                    let pat = mk().set_mutbl(mutbl).ident_pat(rust_name.clone());

                    let type_annotation = if self.tcfg.reduce_type_annotations &&
                                            !self.should_assign_type_annotation(typ.ctype, initializer) {
                        None
                    } else {
                        Some(ty)
                    };

                    let local = mk().local(pat, type_annotation, Some(init.val.clone()));
                    let assign = mk().assign_expr(mk().ident_expr(rust_name), init.val);

                    let mut assign_stmts = stmts.clone();
                    assign_stmts.push(mk().semi_stmt(assign));

                    let mut decl_and_assign = stmts;
                    decl_and_assign.push(mk().local_stmt(P(local)));

                    Ok(cfg::DeclStmtInfo::new(
                        vec![mk().local_stmt(P(local_mut))],
                        assign_stmts,
                        decl_and_assign,
                    ))
                }
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
                    ))
                } else {
                    let item = match self.convert_decl(false, decl_id)? {
                        ConvertedDecl::Item(item) => item,
                        ConvertedDecl::ForeignItem(item) => mk().abi("C").foreign_items(vec![item]),
                    };

                    Ok(cfg::DeclStmtInfo::new(
                        vec![mk().item_stmt(item.clone())],
                        vec![],
                        vec![mk().item_stmt(item)],
                    ))
                }
            },
        }
    }

    fn should_assign_type_annotation(&self, ctypeid: CTypeId, initializer: Option<CExprId>) -> bool {
        let initializer_kind = initializer.map(|expr_id| &self.ast_context[expr_id].kind);

        // If the RHS is a func call, we should be able to skip type annotation
        // because we get a type from the function return type
        if let Some(CExprKind::Call(_, _, _)) = initializer_kind {
            return false;
        }

        match self.ast_context.resolve_type(ctypeid).kind {
            CTypeKind::Pointer(CQualTypeId { ctype, .. }) => {
                match self.ast_context.resolve_type(ctype).kind {
                    CTypeKind::Function(_, _, _, _) => {
                        // Fn pointers need to be type annotated if null
                        if initializer.is_none() {
                            return true;
                        }

                        // None assignments don't prove enough type information unless there are follow-up assignments
                        if let Some(CExprKind::ImplicitCast(_, _, CastKind::NullToPointer, _, _)) = initializer_kind {
                            return true;
                        }

                        // We could set this to false and skip non null fn ptr annotations. This will work
                        // 99% of the time, however there is a strange case where fn ptr comparisons
                        // complain PartialEq is not implemented for the type inferred function type,
                        // but the identical type that is explicitly defined doesn't seem to have that issue
                        // Probably a rustc bug. See https://github.com/rust-lang/rust/issues/53861
                        true
                    },
                    _ => {
                        // Non function null ptrs provide enough information to skip
                        // type annotations; ie `= 0 as *const MyStruct;`
                        if initializer.is_none() {
                            return false;
                        }

                        if let Some(CExprKind::ImplicitCast(_, _, cast_kind, _, _)) = initializer_kind {
                            match cast_kind {
                                CastKind::NullToPointer => return false,
                                CastKind::ConstCast => return true,
                                _ => {},
                            };
                        }

                        // ref decayed ptrs generally need a type annotation
                        if let Some(CExprKind::Unary(_, c_ast::UnOp::AddressOf, _, _)) = initializer_kind {
                            return true;
                        }

                        false
                    },
                }
            },
            // For some reason we don't seem to apply type suffixes when 0-initializing
            // so type annotation is need for 0-init ints and floats at the moment, but
            // they could be simplified in favor of type suffixes
            CTypeKind::Bool | CTypeKind::Char | CTypeKind::SChar |
            CTypeKind::Short | CTypeKind::Int | CTypeKind::Long | CTypeKind::LongLong |
            CTypeKind::UChar | CTypeKind::UShort | CTypeKind::UInt | CTypeKind::ULong |
            CTypeKind::ULongLong | CTypeKind::LongDouble | CTypeKind::Int128 |
            CTypeKind::UInt128 => initializer.is_none(),
            CTypeKind::Float | CTypeKind::Double => initializer.is_none(),
            CTypeKind::Struct(_) |
            CTypeKind::Union(_) |
            CTypeKind::Enum(_) => false,
            CTypeKind::Function(_, _, _, _) => unreachable!("Can't have a function directly as a type"),
            CTypeKind::Typedef(_) => unreachable!("Typedef should be expanded though resolve_type"),
            _ => true,
        }
    }

    fn convert_variable(
        &self,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
        is_static: bool,
    ) -> Result<(P<Ty>, Mutability, Result<WithStmts<P<Expr>>,String>), String> {
        let init = match initializer {
            Some(x) => self.convert_expr(ExprUse::RValue, x, is_static, DecayRef::Default),
            None => self.implicit_default_expr(typ.ctype, is_static).map(WithStmts::new),
        };

        // Variable declarations for variable-length arrays use the type of a pointer to the
        // underlying array element
        let ty = if let CTypeKind::VariableArray(mut elt, _) = self.ast_context.resolve_type(typ.ctype).kind {
            elt = self.variable_array_base_type(elt);
            let ty = self.convert_type(elt)?;
            mk().path_ty(vec![mk().path_segment_with_params("Vec", mk().angle_bracketed_param_types(vec![ty]))])
        } else {
            self.convert_type(typ.ctype)?
        };

        let mutbl = if typ.qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };

        Ok((ty, mutbl, init))
    }

    fn convert_type(&self, type_id: CTypeId) -> Result<P<Ty>, String> {
        self.type_converter.borrow_mut().convert(&self.ast_context, type_id)
    }

    /// Construct an expression for a NULL at any type, including forward declarations,
    /// function pointers, and normal pointers.
    fn null_ptr(&self, type_id: CTypeId, is_static: bool) -> Result<P<Expr>, String> {

        if self.ast_context.is_function_pointer(type_id) {
            return Ok(mk().path_expr(vec!["None"]))
        }

        let pointee = match self.ast_context.resolve_type(type_id).kind {
            CTypeKind::Pointer(pointee) => pointee,
            _ => return Err(format!("null_ptr requires a pointer")),
        };
        let ty = self.convert_type(type_id)?;
        let mut zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
        if is_static && !pointee.qualifiers.is_const {
            let mut qtype = pointee;
            qtype.qualifiers.is_const = true;
            let ty_ = self.type_converter.borrow_mut().convert_pointer(&self.ast_context, qtype)?;
            zero = mk().cast_expr(zero, ty_);
        }
        Ok(mk().cast_expr(zero, ty))
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

        // We explicitly annotate the type of pointer we're reading from
        // in order to avoid omitted bit-casts to const from causing the
        // wrong type to be inferred via the result of the pointer.
        let mut path_parts: Vec<PathSegment> = vec![];
        for elt in vec!["", "std", "ptr"] {
            path_parts.push(mk().path_segment(elt))
        }
        let elt_ty = self.convert_type(lhs_type.ctype)?;
        let ty_params = mk().angle_bracketed_param_types(vec![elt_ty]);
        let elt = mk().path_segment_with_params("read_volatile", ty_params);
        path_parts.push(elt);

        let read_volatile_expr = mk().path_expr(path_parts);
        Ok(mk().call_expr(read_volatile_expr, vec![addr_lhs]))
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
                CExprKind::Unary(_type_id, c_ast::UnOp::AddressOf, inner_expr_id, _) if level == 0  => {
                    _get_declref_type(ast_context, inner_expr_id, level + 1)
                }
                CExprKind::ImplicitCast(_type_id, inner_expr_id, CastKind::FunctionToPointerDecay, _field_id, _) if level == 0 => {
                    _get_declref_type(ast_context, inner_expr_id, level + 1)
                }
                // level 1 arms
                CExprKind::DeclRef(_type_id, decl_id, _) if level == 1 => {
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
        match self.ast_context.resolve_type(type_id).kind {
            CTypeKind::VariableArray(elts, Some(counts)) => {
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

    /// Variable element arrays are represented by a flat array of non-variable-length array
    /// elements. This function traverses potentially multiple levels of variable-length array
    /// to find the underlying element type.
    fn variable_array_base_type(&self, mut elt: CTypeId) -> CTypeId {
        while let CTypeKind::VariableArray(elt_, _) = self.ast_context.resolve_type(elt).kind {
            elt = elt_;
        }
        elt
    }

    /// This generates variables that store the computed sizes of the variable-length arrays in
    /// the given type.
    pub fn compute_variable_array_sizes(&self, mut type_id: CTypeId) -> Result<Vec<Stmt>, String> {

        let mut stmts = vec![];

        loop {
            match self.ast_context.resolve_type(type_id).kind {
                CTypeKind::Pointer(elt) => type_id = elt.ctype,
                CTypeKind::ConstantArray(elt, _) => type_id = elt,
                CTypeKind::VariableArray(elt, Some(expr_id)) => {
                    type_id = elt;

                    // Convert this expression
                    let mut expr = self.convert_expr(ExprUse::RValue, expr_id, false, DecayRef::Default)?;
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
        if let CTypeKind::VariableArray(elts, len) =
            self.ast_context.resolve_type(type_id).kind {

            let len = len.expect("Sizeof a VLA type with count expression omitted");

            let mut elts = self.compute_size_of_type(elts)?;
            let mut len = self.convert_expr(ExprUse::RValue, len, false, DecayRef::Default)?;

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

        type_id = self.variable_array_base_type(type_id);

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
    pub fn convert_expr(&self, use_: ExprUse, expr_id: CExprId, is_static: bool, decay_ref: DecayRef) -> Result<WithStmts<P<Expr>>, String> {
        match self.ast_context[expr_id].kind {
            CExprKind::DesignatedInitExpr(..) => Err(format!("Unexpected designated init expr")),
            CExprKind::BadExpr => Err(format!("convert_expr: expression kind not supported")),
            CExprKind::ShuffleVector(..) => Err(format!("shuffle vector not supported")),
            CExprKind::ConvertVector(..) => Err(format!("convert vector not supported")),

            CExprKind::UnaryType(_ty, kind, opt_expr, arg_ty) => {
                let result = match kind {
                    UnTypeOp::SizeOf =>
                        match opt_expr {
                            None => self.compute_size_of_type(arg_ty.ctype)?,
                            Some(_) =>
                                {
                                    let inner = self.variable_array_base_type(arg_ty.ctype);
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

            CExprKind::DeclRef(qual_ty, decl_id, _) => {
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

                if let CTypeKind::VariableArray(..) = self.ast_context.resolve_type(qual_ty.ctype).kind {
                    val = mk().method_call_expr(val, "as_mut_ptr", vec![] as Vec<P<Expr>>);
                }

                Ok(WithStmts::new(val))
            }

            CExprKind::OffsetOf(ty, val) =>
                Ok(WithStmts::new(self.mk_int_lit(ty, val, IntBase::Dec))),

            CExprKind::Literal(ty, CLiteral::Integer(val, base)) =>
                Ok(WithStmts::new(self.mk_int_lit(ty, val, base))),

            CExprKind::Literal(_, CLiteral::Character(val)) => {
                let expr = match char::from_u32(val as u32) {
                    Some(c) => {
                        let lit = mk().char_lit(c);
                        let expr = mk().lit_expr(lit);
                        let i32_type = mk().path_ty(vec!["i32"]);
                        mk().cast_expr(expr, i32_type)
                    }
                    None => {
                        // Fallback for characters outside of the valid Unicode range
                        let lit = mk().int_lit(val as u128, LitIntType::Signed(IntTy::I32));
                        mk().lit_expr(lit)
                    }
                };
                Ok(WithStmts::new(expr))
            }

            CExprKind::Literal(ty, CLiteral::Floating(val)) => {
                let mut bytes: Vec<u8> = vec![];
                dtoa::write(&mut bytes, val).unwrap();
                let str = String::from_utf8(bytes).unwrap();
                let float_ty = match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::LongDouble => FloatTy::F64,
                    CTypeKind::Double => FloatTy::F64,
                    CTypeKind::Float => FloatTy::F32,
                    ref k => panic!("Unsupported floating point literal type {:?}", k),
                };
                Ok(WithStmts::new(mk().lit_expr(mk().float_lit(str, float_ty))))
            }

            CExprKind::Literal(ty, CLiteral::String(ref val, width)) => {
                let mut val = val.to_owned();

                match self.ast_context.resolve_type(ty.ctype).kind {
                    // Match the literal size to the expected size padding with zeros as needed
                    CTypeKind::ConstantArray(_, size) => val.resize(size*(width as usize),0),

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

            CExprKind::ImplicitCast(ty, expr, kind, opt_field_id, _) =>
                self.convert_cast(use_, ty, expr, kind, opt_field_id, false, is_static, decay_ref),

            CExprKind::ExplicitCast(ty, expr, kind, opt_field_id, _) =>
                self.convert_cast(use_, ty, expr, kind, opt_field_id, true, is_static, decay_ref),

            CExprKind::Unary(type_id, op, arg, _) =>
                self.convert_unary_operator(use_, op, type_id, arg, is_static, decay_ref),

            CExprKind::Conditional(_, cond, lhs, rhs) => {
                let cond = self.convert_condition(true, cond, is_static)?;

                let lhs = self.convert_expr(use_, lhs, is_static, decay_ref)?;
                let rhs = self.convert_expr(use_, rhs, is_static, decay_ref)?;

                if use_ == ExprUse::Unused {
                    let then: P<Block> = mk().block(lhs.stmts);
                    let els: P<Expr> = mk().block_expr(mk().block(rhs.stmts));

                    Ok(cond.and_then(|c| WithStmts {
                        stmts: vec![mk().semi_stmt(mk().ifte_expr(c, then, Some(els)))],
                        val: self.panic("Conditional expression is not supposed to be used"),
                    }))
                } else {
                    let then: P<Block> = lhs.to_block();
                    let els: P<Expr> = rhs.to_expr();

                    Ok(cond.map(|c| mk().ifte_expr(c, then, Some(els))))
                }
            },

            CExprKind::BinaryConditional(ty, lhs, rhs) => {
                if use_ == ExprUse::Unused {
                    let mut lhs = self.convert_condition(false, lhs, is_static)?;

                    lhs.stmts.push(
                        mk().semi_stmt(
                            mk().ifte_expr(lhs.val,
                                           mk().block(self.convert_expr(ExprUse::Unused,
                                                                        rhs, is_static, decay_ref)?.stmts),
                                           None as Option<P<Expr>>)));
                    Ok(WithStmts {
                        stmts: lhs.stmts,
                        val: self.panic("Binary conditional expression is not supposed to be used"),
                    })
                } else {
                    self.name_reference_write_read(lhs)?.result_map(|(_, lhs_val)| {
                        let cond = self.match_bool(true, ty.ctype, lhs_val.clone());
                        let ite = mk().ifte_expr(cond,
                                       mk().block(vec![mk().expr_stmt(lhs_val)]),
                                       Some(self.convert_expr(use_, rhs, is_static, decay_ref)?.to_expr()));
                        Ok(ite)
                    })
                }
            },

            CExprKind::Binary(type_id, op, lhs, rhs, opt_lhs_type_id, opt_res_type_id) => {
                match op {
                    c_ast::BinOp::Comma => {

                        // The value of the LHS of a comma expression is always discarded
                        let lhs = self.convert_expr(ExprUse::Unused, lhs, is_static, decay_ref)?;
                        let rhs = self.convert_expr(use_, rhs, is_static, decay_ref)?;

                        Ok(WithStmts {
                            stmts: lhs.stmts.into_iter().chain(rhs.stmts).collect(),
                            val: rhs.val,
                        })
                    }

                    c_ast::BinOp::And => {
                        let lhs = self.convert_condition(true, lhs, is_static)?;
                        let rhs = self.convert_condition(true, rhs, is_static)?;
                        let mut out = lhs.map(|x| bool_to_int(mk().binary_expr(BinOpKind::And, x, rhs.to_expr())));

                        if use_ == ExprUse::Unused {
                            let out_val = mem::replace(
                                &mut out.val,
                                self.panic("Binary expression is not supposed to be used"),
                            );
                            out.stmts.push(mk().semi_stmt(out_val));
                        }

                        Ok(out)

                    }

                    c_ast::BinOp::Or => {
                        let lhs = self.convert_condition(true, lhs, is_static)?;
                        let rhs = self.convert_condition(true, rhs, is_static)?;
                        let mut out = lhs.map(|x| bool_to_int(mk().binary_expr(BinOpKind::Or, x, rhs.to_expr())));

                        if use_ == ExprUse::Unused {
                            let out_val = mem::replace(
                                &mut out.val,
                                self.panic("Binary expression is not supposed to be used"),
                            );
                            out.stmts.push(mk().semi_stmt(out_val));
                        }

                        Ok(out)
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
                        self.convert_assignment_operator(use_, op, type_id, lhs, rhs, opt_lhs_type_id, opt_res_type_id)
                    },

                    _ => {
                        let ty = self.convert_type(type_id.ctype)?;

                        let lhs_type = self.ast_context.index(lhs).kind.get_qual_type().ok_or_else(|| format!("bad lhs type"))?;
                        let rhs_type = self.ast_context.index(rhs).kind.get_qual_type().ok_or_else(|| format!("bad rhs type"))?;

                        let mut stmts = vec![];

                        if use_ == ExprUse::Unused {
                            stmts.extend(self.convert_expr(ExprUse::Unused, lhs, is_static, decay_ref)?.stmts);
                            stmts.extend(self.convert_expr(ExprUse::Unused, rhs, is_static, decay_ref)?.stmts);

                            Ok(WithStmts {
                                stmts,
                                val: self.panic("Binary expression is not supposed to be used"),
                            })
                        } else {
                            let WithStmts { val: lhs_val, stmts: lhs_stmts } = self.convert_expr(ExprUse::RValue, lhs, is_static, decay_ref)?;
                            let WithStmts { val: rhs_val, stmts: rhs_stmts } = self.convert_expr(ExprUse::RValue, rhs, is_static, decay_ref)?;

                            stmts.extend(lhs_stmts);
                            stmts.extend(rhs_stmts);
                            let expr_ids = Some((lhs, rhs));
                            let val = self.convert_binary_operator(op, ty, type_id.ctype, lhs_type, rhs_type, lhs_val, rhs_val, expr_ids);

                            Ok(WithStmts { stmts, val })
                        }

                    }
                }
            }

            CExprKind::ArraySubscript(_, ref lhs, ref rhs, _) => {
                let lhs_node = &self.ast_context.index(*lhs).kind;
                let rhs_node = &self.ast_context.index(*rhs).kind;

                let lhs_node_type = lhs_node.get_type().ok_or_else(|| format!("lhs node bad type"))?;
                let lhs_is_pointer = self.ast_context.resolve_type(lhs_node_type).kind.is_pointer();

                // From here on in, the LHS is the pointer/array and the RHS the index
                let (lhs, rhs, lhs_node) =
                    if lhs_is_pointer { (lhs, rhs, lhs_node) } else { (rhs, lhs, rhs_node) };

                let mut stmts = vec![];

                let mut rhs = self.convert_expr(ExprUse::RValue, *rhs, is_static, decay_ref)?;
                stmts.extend(rhs.stmts);

                let simple_index_array =
                match lhs_node {
                    &CExprKind::ImplicitCast(_, arr, CastKind::ArrayToPointerDecay, _, _) => {
                        let arr_type = self.ast_context[arr].kind.get_type().ok_or_else(|| format!("bad arr type"))?;
                        match self.ast_context.resolve_type(arr_type).kind {
                            // These get translated to 0-element arrays, this avoids the bounds check
                            // that using an array subscript in Rust would cause
                            CTypeKind::IncompleteArray(_) => None,
                            _ => Some(arr),
                        }
                    }
                    _ => None,
                };

                let val = if let Some(arr) = simple_index_array {
                    // If the LHS just underwent an implicit cast from array to pointer, bypass that
                    // to make an actual Rust indexing operation

                    let t = self.ast_context[arr].kind.get_type().ok_or_else(|| format!("bad arr type"))?;
                    let var_elt_type_id = match self.ast_context.resolve_type(t).kind {
                        CTypeKind::ConstantArray(..) => None,
                        CTypeKind::IncompleteArray(..) => None,
                        CTypeKind::VariableArray(elt, _) => Some(elt),
                        ref other => panic!("Unexpected array type {:?}", other),
                    };

                    let lhs = self.convert_expr(use_, arr, is_static, decay_ref)?;
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

                    let lhs = self.convert_expr(ExprUse::RValue, *lhs, is_static, decay_ref)?;
                    stmts.extend(lhs.stmts);

                    let lhs_type_id = lhs_node.get_type().ok_or_else(|| format!("bad lhs type"))?;

                    // Determine the type of element being indexed
                    let pointee_type_id = match self.ast_context.resolve_type(lhs_type_id).kind {
                        CTypeKind::Pointer(pointee_id) => pointee_id,
                        _ => return Err(format!("Subscript applied to non-pointer")),
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
                let is_variadic = self.fn_expr_is_variadic(func);
                let WithStmts { mut stmts, val: func } = match self.ast_context.index(func).kind {
                    CExprKind::ImplicitCast(_, fexp, CastKind::FunctionToPointerDecay, _, _) =>
                        self.convert_expr(ExprUse::RValue, fexp, is_static, decay_ref)?,

                    CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) =>
                        return self.convert_builtin(fexp, args, is_static),

                    _ =>
                        self.convert_expr(ExprUse::RValue, func, is_static, decay_ref)?
                            .map(unwrap_function_pointer),
                };

                let mut args_new: Vec<P<Expr>> = vec![];
                let decay_ref = DecayRef::from(is_variadic);

                for arg in args {
                    // We want to decay refs only when function is variadic
                    let WithStmts { stmts: ss, val } = self.convert_expr(ExprUse::RValue, *arg, is_static, decay_ref)?;
                    stmts.extend(ss);
                    args_new.push(val);
                }

                let call_expr = mk().call_expr(func, args_new);

                if use_ == ExprUse::Unused {
                    // Recall that if `used` is false, the `stmts` field of the output must contain
                    // all side-effects (and a function call can always have side-effects)
                    stmts.push(mk().semi_stmt(call_expr));

                    let val = self.panic("Function call expression is not supposed to be used");
                    Ok(WithStmts { stmts, val })
                } else {
                    Ok(WithStmts { stmts, val: call_expr })
                }
            }

            CExprKind::Member(_, expr, decl, kind, _) => {

                if use_ == ExprUse::Unused {
                    self.convert_expr(use_, expr, is_static, decay_ref)
                } else {
                    let field_name = self.type_converter.borrow().resolve_field_name(None, decl).unwrap();
                    match kind {
                        MemberKind::Dot => {
                            let val = self.convert_expr(use_, expr, is_static, decay_ref)?;
                            Ok(val.map(|v| mk().field_expr(v, field_name)))
                        }
                        MemberKind::Arrow => {
                            if let CExprKind::Unary(_, c_ast::UnOp::AddressOf, subexpr_id, _)
                            = self.ast_context[expr].kind {
                                let val = self.convert_expr(use_, subexpr_id, is_static, decay_ref)?;
                                Ok(val.map(|v| mk().field_expr(v, field_name)))
                            } else {
                                let val = self.convert_expr(use_, expr, is_static, decay_ref)?;
                                Ok(val.map(|v| mk().field_expr(mk().unary_expr(ast::UnOp::Deref, v), field_name)))
                            }
                        }
                    }
                }
            }

            CExprKind::CompoundLiteral(_, val) =>
                self.convert_expr(use_, val, is_static, decay_ref),

            CExprKind::InitList(ty, ref ids, opt_union_field_id, _) => {

                match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::ConstantArray(ty, n) => {
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
                            let mut x = self.convert_expr(ExprUse::RValue, *v, is_static, decay_ref)?;
                            stmts.append(&mut x.stmts);
                            x.val
                        } else  {
                            let mut vals: Vec<P<Expr>> = vec![];
                            for v in ids {
                                let mut x = self.convert_expr(ExprUse::RValue, *v, is_static, decay_ref)?;
                                stmts.append(&mut x.stmts);
                                vals.push(x.val);
                            }
                            // Pad out the array literal with default values to the desired size
                            for _i in ids.len()..n {
                                vals.push(self.implicit_default_expr(ty, is_static)?)
                            }
                            mk().array_expr(vals)
                        };

                        Ok(WithStmts {stmts, val })
                    }
                    CTypeKind::Struct(struct_id) => {
                        self.convert_struct_literal(struct_id, ids.as_ref(), is_static)
                    }
                    CTypeKind::Union(union_id) => {
                        self.convert_union_literal(union_id, ids.as_ref(), ty, opt_union_field_id, is_static)
                    }
                    CTypeKind::Pointer(_) => {
                        let id = ids.first().unwrap();
                        let mut x = self.convert_expr(ExprUse::RValue, *id, is_static, decay_ref);
                        Ok(x.unwrap())
                    }
                    ref t => {
                        Err(format!("Init list not implemented for {:?}", t))
                    }
                }
            }
            CExprKind::ImplicitValueInit(ty) =>
                Ok(WithStmts::new(self.implicit_default_expr(ty.ctype, is_static)?)),

            CExprKind::Predefined(_, val_id) =>
                self.convert_expr(use_, val_id, is_static, decay_ref),

            CExprKind::Statements(_, compound_stmt_id) =>
                self.convert_statement_expression(use_, compound_stmt_id, is_static),

            CExprKind::VAArg(ty, val_id) => {
                if self.tcfg.translate_valist {
                    // https://github.com/rust-lang/rust/pull/49878/files
                    let val = self.convert_expr(ExprUse::RValue, val_id, is_static, decay_ref)?;
                    let ty = self.convert_type(ty.ctype)?;

                    Ok(val.map(|va| {
                        let path = mk().path_segment_with_params(
                            mk().ident("arg"),
                            mk().angle_bracketed_param_types(vec![ty]));
                        mk().method_call_expr(va, path, vec![] as Vec<P<Expr>>)
                    }))

                } else {
                    Err(format!("Variable argument lists are not supported (requires --translate-valist)"))
                }
            }
        }
    }

    fn fn_expr_is_variadic(&self, expr_id: CExprId) -> bool {
        let fn_expr = &self.ast_context.c_exprs[&expr_id];
        let fn_ty = &self.ast_context.c_types[&fn_expr.kind.get_type().unwrap()];
        if let CTypeKind::Pointer(qual_ty) = fn_ty.kind {
            match self.ast_context.c_types[&qual_ty.ctype].kind {
                CTypeKind::Function(_, _, is_variadic, _) => is_variadic,
                _ => false,
            }
        } else {
            false
        }
    }

    fn convert_builtin(
        &self,
        fexp: CExprId,
        args: &[CExprId],
        is_static: bool,
    ) -> Result<WithStmts<P<Expr>>, String> {

        let decl_id =
            match self.ast_context[fexp].kind {
                CExprKind::DeclRef(_, decl_id, _) => decl_id,
                _ => return Err(format!("Expected declref when processing builtin")),
            };

        let builtin_name: &str =
            match self.ast_context[decl_id].kind {
                CDeclKind::Function { ref name, .. } => name,
                _ => return Err(format!("Expected function when processing builtin")),
            };
        let decay_ref = DecayRef::Default;

        match builtin_name {
            "__builtin_huge_valf" =>
                Ok(WithStmts::new(mk().path_expr(vec!["","std","f32","INFINITY"]))),
            "__builtin_huge_val" | "__builtin_huge_vall"=>
                Ok(WithStmts::new(mk().path_expr(vec!["","std","f64","INFINITY"]))),
            "__builtin_inff" =>
                Ok(WithStmts::new(mk().path_expr(vec!["","std","f32","INFINITY"]))),
            "__builtin_inf" | "__builtin_infl" =>
                Ok(WithStmts::new(mk().path_expr(vec!["","std","f64","INFINITY"]))),
            "__builtin_nanf" =>
                Ok(WithStmts::new(mk().path_expr(vec!["","std","f32","NAN"]))),
            "__builtin_nan" =>
                Ok(WithStmts::new(mk().path_expr(vec!["","std","f64","NAN"]))),
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll" => {
                let val = self.convert_expr(ExprUse::RValue, args[0], is_static, decay_ref)?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "leading_zeros", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll" => {
                let val = self.convert_expr(ExprUse::RValue, args[0], is_static, decay_ref)?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "trailing_zeros", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" => {
                let val = self.convert_expr(ExprUse::RValue, args[0], is_static, decay_ref)?;
                Ok(val.map(|x|
                    mk().method_call_expr(x, "swap_bytes", vec![] as Vec<P<Expr>>)
                ))
            }
            "__builtin_fabs" | "__builtin_fabsf" | "__builtin_fabsl" => {
                let val = self.convert_expr(ExprUse::RValue, args[0], is_static, decay_ref)?;
                Ok(val.map(|x|
                    mk().method_call_expr(x, "abs", vec![] as Vec<P<Expr>>)
                ))
            }
            "__builtin_expect" =>
                self.convert_expr(ExprUse::RValue, args[0], is_static, decay_ref),

            "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll" => {
                let val = self.convert_expr(ExprUse::RValue, args[0], is_static, decay_ref)?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "count_ones", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_bzero" => {
                let ptr_stmts = self.convert_expr(ExprUse::RValue, args[0], is_static, decay_ref)?;
                let n_stmts = self.convert_expr(ExprUse::RValue, args[1], is_static, decay_ref)?;
                let write_bytes = mk().path_expr(vec!["", "std", "ptr", "write_bytes"]);
                let zero = mk().lit_expr(mk().int_lit(0, "u8"));
                Ok(ptr_stmts.and_then(|ptr| n_stmts.map(|n| {
                    mk().call_expr(write_bytes, vec![ptr, zero, n])
                })))
            }

            // If the target does not support data prefetch, the address expression is evaluated if
            // it includes side effects but no other code is generated and GCC does not issue a warning.
            // void __builtin_prefetch (const void *addr, ...);
            "__builtin_prefetch" => {
                self.convert_expr(ExprUse::Unused, args[0], is_static, decay_ref)
            }

            "__builtin_va_start" =>
                Err(format!("va_start not supported - currently va_list and va_arg are supported")),
            "__builtin_va_copy" =>
                Err(format!("va_copy not supported - currently va_list and va_arg are supported")),
            "__builtin_va_end" =>
                Err(format!("va_end not supported - currently va_list and va_arg are supported")),

            _ => Err(format!("Unimplemented builtin: {}", builtin_name)),
        }
    }

    fn convert_statement_expression(
        &self,
        use_: ExprUse,
        compound_stmt_id: CStmtId,
        is_static: bool
    ) -> Result<WithStmts<P<Expr>>, String> {

        fn as_semi_return_stmt(stmt: &ast::Stmt) -> Option<Option<P<ast::Expr>>> {
            if let ast::Stmt { node: ast::StmtKind::Semi(ref expr), .. } = *stmt {
                if let ast::Expr { node: ast::ExprKind::Ret(ref ret_val), .. } = **expr {
                    return Some(ret_val.clone())
                }
            }
            None
        }

        match self.ast_context[compound_stmt_id].kind {
            CStmtKind::Compound(ref substmt_ids) if !substmt_ids.is_empty() => {

                let n = substmt_ids.len();
                let result_id = substmt_ids[n - 1];

                if self.tcfg.reloop_cfgs {

                    let name = format!("<stmt-expr_{:?}>", compound_stmt_id);

                    let mut stmts = match self.ast_context[result_id].kind {
                        CStmtKind::Expr(expr_id) => {
                            let ret = cfg::ImplicitReturnType::StmtExpr(use_, expr_id, is_static);
                            self.convert_function_body(&name, &substmt_ids[0 .. (n-1)], ret)?
                        }

                        _ => self.convert_function_body(&name, &substmt_ids, cfg::ImplicitReturnType::Void)?,
                    };

                    if let Some(stmt) = stmts.pop() {
                        match as_semi_return_stmt(&stmt) {
                            Some(val) => return Ok(WithStmts::new(mk().block_expr({
                                match val {
                                    None => mk().block(stmts),
                                    Some(val) => WithStmts { stmts, val }.to_block()
                                }
                            }))),
                            _ => stmts.push(stmt),
                        }
                    }

                    let decl = mk().fn_decl(
                        vec![] as Vec<ast::Arg>,
                        ast::FunctionRetTy::Default(DUMMY_SP),
                        false,
                    );
                    let closure_body = mk().block_expr(mk().block(stmts));
                    let closure = mk().closure_expr(ast::CaptureBy::Ref, ast::Movability::Movable, decl, closure_body);
                    let closure_call = mk().call_expr(closure, vec![] as Vec<P<ast::Expr>>);
                    Ok(WithStmts::new(closure_call))
                } else {

                    let mut stmts = vec![];

                    for &substmt_id in &substmt_ids[0..n - 1] {
                        stmts.append(&mut self.convert_stmt(substmt_id)?);
                    }

                    match self.ast_context[result_id].kind {
                        CStmtKind::Expr(expr_id) => {
                            let mut result = self.convert_expr(use_, expr_id, is_static, DecayRef::Default)?;
                            stmts.append(&mut result.stmts);
                            Ok(WithStmts { stmts, val: result.val })
                        }

                        _ => {
                            stmts.append(&mut self.convert_stmt(result_id)?);
                            Ok(WithStmts { stmts, val: self.panic("Void statement expression")})
                        }
                    }
                }
            }
            _ => {
                if let ExprUse::Unused = use_ {
                    let val = self.panic("Empty statement expression is not supposed to be used");
                    Ok(WithStmts { stmts: vec![], val })
                } else {
                    Err(format!("Bad statement expression"))
                }
            },
        }
    }

    fn convert_cast(
        &self,
        use_: ExprUse,
        ty: CQualTypeId,
        expr: CExprId,
        kind: CastKind,
        opt_field_id: Option<CFieldId>,
        is_explicit: bool,
        is_static: bool,
        mut decay_ref: DecayRef,
    ) -> Result<WithStmts<P<Expr>>, String> {
        // A reference must be decayed if a bitcast is required
        if kind == CastKind::BitCast || kind == CastKind::PointerToIntegral {
            decay_ref = DecayRef::Yes;
        }

        let val = if is_explicit {
            let mut stmts = self.compute_variable_array_sizes(ty.ctype)?;
            let mut val = self.convert_expr(use_, expr, is_static, decay_ref)?;
            stmts.append(&mut val.stmts);
            val.stmts = stmts;
            val
        } else if self.should_force_lvalue(kind) {
            self.convert_expr(ExprUse::LValue, expr, is_static, decay_ref)?
        } else {
            self.convert_expr(use_, expr, is_static, decay_ref)?
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

                    let source_ty_id = self.ast_context[expr].kind.get_type().ok_or_else(|| format!("bad source type"))?;

                    if self.ast_context.is_function_pointer(ty.ctype) ||
                       self.ast_context.is_function_pointer(source_ty_id) {
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

            CastKind::IntegralToPointer if self.ast_context.is_function_pointer(ty.ctype) => {
                let target_ty = self.convert_type(ty.ctype)?;
                Ok(val.map(|x| {
                    let intptr_t = mk().path_ty(vec!["libc","intptr_t"]);
                    let intptr = mk().cast_expr(x, intptr_t.clone());
                    transmute_expr(intptr_t, target_ty, intptr)
                }))
            }


            CastKind::IntegralToPointer | CastKind::PointerToIntegral |
            CastKind::IntegralCast | CastKind::FloatingCast | CastKind::FloatingToIntegral |
            CastKind::IntegralToFloating => {
                let target_ty = self.convert_type(ty.ctype)?;
                let target_ty_ctype = &self.ast_context.resolve_type(ty.ctype).kind;

                let source_ty_ctype_id = self.ast_context[expr].kind.get_type()
                    .ok_or_else(|| format!("bad source expression"))?;

                let source_ty = self.convert_type(source_ty_ctype_id)?;
                if let &CTypeKind::Enum(enum_decl_id) = target_ty_ctype {
                    // Casts targeting `enum` types...
                    Ok(self.enum_cast(ty.ctype, enum_decl_id, expr, val, source_ty, target_ty))
                } else {
                    // Other numeric casts translate to Rust `as` casts,
                    // unless the cast is to a function pointer then use `transmute`.
                    Ok(val.map(|x| {
                        if self.ast_context.is_function_pointer(source_ty_ctype_id) {
                            transmute_expr(source_ty, target_ty, x)
                        } else  {
                            mk().cast_expr(x, target_ty)
                        }
                    }))
                }
            }

            CastKind::LValueToRValue | CastKind::NoOp | CastKind::ToVoid | CastKind::ConstCast => Ok(val),

            CastKind::FunctionToPointerDecay =>
                Ok(val.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x]))),

            CastKind::BuiltinFnToFnPtr =>
                Ok(val.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x]))),

            CastKind::ArrayToPointerDecay => {
                let pointee = match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::Pointer(pointee) => pointee,
                    _ => panic!("Dereferencing a non-pointer"),
                };

                let is_const = pointee.qualifiers.is_const;

                match self.ast_context.index(expr).kind {
                    CExprKind::Literal(_,CLiteral::String(ref bytes,1)) if is_const => {
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
                        let source_ty = self.ast_context[expr].kind.get_type().ok_or_else(|| format!("bad variable array source type"))?;
                        if let CTypeKind::VariableArray(..) = self.ast_context.resolve_type(source_ty).kind {
                            Ok(val)
                        } else {
                            let method = if is_const || is_static {
                                "as_ptr"
                            } else {
                                "as_mut_ptr"
                            };

                            let mut call = val.map(|x| mk().method_call_expr(x, method, vec![] as Vec<P<Expr>>));

                            // Static arrays can now use as_ptr with the const_slice_as_ptr feature
                            // enabled. Can also cast that const ptr to a mutable pointer as we do here:
                            if is_static {
                                self.use_feature("const_slice_as_ptr");

                                if !is_const {
                                    let WithStmts { val, stmts } = call;
                                    let inferred_type = mk().infer_ty();
                                    let ptr_type = mk().mutbl().ptr_ty(inferred_type);
                                    let val = mk().cast_expr(val, ptr_type);

                                    call = WithStmts { val, stmts };
                                }
                            }

                            Ok(call)
                        }
                    },
                }

            }

            CastKind::NullToPointer => {
                assert!(val.stmts.is_empty());
                Ok(WithStmts::new(self.null_ptr(ty.ctype, is_static)?))
            }

            CastKind::ToUnion => {
                let field_id = opt_field_id.expect("Missing field ID in union cast");
                let union_id = self.ast_context.parents[&field_id];

                let union_name = self.type_converter.borrow().resolve_decl_name(union_id).expect("required union name");
                let field_name = self.type_converter.borrow().resolve_field_name(Some(union_id), field_id).expect("field name required");

                Ok(val.map(|x|
                    mk().struct_expr(mk().path(vec![union_name]), vec![mk().field(field_name, x)])
                ))
            },

            CastKind::IntegralToBoolean | CastKind::FloatingToBoolean | CastKind::PointerToBoolean => {
                self.convert_condition(true, expr, is_static)
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

        let def_id = match self.ast_context.resolve_type(enum_type_id).kind {
            CTypeKind::Enum(def_id) => def_id,
            _ => panic!("{:?} does not point to an `enum` type"),
        };

        let (variants, underlying_type_id) = match self.ast_context[def_id].kind {
            CDeclKind::Enum { ref variants, integral_type, .. } => (variants, integral_type),
            _ => panic!("{:?} does not point to an `enum` declaration")
        };

        for &variant_id in variants {
            match self.ast_context[variant_id].kind {
                CDeclKind::EnumConstant { value: v, .. } =>
                if v == ConstIntExpr::I(value) || v == ConstIntExpr::U(value as u64) {
                    let name = self.renamer.borrow().get(&variant_id).unwrap();
                    return mk().path_expr(vec![name])
                }
                _ => panic!("{:?} does not point to an enum variant", variant_id),
            }
        }

        let underlying_type_id = underlying_type_id.expect("Attempt to construct value of forward declared enum");
        let value = match self.ast_context.resolve_type(underlying_type_id.ctype).kind {
            CTypeKind::UInt => mk().lit_expr(mk().int_lit((value as u32) as u128, LitIntType::Unsuffixed)),
            CTypeKind::ULong => mk().lit_expr(mk().int_lit((value as u64) as u128, LitIntType::Unsuffixed)),
            _ => signed_int_expr(value),
        };

        let target_ty = self.convert_type(enum_type_id).unwrap();

        mk().cast_expr(value, target_ty)
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
        _source_ty: P<Ty>,        // source type of cast
        target_ty: P<Ty>,        // target type of cast
    ) -> WithStmts<P<Expr>> {

        // Extract the IDs of the `EnumConstant` decls underlying the enum.
        let variants = match self.ast_context.index(enum_decl).kind {
            CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration")
        };

        match self.ast_context.index(expr).kind {
            // This is the case of finding a variable which is an `EnumConstant` of the same enum
            // we are casting to. Here, we can just remove the extraneous cast instead of generating
            // a new one.
            CExprKind::DeclRef(_, decl_id, _) if variants.contains(&decl_id) =>
                return val.map(|x| match x.node {
                    ast::ExprKind::Cast(ref e, _) => e.clone(),
                    _ => panic!(format!("DeclRef {:?} of enum {:?} is not cast", expr, enum_decl)),
                }),

            CExprKind::Literal(_, CLiteral::Integer(i,_)) => {
                let new_val = self.enum_for_i64(enum_type, i as i64);
                return WithStmts { stmts: val.stmts, val: new_val }
            }

            CExprKind::Unary(_, c_ast::UnOp::Negate, subexpr_id, _) => {
                if let &CExprKind::Literal(_, CLiteral::Integer(i,_)) = &self.ast_context[subexpr_id].kind {
                    let new_val = self.enum_for_i64(enum_type, -(i as i64));
                    return WithStmts { stmts: val.stmts, val: new_val }
                }
            }

            // In all other cases, a cast to an enum requires a `transmute` - Rust enums cannot be
            // converted into integral types as easily as C ones.
            _ => {},
        }

        val.map(|x| mk().cast_expr(x, target_ty))
    }

    fn convert_union_literal(
        &self,
        union_id: CRecordId,
        ids: &[CExprId],
        _ty: CQualTypeId,
        opt_union_field_id: Option<CFieldId>,
        is_static: bool,
    ) -> Result<WithStmts<P<Expr>>, String> {
        let union_field_id = opt_union_field_id.expect("union field ID");

        match self.ast_context.index(union_id).kind {
            CDeclKind::Union { .. } => {
                let union_name = self.type_converter.borrow().resolve_decl_name(union_id).unwrap();
                match self.ast_context.index(union_field_id).kind {
                    CDeclKind::Field { typ: field_ty, .. } => {
                        let val = if ids.is_empty() {
                            WithStmts {
                                stmts: vec![],
                                val: self.implicit_default_expr(field_ty.ctype, is_static)?,
                            }
                        } else {
                            self.convert_expr(ExprUse::RValue, ids[0], is_static, DecayRef::Default)?
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

    fn convert_struct_literal(&self, struct_id: CRecordId, ids: &[CExprId], is_static: bool)
                              -> Result<WithStmts<P<Expr>>, String> {

        let field_decls = match self.ast_context.index(struct_id).kind {
            CDeclKind::Struct { ref fields, .. } => {
                let mut fieldnames = vec![];

                let fields = match fields {
                    &Some(ref fields) => fields,
                    &None => return Err(format!("Attempted to construct forward-declared struct")),
                };

                for &x in fields {
                    let name = self.type_converter.borrow().resolve_field_name(Some(struct_id), x).unwrap();
                    if let CDeclKind::Field { typ, .. } = self.ast_context.index(x).kind {
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

            let mut x = self.convert_expr(ExprUse::RValue, v, is_static, DecayRef::Default)?;
            stmts.append(&mut x.stmts);
            fields.push(mk().field(field_name, x.val));
        }

        // Pad out remaining omitted record fields
        for i in ids.len()..fields.len() {
            let &(ref field_name, ty) = &field_decls[i];
            fields.push(mk().field(field_name, self.implicit_default_expr(ty.ctype, is_static)?));
        }

        Ok(WithStmts {
            stmts,
            val: mk().struct_expr(vec![mk().path_segment(struct_name)], fields)
        })
    }

    pub fn implicit_default_expr(&self, ty_id: CTypeId, is_static: bool) -> Result<P<Expr>, String> {
        let resolved_ty_id = self.ast_context.resolve_type_id(ty_id);
        let resolved_ty = &self.ast_context.index(resolved_ty_id).kind;

        if resolved_ty.is_bool() {
            Ok(mk().lit_expr(mk().bool_lit(false)))
        } else if resolved_ty.is_integral_type() {
            Ok(mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed)))
        } else if resolved_ty.is_floating_type() {
            Ok(mk().lit_expr(mk().float_unsuffixed_lit("0.")))
        } else if let &CTypeKind::Pointer(_) = resolved_ty {
            self.null_ptr(resolved_ty_id, is_static)
        } else if let &CTypeKind::ConstantArray(elt, sz) = resolved_ty {
            let sz = mk().lit_expr(mk().int_lit(sz as u128, LitIntType::Unsuffixed));
            Ok(mk().repeat_expr(self.implicit_default_expr(elt, is_static)?, sz))
        } else if let &CTypeKind::IncompleteArray(_) = resolved_ty {
            // Incomplete arrays are translated to zero length arrays
            Ok(mk().array_expr(vec![] as Vec<P<Expr>>))
        } else if let Some(decl_id) = resolved_ty.as_underlying_decl() {
            self.zero_initializer(decl_id, ty_id, is_static)
        } else if let &CTypeKind::VariableArray(elt, _) = resolved_ty {

            // Variable length arrays unnested and implemented as a flat array of the underlying
            // element type.

            // Find base element type of potentially nested arrays
            let inner = self.variable_array_base_type(elt);

            let count = self.compute_size_of_expr(ty_id).unwrap();
            let val = self.implicit_default_expr(inner, is_static)?;
            let from_elem = mk().path_expr(vec!["", "std", "vec", "from_elem"]);
            let alloc = mk().call_expr(from_elem, vec![val, count]);
            Ok(alloc)
        } else {
            Err(format!("Unsupported default initializer: {:?}", resolved_ty))
        }
    }

    /// Produce zero-initializers for structs/unions/enums, looking them up when possible.
    fn zero_initializer(&self, decl_id: CDeclId, type_id: CTypeId, is_static: bool) -> Result<P<Expr>, String> {

        // Look up the decl in the cache and return what we find (if we find anything)
        if let Some(init) = self.zero_inits.borrow().get(&decl_id) {
            return init.clone()
        }

        // Otherwise, construct the initializer
        let init = match self.ast_context.index(decl_id).kind {

            // Zero initialize all of the fields
            CDeclKind::Struct { ref fields, .. } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                let fields = match *fields {
                    Some(ref fields) => fields,
                    None => return Err(format!("Attempted to zero-initialize forward-declared struct")),
                };

                let fields: Result<Vec<Field>, String> = fields
                    .into_iter()
                    .map(|field_id: &CFieldId| -> Result<Field, String> {
                        let name = self.type_converter.borrow().resolve_field_name(Some(decl_id), *field_id).unwrap();

                        match self.ast_context.index(*field_id).kind {
                            CDeclKind::Field { typ, .. } => {
                                let field_init = self.implicit_default_expr(typ.ctype, is_static)?;
                                Ok(mk().field(name, field_init))
                            }
                            _ => Err(format!("Found non-field in record field list"))
                        }
                    })
                    .collect();

                Ok(mk().struct_expr(vec![name], fields?))
            },

            // Zero initialize the first field
            CDeclKind::Union { ref fields, .. } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                let fields = match *fields {
                    Some(ref fields) => fields,
                    None => return Err(format!("Attempted to zero-initialize forward-declared struct")),
                };

                let &field_id = fields.first().ok_or(format!("A union should have a field"))?;

                let field = match self.ast_context.index(field_id).kind {
                    CDeclKind::Field { typ, .. } => {
                        let field_init = self.implicit_default_expr(typ.ctype, is_static)?;
                        let name = self.type_converter.borrow().resolve_field_name(Some(decl_id), field_id).unwrap();

                        Ok(mk().field(name, field_init))
                    }
                    _ => Err(format!("Found non-field in record field list"))
                }?;

                Ok(mk().struct_expr(vec![name], vec![field]))
            },

            // Transmute the number `0` into the enum type
            CDeclKind::Enum { .. } => Ok(self.enum_for_i64(type_id, 0)),

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
        let reference_ty = self.ast_context.index(reference).kind.get_qual_type().ok_or_else(|| format!("bad reference type"))?;
        let WithStmts {
            val: reference,
            mut stmts,
        } = self.convert_expr(ExprUse::LValue, reference, false, DecayRef::Default)?;

        /// Check if something is a valid Rust lvalue. Inspired by `librustc::ty::expr_is_lval`.
        fn is_lvalue(e: &Expr) -> bool {
            match e.node {
                ExprKind::Path(..) |
                ExprKind::Unary(ast::UnOp::Deref, _) |
                ExprKind::Field(..) |
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
        let arg_type = self.ast_context[arg].kind.get_qual_type().ok_or_else(|| format!("bad arg type"))?;
        self.convert_assignment_operator_with_rhs(ExprUse::RValue, op, arg_type, arg, ty, one, Some(arg_type), Some(arg_type))
    }

    fn convert_post_increment(&self, use_: ExprUse, ty: CQualTypeId, up: bool, arg: CExprId) -> Result<WithStmts<P<Expr>>, String> {

        // If we aren't going to be using the result, may as well do a simple pre-increment
        if use_ == ExprUse::Unused {
            return self.convert_pre_increment(ty, up, arg)
        }

        let ty = self.ast_context.index(arg).kind.get_qual_type().ok_or_else(|| format!("bad post inc type"))?;

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
        is_static: bool,
        mut decay_ref: DecayRef,
    ) -> Result<WithStmts<P<Expr>>, String> {
        let CQualTypeId { ctype, .. } = cqual_type;
        let ty = self.convert_type(ctype)?;
        let resolved_ctype = self.ast_context.resolve_type(ctype);

        match name {
            c_ast::UnOp::AddressOf => {
                let arg_kind = &self.ast_context[arg].kind;

                match arg_kind {
                    // C99 6.5.3.2 para 4
                    CExprKind::Unary(_, c_ast::UnOp::Deref, target, _) =>
                        return self.convert_expr(use_, *target, is_static, decay_ref),
                    // An AddrOf DeclRef/Member is safe to not decay if the translator isn't already giving a hard
                    // yes to decaying (ie, BitCasts). So we only convert default to no decay.
                    CExprKind::DeclRef(..) |
                    CExprKind::Member(..) => decay_ref.set_default_to_no(),
                    _ => (),
                };

                // In this translation, there are only pointers to functions and
                // & becomes a no-op when applied to a function.

                let arg = self.convert_expr(ExprUse::LValue, arg, is_static, decay_ref)?;

                if self.ast_context.is_function_pointer(ctype) {
                    Ok(arg.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x])))
                } else {
                    let pointee = match resolved_ctype.kind {
                        CTypeKind::Pointer(pointee) => pointee,
                        _ => return Err(format!("Address-of should return a pointer")),
                    };

                    let mutbl = if pointee.qualifiers.is_const {
                        Mutability::Immutable
                    } else {
                        Mutability::Mutable
                    };

                    arg.result_map(|a| {
                        let mut addr_of_arg: P<Expr>;

                        if is_static {
                            // static variable initializers aren't able to use &mut,
                            // so we work around that by using & and an extra cast
                            // through & to *const to *mut
                            addr_of_arg = mk().addr_of_expr(a);
                            if mutbl == Mutability::Mutable {
                                let mut qtype = pointee;
                                qtype.qualifiers.is_const = true;
                                let ty_ = self.type_converter.borrow_mut().convert_pointer(&self.ast_context, qtype)?;
                                addr_of_arg = mk().cast_expr(addr_of_arg, ty_);
                            }
                        } else {
                            // Normal case is allowed to use &mut if needed
                            addr_of_arg = mk().set_mutbl(mutbl).addr_of_expr(a);

                            // Avoid unnecessary reference to pointer decay in fn call args:
                            if decay_ref.is_no() {
                                return Ok(addr_of_arg);
                            }
                        }

                        Ok(mk().cast_expr(addr_of_arg, ty))
                    })
                }
            },
            c_ast::UnOp::PreIncrement => self.convert_pre_increment(cqual_type, true, arg),
            c_ast::UnOp::PreDecrement => self.convert_pre_increment(cqual_type, false, arg),
            c_ast::UnOp::PostIncrement => self.convert_post_increment(use_, cqual_type, true, arg),
            c_ast::UnOp::PostDecrement => self.convert_post_increment(use_, cqual_type, false, arg),
            c_ast::UnOp::Deref => {

                if let CExprKind::Unary(_, c_ast::UnOp::AddressOf, arg_, _) = self.ast_context[arg].kind {
                    self.convert_expr(ExprUse::RValue, arg_, is_static, decay_ref)
                } else {
                    self.convert_expr(ExprUse::RValue, arg, is_static, decay_ref)?.result_map(|val: P<Expr>| {
                        if let CTypeKind::Function(..) = self.ast_context.resolve_type(ctype).kind {
                            Ok(unwrap_function_pointer(val))
                        } else if let Some(_vla) = self.compute_size_of_expr(ctype) {
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
                }
            },
            c_ast::UnOp::Plus => self.convert_expr(ExprUse::RValue, arg, is_static, decay_ref), // promotion is explicit in the clang AST

            c_ast::UnOp::Negate => {
                let val = self.convert_expr(ExprUse::RValue, arg, is_static, decay_ref)?;

                if resolved_ctype.kind.is_unsigned_integral_type() {
                    Ok(val.map(wrapping_neg_expr))
                } else {
                    Ok(val.map(neg_expr))
                }
            }
            c_ast::UnOp::Complement =>
                Ok(self.convert_expr(ExprUse::RValue, arg, is_static, decay_ref)?
                    .map(|a| mk().unary_expr(ast::UnOp::Not, a))),

            c_ast::UnOp::Not => {
                let val = self.convert_condition(false, arg, is_static)?;
                Ok(val.map(|x| mk().cast_expr(x, mk().path_ty(vec!["libc", "c_int"]))))
            },
            c_ast::UnOp::Extension => {
                let arg = self.convert_expr(use_, arg, is_static, decay_ref)?;
                Ok(arg)
            },
            c_ast::UnOp::Real | c_ast::UnOp::Imag | c_ast::UnOp::Coawait =>
                panic!("Unsupported extension operator"),
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
            let lhs = mk().cast_expr(read, lhs_type.clone());
            let ty = self.convert_type(compute_res_ty.ctype)?;
            let val = self.convert_binary_operator(bin_op, ty, compute_res_ty.ctype, compute_lhs_ty, rhs_ty, lhs, rhs, None);

            let is_enum_result = self.ast_context[self.ast_context.resolve_type_id(lhs_ty.ctype)].kind.is_enum();
            let result_type = self.convert_type(lhs_ty.ctype)?;
            let val = if is_enum_result {
                transmute_expr(lhs_type, result_type, val)
            } else {
                mk().cast_expr(val, result_type)
            };
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

        let rhs_type_id = self.ast_context.index(rhs).kind.get_qual_type().ok_or_else(|| format!("bad assignment rhs type"))?;
        let rhs_translation = self.convert_expr(ExprUse::RValue, rhs, false, DecayRef::Default)?;
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
        let initial_lhs_type_id = self.ast_context.index(lhs).kind.get_qual_type().ok_or_else(|| format!("bad initial lhs type"))?;

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
            (write, self.panic("Volatile value is not supposed to be read"), lhs_stmts)
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
                    self.convert_binary_operator(op, ty, qtype.ctype, initial_lhs_type_id, rhs_type_id, read.clone(), rhs, None)
                } else {
                    let lhs_type = self.convert_type(compute_type.unwrap().ctype)?;
                    let write_type = self.convert_type(qtype.ctype)?;
                    let lhs = mk().cast_expr(read.clone(), lhs_type.clone());
                    let ty = self.convert_type(result_type_id.ctype)?;
                    let val = self.convert_binary_operator(op, ty, result_type_id.ctype, compute_lhs_type_id, rhs_type_id, lhs, rhs, None);

                    let is_enum_result = self.ast_context[self.ast_context.resolve_type_id(qtype.ctype)].kind.is_enum();
                    let result_type = self.convert_type(qtype.ctype)?;
                    let val = if is_enum_result {
                        transmute_expr(lhs_type, result_type, val)
                    } else {
                        mk().cast_expr(val, result_type)
                    };
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
        lhs_rhs_ids: Option<(CExprId, CExprId)>,
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

            c_ast::BinOp::EqualEqual => {
                // Using is_none method for null comparison means we don't have to
                // rely on the PartialEq trait as much and is also more idiomatic
                let expr = if let Some((lhs_expr_id, rhs_expr_id)) = lhs_rhs_ids {
                    let fn_eq_null = self.ast_context.is_function_pointer(lhs_type.ctype) &&
                                     self.ast_context.is_null_expr(rhs_expr_id);
                    let null_eq_fn = self.ast_context.is_function_pointer(rhs_type.ctype) &&
                                     self.ast_context.is_null_expr(lhs_expr_id);

                    if fn_eq_null {
                        mk().method_call_expr(lhs, "is_none", vec![] as Vec<P<Expr>>)
                    } else if null_eq_fn {
                        mk().method_call_expr(rhs, "is_none", vec![] as Vec<P<Expr>>)
                    } else {
                        mk().binary_expr(BinOpKind::Eq, lhs, rhs)
                    }
                } else {
                    mk().binary_expr(BinOpKind::Eq, lhs, rhs)
                };

                bool_to_int(expr)
            },
            c_ast::BinOp::NotEqual => {
                // Using is_some method for null comparison means we don't have to
                // rely on the PartialEq trait as much and is also more idiomatic
                let expr = if let Some((lhs_expr_id, rhs_expr_id)) = lhs_rhs_ids {
                    let fn_eq_null = self.ast_context.is_function_pointer(lhs_type.ctype) &&
                                     self.ast_context.is_null_expr(rhs_expr_id);
                    let null_eq_fn = self.ast_context.is_function_pointer(rhs_type.ctype) &&
                                     self.ast_context.is_null_expr(lhs_expr_id);

                    if fn_eq_null {
                        mk().method_call_expr(lhs, "is_some", vec![] as Vec<P<Expr>>)
                    } else if null_eq_fn {
                        mk().method_call_expr(rhs, "is_some", vec![] as Vec<P<Expr>>)
                    } else {
                        mk().binary_expr(BinOpKind::Ne, lhs, rhs)
                    }
                } else {
                    mk().binary_expr(BinOpKind::Ne, lhs, rhs)
                };

                bool_to_int(expr)
            },
            c_ast::BinOp::Less => bool_to_int(mk().binary_expr(BinOpKind::Lt, lhs, rhs)),
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
            // The wrapping_offset_from method is locked behind a feature gate
            // and replaces the now deprecated offset_to (opposite argument order)
            // wrapping_offset_from panics when the pointee is a ZST
            self.use_feature("ptr_wrapping_offset_from");

            let mut offset = mk().method_call_expr(lhs, "wrapping_offset_from", vec![rhs]);

            if let Some(sz) = self.compute_size_of_expr(pointee.ctype) {
                offset = mk().binary_expr(BinOpKind::Div, offset, cast_int(sz, "isize"))
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

        if self.ast_context.is_function_pointer(ty_id) {
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

    pub fn with_scope<F, A>(&self, f: F) -> A
        where F: FnOnce() -> A {
        self.renamer.borrow_mut().add_scope();
        let result = f();
        self.renamer.borrow_mut().drop_scope();
        result
    }

    /// This predicate checks for control-flow statements under a declaration
    /// that will require relooper to be enabled to be handled.
    fn function_requires_relooper(&self, stmt_ids: &[CStmtId]) -> bool {
        stmt_ids
        .iter()
        .flat_map(|&stmt_id| DFExpr::new(&self.ast_context, stmt_id.into()))
        .flat_map(SomeId::stmt)
        .any(|x| {
            match self.ast_context[x].kind {
                CStmtKind::Goto(..) | CStmtKind::Label(..) | CStmtKind::Switch{..} => true,
                _ => false,
            }
        })
    }

    fn mk_int_lit(&self, ty: CQualTypeId, val: u64, base: IntBase) -> P<Expr> {
        // Note that C doesn't have anything smaller than integer literals
        let (intty,suffix) = match self.ast_context.resolve_type(ty.ctype).kind {
                CTypeKind::Int => (LitIntType::Signed(IntTy::I32), "i32"),
                CTypeKind::Long => (LitIntType::Signed(IntTy::I64), "i64"),
                CTypeKind::LongLong => (LitIntType::Signed(IntTy::I64), "i64"),
                CTypeKind::UInt => (LitIntType::Unsigned(UintTy::U32), "u32"),
                CTypeKind::ULong => (LitIntType::Unsigned(UintTy::U64), "u64"),
                CTypeKind::ULongLong => (LitIntType::Unsigned(UintTy::U64), "u64"),
                _ => (LitIntType::Unsuffixed, ""),
            };

        let lit = match base {
            IntBase::Dec => mk().int_lit(val.into(), intty),
            IntBase::Hex => mk().float_unsuffixed_lit(format!("0x{:x}{}", val, suffix)),
            IntBase::Oct => mk().float_unsuffixed_lit(format!("0o{:o}{}", val, suffix)),
        };

        mk().lit_expr(lit)
    }

    fn should_force_lvalue(&self, cast_kind: CastKind) -> bool {
        match cast_kind {
            CastKind::ArrayToPointerDecay | CastKind::FunctionToPointerDecay |
            CastKind::LValueToRValue | CastKind::ToVoid => true,
            _ => false
        }
    }
}
