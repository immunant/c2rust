use std::cell::RefCell;
use std::{char,io,mem};
use std::ops::Index;
use std::path::{self, PathBuf};

use dtoa;

use indexmap::{IndexMap, IndexSet};

use syntax::ast::*;
use syntax::parse::token::{DelimToken, Token, Nonterminal};
use syntax::print::pprust::*;
use syntax::ptr::*;
use syntax::tokenstream::{TokenStream, TokenTree};
use syntax::{with_globals, ast};
use syntax_pos::{DUMMY_SP, Span};

use rust_ast::comment_store::CommentStore;
use rust_ast::item_store::ItemStore;
use rust_ast::traverse::Traversal;
use c2rust_ast_builder::{mk, Builder};

use TranspilerConfig;
use c_ast::*;
use c_ast::iterators::{DFExpr, SomeId};
use c_ast;
use cfg;
use c2rust_ast_exporter::clang_ast::LRValue;
use c2rust_ast_exporter::get_clang_major_version;
use convert_type::TypeConverter;
use renamer::Renamer;
use with_stmts::WithStmts;

mod assembly;
mod bitfields;
mod builtins;
mod literals;
mod main_function;
mod named_references;
mod operators;
mod simd;
mod variadic;

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

    #[inline]
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

#[derive(Copy, Clone, Debug)]
pub struct ExprContext {
    used: bool,
    is_static: bool,
    decay_ref: DecayRef,
    va_decl: Option<CDeclId>,
    is_bitfield_write: bool,
    needs_address: bool,
}

impl ExprContext {
    pub fn used(self) -> Self { ExprContext { used: true, .. self } }
    pub fn unused(self) -> Self { ExprContext { used: false, .. self } }
    pub fn is_used(&self) -> bool { self.used }
    pub fn is_unused(&self) -> bool { !self.used }
    pub fn decay_ref(self) -> Self { ExprContext { decay_ref: DecayRef::Yes, .. self } }
    pub fn not_static(self) -> Self { ExprContext { is_static: false, .. self } }
    pub fn static_(self) -> Self { ExprContext { is_static: true, .. self } }
    pub fn set_static(self, is_static: bool) -> Self { ExprContext { is_static, .. self } }
    pub fn is_va_decl(&self, decl_id: CDeclId) -> bool { Some(decl_id) == self.va_decl }
    pub fn is_bitfield_write(&self) -> bool { self.is_bitfield_write }
    pub fn set_bitfield_write(self, is_bitfield_write: bool) -> Self {
        ExprContext { is_bitfield_write, .. self }
    }
    pub fn needs_address(&self) -> bool { self.needs_address }
    pub fn set_needs_address(self, needs_address: bool) -> Self {
        ExprContext { needs_address, .. self }
    }
}

pub struct Translation<'c> {

    // Translation environment
    pub ast_context: TypedAstContext,
    pub tcfg: &'c TranspilerConfig,

    // Accumulated outputs
    pub features: RefCell<IndexSet<&'static str>>,
    pub item_store: RefCell<ItemStore>,
    sectioned_static_initializers: RefCell<Vec<Stmt>>,
    extern_crates: RefCell<IndexSet<&'static str>>,

    // Translation state and utilities
    type_converter: RefCell<TypeConverter>,
    renamer: RefCell<Renamer<CDeclId>>,
    zero_inits: RefCell<IndexMap<CDeclId, Result<P<Expr>, String>>>,

    // Comment support
    pub comment_context: RefCell<CommentContext>, // Incoming comments
    pub comment_store: RefCell<CommentStore>, // Outgoing comments

    // Mod block defintion reorganization
    mod_blocks: RefCell<IndexMap<PathBuf, ItemStore>>,

    // Mod names to try to stop collisions from happening
    mod_names: RefCell<IndexMap<String, PathBuf>>,

    // The file that the translator is operating on w/o its extension
    main_file: PathBuf,
}

fn simple_metaitem(name: &str) -> NestedMetaItem {
    let meta_item = mk().meta_item(vec![name], MetaItemKind::Word);

    mk().nested_meta_item(NestedMetaItemKind::MetaItem(meta_item))
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

fn transmute_expr(source_ty: P<Ty>, target_ty: P<Ty>, expr: P<Expr>, no_std: bool) -> P<Expr> {
    let type_args = match (&source_ty.node, &target_ty.node) {
        (TyKind::Infer, TyKind::Infer) => Vec::new(),
        (_, TyKind::Infer) => vec![source_ty],
        _ => vec![source_ty, target_ty],
    };
    let std_or_core = if no_std {
        "core"
    } else {
        "std"
    };
    let mut path = vec![
        mk().path_segment(""),
        mk().path_segment(std_or_core),
        mk().path_segment("mem"),
    ];

    if type_args.is_empty() {
        path.push(mk().path_segment("transmute"));
    } else {
        path.push(mk().path_segment_with_args("transmute",
                  mk().angle_bracketed_args(type_args)));
    }

    mk().call_expr(mk().path_expr(path), vec![expr])
}

fn vec_expr(val: P<Expr>, count: P<Expr>) -> P<Expr> {
    let from_elem = mk().path_expr(vec!["", "std", "vec", "from_elem"]);
    mk().call_expr(from_elem, vec![val, count])
}

pub fn stmts_block(mut stmts: Vec<Stmt>) -> P<Block> {
    if stmts.len() == 1 {
        if let StmtKind::Expr(ref e) = stmts[0].node {
            if let ExprKind::Block(ref b, None) = e.node {
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
                // SIMD types are imported and do not need to be renamed
                if name.starts_with("_mm") {
                    continue;
                }

                name.insert_str(0, prefix);

                translation.renamer.borrow_mut().insert(decl_id, &name);
            },
            CDeclKind::Variable { ref mut ident, has_static_duration, has_thread_duration, .. }
            if has_static_duration || has_thread_duration => ident.insert_str(0, &prefix),
            _ => (),
        }
    }
}

// This function is meant to create module names, for modules being created with the
// `--reorganize-modules` flag. So what is done is, change '.' && '-' to '_', and depending
// on whether there is a collision or not prepend the prior directory name to the path name.
// To check for collisions, a IndexMap with the path name(key) and the path(value) associated with
// the name. If the path name is in use, but the paths differ there is a collision.
fn clean_path(mod_names: &RefCell<IndexMap<String, PathBuf>>, path: &path::Path) -> String {
    fn path_to_str(path: &path::Path) -> String {
        path.file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .replace('.', "_")
            .replace('-', "_")
    }

    let mut file_path: String = path_to_str(path);
    let mut mod_names = mod_names.borrow_mut();
    if !mod_names.contains_key(&file_path.clone()) {
        mod_names.insert(file_path.clone(), path.to_path_buf());
    } else {
        let mod_path = mod_names.get(&file_path.clone()).unwrap();
        // A collision in the module names has occured.
        // Ex: types.h can be included from
        // /usr/include/bits and /usr/include/sys
        if mod_path != path {
            let path_copy = path.to_path_buf();
            let split_path: Vec<PathBuf> = path_copy.parent().unwrap()
                .iter().map(|os| PathBuf::from(os)).collect();

            let mut to_prepend = path_to_str(split_path.last().unwrap());
            to_prepend.push('_');
            file_path.insert_str(0, &to_prepend);
        }
    }
    file_path
}

pub fn translate_failure(tcfg: &TranspilerConfig, msg: &str) {
    if tcfg.fail_on_error {
        panic!("{}", msg)
    } else {
        eprintln!("{}", msg)
    }
}

pub fn translate(ast_context: TypedAstContext, tcfg: &TranspilerConfig, main_file: PathBuf) -> String {

    let mut t = Translation::new(ast_context, tcfg, main_file);
    let ctx = ExprContext {
        used: true,
        is_static: false,
        decay_ref: DecayRef::Default,
        va_decl: None,
        is_bitfield_write: false,
        needs_address: false,
    };

    if t.tcfg.reorganize_definitions {
        t.features.borrow_mut().insert("custom_attribute");
    }

    if tcfg.emit_no_std {
        t.extern_crates.borrow_mut().insert("core");
    }

    t.extern_crates.borrow_mut().insert("libc");

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
    if let Some(ref prefix) = t.tcfg.prefix_function_names {
        prefix_names(&mut t, prefix);
    }

    // `with_globals` sets up a thread-local variable required by the syntax crate.
    with_globals(|| {
        // Identify typedefs that name unnamed types and collapse the two declarations
        // into a single name and declaration, eliminating the typedef altogether.
        for (&decl_id, decl) in &t.ast_context.c_decls {
            if let CDeclKind::Typedef { ref name, typ, .. } = decl.kind {
                if let Some(subdecl_id) = t.ast_context.resolve_type(typ.ctype).kind.as_underlying_decl() {
                    let is_unnamed = match t.ast_context[subdecl_id].kind {
                        CDeclKind::Struct { name: None, .. } |
                        CDeclKind::Union { name: None, .. } |
                        CDeclKind::Enum { name: None, .. } => true,

                        // Detect case where typedef and struct share the same name.
                        // In this case the purpose of the typedef was simply to eliminate
                        // the need for the 'struct' tag when refering to the type name.
                        CDeclKind::Struct { name: Some(ref target_name), ..} |
                        CDeclKind::Union { name: Some(ref target_name), .. } |
                        CDeclKind::Enum { name: Some(ref target_name), .. } => name == target_name,

                        _ => false,
                    };

                    if is_unnamed && !t.ast_context.prenamed_decls.values().find(|decl_id| *decl_id == &subdecl_id).is_some() {
                        t.ast_context.prenamed_decls.insert(decl_id, subdecl_id);

                        t.type_converter.borrow_mut().declare_decl_name(decl_id, name);
                        t.type_converter.borrow_mut().alias_decl_name(subdecl_id, decl_id);
                    }
                }
            }
        }

        // Helper function that returns true if there is either a matching typedef or its
        // corresponding struct/union/enum
        fn contains(prenamed_decls: &IndexMap<CDeclId, CDeclId>, decl_id: &CDeclId) -> bool {
            (prenamed_decls.contains_key(decl_id) ||
             prenamed_decls.values().find(|id| *id == decl_id).is_some())
        }

        // Populate renamer with top-level names
        for (&decl_id, decl) in &t.ast_context.c_decls {
            let decl_name = match decl.kind {
                _ if contains(&t.ast_context.prenamed_decls, &decl_id) => Name::NoName,
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
                    // Only check the key as opposed to `contains` because the key should be the
                    // typedef id
                    !t.ast_context.prenamed_decls.contains_key(&decl_id),
                _ => false,
            };
            if needs_export {
                let decl_file_path = decl.loc.as_ref().map(|loc| &loc.file_path).into_iter().flatten().next();
                let main_file_path = &t.main_file;

                if t.tcfg.reorganize_definitions && decl_file_path != Some(main_file_path) {
                    t.generate_submodule_imports(decl_id, decl_file_path);
                }

                match t.convert_decl(ctx, true, decl_id) {
                    Ok(ConvertedDecl::Item(item)) => t.insert_item(item, decl_file_path, main_file_path),
                    Ok(ConvertedDecl::ForeignItem(item)) => t.insert_foreign_item(item, decl_file_path, &main_file_path),
                    Ok(ConvertedDecl::NoItem) => {},
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
                let decl_opt = t.ast_context.c_decls.get(top_id);
                let decl = decl_opt.as_ref().unwrap();
                let decl_file_path = match decl.loc.as_ref().map(|loc| &loc.file_path) {
                    Some(Some(s)) => Some(s),
                    _ => None,
                };
                let main_file_path = &t.main_file;

                if t.tcfg.reorganize_definitions && decl_file_path != Some(main_file_path) {
                    t.generate_submodule_imports(*top_id, decl_file_path);
                }

                match t.convert_decl(ctx, true, *top_id) {
                    Ok(ConvertedDecl::Item(mut item)) => t.item_store.borrow_mut().items.push(item),
                    Ok(ConvertedDecl::ForeignItem(item)) => t.insert_foreign_item(item, decl_file_path, main_file_path),
                    Ok(ConvertedDecl::NoItem) => {},
                    Err(e) => {
                        let ref decl = t.ast_context.c_decls.get(top_id);
                        let msg = format!("Failed translating declaration due to error: {}, decl: {:?}", e, decl);
                        translate_failure(&t.tcfg, &msg)
                    },
                }
            }
        }

        // Add the main entry point
        if let Some(main_id) = t.ast_context.c_main {
            match t.convert_main(main_id) {
                Ok(item) => t.item_store.borrow_mut().items.push(item),
                Err(e) => {
                    let msg = format!("Failed translating main declaration due to error: {}", e);
                    translate_failure(&t.tcfg, &msg)
                }
            }
        }

        // Initialize global statics when necessary
        if !t.sectioned_static_initializers.borrow().is_empty() {
            let (initializer_fn, initializer_static) = t.generate_global_static_init();
            let items = &mut t.item_store.borrow_mut().items;

            items.push(initializer_fn);
            items.push(initializer_static);
        }

        // pass all converted items to the Rust pretty printer
        to_string(|s| {

            print_header(s,&t)?;

            // Re-order comments
            let mut traverser = t.comment_store.into_inner().into_comment_traverser();
            let mut mod_items: Vec<P<Item>> = Vec::new();

            // Header Reorganization: Submodule Item Stores
            for (file_path, ref mut mod_item_store) in t.mod_blocks.borrow_mut().iter_mut() {
                mod_items.push(make_submodule(mod_item_store, file_path, &t.item_store, &t.mod_names));
            }

            // Global Item Store
            let (items, foreign_items, uses) = t.item_store.borrow_mut().drain();

            mod_items = mod_items.into_iter()
                .map(|p_i| p_i.map(|i| traverser.traverse_item(i)))
                .collect();
            let foreign_items: Vec<ForeignItem> = foreign_items
                .into_iter()
                .map(|fi| traverser.traverse_foreign_item(fi))
                .collect();
            let items: Vec<P<Item>> = items
                .into_iter()
                .map(|p_i| p_i.map(|i| traverser.traverse_item(i)))
                .collect();

            s.comments().get_or_insert(vec![]).extend(traverser.into_comment_store().into_comments());

            for mod_item in mod_items {
                s.print_item(&*mod_item)?;
            }

            // This could have been merged in with items below; however, it's more idiomatic to have
            // imports near the top of the file than randomly scattered about. Also, there is probably
            // no reason to have comments associated with imports so it doesn't need to go through
            // the above comment store process
            for use_item in uses.into_items() {
                s.print_item(&use_item)?;
            }

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

fn make_submodule(submodule_item_store: &mut ItemStore, file_path: &path::Path,
                  global_item_store: &RefCell<ItemStore>,
                  mod_names: &RefCell<IndexMap<String, PathBuf>>) -> P<Item> {
    let (mut items, foreign_items, uses) = submodule_item_store.drain();
    let file_path_str = file_path.to_str().expect("Found invalid unicode");
    let mod_name = clean_path(mod_names, file_path);
    let mut global_item_store = global_item_store.borrow_mut();

    for item in items.iter() {
        let ident_name = item.ident.name.as_str();
        let use_path = vec!["self".into(), mod_name.clone()];

        global_item_store
            .uses
            .get_mut(use_path)
            .insert(&*ident_name);
    }

    for foreign_item in foreign_items.iter() {
        let ident_name = foreign_item.ident.name.as_str();
        let use_path = vec!["self".into(), mod_name.clone()];

        global_item_store
            .uses
            .get_mut(use_path)
            .insert(&*ident_name);
    }

    for item in uses.into_items() {
        items.push(item);
    }

    if !foreign_items.is_empty() {
        items.push(mk().abi("C").foreign_items(foreign_items));
    }

    mk().vis("pub")
        .str_attr("header_src", file_path_str)
        .mod_item(mod_name, mk().mod_(items))
}

/// Pretty-print the leading pragmas and extern crate declarations
fn print_header(s: &mut State, t: &Translation) -> io::Result<()> {
    if t.tcfg.emit_modules {
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
                let file_item = mk().meta_item(vec!["config_file"], file_lit);
                xcheck_plugin_args.push(mk().nested_meta_item(file_item));
            }
            let xcheck_plugin_item = mk().meta_item(
                vec!["c2rust_xcheck_plugin"],
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

        if t.tcfg.emit_no_std {
            s.print_attribute(&mk().single_attr("no_std").as_inner_attrs()[0])?;
        }

        // Add `extern crate X;` to the top of the file
        for crate_name in t.extern_crates.borrow().iter() {
            s.print_item(&mk().extern_crate_item(*crate_name, None))?;
        }

        if t.tcfg.cross_checks {
            s.print_item(&mk().single_attr("macro_use")
                .extern_crate_item("c2rust_xcheck_derive", None))?;
            s.print_item(&mk().single_attr("macro_use")
                .extern_crate_item("c2rust_xcheck_runtime", None))?;
            // When cross-checking, always use the system allocator
            let sys_alloc_path = vec!["", "std", "alloc", "System"];
            s.print_item(&mk().single_attr("global_allocator")
                .static_item("C2RUST_ALLOC",
                             mk().path_ty(sys_alloc_path.clone()),
                             mk().path_expr(sys_alloc_path)))?;
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
    /// expressions used for their values
    Used,
}

/// Declarations can be converted into a normal item, or into a foreign item.
/// Foreign items are called out specially because we'll combine all of them
/// into a single extern block at the end of translation.
#[derive(Debug)]
pub enum ConvertedDecl {
    ForeignItem(ForeignItem),
    Item(P<Item>),
    NoItem,
}

impl<'c> Translation<'c> {
    pub fn new(mut ast_context: TypedAstContext, tcfg: &'c TranspilerConfig, main_file: PathBuf) -> Self {
        let comment_context = RefCell::new(CommentContext::new(&mut ast_context));
        let mut type_converter = TypeConverter::new(tcfg.emit_no_std);

        if tcfg.translate_valist { type_converter.translate_valist = true }

        Translation {
            features: RefCell::new(IndexSet::new()),
            item_store: RefCell::new(ItemStore::new()),
            type_converter: RefCell::new(type_converter),
            ast_context,
            tcfg,
            renamer: RefCell::new(Renamer::new(&[
                // Keywords currently in use
                "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
                "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
                "ref", "return", "Self", "self", "static", "struct", "super", "trait", "true",
                "type", "unsafe", "use", "where", "while", "dyn",

                // Keywords reserved for future use
                "abstract", "alignof", "become", "box", "do", "final", "macro", "offsetof",
                "override", "priv", "proc", "pure", "sizeof", "typeof", "unsized", "virtual",
                "yield",

                // Prevent use for other reasons
                "main",

                // prelude names
                "drop", "Some", "None", "Ok", "Err",
            ])),
            zero_inits: RefCell::new(IndexMap::new()),
            comment_context,
            comment_store: RefCell::new(CommentStore::new()),
            sectioned_static_initializers: RefCell::new(Vec::new()),
            mod_blocks: RefCell::new(IndexMap::new()),
            mod_names: RefCell::new(IndexMap::new()),
            main_file,
            extern_crates: RefCell::new(IndexSet::new()),
        }
    }

    /// Called when translation makes use of a language feature that will require a feature-gate.
    pub fn use_feature(&self, feature: &'static str) {
        self.features.borrow_mut().insert(feature);
    }

    // This node should _never_ show up in the final generated code. This is an easy way to notice
    // if it does.
    pub fn panic(&self, msg: &str) -> P<Expr> {
        let macro_name = if self.tcfg.panic_on_translator_failure { "panic" } else { "compile_error" };
        let macro_msg = vec![
            Token::interpolated(Nonterminal::NtExpr(mk().lit_expr(mk().str_lit(msg)))),
        ].into_iter().collect::<TokenStream>();
        mk().mac_expr(mk().mac(vec![macro_name], macro_msg, MacDelimiter::Parenthesis))
    }

    fn mk_cross_check(&self, mk: Builder, args: Vec<&str>) -> Builder {
        if self.tcfg.cross_checks {
            mk.call_attr("cross_check", args)
        } else { mk }
    }

    fn static_initializer_is_unsafe(&self, expr_id: Option<CExprId>, qty: CQualTypeId) -> bool {
        // SIMD types are always unsafe in statics
        match self.ast_context.resolve_type(qty.ctype).kind {
            CTypeKind::Vector(..) => return true,
            CTypeKind::ConstantArray(ctype, ..) => {
                let kind = &self.ast_context.resolve_type(ctype).kind;

                if let CTypeKind::Vector(..) = kind {
                    return true;
                }
            }
            _ => {},
        }

        // Get the initializer if there is one
        let expr_id = match expr_id {
            Some(expr_id) => expr_id,
            None => return false,
        };

        // Look for code which can only be translated unsafely
        let iter = DFExpr::new(&self.ast_context, expr_id.into());

        for i in iter {
            let expr_id = match i {
                SomeId::Expr(expr_id) => expr_id,
                _ => unreachable!("Found static initializer type other than expr"),
            };

            match self.ast_context[expr_id].kind {
                CExprKind::DeclRef(_, _, LRValue::LValue) => return true,
                CExprKind::ImplicitCast(_, _, CastKind::IntegralToPointer, _, _) |
                CExprKind::ExplicitCast(_, _, CastKind::IntegralToPointer, _, _) => {
                    return true;
                },
                _ => {},
            }
        }

        false
    }

    /// The purpose of this function is to decide on whether or not a static initializer's
    /// translation is able to be compiled as a valid rust static initializer
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
                CExprKind::Member(..) => return true,

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
                CExprKind::InitList(qtype, _, _, _) => {
                    let ty = &self.ast_context.resolve_type(qtype.ctype).kind;

                    match ty {
                        CTypeKind::Struct(decl_id) => {
                            let decl = &self.ast_context[*decl_id].kind;

                            if let CDeclKind::Struct { fields: Some(fields), .. } = decl {
                                for field_id in fields {
                                    let field_decl = &self.ast_context[*field_id].kind;

                                    if let CDeclKind::Field { bitfield_width: Some(_), .. } = field_decl {
                                        return true;
                                    }
                                }
                            }
                        },
                        _ => {},
                    }

                },
                _ => {},
            }
        }

        false
    }

    fn add_static_initializer_to_section(&self, name: &str, typ: CQualTypeId, init: &mut P<Expr>) -> Result<(), String> {
        let mut default_init = self.implicit_default_expr(typ.ctype, true)?;

        std::mem::swap(init, &mut default_init);

        let root_lhs_expr = mk().path_expr(vec![name]);
        let assign_expr = mk().assign_expr(root_lhs_expr, default_init);
        let stmt = mk().expr_stmt(assign_expr);

        self.sectioned_static_initializers.borrow_mut().push(stmt);

        Ok(())
    }

    fn generate_global_static_init(&mut self) -> (P<Item>, P<Item>) {
        // If we don't want to consume self.sectioned_static_initializers for some reason, we could clone the vec
        let sectioned_static_initializers = self.sectioned_static_initializers.replace(Vec::new());

        let fn_name = self.renamer.borrow_mut().pick_name("run_static_initializers");
        let fn_ty = FunctionRetTy::Default(DUMMY_SP);
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

    fn convert_decl(&self, ctx: ExprContext, toplevel: bool, decl_id: CDeclId) -> Result<ConvertedDecl, String> {

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
                let extern_item = mk().span(s).pub_().ty_foreign_item(name);
                Ok(ConvertedDecl::ForeignItem(extern_item))
            }

            CDeclKind::Struct {
                fields: Some(ref fields),
                is_packed,
                manual_alignment,
                max_field_alignment,
                platform_byte_size,
                ..
            } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();
                let mut has_bitfields = false;

                // Gather up all the field names and field types
                let mut field_entries = vec![];
                let mut field_info = Vec::new();

                for &x in fields {
                    match self.ast_context.index(x).kind {
                        CDeclKind::Field { ref name, typ, bitfield_width, platform_bit_offset, platform_type_bitwidth } => {
                            let name = self.type_converter.borrow_mut().declare_field_name(decl_id, x, name);

                            has_bitfields |= bitfield_width.is_some();

                            field_info.push((name.clone(), typ.clone(), bitfield_width, platform_bit_offset, platform_type_bitwidth));

                            let typ = self.convert_type(typ.ctype)?;

                            field_entries.push(mk().span(s).pub_().struct_field(name, typ));
                        }
                        _ => return Err(format!("Found non-field in record field list")),
                    }
                }

                if has_bitfields {
                    return self.convert_bitfield_struct_decl(name, manual_alignment, platform_byte_size, s, field_info);
                }

                let mut reprs = vec![simple_metaitem("C")];

                if is_packed || max_field_alignment == Some(1) { reprs.push(simple_metaitem("packed")); };
                // https://github.com/rust-lang/rust/issues/33626
                if let Some(alignment) = manual_alignment {
                    let lit = mk().int_lit(alignment as u128, LitIntType::Unsuffixed);
                    let inner = mk().meta_item(
                        vec!["align"],
                        MetaItemKind::List(
                            vec![mk().nested_meta_item(
                                NestedMetaItemKind::Literal(lit))]));
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
                        CDeclKind::Field { ref name, typ, .. } => {
                            let name = self.type_converter.borrow_mut().declare_field_name(decl_id, x, name);
                            let typ = self.convert_type(typ.ctype)?;
                            field_syns.push(mk().span(s).pub_().struct_field(name, typ))
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
            CDeclKind::Function { is_global, is_inline, is_extern, typ, ref name, ref parameters, body, ref attrs, .. } => {
                let new_name = &self.renamer.borrow().get(&decl_id).expect("Functions should already be renamed");

                if self.import_simd_function(new_name)? {
                    return Ok(ConvertedDecl::NoItem);
                }

                let (ret, is_var): (Option<CQualTypeId>, bool) = match self.ast_context.resolve_type(typ).kind {
                    CTypeKind::Function(ret, _, is_var, is_noreturn, _) => (if is_noreturn { None } else { Some(ret) }, is_var),
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
                    self.convert_function(ctx, s, is_global, is_inline, is_main, is_var,
                                          is_extern, new_name, name, &args, ret, body, attrs);

                converted_function.or_else(|e|
                    match self.tcfg.replace_unsupported_decls {
                        ReplaceMode::Extern if body.is_none() =>
                            self.convert_function(ctx, s, is_global, false, is_main, is_var,
                                                  is_extern, new_name, name, &args, ret, None, attrs),
                        _ => Err(e),
                    })
            },

            CDeclKind::Typedef { ref typ, .. } => {
                let new_name = &self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                if self.import_simd_typedef(new_name) {
                    return Ok(ConvertedDecl::NoItem);
                }

                let ty = self.convert_type(typ.ctype)?;
                Ok(ConvertedDecl::Item(mk().span(s).pub_().type_item(new_name, ty)))
            },

            // Externally-visible variable without initializer (definition elsewhere)
            CDeclKind::Variable { is_externally_visible: true, has_static_duration, has_thread_duration, is_defn: false, ref ident, initializer, typ, ref attrs, .. } => {
                assert!(has_static_duration || has_thread_duration, "An extern variable must be static or thread-local");
                assert!(initializer.is_none(), "An extern variable that isn't a definition can't have an initializer");

                if has_thread_duration {
                    self.use_feature("thread_local");
                }

                let new_name = self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");
                let (ty, mutbl, _) = self.convert_variable(ctx.static_(), None, typ)?;
                // When putting extern statics into submodules, they need to be public to be accessible
                let visibility = if self.tcfg.reorganize_definitions {
                    "pub"
                } else {
                    ""
                };
                let mut extern_item = mk_linkage(true, &new_name, ident)
                    .span(s)
                    .set_mutbl(mutbl)
                    .vis(visibility);
                if has_thread_duration {
                    extern_item = extern_item.single_attr("thread_local");
                }

                for attr in attrs {
                    extern_item = match attr {
                        c_ast::Attribute::Alias(aliasee) => extern_item.str_attr("link_name", aliasee),
                        _ => continue,
                    };
                }

                Ok(ConvertedDecl::ForeignItem(extern_item.static_foreign_item(&new_name, ty)))
            }

            // Static-storage or thread-local variable with initializer (definition here)
            CDeclKind::Variable { has_static_duration, has_thread_duration, is_externally_visible, ref ident, initializer, typ, ref attrs, .. }
            if has_static_duration || has_thread_duration => {
                if has_thread_duration {
                    self.use_feature("thread_local");
                }

                let new_name = &self.renamer.borrow().get(&decl_id).expect("Variables should already be renamed");

                // Collect problematic static initializers and offload them to sections for the linker
                // to initialize for us
                let (ty, init) = if self.static_initializer_is_uncompilable(initializer) {
                    // Note: We don't pass has_static_duration through here. Extracted initializers
                    // are run outside of the static initializer.
                    let (ty, _, init) = self.convert_variable(ctx.not_static(), initializer, typ)?;

                    let mut init = init?;
                    init.stmts.push(mk().expr_stmt(init.val));
                    let init = mk().block(init.stmts);
                    let mut init = mk().block_expr(init);

                    let comment = String::from("// Initialized in run_static_initializers");
                    // REVIEW: We might want to add the comment to the original span comments
                    s = self.comment_store.borrow_mut().add_comment_lines(vec![comment]);

                    self.add_static_initializer_to_section(new_name, typ, &mut init)?;

                    (ty, init)
                } else {
                    let (ty, _, init) = self.convert_variable(ctx.static_(), initializer, typ)?;

                    let init = if self.static_initializer_is_unsafe(initializer, typ) {
                        let mut init = init?;
                        init.stmts.push(mk().expr_stmt(init.val));
                        let init = mk().unsafe_().block(init.stmts);

                        mk().block_expr(init)
                    } else {
                        let init = init?;
                        assert!(init.stmts.is_empty(), "Expected no side-effects in static initializer");
                        init.val
                    };

                    (ty, init)
                };

                let static_def = if is_externally_visible {
                    mk_linkage(false, new_name, ident).pub_().abi("C")
                } else {
                    mk()
                };

                // Force mutability due to the potential for raw pointers occuring in the type
                // and because we may be assigning to these variables in the external initializer
                let mut static_def = static_def.span(s).mutbl();
                if has_thread_duration {
                    static_def = static_def.single_attr("thread_local");
                }

                // Add static attributes
                for attr in attrs {
                    static_def = match attr {
                        c_ast::Attribute::Used => static_def.single_attr("used"),
                        c_ast::Attribute::Section(name) => static_def.str_attr("link_section", name),
                        _ => continue,
                    }
                }

                Ok(ConvertedDecl::Item(static_def.static_item(new_name, ty, init)))
            }

            CDeclKind::Variable { .. } => Err(format!("This should be handled in 'convert_decl_stmt'")),

            //ref k => Err(format!("Translation not implemented for {:?}", k)),
        }
    }

    fn convert_function(
        &self,
        mut ctx: ExprContext,
        span: Span,
        is_global: bool,
        is_inline: bool,
        is_main: bool,
        is_variadic: bool,
        is_extern: bool,
        new_name: &str,
        name: &str,
        arguments: &[(CDeclId, String, CQualTypeId)],
        return_type: Option<CQualTypeId>,
        body: Option<CStmtId>,
        attrs: &IndexSet<c_ast::Attribute>,
    ) -> Result<ConvertedDecl, String> {

        if is_variadic {
            if let Some(body_id) = body {
                match self.well_formed_variadic(body_id) {
                    None =>
                        return Err(format!(
                            "Failed to translate {}; unsupported variadic function.", name)),
                    Some(va_id) => {
                        self.register_va_arg(va_id);
                        ctx.va_decl = Some(va_id);
                    }
                }
            }
        }

        self.with_scope(|| {
            let mut args: Vec<Arg> = vec![];

            for &(decl_id, ref var, typ) in arguments {


                let (ty, mutbl, _) = self.convert_variable(ctx, None, typ)?;

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
            let is_void_ret = return_type.map(|qty| self.ast_context[qty.ctype].kind == CTypeKind::Void).unwrap_or(false);

            // If a return type is void, we should instead omit the unit type return,
            // -> (), to be more idiomatic
            let ret = if is_void_ret {
                FunctionRetTy::Default(DUMMY_SP)
            } else {
                FunctionRetTy::Ty(ret)
            };

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
                    body_stmts.append(&mut self.compute_variable_array_sizes(ctx, typ.ctype)?);
                }

                let body_ids = match self.ast_context.index(body).kind {
                    CStmtKind::Compound(ref stmts) => stmts,
                    _ => panic!("function body expects to be a compound statement"),
                };
                body_stmts.append(&mut self.convert_function_body(ctx, name, body_ids, ret)?);
                let block = stmts_block(body_stmts);

                // Only add linkage attributes if the function is `extern`
                let mut mk_ = if is_main {
                    // Cross-check this function as if it was called `main`
                    // FIXME: pass in a vector of NestedMetaItem elements,
                    // but strings have to do for now
                    self.mk_cross_check(mk(), vec!["entry(djb2=\"main\")",
                                                   "exit(djb2=\"main\")"])
                } else if is_global && !is_inline {
                    mk_linkage(false, new_name, name)
                        .abi("C")
                        .pub_()
                } else if is_inline && is_extern && !attrs.contains(&c_ast::Attribute::GnuInline) {
                    // c99 extern inline functions should be pub, but not gnu_inline attributed
                    // extern inlines, which become subject to their gnu89 visibility (private)

                    mk_linkage(false, new_name, name)
                        .abi("C")
                        .pub_()
                } else {
                    mk().abi("C")
                };

                for attr in attrs {
                    mk_ = match attr {
                        c_ast::Attribute::AlwaysInline => mk_.single_attr("inline(always)"),
                        c_ast::Attribute::Cold => mk_.single_attr("cold"),
                        c_ast::Attribute::NoInline => mk_.single_attr("inline(never)"),
                        _ => continue,
                    };
                }

                // If this function is just a regular inline
                if is_inline && !attrs.contains(&c_ast::Attribute::AlwaysInline) {
                    mk_ = mk_.single_attr("inline");
                }

                Ok(ConvertedDecl::Item(mk_.span(span).unsafe_().fn_item(new_name, decl, block)))
            } else {
                // Translating an extern function declaration

                // When putting extern fns into submodules, they need to be public to be accessible
                let visibility = if self.tcfg.reorganize_definitions {
                    "pub"
                } else {
                    ""
                };

                let mut mk_ = mk_linkage(true, new_name, name)
                    .span(span)
                    .vis(visibility);

                for attr in attrs {
                    mk_ = match attr {
                        c_ast::Attribute::Alias(aliasee) => mk_.str_attr("link_name", aliasee),
                        _ => continue,
                    };
                }

                let function_decl = mk_.fn_foreign_item(new_name, decl);

                Ok(ConvertedDecl::ForeignItem(function_decl))
            }
        })
    }

    pub fn convert_cfg(
        &self,
        name: &str,
        graph: cfg::Cfg<cfg::Label, cfg::StmtOrDecl>,
        store: cfg::DeclStmtStore,
        live_in: IndexSet<CDeclId>,
        cut_out_trailing_ret: bool,
    ) -> Result<Vec<Stmt>, String> {

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
            live_in,
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
            self.tcfg.debug_relooper_labels,
            cut_out_trailing_ret,
        )?);
        Ok(stmts)
    }

    fn convert_function_body(
        &self,
        ctx: ExprContext,
        name: &str,
        body_ids: &[CStmtId],
        ret: cfg::ImplicitReturnType,
    ) -> Result<Vec<Stmt>, String> {

        // Function body scope
        self.with_scope(|| {
            let (graph, store) = cfg::Cfg::from_stmts(self, ctx, body_ids, ret)?;
            self.convert_cfg(name, graph, store, IndexSet::new(), true)
        })
    }

    /// Convert a C expression to a rust boolean expression
    pub fn convert_condition(&self, ctx: ExprContext, target: bool, cond_id: CExprId) -> Result<WithStmts<P<Expr>>, String> {
        let ty_id = self.ast_context[cond_id].kind.get_type().ok_or_else(|| format!("bad condition type"))?;

        let null_pointer_case =
            |negated: bool, ptr: CExprId| -> Result<WithStmts<P<Expr>>, String> {
                    let val = self.convert_expr(ctx.used().decay_ref(), ptr)?;
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
                self.convert_condition(ctx, !target, subexpr_id)
            }

            _ => {
                // DecayRef could (and probably should) be Default instead of Yes here; however, as noted
                // in https://github.com/rust-lang/rust/issues/53772, you cant compare a reference (lhs) to
                // a ptr (rhs) (even though the reverse works!). We could also be smarter here and just
                // specify Yes for that particular case, given enough analysis.
                let val = self.convert_expr(ctx.used().decay_ref(), cond_id)?;
                Ok(val.map(|e| self.match_bool(target, ty_id, e)))
            }
        }
    }

    pub fn convert_decl_stmt(&self, ctx: ExprContext, decl_id: CDeclId) -> Result<Vec<Stmt>, String> {

        match self.convert_decl_stmt_info(ctx, decl_id)? {
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

    pub fn convert_decl_stmt_info(&self, ctx: ExprContext, decl_id: CDeclId) -> Result<cfg::DeclStmtInfo, String> {
        if ctx.is_va_decl(decl_id) {
            return Ok(cfg::DeclStmtInfo::empty())
        }

        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable { ref ident, has_static_duration: true, is_externally_visible: false, is_defn: true, initializer, typ, .. } => {
                if self.static_initializer_is_uncompilable(initializer) {
                    let err_msg = || String::from("Unable to rename function scoped static initializer");
                    let ident2 = self.renamer.borrow_mut().insert_root(decl_id, ident).ok_or_else(err_msg)?;
                    let (ty, _, init) = self.convert_variable(ctx.static_(), initializer, typ)?;
                    let default_init = self.implicit_default_expr(typ.ctype, true)?;
                    let comment = String::from("// Initialized in run_static_initializers");
                    let span = self.comment_store.borrow_mut().add_comment_lines(vec![comment]);
                    let static_item = mk().span(span).mutbl().static_item(&ident2, ty, default_init);
                    let mut init = init?;

                    init.stmts.push(mk().expr_stmt(init.val));

                    let init = mk().unsafe_().block(init.stmts);
                    let mut init = mk().block_expr(init);

                    self.add_static_initializer_to_section(&ident2, typ, &mut init)?;
                    self.item_store.borrow_mut().items.push(static_item);

                    return Ok(cfg::DeclStmtInfo::empty());
                }
            },
            _ => {},
        };

        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable { has_static_duration: false, has_thread_duration: false, is_externally_visible: false, is_defn, ref ident, initializer, typ, .. } => {
                assert!(is_defn, "Only local variable definitions should be extracted");

                let has_self_reference =
                    if let Some(expr_id) = initializer {
                        self.has_decl_reference(decl_id, expr_id)
                    } else {
                        false
                    };

                let mut stmts = self.compute_variable_array_sizes(ctx, typ.ctype)?;

                let rust_name = self.renamer.borrow_mut()
                    .insert(decl_id, &ident)
                    .expect(&format!("Failed to insert variable '{}'", ident));
                let (ty, mutbl, init) = self.convert_variable(ctx, initializer, typ)?;
                let mut init = init?;

                stmts.append(&mut init.stmts);

                if has_self_reference {
                    let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name.clone());
                    let zeroed = self.implicit_default_expr(typ.ctype, false)?;
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
                    let zeroed = self.implicit_default_expr(typ.ctype, false)?;
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
                    let item = match self.convert_decl(ctx, false, decl_id)? {
                        ConvertedDecl::Item(item) => item,
                        ConvertedDecl::ForeignItem(item) => mk().abi("C").foreign_items(vec![item]),
                        ConvertedDecl::NoItem => return Ok(cfg::DeclStmtInfo::empty()),
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
                    CTypeKind::Function(..) => {
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
            CTypeKind::Function(..) => unreachable!("Can't have a function directly as a type"),
            CTypeKind::Typedef(_) => unreachable!("Typedef should be expanded though resolve_type"),
            _ => true,
        }
    }

    fn convert_variable(
        &self,
        ctx: ExprContext,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
    ) -> Result<(P<Ty>, Mutability, Result<WithStmts<P<Expr>>,String>), String> {
        let init = match initializer {
            Some(x) => self.convert_expr(ctx.used(), x),
            None => self.implicit_default_expr(typ.ctype, ctx.is_static).map(WithStmts::new),
        };

        // Variable declarations for variable-length arrays use the type of a pointer to the
        // underlying array element
        let ty = if let CTypeKind::VariableArray(mut elt, _) = self.ast_context.resolve_type(typ.ctype).kind {
            elt = self.variable_array_base_type(elt);
            let ty = self.convert_type(elt)?;
            mk().path_ty(vec![mk().path_segment_with_args("Vec", mk().angle_bracketed_args(vec![ty]))])
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
            self.use_feature("const_raw_ptr_to_usize_cast");
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
        let std_or_core = if self.tcfg.emit_no_std {
            "core"
        } else {
            "std"
        };

        Ok(mk().call_expr(mk().path_expr(vec!["", std_or_core, "ptr", "write_volatile"]), vec![addr_lhs, rhs]))
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
        let std_or_core = if self.tcfg.emit_no_std {
            "core"
        } else {
            "std"
        };

        // We explicitly annotate the type of pointer we're reading from
        // in order to avoid omitted bit-casts to const from causing the
        // wrong type to be inferred via the result of the pointer.
        let mut path_parts: Vec<PathSegment> = vec![];
        for elt in vec!["", std_or_core, "ptr"] {
            path_parts.push(mk().path_segment(elt))
        }
        let elt_ty = self.convert_type(lhs_type.ctype)?;
        let ty_params = mk().angle_bracketed_args(vec![elt_ty]);
        let elt = mk().path_segment_with_args("read_volatile", ty_params);
        path_parts.push(elt);

        let read_volatile_expr = mk().path_expr(path_parts);
        Ok(mk().call_expr(read_volatile_expr, vec![addr_lhs]))
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
    pub fn compute_variable_array_sizes(&self, ctx: ExprContext, mut type_id: CTypeId) -> Result<Vec<Stmt>, String> {

        let mut stmts = vec![];

        loop {
            match self.ast_context.resolve_type(type_id).kind {
                CTypeKind::Pointer(elt) => type_id = elt.ctype,
                CTypeKind::ConstantArray(elt, _) => type_id = elt,
                CTypeKind::VariableArray(elt, Some(expr_id)) => {
                    type_id = elt;

                    // Convert this expression
                    let mut expr = self.convert_expr(ctx.used(), expr_id)?;
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
    pub fn compute_size_of_type(&self, ctx: ExprContext, type_id: CTypeId) -> Result<WithStmts<P<Expr>>, String> {
        if let CTypeKind::VariableArray(elts, len) =
            self.ast_context.resolve_type(type_id).kind {

            let len = len.expect("Sizeof a VLA type with count expression omitted");

            let mut elts = self.compute_size_of_type(ctx, elts)?;
            let mut len = self.convert_expr(ctx.used().not_static(), len)?;

            let mut stmts = elts.stmts;
            stmts.append(&mut len.stmts);

            let lhs = elts.val;
            let rhs = cast_int(len.val, "usize");

            let val = mk().binary_expr(BinOpKind::Mul, lhs, rhs);

            return Ok(WithStmts { stmts, val })
        }
        let std_or_core = if self.tcfg.emit_no_std {
            "core"
        } else {
            "std"
        };
        let ty = self.convert_type(type_id)?;
        let name = "size_of";
        let params = mk().angle_bracketed_args(vec![ty]);
        let path = vec![mk().path_segment(""),
                        mk().path_segment(std_or_core),
                        mk().path_segment("mem"),
                        mk().path_segment_with_args(name, params)];
        let call = mk().call_expr(mk().path_expr(path), vec![] as Vec<P<Expr>>);

        Ok(WithStmts::new(call))
    }

    pub fn compute_align_of_type(&self, mut type_id: CTypeId)
        -> Result<WithStmts<P<Expr>>, String> {

        type_id = self.variable_array_base_type(type_id);

        let ty = self.convert_type(type_id)?;
        let std_or_core = if self.tcfg.emit_no_std {
            "core"
        } else {
            "std"
        };
        let name = "align_of";
        let tys = vec![ty];
        let path = vec![mk().path_segment(""),
                        mk().path_segment(std_or_core),
                        mk().path_segment("mem"),
                        mk().path_segment_with_args(name,
                                                      mk().angle_bracketed_args(tys)),
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
    /// In the case that `use_` is unused, all side-effecting components will be in the
    /// `stmts` field of the output and it is expected that the `val` field of the output will be
    /// ignored.
    pub fn convert_expr(&self, mut ctx: ExprContext, expr_id: CExprId) -> Result<WithStmts<P<Expr>>, String> {
        let src_loc = &self.ast_context[expr_id].loc;
        match self.ast_context[expr_id].kind {
            CExprKind::DesignatedInitExpr(..) => Err(format!("Unexpected designated init expr")),
            CExprKind::BadExpr => Err(format!("convert_expr: expression kind not supported")),
            CExprKind::ShuffleVector(_, ref child_expr_ids) =>
                self.convert_shuffle_vector(ctx, child_expr_ids),
            CExprKind::ConvertVector(..) => Err(format!("convert vector not supported")),

            CExprKind::UnaryType(_ty, kind, opt_expr, arg_ty) => {
                let result = match kind {
                    UnTypeOp::SizeOf =>
                        match opt_expr {
                            None => self.compute_size_of_type(ctx, arg_ty.ctype)?,
                            Some(_) =>
                                {
                                    let inner = self.variable_array_base_type(arg_ty.ctype);
                                    let inner_size = self.compute_size_of_type(ctx, inner)?;

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

            CExprKind::DeclRef(qual_ty, decl_id, lrvalue) => {
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
                if lrvalue.is_rvalue() && qual_ty.qualifiers.is_volatile {
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

            CExprKind::OffsetOf(ty, ref kind) => match kind {
                OffsetOfKind::Constant(val) => Ok(WithStmts::new(self.mk_int_lit(ty, *val, IntBase::Dec))),
                OffsetOfKind::Variable(qty, field_id, expr_id) => {
                    self.extern_crates.borrow_mut().insert("memoffset");
                    self.item_store.borrow_mut()
                        .uses
                        .get_mut(vec!["memoffset".into()])
                        .insert("offset_of");

                    // Struct Type
                    let decl_id = {
                        let kind = match self.ast_context.c_types[&qty.ctype].kind {
                            CTypeKind::Elaborated(ty_id) => &self.ast_context[ty_id].kind,
                            ref kind => kind,
                        };

                        kind.as_decl_or_typedef()
                            .expect("Did not find decl_id for offsetof struct")
                    };
                    let name = self.type_converter
                        .borrow()
                        .resolve_decl_name(decl_id)
                        .expect("Did not find name for offsetof struct");
                    let ty_ident = Nonterminal::NtIdent(mk().ident(name), false);

                    // Field name
                    let field_name = self.type_converter
                        .borrow()
                        .resolve_field_name(None, *field_id)
                        .expect("Did not find name for offsetof struct field");
                    let field_ident = Nonterminal::NtIdent(mk().ident(field_name), false);

                    // Index Expr
                    let expr = self.convert_expr(ctx, *expr_id)?.val;
                    let expr = mk().cast_expr(expr, mk().ident_ty("usize"));
                    let index_expr = Nonterminal::NtExpr(expr);

                    // offset_of!(Struct, field[expr as usize]) as ty
                    let mut macro_body = vec![
                        TokenTree::Token(DUMMY_SP, Token::interpolated(ty_ident)),
                        TokenTree::Token(DUMMY_SP, Token::Comma),
                        TokenTree::Token(DUMMY_SP, Token::interpolated(field_ident)),
                        TokenTree::Token(DUMMY_SP, Token::OpenDelim(DelimToken::Bracket)),
                        TokenTree::Token(DUMMY_SP, Token::interpolated(index_expr)),
                        TokenTree::Token(DUMMY_SP, Token::CloseDelim(DelimToken::Bracket)),
                    ];
                    let path = mk().path("offset_of");
                    let mac = mk().mac_expr(mk().mac(path, macro_body, MacDelimiter::Parenthesis));

                    // Cast type
                    let cast_ty = self.convert_type(ty.ctype)?;
                    let cast_expr = mk().cast_expr(mac, cast_ty);

                    Ok(WithStmts::new(cast_expr))
                },
            }

            CExprKind::Literal(ty, ref kind) => self.convert_literal(ctx.is_static, ty, kind),

            CExprKind::ImplicitCast(ty, expr, kind, opt_field_id, _) =>
                self.convert_cast(ctx, ty, expr, kind, opt_field_id, false),

            CExprKind::ExplicitCast(ty, expr, kind, opt_field_id, _) =>
                self.convert_cast(ctx, ty, expr, kind, opt_field_id, true),

            CExprKind::Unary(type_id, op, arg, lrvalue) =>
                self.convert_unary_operator(ctx, op, type_id, arg, lrvalue),

            CExprKind::Conditional(_, cond, lhs, rhs) => {
                let cond = self.convert_condition(ctx, true, cond)?;

                let lhs = self.convert_expr(ctx, lhs)?;
                let rhs = self.convert_expr(ctx, rhs)?;

                if ctx.is_unused() {
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
                if ctx.is_unused() {
                    let mut lhs = self.convert_condition(ctx, false, lhs)?;

                    lhs.stmts.push(
                        mk().semi_stmt(
                            mk().ifte_expr(lhs.val,
                                           mk().block(self.convert_expr(ctx, rhs)?.stmts),
                                           None as Option<P<Expr>>)));
                    Ok(WithStmts {
                        stmts: lhs.stmts,
                        val: self.panic("Binary conditional expression is not supposed to be used"),
                    })
                } else {
                    self.name_reference_write_read(ctx, lhs)?.result_map(|(_, lhs_val)| {
                        let cond = self.match_bool(true, ty.ctype, lhs_val.clone());
                        let ite = mk().ifte_expr(cond,
                                       mk().block(vec![mk().expr_stmt(lhs_val)]),
                                       Some(self.convert_expr(ctx, rhs)?.to_expr()));
                        Ok(ite)
                    })
                }
            },

            CExprKind::Binary(type_id, op, lhs, rhs, opt_lhs_type_id, opt_res_type_id) =>
                self.convert_binary_expr(ctx, type_id, op, lhs, rhs, opt_lhs_type_id, opt_res_type_id),

            CExprKind::ArraySubscript(_, ref lhs, ref rhs, _) => {
                let lhs_node = &self.ast_context.index(*lhs).kind;
                let rhs_node = &self.ast_context.index(*rhs).kind;

                let lhs_node_type = lhs_node.get_type().ok_or_else(|| format!("lhs node bad type"))?;
                let lhs_node_kind = &self.ast_context.resolve_type(lhs_node_type).kind;
                let lhs_is_indexable = lhs_node_kind.is_pointer() || lhs_node_kind.is_vector();

                // From here on in, the LHS is the pointer/array and the RHS the index
                let (lhs, rhs, lhs_node) =
                    if lhs_is_indexable { (lhs, rhs, lhs_node) } else { (rhs, lhs, rhs_node) };

                let lhs_node_type = lhs_node.get_type().ok_or_else(|| format!("lhs node bad type"))?;
                if self.ast_context.resolve_type(lhs_node_type).kind.is_vector() {
                    if get_clang_major_version().map_or(false, |v| v < 7) {
                        return Err(format!("Vector instructions require LLVM 7 or greater. Please build C2Rust against a newer LLVM version. Invalid indexing occured at: {:?}", src_loc));
                    } else {
                        return Err(format!("Vector value {:?} is indexed as an array at: {:?}", lhs_node, src_loc));
                    }
                }

                let mut stmts = vec![];

                let mut rhs = self.convert_expr(ctx.used(), *rhs)?;
                stmts.extend(rhs.stmts);

                let simple_index_array = if ctx.needs_address() {
                    // We can't necessarily index into an array if we're using
                    // that element to compute an address.
                    None
                } else {
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
                    }
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

                    let lhs = self.convert_expr(ctx.used(), arr)?;
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

                    let lhs = self.convert_expr(ctx.used(), *lhs)?;
                    stmts.extend(lhs.stmts);

                    let lhs_type_id = lhs_node.get_type().ok_or_else(|| format!("bad lhs type"))?;

                    // Determine the type of element being indexed
                    let pointee_type_id = match self.ast_context.resolve_type(lhs_type_id).kind {
                        CTypeKind::Pointer(pointee_id) => pointee_id,
                        _ => return Err(format!("Subscript applied to non-pointer: {:?}", lhs.val)),
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
                        self.convert_expr(ctx.used(), fexp)?,
                    CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) =>
                        return self.convert_builtin(ctx, fexp, args),

                    _ =>
                        self.convert_expr(ctx.used(), func)?
                            .map(unwrap_function_pointer),
                };

                let mut args_new: Vec<P<Expr>> = vec![];
                ctx.decay_ref = DecayRef::from(is_variadic);

                for arg in args {
                    // We want to decay refs only when function is variadic
                    let WithStmts { stmts: ss, val } = self.convert_expr(ctx.used(), *arg)?;
                    stmts.extend(ss);
                    args_new.push(val);
                }

                let call_expr = mk().call_expr(func, args_new);
                Ok(self.convert_side_effects_expr(ctx, stmts, call_expr,
                                                  "Function call expression is not supposed to be used"))
            }

            CExprKind::Member(_, expr, decl, kind, _) => {
                let is_bitfield = match &self.ast_context[decl].kind {
                    CDeclKind::Field { bitfield_width, .. } => bitfield_width.is_some(),
                    _ => unreachable!("Found a member which is not a field"),
                };

                if is_bitfield {
                    let field_name = self.type_converter.borrow().resolve_field_name(None, decl).unwrap();

                    self.convert_bitfield_member_expr(ctx, field_name, expr, kind)
                } else if ctx.is_unused() {
                    self.convert_expr(ctx, expr)
                } else {
                    let field_name = self.type_converter.borrow().resolve_field_name(None, decl).unwrap();
                    match kind {
                        MemberKind::Dot => {
                            let val = self.convert_expr(ctx, expr)?;
                            Ok(val.map(|v| mk().field_expr(v, field_name)))
                        }
                        MemberKind::Arrow => {
                            if let CExprKind::Unary(_, c_ast::UnOp::AddressOf, subexpr_id, _)
                            = self.ast_context[expr].kind {
                                let val = self.convert_expr(ctx, subexpr_id)?;
                                Ok(val.map(|v| mk().field_expr(v, field_name)))
                            } else {
                                let val = self.convert_expr(ctx, expr)?;
                                Ok(val.map(|v| mk().field_expr(mk().unary_expr(ast::UnOp::Deref, v), field_name)))
                            }
                        }
                    }
                }
            }

            CExprKind::CompoundLiteral(_, val) =>
                self.convert_expr(ctx, val),

            CExprKind::InitList(ty, ref ids, opt_union_field_id, _) =>
                self.convert_init_list(ctx, ty, ids, opt_union_field_id),

            CExprKind::ImplicitValueInit(ty) =>
                Ok(WithStmts::new(self.implicit_default_expr(ty.ctype, ctx.is_static)?)),

            CExprKind::Predefined(_, val_id) =>
                self.convert_expr(ctx, val_id),

            CExprKind::Statements(_, compound_stmt_id) =>
                self.convert_statement_expression(ctx, compound_stmt_id),

            CExprKind::VAArg(ty, val_id) =>
                self.convert_vaarg(ctx, ty, val_id),
        }
    }

    fn fn_expr_is_variadic(&self, expr_id: CExprId) -> bool {
        let fn_expr = &self.ast_context.c_exprs[&expr_id];
        let fn_ty = &self.ast_context.c_types[&fn_expr.kind.get_type().unwrap()];
        if let CTypeKind::Pointer(qual_ty) = fn_ty.kind {
            match self.ast_context.c_types[&qual_ty.ctype].kind {
                CTypeKind::Function(_, _, is_variadic, _, _) => is_variadic,
                _ => false,
            }
        } else {
            false
        }
    }

    fn convert_side_effects_expr(
        &self,
        ctx: ExprContext,
        mut stmts: Vec<Stmt>,
        expr: P<Expr>,
        panic_msg: &str,
    ) -> WithStmts<P<Expr>> {
        if ctx.is_unused() {
            // Recall that if `used` is false, the `stmts` field of the output must contain
            // all side-effects (and a function call can always have side-effects)
            stmts.push(mk().semi_stmt(expr));
            WithStmts { stmts, val: self.panic(panic_msg) }
        } else {
            WithStmts { stmts, val: expr }
        }
    }

    fn convert_statement_expression(
        &self,
        ctx: ExprContext,
        compound_stmt_id: CStmtId,
    ) -> Result<WithStmts<P<Expr>>, String> {

        fn as_semi_break_stmt(stmt: &ast::Stmt, lbl: &cfg::Label) -> Option<Option<P<ast::Expr>>> {
            if let ast::Stmt { node: ast::StmtKind::Semi(ref expr), .. } = *stmt {
                if let ast::Expr { node: ast::ExprKind::Break(Some(ref blbl), ref ret_val), .. } = **expr {
                    if blbl.ident == mk().label(lbl.pretty_print()).ident {
                        return Some(ret_val.clone())
                    }
                }
            }
            None
        }

        match self.ast_context[compound_stmt_id].kind {
            CStmtKind::Compound(ref substmt_ids) if !substmt_ids.is_empty() => {

            let n = substmt_ids.len();
            let result_id = substmt_ids[n - 1];


                let name = format!("<stmt-expr_{:?}>", compound_stmt_id);
                let lbl = cfg::Label::FromC(compound_stmt_id);

                let mut stmts = match self.ast_context[result_id].kind {
                    CStmtKind::Expr(expr_id) => {
                        let ret = cfg::ImplicitReturnType::StmtExpr(ctx, expr_id, lbl);
                        self.convert_function_body(ctx, &name, &substmt_ids[0 .. (n-1)], ret)?
                    }

                    _ => self.convert_function_body(ctx, &name, &substmt_ids, cfg::ImplicitReturnType::Void)?,
                };

                if let Some(stmt) = stmts.pop() {
                    match as_semi_break_stmt(&stmt, &lbl) {
                        Some(val) => return Ok(WithStmts::new(mk().block_expr({
                            match val {
                                None => mk().block(stmts),
                                Some(val) => WithStmts { stmts, val }.to_block()
                            }
                        }))),
                        _ => {
                            self.use_feature("label_break_value");
                            stmts.push(stmt)
                        },
                    }
                }

                let block_body = mk().block(stmts.clone());
                let val: P<Expr> = mk().labelled_block_expr(block_body, lbl.pretty_print());

                Ok(WithStmts { stmts, val })
            }
            _ => {
                if ctx.is_unused()  {
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
        mut ctx: ExprContext,
        ty: CQualTypeId,
        expr: CExprId,
        kind: CastKind,
        opt_field_id: Option<CFieldId>,
        is_explicit: bool,
    ) -> Result<WithStmts<P<Expr>>, String> {
        // A reference must be decayed if a bitcast is required
        if kind == CastKind::BitCast || kind == CastKind::PointerToIntegral {
            ctx.decay_ref = DecayRef::Yes;
        }

        if kind == CastKind::IntegralToPointer && ctx.is_static {
            self.features.borrow_mut().insert("const_transmute");
        }

        let val = if is_explicit {
            let mut stmts = self.compute_variable_array_sizes(ctx, ty.ctype)?;
            let mut val = self.convert_expr(ctx, expr)?;
            stmts.append(&mut val.stmts);
            val.stmts = stmts;
            val
        } else {
            self.convert_expr(ctx, expr)?
        };

        // Shuffle Vector "function" builtins will add a cast to the output of the
        // builtin call which is unnecessary for translation purposes
        if self.casting_simd_builtin_call(expr, is_explicit, kind) {
            return Ok(val);
        }

        match kind {
            CastKind::BitCast => {
                val.result_map(|x| {
                    let source_ty_id = self.ast_context[expr].kind.get_type().ok_or_else(|| format!("bad source type"))?;

                    if self.ast_context.is_function_pointer(ty.ctype) ||
                       self.ast_context.is_function_pointer(source_ty_id) {
                        let source_ty = self.convert_type(source_ty_id)?;
                        let target_ty = self.convert_type(ty.ctype)?;
                        Ok(transmute_expr(source_ty, target_ty, x, self.tcfg.emit_no_std))
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
                    transmute_expr(intptr_t, target_ty, intptr, self.tcfg.emit_no_std)
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
                if let CTypeKind::LongDouble = target_ty_ctype {
                    self.extern_crates.borrow_mut().insert("f128");

                    let fn_path = mk().path_expr(vec!["f128", "f128", "new"]);
                    let args = vec![val.val];

                    Ok(WithStmts::new(mk().call_expr(fn_path, args)))
                } else if let CTypeKind::LongDouble = self.ast_context[source_ty_ctype_id].kind {
                    self.extern_crates.borrow_mut().insert("num_traits");
                    self.item_store.borrow_mut()
                        .uses
                        .get_mut(vec!["num_traits".into()])
                        .insert("ToPrimitive");

                    let to_method_name = match target_ty_ctype {
                        CTypeKind::Float => "to_f32",
                        CTypeKind::Double => "to_f64",
                        CTypeKind::Char => "to_i8",
                        CTypeKind::UChar => "to_u8",
                        CTypeKind::Short => "to_i16",
                        CTypeKind::UShort => "to_u16",
                        CTypeKind::Int => "to_i32",
                        CTypeKind::UInt => "to_u32",
                        CTypeKind::Long => "to_i64",
                        CTypeKind::ULong => "to_u64",
                        CTypeKind::LongLong => "to_i128",
                        CTypeKind::ULongLong => "to_u128",
                        _ => return Err(format!("Tried casting long double to unsupported type: {:?}", target_ty_ctype)),
                    };

                    let to_call = mk().method_call_expr(val.val, to_method_name, Vec::<P<Expr>>::new());

                    Ok(WithStmts::new(mk().method_call_expr(to_call, "unwrap", Vec::<P<Expr>>::new())))
                } else if let &CTypeKind::Enum(enum_decl_id) = target_ty_ctype {
                    // Casts targeting `enum` types...
                    Ok(self.enum_cast(ty.ctype, enum_decl_id, expr, val, source_ty, target_ty))
                } else {
                    // Other numeric casts translate to Rust `as` casts,
                    // unless the cast is to a function pointer then use `transmute`.
                    Ok(val.map(|x| {
                        if self.ast_context.is_function_pointer(source_ty_ctype_id) {
                            transmute_expr(source_ty, target_ty, x, self.tcfg.emit_no_std)
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

                // Because va_list is defined as a single-element array in order for it to allocate
                // memory as a local variable and to be a pointer as a function argument we would
                // get spurious casts when trying to treat it like a VaList which has reference
                // semantics.
                if let CTypeKind::Struct(struct_id) = self.ast_context[pointee.ctype].kind {
                    if let CDeclKind::Struct { name: Some(ref struct_name), .. } = self.ast_context[struct_id].kind {
                        if struct_name == "__va_list_tag" {
                            return Ok(val)
                        }
                    }
                }

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
                            let method = if is_const || ctx.is_static {
                                "as_ptr"
                            } else {
                                "as_mut_ptr"
                            };

                            let mut call = val.map(|x| mk().method_call_expr(x, method, vec![] as Vec<P<Expr>>));

                            // Static arrays can now use as_ptr with the const_slice_as_ptr feature
                            // enabled. Can also cast that const ptr to a mutable pointer as we do here:
                            if ctx.is_static {
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
                Ok(WithStmts::new(self.null_ptr(ty.ctype, ctx.is_static)?))
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
                self.convert_condition(ctx, true, expr)
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

            CastKind::VectorSplat =>
                Err(format!("TODO vector splat casts not supported")),
        }
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

    pub fn implicit_default_expr(&self, ty_id: CTypeId, is_static: bool) -> Result<P<Expr>, String> {
        let resolved_ty_id = self.ast_context.resolve_type_id(ty_id);
        let resolved_ty = &self.ast_context.index(resolved_ty_id).kind;

        if resolved_ty.is_bool() {
            Ok(mk().lit_expr(mk().bool_lit(false)))
        } else if resolved_ty.is_integral_type() {
            Ok(mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed)))
        } else if resolved_ty.is_floating_type() {
            match self.ast_context[ty_id].kind {
                CTypeKind::LongDouble => Ok(mk().path_expr(vec!["f128", "f128", "ZERO"])),
                _ => Ok(mk().lit_expr(mk().float_unsuffixed_lit("0."))),
            }
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
            Ok(vec_expr(val, count))
        } else if let &CTypeKind::Vector(CQualTypeId { ctype, .. }, len) = resolved_ty {
            self.implicit_vector_default(ctype, len, is_static)
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
            CDeclKind::Struct { ref fields, platform_byte_size, .. } => {
                let name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                let fields = match *fields {
                    Some(ref fields) => fields,
                    None => return Err(format!("Attempted to zero-initialize forward-declared struct")),
                };

                let has_bitfields = fields.iter().map(|field_id| match self.ast_context.index(*field_id).kind {
                    CDeclKind::Field { bitfield_width, .. } => bitfield_width.is_some(),
                    _ => unreachable!("Found non-field in record field list"),
                }).any(|x| x);

                if has_bitfields {
                    self.bitfield_zero_initializer(name, fields, platform_byte_size, is_static)
                } else {
                    let fields: Result<Vec<Field>, String> = fields
                        .into_iter()
                        .map(|field_id| {
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
                }
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

    /// If we're trying to organize item definitions into submodules, add them to a module
    /// scoped "namespace" if we have a path available, otherwise add it to the global "namespace"
    fn insert_item(&self, item: P<Item>, decl_file_path: Option<&PathBuf>, main_file_path: &PathBuf) {
        if self.tcfg.reorganize_definitions && decl_file_path.expect("There should be a decl file path.") != main_file_path {
            let mut mod_blocks = self.mod_blocks.borrow_mut();
            let mod_block_items = mod_blocks.entry(decl_file_path.unwrap().clone()).or_insert(ItemStore::new());

            mod_block_items.items.push(item);
        } else {
            self.item_store.borrow_mut().items.push(item)
        }
    }

    /// If we're trying to organize foreign item definitions into submodules, add them to a module
    /// scoped "namespace" if we have a path available, otherwise add it to the global "namespace"
    fn insert_foreign_item(&self, item: ForeignItem, decl_file_path: Option<&PathBuf>, main_file_path: &PathBuf) {
        if self.tcfg.reorganize_definitions && decl_file_path.unwrap() != main_file_path {
            let mut mod_blocks = self.mod_blocks.borrow_mut();
            let mod_block_items = mod_blocks.entry(decl_file_path.unwrap().clone()).or_insert(ItemStore::new());

            mod_block_items.foreign_items.push(item);
        } else {
            self.item_store.borrow_mut().foreign_items.push(item)
        }
    }

    fn match_type_kind(&self, ctype: CTypeId, store: &mut ItemStore, decl_file_path: &path::Path) {
        use self::CTypeKind::*;

        match self.ast_context[ctype].kind {
            Void | Char | SChar | UChar | Short | UShort | Int | UInt |
            Long | ULong | LongLong | ULongLong | Int128 | UInt128 |
            Half | Float | Double => {
                store.uses
                    .get_mut(vec!["super".into()])
                    .insert("libc");
            },
            LongDouble => {
                store.uses
                    .get_mut(vec!["super".into()])
                    .insert("f128");
            },
            // Bool uses the bool type, so no dependency on libc
            Bool => {},
            Paren(ctype) |
            Decayed(ctype) |
            IncompleteArray(ctype) |
            ConstantArray(ctype, _) |
            Elaborated(ctype) |
            Pointer(CQualTypeId { ctype, .. }) => self.match_type_kind(ctype, store, decl_file_path),
            Enum(decl_id) |
            Typedef(decl_id) |
            Union(decl_id) |
            Struct(decl_id) => {
                let mut decl_id = decl_id.clone();
                // if the `decl` has been "squashed", get the corresponding `decl_id`
                if self.ast_context.prenamed_decls.contains_key(&decl_id) {
                    decl_id = *self.ast_context.prenamed_decls.get(&decl_id).unwrap();
                }
                let decl = &self.ast_context.c_decls[&decl_id];
                let decl_loc = &decl.loc.as_ref().unwrap();

                // If the definition lives in the same header, there is no need to import it
                // in fact, this would be a hard rust error
                if decl_loc.file_path.as_ref().unwrap() == decl_file_path {
                    return;
                }

                let ident_name = self.type_converter.borrow().resolve_decl_name(decl_id).unwrap();

                // Either the decl lives in the parent module, or else in a sibling submodule
                match decl_loc.file_path {
                    Some(ref decl_path) if decl_path == &self.main_file => {
                        store.uses
                            .get_mut(vec!["super".into()])
                            .insert(ident_name);
                    }
                    _ => {
                        let file_path = decl_loc.file_path.as_ref().unwrap();
                        let file_name = clean_path(&self.mod_names, &file_path);

                        store.uses
                            .get_mut(vec!["super".into(), file_name])
                            .insert(ident_name);
                    }
                };
            },
            Function(CQualTypeId { ctype, .. }, ref params, ..) => {
                // Return Type
                let type_kind = &self.ast_context[ctype].kind;

                // Rust doesn't use void for return type, so skip
                if *type_kind != Void {
                    self.match_type_kind(ctype, store, decl_file_path);
                }

                // Param Types
                for param_id in params {
                    self.match_type_kind(param_id.ctype, store, decl_file_path);
                }
            },
            Vector(CQualTypeId { ctype, .. }, len) => {
                // Since vector imports are global, we can find the correct type name in the parent scope
                let type_name = match (&self.ast_context[ctype].kind, len) {
                    (CTypeKind::Float, 4) => "__m128",
                    (CTypeKind::Float, 8) => "__m256",
                    (CTypeKind::Double, 2) => "__m128d",
                    (CTypeKind::Double, 4) => "__m256d",
                    (CTypeKind::LongLong, 4) => "__m256i",
                    (CTypeKind::LongLong, 2) |
                    (CTypeKind::Char, 16) |
                    (CTypeKind::Int, 4) |
                    (CTypeKind::Short, 8) => "__m128i",
                    (CTypeKind::LongLong, 1) |
                    (CTypeKind::Int, 2) => "__m64",
                    (kind, len) => unimplemented!("Unknown vector type: {:?} x {}", kind, len),
                };

                store.uses
                    .get_mut(vec!["super".into()])
                    .insert_with_attr(type_name, mk().pub_());
            },
            ref e => unimplemented!("{:?}", e),
        }
    }

    fn generate_submodule_imports(&self, decl_id: CDeclId, decl_file_path: Option<&PathBuf>) {
        let decl_file_path = decl_file_path.expect("There should be a decl file path");
        let decl = self.ast_context.c_decls.get(&decl_id).unwrap();
        let mut sumbodule_items = self.mod_blocks.borrow_mut();
        let item_store = sumbodule_items.entry(decl_file_path.to_path_buf()).or_insert(ItemStore::new());

        match decl.kind {
            CDeclKind::Struct { ref fields, .. } |
            CDeclKind::Union { ref fields, .. } => {
                let field_ids = fields.as_ref().map(|vec| vec.as_slice()).unwrap_or(&[]);

                for field_id in field_ids.iter() {
                    match self.ast_context.c_decls[field_id].kind {
                        CDeclKind::Field { typ, .. } => self.match_type_kind(typ.ctype, item_store, decl_file_path),
                        _ => unreachable!("Found something in a struct other than a field"),
                    }
                }
            },
            CDeclKind::EnumConstant { .. } => {},
            // REVIEW: Enums can only be integer types? So libc is likely always required?
            CDeclKind::Enum { .. } => {
                item_store.uses
                    .get_mut(vec!["super".into()])
                    .insert("libc");
            }
            CDeclKind::Variable { has_static_duration: true, is_externally_visible: true, typ, .. } |
            CDeclKind::Variable { has_thread_duration: true, is_externally_visible: true, typ, .. } |
            CDeclKind::Typedef { typ, .. } => self.match_type_kind(typ.ctype, item_store, decl_file_path),
            CDeclKind::Function { is_global: true, typ, .. } => self.match_type_kind(typ, item_store, decl_file_path),
            CDeclKind::Function { .. } => {
                // TODO: We may need to explicitly skip SIMD functions here when getting types for
                // a fn definition in a header since SIMD headers define functions but we're using imports
                // rather than translating the original definition
            },
            CDeclKind::Variable { has_static_duration, has_thread_duration, is_externally_visible: false, .. }
            if has_static_duration || has_thread_duration => {},
            ref e => unimplemented!("{:?}", e),
        }
    }
}
