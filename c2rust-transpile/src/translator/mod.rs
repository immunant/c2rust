use std::cell::RefCell;
use std::ops::Index;
use std::path::{self, PathBuf};
use std::{char, io};

use dtoa;

use failure::{err_msg, Fail};
use indexmap::{IndexMap, IndexSet};

use rustc_data_structures::sync::Lrc;
use syntax::attr;
use syntax::ast::*;
use syntax::parse::token::{self, DelimToken, Nonterminal};
use syntax::print::pprust::*;
use syntax::ptr::*;
use syntax::source_map::dummy_spanned;
use syntax::tokenstream::{TokenStream, TokenTree};
use syntax::{ast, with_globals};
use syntax_pos::{Span, DUMMY_SP};
use syntax_pos::edition::Edition;

use crate::rust_ast::comment_store::CommentStore;
use crate::rust_ast::item_store::ItemStore;
use crate::rust_ast::traverse::Traversal;
use c2rust_ast_builder::{mk, Builder, IntoSymbol};

use crate::c_ast;
use crate::c_ast::iterators::{DFExpr, SomeId};
use crate::c_ast::*;
use crate::cfg;
use crate::convert_type::TypeConverter;
use crate::renamer::Renamer;
use crate::with_stmts::WithStmts;
use crate::TranspilerConfig;
use c2rust_ast_exporter::clang_ast::LRValue;

mod assembly;
mod bitfields;
mod builtins;
mod literals;
mod main_function;
mod named_references;
mod operators;
mod simd;
mod variadic;

pub use crate::diagnostics::{TranslationError, TranslationErrorKind};
use crate::CrateSet;
use crate::PragmaVec;

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
    is_const: bool,
    decay_ref: DecayRef,
    is_bitfield_write: bool,

    // We will be refering to the expression by address. In this context we
    // can't index arrays because they may legally go out of bounds. We also
    // need to explicitly cast function references to fn() so we get their
    // address in function pointer literals.
    needs_address: bool,

    ternary_needs_parens: bool,
    expanding_macro: Option<CDeclId>,
}

impl ExprContext {
    pub fn used(self) -> Self {
        ExprContext { used: true, ..self }
    }
    pub fn unused(self) -> Self {
        ExprContext {
            used: false,
            ..self
        }
    }
    pub fn is_used(&self) -> bool {
        self.used
    }
    pub fn is_unused(&self) -> bool {
        !self.used
    }
    pub fn decay_ref(self) -> Self {
        ExprContext {
            decay_ref: DecayRef::Yes,
            ..self
        }
    }
    pub fn not_static(self) -> Self {
        ExprContext {
            is_static: false,
            ..self
        }
    }
    pub fn static_(self) -> Self {
        ExprContext {
            is_static: true,
            ..self
        }
    }
    pub fn set_static(self, is_static: bool) -> Self {
        ExprContext { is_static, ..self }
    }
    pub fn set_const(self, is_const: bool) -> Self {
        ExprContext { is_const, ..self }
    }
    pub fn is_bitfield_write(&self) -> bool {
        self.is_bitfield_write
    }
    pub fn set_bitfield_write(self, is_bitfield_write: bool) -> Self {
        ExprContext {
            is_bitfield_write,
            ..self
        }
    }
    pub fn needs_address(&self) -> bool {
        self.needs_address
    }
    pub fn set_needs_address(self, needs_address: bool) -> Self {
        ExprContext {
            needs_address,
            ..self
        }
    }

    /// Are we expanding the given macro in the current context?
    pub fn expanding_macro(&self, mac: &CDeclId) -> bool {
        match self.expanding_macro {
            Some(expanding) => expanding == *mac,
            None => false,
        }
    }
    pub fn set_expanding_macro(self, mac: CDeclId) -> Self {
        ExprContext {
            expanding_macro: Some(mac),
            ..self
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunContext {
    /// The name of the function we're currently translating
    name: Option<String>,
    /// The va_list decl that we promote to a Rust function arg
    promoted_va_decl: Option<CDeclId>,
    /// The va_list decls that we did not promote because they were `va_copy`ed.
    copied_va_decls: Option<IndexSet<CDeclId>>,
}

impl FunContext {
    pub fn new() -> Self {
        FunContext {
            name: None,
            promoted_va_decl: None,
            copied_va_decls: None,
        }
    }

    pub fn enter_new(&mut self, fn_name: &str) {
        self.name = Some(fn_name.to_string());
        self.promoted_va_decl = None;
        self.copied_va_decls = None;
    }

    pub fn get_name<'a>(&'a self) -> &'a str {
        return self.name.as_ref().unwrap();
    }
}

pub struct Translation<'c> {
    // Translation environment
    pub ast_context: TypedAstContext,
    pub tcfg: &'c TranspilerConfig,

    // Accumulated outputs
    pub features: RefCell<IndexSet<&'static str>>,
    sectioned_static_initializers: RefCell<Vec<Stmt>>,
    extern_crates: RefCell<IndexSet<&'static str>>,

    // Translation state and utilities
    type_converter: RefCell<TypeConverter>,
    renamer: RefCell<Renamer<CDeclId>>,
    zero_inits: RefCell<IndexMap<CDeclId, WithStmts<P<Expr>>>>,
    function_context: RefCell<FunContext>,
    potential_flexible_array_members: RefCell<IndexSet<CDeclId>>,
    macro_types: RefCell<IndexMap<CDeclId, CTypeId>>,

    // Comment support
    pub comment_context: CommentContext, // Incoming comments
    pub comment_store: RefCell<CommentStore>,     // Outgoing comments

    // Items indexed by file id of the source
    items: RefCell<IndexMap<FileId, ItemStore>>,

    // Mod names to try to stop collisions from happening
    mod_names: RefCell<IndexMap<String, PathBuf>>,

    // The main file id that the translator is operating on
    main_file: FileId,

    // While expanding an item, store the current file id that item is
    // expanded from. This is needed in order to note imports in items when
    // encountering DeclRefs.
    cur_file: RefCell<Option<FileId>>,
}

fn simple_metaitem(name: &str) -> NestedMetaItem {
    let meta_item = mk().meta_item(vec![name], MetaItemKind::Word);

    mk().nested_meta_item(NestedMetaItem::MetaItem(meta_item))
}

fn cast_int(val: P<Expr>, name: &str) -> P<Expr> {
    let opt_literal_val = match val.node {
        ExprKind::Lit(ref l) => match l.node {
            LitKind::Int(i, _) => Some(i),
            _ => None,
        },
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
    let std_or_core = if no_std { "core" } else { "std" };
    let mut path = vec![
        mk().path_segment(""),
        mk().path_segment(std_or_core),
        mk().path_segment("mem"),
    ];

    if type_args.is_empty() {
        path.push(mk().path_segment("transmute"));
    } else {
        path.push(mk().path_segment_with_args("transmute", mk().angle_bracketed_args(type_args)));
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
                return b.clone();
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
        mk().single_attr("no_mangle") // Don't touch my name Rust!
    } else if in_extern_block {
        mk().str_attr("link_name", old_name) // Look for this name
    } else {
        mk().str_attr("export_name", old_name) // Make sure you actually name it this
    }
}

pub fn signed_int_expr(value: i64) -> P<Expr> {
    if value < 0 {
        mk().unary_expr(
            ast::UnOp::Neg,
            mk().lit_expr(mk().int_lit((-value) as u128, "")),
        )
    } else {
        mk().lit_expr(mk().int_lit(value as u128, ""))
    }
}

// This should only be used for tests
fn prefix_names(translation: &mut Translation, prefix: &str) {
    for (&decl_id, ref mut decl) in translation.ast_context.iter_mut_decls() {
        match decl.kind {
            CDeclKind::Function {
                ref mut name,
                ref body,
                ..
            } if body.is_some() => {
                // SIMD types are imported and do not need to be renamed
                if name.starts_with("_mm") {
                    continue;
                }

                name.insert_str(0, prefix);

                translation.renamer.borrow_mut().insert(decl_id, &name);
            }
            CDeclKind::Variable {
                ref mut ident,
                has_static_duration,
                has_thread_duration,
                ..
            } if has_static_duration || has_thread_duration => ident.insert_str(0, &prefix),
            _ => (),
        }
    }
}

// This function is meant to create module names, for modules being created with the
// `--reorganize-modules` flag. So what is done is, change '.' && '-' to '_', and depending
// on whether there is a collision or not prepend the prior directory name to the path name.
// To check for collisions, a IndexMap with the path name(key) and the path(value) associated with
// the name. If the path name is in use, but the paths differ there is a collision.
fn clean_path(mod_names: &RefCell<IndexMap<String, PathBuf>>, path: Option<&path::Path>) -> String {
    fn path_to_str(path: &path::Path) -> String {
        path.file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .replace('.', "_")
            .replace('-', "_")
    }

    let mut file_path: String = path.map_or("internal".to_string(), |path| path_to_str(path));
    let path = path.unwrap_or(path::Path::new(""));
    let mut mod_names = mod_names.borrow_mut();
    if !mod_names.contains_key(&file_path.clone()) {
        mod_names.insert(file_path.clone(), path.to_path_buf());
    } else {
        let mod_path = mod_names.get(&file_path.clone()).unwrap();
        // A collision in the module names has occured.
        // Ex: types.h can be included from
        // /usr/include/bits and /usr/include/sys
        if mod_path != path {
            let split_path: Vec<PathBuf> = path
                .to_path_buf()
                .parent()
                .unwrap()
                .iter()
                .map(|os| PathBuf::from(os))
                .collect();

            let mut to_prepend = path_to_str(split_path.last().unwrap());
            to_prepend.push('_');
            file_path.insert_str(0, &to_prepend);
        }
    }
    file_path
}

pub fn translate_failure(tcfg: &TranspilerConfig, msg: &str) {
    error!("{}", msg);
    if tcfg.fail_on_error {
        panic!();
    }
}

pub fn translate(
    ast_context: TypedAstContext,
    tcfg: &TranspilerConfig,
    main_file: PathBuf,
) -> (String, PragmaVec, CrateSet) {
    let mut t = Translation::new(ast_context, tcfg, main_file);
    let ctx = ExprContext {
        used: true,
        is_static: false,
        is_const: false,
        decay_ref: DecayRef::Default,
        is_bitfield_write: false,
        needs_address: false,
        ternary_needs_parens: false,
        expanding_macro: None,
    };

    if t.tcfg.reorganize_definitions {
        t.use_feature("custom_attribute");
    }

    if tcfg.emit_no_std {
        t.extern_crates.borrow_mut().insert("core");
    }

    t.extern_crates.borrow_mut().insert("libc");

    // Headers often pull in declarations that are unused;
    // we simplify the translator output by omitting those.
    t.ast_context.prune_unused_decls();

    // Sort the top-level declarations by file and source location so that we
    // preserve the ordering of all declarations in each file.
    t.ast_context.sort_top_decls();

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
    with_globals(Edition::Edition2018, || {
        // Identify typedefs that name unnamed types and collapse the two declarations
        // into a single name and declaration, eliminating the typedef altogether.
        let mut prenamed_decls: IndexMap<CDeclId, CDeclId> = IndexMap::new();
        for (&decl_id, decl) in t.ast_context.iter_decls() {
            if let CDeclKind::Typedef { ref name, typ, .. } = decl.kind {
                if let Some(subdecl_id) = t
                    .ast_context
                    .resolve_type(typ.ctype)
                    .kind
                    .as_underlying_decl()
                {
                    let is_unnamed = match t.ast_context[subdecl_id].kind {
                        CDeclKind::Struct { name: None, .. }
                        | CDeclKind::Union { name: None, .. }
                        | CDeclKind::Enum { name: None, .. } => true,

                        // Detect case where typedef and struct share the same name.
                        // In this case the purpose of the typedef was simply to eliminate
                        // the need for the 'struct' tag when refering to the type name.
                        CDeclKind::Struct {
                            name: Some(ref target_name),
                            ..
                        }
                        | CDeclKind::Union {
                            name: Some(ref target_name),
                            ..
                        }
                        | CDeclKind::Enum {
                            name: Some(ref target_name),
                            ..
                        } => name == target_name,

                        _ => false,
                    };

                    if is_unnamed
                        && !prenamed_decls
                            .values()
                            .find(|decl_id| *decl_id == &subdecl_id)
                            .is_some()
                    {
                        prenamed_decls.insert(decl_id, subdecl_id);

                        t.type_converter
                            .borrow_mut()
                            .declare_decl_name(decl_id, name);
                        t.type_converter
                            .borrow_mut()
                            .alias_decl_name(subdecl_id, decl_id);
                    }
                }
            }
        }

        t.ast_context.prenamed_decls = prenamed_decls;

        // Helper function that returns true if there is either a matching typedef or its
        // corresponding struct/union/enum
        fn contains(prenamed_decls: &IndexMap<CDeclId, CDeclId>, decl_id: &CDeclId) -> bool {
            (prenamed_decls.contains_key(decl_id)
                || prenamed_decls.values().find(|id| *id == decl_id).is_some())
        }

        // Populate renamer with top-level names
        for (&decl_id, decl) in t.ast_context.iter_decls() {
            let decl_name = match decl.kind {
                _ if contains(&t.ast_context.prenamed_decls, &decl_id) => Name::NoName,
                CDeclKind::Struct { ref name, .. } => {
                    some_type_name(name.as_ref().map(String::as_str))
                }
                CDeclKind::Enum { ref name, .. } => {
                    some_type_name(name.as_ref().map(String::as_str))
                }
                CDeclKind::Union { ref name, .. } => {
                    some_type_name(name.as_ref().map(String::as_str))
                }
                CDeclKind::Typedef { ref name, .. } => Name::TypeName(name),
                CDeclKind::Function { ref name, .. } => Name::VarName(name),
                CDeclKind::EnumConstant { ref name, .. } => Name::VarName(name),
                CDeclKind::Variable { ref ident, .. }
                    if t.ast_context.c_decls_top.contains(&decl_id) =>
                {
                    Name::VarName(ident)
                }
                CDeclKind::MacroObject { ref name, .. } => Name::VarName(name),
                _ => Name::NoName,
            };
            match decl_name {
                Name::NoName => (),
                Name::AnonymousType => {
                    t.type_converter
                        .borrow_mut()
                        .declare_decl_name(decl_id, "C2RustUnnamed");
                }
                Name::TypeName(name) => {
                    t.type_converter
                        .borrow_mut()
                        .declare_decl_name(decl_id, name);
                }
                Name::VarName(name) => {
                    t.renamer.borrow_mut().insert(decl_id, &name);
                }
            }
        }

        {
            let convert_type = |decl_id: CDeclId, decl: &CDecl| {
                let decl_file_id = t.ast_context.file_id(decl);
                if t.tcfg.reorganize_definitions {
                    *t.cur_file.borrow_mut() = decl_file_id;
                }
                match t.convert_decl(ctx, decl_id) {
                    Ok(ConvertedDecl::Item(item)) => {
                        t.insert_item(item, decl);
                    }
                    Ok(ConvertedDecl::ForeignItem(item)) => {
                        t.insert_foreign_item(item, decl);
                    }
                    Ok(ConvertedDecl::NoItem) => {}
                    Err(e) => {
                        let ref k = t.ast_context.get_decl(&decl_id).map(|x| &x.kind);
                        let msg = format!("Skipping declaration {:?} due to error: {}", k, e);
                        translate_failure(&t.tcfg, &msg);
                    }
                }
                t.cur_file.borrow_mut().take();

                if t.tcfg.reorganize_definitions
                    && decl_file_id.map_or(false, |id| id != t.main_file)
                {
                    t.generate_submodule_imports(decl_id, decl_file_id);
                }
            };

            // Export all types
            for (&decl_id, decl) in t.ast_context.iter_decls() {
                let needs_export = match decl.kind {
                    CDeclKind::Struct { .. } => true,
                    CDeclKind::Enum { .. } => true,
                    CDeclKind::EnumConstant { .. } => true,
                    CDeclKind::Union { .. } => true,
                    CDeclKind::Typedef { .. } =>
                    // Only check the key as opposed to `contains` because the key should be the
                    // typedef id
                    {
                        !t.ast_context.prenamed_decls.contains_key(&decl_id)
                    }
                    _ => false,
                };
                if needs_export {
                    convert_type(decl_id, decl);
                }
            }
        }

        // Export top-level value declarations
        for top_id in &t.ast_context.c_decls_top {
            let needs_export = match t.ast_context[*top_id].kind {
                CDeclKind::Function { is_implicit, .. } => !is_implicit,
                CDeclKind::Variable { .. } => true,
                CDeclKind::MacroObject { .. } => tcfg.translate_const_macros,
                _ => false,
            };
            if needs_export {
                let decl_opt = t.ast_context.get_decl(top_id);
                let decl = decl_opt.as_ref().unwrap();
                let decl_file_id = t.ast_context.file_id(decl);

                if t.tcfg.reorganize_definitions
                    && decl_file_id.map_or(false, |id| id != t.main_file)
                {
                    *t.cur_file.borrow_mut() = decl_file_id;
                }
                match t.convert_decl(ctx, *top_id) {
                    Ok(ConvertedDecl::Item(item)) => {
                        t.insert_item(item, decl);
                    }
                    Ok(ConvertedDecl::ForeignItem(item)) => {
                        t.insert_foreign_item(item, decl);
                    }
                    Ok(ConvertedDecl::NoItem) => {}
                    Err(e) => {
                        let ref decl = t.ast_context.get_decl(top_id);
                        let msg = match decl {
                            Some(decl) => {
                                let decl_identifier = decl.kind.get_name().map_or_else(
                                    || {
                                        t.ast_context.display_loc(&decl.loc)
                                            .map_or("Unknown".to_string(), |l| format!("at {}", l))
                                    },
                                    |name| name.clone(),
                                );
                                format!("Failed to translate {}: {}", decl_identifier, e)
                            }
                            _ => format!("Failed to translate declaration: {}", e,),
                        };
                        translate_failure(&t.tcfg, &msg);
                    }
                }
                t.cur_file.borrow_mut().take();

                if t.tcfg.reorganize_definitions
                    && decl_file_id.map_or(false, |id| id != t.main_file)
                {
                    t.generate_submodule_imports(*top_id, decl_file_id);
                }
            }
        }

        // Add the main entry point
        if let Some(main_id) = t.ast_context.c_main {
            match t.convert_main(main_id) {
                Ok(item) => t.items.borrow_mut()[&t.main_file].add_item(item),
                Err(e) => {
                    let msg = format!("Failed to translate main: {}", e);
                    translate_failure(&t.tcfg, &msg)
                }
            }
        }

        // Initialize global statics when necessary
        if !t.sectioned_static_initializers.borrow().is_empty() {
            let (initializer_fn, initializer_static) = t.generate_global_static_init();
            let store = &mut t.items.borrow_mut()[&t.main_file];

            store.add_item(initializer_fn);
            store.add_item(initializer_static);
        }

        let pragmas = t.get_pragmas();
        let crates = t.extern_crates.borrow().clone();

        // pass all converted items to the Rust pretty printer
        let translation = to_string(|s| {
            print_header(s, &t)?;

            // Re-order comments
            let mut traverser = t.comment_store.into_inner().into_comment_traverser();
            let mut mod_items: Vec<P<Item>> = Vec::new();

            // Keep track of new uses we need while building header submodules
            let mut new_uses = ItemStore::new();

            // Header Reorganization: Submodule Item Stores
            for (file_id, ref mut mod_item_store) in t.items.borrow_mut().iter_mut() {
                if *file_id != t.main_file {
                    mod_items.push(make_submodule(
                        &t.ast_context,
                        mod_item_store,
                        *file_id,
                        &mut new_uses,
                        &t.mod_names,
                    ));
                }
            }

            // Main file item store
            let (items, foreign_items, uses) = t.items.borrow_mut()[&t.main_file].drain();

            // Add a comment mapping span to each node that should have a
            // comment printed before it. The pretty printer picks up these
            // spans and uses them to decide when to emit comments.
            mod_items = mod_items
                .into_iter()
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

            s.comments()
                .get_or_insert(vec![])
                .extend(traverser.into_comment_store().into_comments());

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

            // Print new uses from submodules
            let (_, _, new_uses) = new_uses.drain();
            for use_item in new_uses.into_items() {
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
        });
        (translation, pragmas, crates)
    })
}

fn make_submodule(
    ast_context: &TypedAstContext,
    item_store: &mut ItemStore,
    file_id: FileId,
    use_item_store: &mut ItemStore,
    mod_names: &RefCell<IndexMap<String, PathBuf>>,
) -> P<Item> {
    let (mut items, foreign_items, uses) = item_store.drain();
    let file_path = ast_context.get_file_path(file_id);
    let include_line_number = ast_context.get_file_include_line_number(file_id).unwrap_or(0);
    let mod_name = clean_path(mod_names, file_path);

    for item in items.iter() {
        let ident_name = item.ident.name.as_str();
        let use_path = vec!["self".into(), mod_name.clone()];

        let vis = match item.vis.node {
            VisibilityKind::Public => mk().pub_(),
            _ => mk(),
        };

        use_item_store.add_use_with_attr(use_path, &ident_name, vis);
    }

    for foreign_item in foreign_items.iter() {
        let ident_name = foreign_item.ident.name.as_str();
        let use_path = vec!["self".into(), mod_name.clone()];

        use_item_store.add_use(use_path, &ident_name);
    }

    for item in uses.into_items() {
        items.push(item);
    }

    if !foreign_items.is_empty() {
        items.push(mk().abi("C").foreign_items(foreign_items));
    }

    let file_path_str = file_path.map_or(
        mod_name.as_str(),
        |path| path.to_str().expect("Found invalid unicode"),
    );
    mk().vis("pub")
        .str_attr("header_src", format!("{}:{}", file_path_str, include_line_number))
        .mod_item(mod_name, mk().mod_(items))
}

/// Pretty-print the leading pragmas and extern crate declarations
fn print_header(s: &mut State, t: &Translation) -> io::Result<()> {
    if t.tcfg.emit_modules {
        s.print_item(&mk().use_simple_item(vec!["libc"], None as Option<Ident>))?;
    } else {
        let pragmas = t.get_pragmas();
        for (key, mut values) in pragmas {
            values.sort();
            let value_attr_vec = values
                .into_iter()
                .map(|value| mk().nested_meta_item(mk().meta_item(vec![value], MetaItemKind::Word)))
                .collect::<Vec<_>>();
            let item = mk().meta_item(vec![key], MetaItemKind::List(value_attr_vec));
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
            let plugin_item = mk().meta_item(vec!["plugin"], MetaItemKind::List(plugin_args));
            for attr in mk()
                .meta_item_attr(AttrStyle::Inner, plugin_item)
                .as_inner_attrs()
            {
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
            s.print_item(
                &mk()
                    .single_attr("macro_use")
                    .extern_crate_item("c2rust_xcheck_derive", None),
            )?;
            s.print_item(
                &mk()
                    .single_attr("macro_use")
                    .extern_crate_item("c2rust_xcheck_runtime", None),
            )?;
            // When cross-checking, always use the system allocator
            let sys_alloc_path = vec!["", "std", "alloc", "System"];
            s.print_item(&mk().single_attr("global_allocator").static_item(
                "C2RUST_ALLOC",
                mk().path_ty(sys_alloc_path.clone()),
                mk().path_expr(sys_alloc_path),
            ))?;
        }
    }
    Ok(())
}

/// Convert a boolean expression to a c_int
fn bool_to_int(val: P<Expr>) -> P<Expr> {
    mk().cast_expr(val, mk().path_ty(vec!["libc", "c_int"]))
}

/// Add a src_loc = "line:col" attribute to an item/foreign_item
fn add_src_loc_attr(attrs: &mut Vec<ast::Attribute>, src_loc: &Option<SrcLoc>) {
    if let Some(src_loc) = src_loc.as_ref() {
        let loc_str = format!("{}:{}", src_loc.line, src_loc.column);
        attrs.push(attr::mk_attr_outer(DUMMY_SP, attr::mk_attr_id(), attr::mk_name_value_item_str(
            Ident::from_str("src_loc"),
            dummy_spanned(loc_str.into_symbol()),
        )));
    }
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
    pub fn new(
        mut ast_context: TypedAstContext,
        tcfg: &'c TranspilerConfig,
        main_file: PathBuf,
    ) -> Self {
        let comment_context = CommentContext::new(&mut ast_context);
        let mut type_converter = TypeConverter::new(tcfg.emit_no_std);

        if tcfg.translate_valist {
            type_converter.translate_valist = true
        }

        let main_file = ast_context.find_file_id(&main_file).unwrap_or(0);
        let items = indexmap!{main_file => ItemStore::new()};

        Translation {
            features: RefCell::new(IndexSet::new()),
            type_converter: RefCell::new(type_converter),
            ast_context,
            tcfg,
            renamer: RefCell::new(Renamer::new(&[
                // Keywords currently in use
                "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false",
                "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut",
                "pub", "ref", "return", "Self", "self", "static", "struct", "super", "trait",
                "true", "type", "unsafe", "use", "where", "while", "dyn",
                // Keywords reserved for future use
                "abstract", "alignof", "become", "box", "do", "final", "macro", "offsetof",
                "override", "priv", "proc", "pure", "sizeof", "typeof", "unsized", "virtual",
                "async", "try", "yield", // Prevent use for other reasons
                "main",  // prelude names
                "drop", "Some", "None", "Ok", "Err",
            ])),
            zero_inits: RefCell::new(IndexMap::new()),
            function_context: RefCell::new(FunContext::new()),
            potential_flexible_array_members: RefCell::new(IndexSet::new()),
            macro_types: RefCell::new(IndexMap::new()),
            comment_context,
            comment_store: RefCell::new(CommentStore::new()),
            sectioned_static_initializers: RefCell::new(Vec::new()),
            items: RefCell::new(items),
            mod_names: RefCell::new(IndexMap::new()),
            main_file,
            extern_crates: RefCell::new(IndexSet::new()),
            cur_file: RefCell::new(None),
        }
    }

    /// Called when translation makes use of a language feature that will require a feature-gate.
    pub fn use_feature(&self, feature: &'static str) {
        self.features.borrow_mut().insert(feature);
    }

    pub fn get_pragmas(&self) -> PragmaVec {
        let mut features = vec![];
        features.extend(self.features.borrow().iter());
        features.extend(self.type_converter.borrow().features_used());
        let mut pragmas: PragmaVec = vec![(
            "allow",
            vec![
                "non_upper_case_globals",
                "non_camel_case_types",
                "non_snake_case",
                "dead_code",
                "mutable_transmutes",
                "unused_mut",
                "unused_assignments",
            ],
        )];
        if self.tcfg.cross_checks {
            features.append(&mut vec!["plugin", "custom_attribute"]);
            pragmas.push(("cross_check", vec!["yes"]));
        }

        if !features.is_empty() {
            pragmas.push(("feature", features));
        }
        pragmas
    }

    // This node should _never_ show up in the final generated code. This is an easy way to notice
    // if it does.
    pub fn panic_or_err(&self, msg: &str) -> P<Expr> {
        self.panic_or_err_helper(msg, self.tcfg.panic_on_translator_failure)
    }

    pub fn panic(&self, msg: &str) -> P<Expr> {
        self.panic_or_err_helper(msg, true)
    }

    fn panic_or_err_helper(&self, msg: &str, panic: bool) -> P<Expr> {
        let macro_name = if panic { "panic" } else { "compile_error" };
        let macro_msg = vec![TokenTree::token(token::Interpolated(Lrc::new(Nonterminal::NtExpr(
            mk().lit_expr(mk().str_lit(msg))))), DUMMY_SP)]
        .into_iter()
        .collect::<TokenStream>();
        mk().mac_expr(mk().mac(vec![macro_name], macro_msg, MacDelimiter::Parenthesis))
    }

    fn mk_cross_check(&self, mk: Builder, args: Vec<&str>) -> Builder {
        if self.tcfg.cross_checks {
            mk.call_attr("cross_check", args)
        } else {
            mk
        }
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
            _ => {}
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
                | CExprKind::ImplicitCast(_, _, cast_kind, _, _)
                | CExprKind::ExplicitCast(_, _, cast_kind, _, _) => match cast_kind {
                    CastKind::IntegralToPointer
                    | CastKind::FunctionToPointerDecay
                    | CastKind::PointerToIntegral => return true,
                    _ => {},
                }
                _ => {}
            }
        }

        false
    }

    /// The purpose of this function is to decide on whether or not a static initializer's
    /// translation is able to be compiled as a valid rust static initializer
    fn static_initializer_is_uncompilable(&self, expr_id: Option<CExprId>, qtype: CQualTypeId) -> bool {
        use crate::c_ast::BinOp::{Add, Divide, Modulus, Multiply, Subtract};
        use crate::c_ast::CastKind::{IntegralToPointer, PointerToIntegral};
        use crate::c_ast::UnOp::{AddressOf, Negate};

        let expr_id = match expr_id {
            Some(expr_id) => expr_id,
            None => return false,
        };

        // The f128 crate doesn't currently provide a way to const initialize
        // values, except for common mathematical constants
        if let CTypeKind::LongDouble = self.ast_context[qtype.ctype].kind {
            return true;
        }

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
                    if self
                        .ast_context
                        .resolve_type(typ.ctype)
                        .kind
                        .is_unsigned_integral_type()
                    {
                        return true;
                    }
                }
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
                }
                CExprKind::Unary(_, AddressOf, expr_id, _) => {
                    if let CExprKind::Member(_, expr_id, _, _, _) = self.ast_context[expr_id].kind {
                        if let CExprKind::DeclRef(..) = self.ast_context[expr_id].kind {
                            return true;
                        }
                    }
                }
                CExprKind::InitList(qtype, _, _, _) => {
                    let ty = &self.ast_context.resolve_type(qtype.ctype).kind;

                    match ty {
                        CTypeKind::Struct(decl_id) => {
                            let decl = &self.ast_context[*decl_id].kind;

                            if let CDeclKind::Struct {
                                fields: Some(fields),
                                ..
                            } = decl
                            {
                                for field_id in fields {
                                    let field_decl = &self.ast_context[*field_id].kind;

                                    if let CDeclKind::Field {
                                        bitfield_width: Some(_),
                                        ..
                                    } = field_decl
                                    {
                                        return true;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                CExprKind::ImplicitCast(qtype, _, IntegralToPointer, _, _)
                | CExprKind::ExplicitCast(qtype, _, IntegralToPointer, _, _) => {
                    if let CTypeKind::Pointer(qtype) =
                        self.ast_context.resolve_type(qtype.ctype).kind
                    {
                        if let CTypeKind::Function(..) =
                            self.ast_context.resolve_type(qtype.ctype).kind
                        {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }

        false
    }

    fn add_static_initializer_to_section(
        &self,
        name: &str,
        typ: CQualTypeId,
        init: &mut P<Expr>,
    ) -> Result<(), TranslationError> {
        let mut default_init = self.implicit_default_expr(typ.ctype, true)?.to_expr();

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

        let fn_name = self
            .renamer
            .borrow_mut()
            .pick_name("run_static_initializers");
        let fn_ty = FunctionRetTy::Default(DUMMY_SP);
        let fn_decl = mk().fn_decl(vec![], fn_ty, false);
        let fn_block = mk().block(sectioned_static_initializers);
        let fn_attributes = self.mk_cross_check(mk(), vec!["none"]);
        let fn_item = fn_attributes
            .unsafe_()
            .abi("C")
            .fn_item(&fn_name, &fn_decl, fn_block);

        let static_attributes = mk()
            .single_attr("used")
            .call_attr(
                "cfg_attr",
                vec!["target_os = \"linux\"", "link_section = \".init_array\""],
            )
            .call_attr(
                "cfg_attr",
                vec!["target_os = \"windows\"", "link_section = \".CRT$XIB\""],
            )
            .call_attr(
                "cfg_attr",
                vec![
                    "target_os = \"macos\"",
                    "link_section = \"__DATA,__mod_init_func\"",
                ],
            );
        let static_array_size = mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed));
        let static_ty = mk().array_ty(
            mk().unsafe_().abi("C").barefn_ty(fn_decl),
            static_array_size,
        );
        let static_val = mk().array_expr(vec![mk().path_expr(vec![fn_name])]);
        let static_item = static_attributes.static_item("INIT_ARRAY", static_ty, static_val);

        (fn_item, static_item)
    }

    fn convert_decl(
        &self,
        ctx: ExprContext,
        decl_id: CDeclId,
    ) -> Result<ConvertedDecl, TranslationError> {
        let mut s = self
            .comment_context
            .get_decl_comment(decl_id)
            .and_then(|decl_cmt| self.comment_store.borrow_mut().add_comment_lines(decl_cmt))
            .unwrap_or(DUMMY_SP);

        let decl = self
            .ast_context
            .get_decl(&decl_id)
            .ok_or_else(|| format_err!("Missing decl {:?}", decl_id))?;

        let _src_loc = &decl.loc;

        match decl.kind {
            CDeclKind::Struct { fields: None, .. }
            | CDeclKind::Union { fields: None, .. }
            | CDeclKind::Enum {
                integral_type: None,
                ..
            } => {
                self.use_feature("extern_types");
                let name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .unwrap();
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
                let name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .unwrap();
                let mut has_bitfields = false;

                // Check if the last field might be a flexible array member
                if let Some(last_id) = fields.last() {
                    let field_decl = &self.ast_context[*last_id];
                    if let CDeclKind::Field { typ, .. } = field_decl.kind {
                        if self.ast_context.maybe_flexible_array(typ.ctype) {
                            self.potential_flexible_array_members
                                .borrow_mut()
                                .insert(*last_id);
                        }
                    }
                }

                // Gather up all the field names and field types
                let mut field_entries = vec![];
                let mut field_info = Vec::new();

                for &x in fields {
                    match self.ast_context.index(x).kind {
                        CDeclKind::Field {
                            ref name,
                            typ,
                            bitfield_width,
                            platform_bit_offset,
                            platform_type_bitwidth,
                        } => {
                            let name = self
                                .type_converter
                                .borrow_mut()
                                .declare_field_name(decl_id, x, name);

                            has_bitfields |= bitfield_width.is_some();

                            field_info.push((
                                name.clone(),
                                typ.clone(),
                                bitfield_width,
                                platform_bit_offset,
                                platform_type_bitwidth,
                            ));

                            let typ = self.convert_type(typ.ctype)?;

                            field_entries.push(mk().pub_().struct_field(name, typ));
                        }
                        _ => {
                            return Err(TranslationError::generic(
                                "Found non-field in record field list",
                            ))
                        }
                    }
                }

                if has_bitfields {
                    return self.convert_bitfield_struct_decl(
                        name,
                        manual_alignment,
                        platform_byte_size,
                        s,
                        field_info,
                    );
                }

                let mut reprs = vec![simple_metaitem("C")];
                let max_field_alignment = if is_packed {
                    // `__attribute__((packed))` forces a max alignment of 1,
                    // overriding `#pragma pack`; this is also what clang does
                    Some(1)
                } else {
                    max_field_alignment
                };
                match max_field_alignment {
                    Some(1) => reprs.push(simple_metaitem("packed")),
                    Some(mfi) if mfi > 1 => {
                        let lit = mk().int_lit(mfi as u128, LitIntType::Unsuffixed);
                        let inner = mk().meta_item(
                            vec!["packed"],
                            MetaItemKind::List(vec![
                                mk().nested_meta_item(NestedMetaItem::Literal(lit))
                            ]),
                        );
                        reprs.push(mk().nested_meta_item(NestedMetaItem::MetaItem(inner)));
                    }
                    _ => { }
                }
                // https://github.com/rust-lang/rust/issues/33626
                if let Some(alignment) = manual_alignment {
                    let lit = mk().int_lit(alignment as u128, LitIntType::Unsuffixed);
                    let inner = mk().meta_item(
                        vec!["align"],
                        MetaItemKind::List(vec![
                            mk().nested_meta_item(NestedMetaItem::Literal(lit))
                        ]),
                    );
                    reprs.push(mk().nested_meta_item(NestedMetaItem::MetaItem(inner)));
                };

                let repr_attr = mk().meta_item(vec!["repr"], MetaItemKind::List(reprs));

                Ok(ConvertedDecl::Item(
                    mk().span(s)
                        .pub_()
                        .call_attr("derive", vec!["Copy", "Clone"])
                        .meta_item_attr(AttrStyle::Outer, repr_attr)
                        .struct_item(name, field_entries),
                ))
            }

            CDeclKind::Union {
                fields: Some(ref fields),
                ..
            } => {
                let name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .unwrap();

                let mut field_syns = vec![];
                for &x in fields {
                    let field_decl = self.ast_context.index(x);
                    match field_decl.kind {
                        CDeclKind::Field { ref name, typ, .. } => {
                            let name = self
                                .type_converter
                                .borrow_mut()
                                .declare_field_name(decl_id, x, name);
                            let typ = self.convert_type(typ.ctype)?;
                            field_syns.push(mk().pub_().struct_field(name, typ))
                        }
                        _ => {
                            return Err(TranslationError::generic(
                                "Found non-field in record field list",
                            ))
                        }
                    }
                }

                Ok(if field_syns.is_empty() {
                    // Empty unions are a GNU extension, but Rust doesn't allow empty unions.
                    ConvertedDecl::Item(
                        mk().span(s)
                            .pub_()
                            .call_attr("derive", vec!["Copy", "Clone"])
                            .call_attr("repr", vec!["C"])
                            .struct_item(name, vec![]),
                    )
                } else {
                    ConvertedDecl::Item(
                        mk().span(s)
                            .pub_()
                            .call_attr("derive", vec!["Copy", "Clone"])
                            .call_attr("repr", vec!["C"])
                            .union_item(name, field_syns),
                    )
                })
            }

            CDeclKind::Field { .. } => Err(TranslationError::generic(
                "Field declarations should be handled inside structs/unions",
            )),

            CDeclKind::Enum {
                integral_type: Some(integral_type),
                ..
            } => {
                let enum_name = &self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .expect("Enums should already be renamed");
                let ty = self.convert_type(integral_type.ctype)?;
                Ok(ConvertedDecl::Item(
                    mk().span(s).pub_().type_item(enum_name, ty),
                ))
            }

            CDeclKind::EnumConstant { value, .. } => {
                let name = self
                    .renamer
                    .borrow_mut()
                    .get(&decl_id)
                    .expect("Enum constant not named");
                let enum_id = self.ast_context.parents[&decl_id];
                let enum_name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(enum_id)
                    .expect("Enums should already be renamed");
                let ty = mk().path_ty(mk().path(vec![enum_name]));
                let val = match value {
                    ConstIntExpr::I(value) => signed_int_expr(value),
                    ConstIntExpr::U(value) => {
                        mk().lit_expr(mk().int_lit(value as u128, LitIntType::Unsuffixed))
                    }
                };

                Ok(ConvertedDecl::Item(
                    mk().span(s).pub_().const_item(name, ty, val),
                ))
            }

            // We can allow non top level function declarations (i.e. extern
            // declarations) without any problem. Clang doesn't support nested
            // functions, so we will never see nested function definitions.

            CDeclKind::Function {
                is_global,
                is_inline,
                is_extern,
                typ,
                ref name,
                ref parameters,
                body,
                ref attrs,
                ..
            } => {
                let new_name = &self
                    .renamer
                    .borrow()
                    .get(&decl_id)
                    .expect("Functions should already be renamed");

                if self.import_simd_function(new_name)? {
                    return Ok(ConvertedDecl::NoItem);
                }

                let (ret, is_var): (Option<CQualTypeId>, bool) =
                    match self.ast_context.resolve_type(typ).kind {
                        CTypeKind::Function(ret, _, is_var, is_noreturn, _) => {
                            (if is_noreturn { None } else { Some(ret) }, is_var)
                        }
                        ref k => {
                            return Err(format_err!(
                                "Type of function {:?} was not a function type, got {:?}",
                                decl_id,
                                k
                            )
                            .into())
                        }
                    };

                let mut args: Vec<(CDeclId, String, CQualTypeId)> = vec![];
                for param_id in parameters {
                    if let CDeclKind::Variable { ref ident, typ, .. } =
                        self.ast_context.index(*param_id).kind
                    {
                        args.push((*param_id, ident.clone(), typ))
                    } else {
                        return Err(TranslationError::generic(
                            "Parameter is not variable declaration",
                        ));
                    }
                }

                let is_main = self.ast_context.c_main == Some(decl_id);

                let converted_function = self.convert_function(
                    ctx, s, is_global, is_inline, is_main, is_var, is_extern, new_name, name,
                    &args, ret, body, attrs,
                );

                converted_function.or_else(|e| match self.tcfg.replace_unsupported_decls {
                    ReplaceMode::Extern if body.is_none() => self.convert_function(
                        ctx, s, is_global, false, is_main, is_var, is_extern, new_name, name,
                        &args, ret, None, attrs,
                    ),
                    _ => Err(e),
                })
            }

            CDeclKind::Typedef { ref typ, .. } => {
                let new_name = &self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .unwrap();

                if self.import_simd_typedef(new_name) {
                    return Ok(ConvertedDecl::NoItem);
                }

                let ty = self.convert_type(typ.ctype)?;
                Ok(ConvertedDecl::Item(
                    mk().span(s).pub_().type_item(new_name, ty),
                ))
            }

            // Externally-visible variable without initializer (definition elsewhere)
            CDeclKind::Variable {
                is_externally_visible: true,
                has_static_duration,
                has_thread_duration,
                is_defn: false,
                ref ident,
                initializer,
                typ,
                ref attrs,
                ..
            } => {
                assert!(
                    has_static_duration || has_thread_duration,
                    "An extern variable must be static or thread-local"
                );
                assert!(
                    initializer.is_none(),
                    "An extern variable that isn't a definition can't have an initializer"
                );

                if has_thread_duration {
                    self.use_feature("thread_local");
                }

                let new_name = self
                    .renamer
                    .borrow()
                    .get(&decl_id)
                    .expect("Variables should already be renamed");
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
                        c_ast::Attribute::Alias(aliasee) => {
                            extern_item.str_attr("link_name", aliasee)
                        }
                        _ => continue,
                    };
                }

                Ok(ConvertedDecl::ForeignItem(
                    extern_item.static_foreign_item(&new_name, ty),
                ))
            }

            // Static-storage or thread-local variable with initializer (definition here)
            CDeclKind::Variable {
                has_static_duration,
                has_thread_duration,
                is_externally_visible,
                ref ident,
                initializer,
                typ,
                ref attrs,
                ..
            } if has_static_duration || has_thread_duration => {
                if has_thread_duration {
                    self.use_feature("thread_local");
                }

                let new_name = &self
                    .renamer
                    .borrow()
                    .get(&decl_id)
                    .expect("Variables should already be renamed");

                // Collect problematic static initializers and offload them to sections for the linker
                // to initialize for us
                let (ty, init) = if self.static_initializer_is_uncompilable(initializer, typ) {
                    // Note: We don't pass has_static_duration through here. Extracted initializers
                    // are run outside of the static initializer.
                    let (ty, _, init) =
                        self.convert_variable(ctx.not_static(), initializer, typ)?;

                    let mut init = init?.to_expr();

                    let comment = String::from("// Initialized in run_static_initializers");
                    // REVIEW: We might want to add the comment to the original span comments
                    s = self
                        .comment_store
                        .borrow_mut()
                        .add_comment_lines(&[comment])
                        .unwrap_or(s);

                    self.add_static_initializer_to_section(new_name, typ, &mut init)?;

                    (ty, init)
                } else {
                    let (ty, _, init) = self.convert_variable(ctx.static_(), initializer, typ)?;
                    let mut init = init?;
                    // TODO: Replace this by relying entirely on
                    // WithStmts.is_unsafe() of the translated variable
                    if self.static_initializer_is_unsafe(initializer, typ) {
                        init.set_unsafe()
                    }
                    let init = init.to_unsafe_pure_expr()
                        .ok_or_else(|| {
                            format_err!("Expected no side-effects in static initializer")
                        })?;

                    (ty, init)
                };

                let static_def = if is_externally_visible {
                    mk_linkage(false, new_name, ident).pub_().abi("C")
                } else if self.cur_file.borrow().is_some() {
                    mk().pub_()
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
                        c_ast::Attribute::Section(name) => {
                            static_def.str_attr("link_section", name)
                        }
                        _ => continue,
                    }
                }

                Ok(ConvertedDecl::Item(
                    static_def.static_item(new_name, ty, init),
                ))
            }

            CDeclKind::Variable { .. } => Err(TranslationError::generic(
                "This should be handled in 'convert_decl_stmt'",
            )),

            CDeclKind::MacroObject {
                ref replacements, ..
            } => {
                let name = self
                    .renamer
                    .borrow_mut()
                    .get(&decl_id)
                    .expect("Macro object not named");

                trace!("Expanding macro {:?}: {:?}", decl_id, self.ast_context[decl_id]);

                let maybe_replacement = self.canonical_macro_replacement(
                    ctx.set_const(true).set_expanding_macro(decl_id),
                    &replacements,
                );

                match maybe_replacement {
                    Ok((replacement, ty)) => {
                        self.macro_types.borrow_mut().insert(decl_id, ty);
                        let ty = self.convert_type(ty)?;

                        Ok(ConvertedDecl::Item(mk().span(s).pub_().const_item(
                            name,
                            ty,
                            replacement,
                        )))
                    }
                    Err(e) => {
                        info!("Could not expand macro {}: {}", name, e);
                        Ok(ConvertedDecl::NoItem)
                    }
                }
            }

            // Do not translate non-canonical decls. They will be translated at
            // their canonical declaration.
            CDeclKind::NonCanonicalDecl { .. } => Ok(ConvertedDecl::NoItem),
        }
    }

    fn canonical_macro_replacement(
        &self,
        ctx: ExprContext,
        replacements: &[CExprId],
    ) -> Result<(P<Expr>, CTypeId), TranslationError> {
        let (val, ty) = replacements
            .iter()
            .try_fold::<Option<(WithStmts<P<Expr>>, CTypeId)>, _, _>(
                None,
                |canonical, id| {
                    let ty = self.ast_context[*id].kind.get_type()
                        .ok_or_else(|| format_err!("Invalid expression type"))?;
                    let (expr_id, ty) = self.ast_context.resolve_expr_type_id(*id)
                        .unwrap_or((*id, ty));
                    let expr = self.convert_expr(ctx, expr_id)?;

                    // Join ty and cur_ty to the smaller of the two types. If the
                    // types are not cast-compatible, abort the fold.
                    let ty_kind = self.ast_context.resolve_type(ty).kind.clone();
                    if let Some((canon_val, canon_ty)) = canonical {
                        let canon_ty_kind = self
                            .ast_context
                            .resolve_type(canon_ty)
                            .kind
                            .clone();
                        if let Some(smaller_ty) = CTypeKind::smaller_compatible_type(canon_ty_kind.clone(), ty_kind.clone()) {
                            if smaller_ty == canon_ty_kind {
                                Ok(Some((canon_val, canon_ty)))
                            } else {
                                Ok(Some((expr, ty)))
                            }
                        } else {
                            Err(format_err!("Not all macro expansions are compatible types"))
                        }
                    } else {
                        Ok(Some((expr, ty)))
                    }
                }
            )?.ok_or_else(|| format_err!("Could not find a valid type for macro"))?;

        val.to_unsafe_pure_expr().map(|val| (val, ty))
            .ok_or_else(|| TranslationError::generic("Macro expansion is not a pure expression"))

        // TODO: Validate that all replacements are equivalent and pick the most
        // common type to minimize casts.
    }

    /// Returns true iff type is a (pointer to)* the `va_list` structure type.
    pub fn is_inner_type_valist(ctxt: &TypedAstContext, qtype: CQualTypeId) -> bool {
        if ctxt.is_va_list(qtype.ctype) {
            true
        } else if let CTypeKind::Pointer(pointer_id) = ctxt.resolve_type(qtype.ctype).kind {
            Self::is_inner_type_valist(ctxt, pointer_id)
        } else {
            false
        }
    }

    fn convert_function(
        &self,
        ctx: ExprContext,
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
    ) -> Result<ConvertedDecl, TranslationError> {
        self.function_context.borrow_mut().enter_new(name);

        let is_valist: bool = arguments
            .iter()
            .any(|&(_, _, typ)| Self::is_inner_type_valist(&self.ast_context, typ));
        if is_variadic || is_valist {
            if let Some(body_id) = body {
                if !self.is_well_formed_variadic(body_id) {
                    return Err(format_err!("Variadic function definition is not well-formed.").into());
                }
            }
        }

        self.with_scope(|| {
            let mut args: Vec<Arg> = vec![];

            // handle regular (non-variadic) arguments
            for &(decl_id, ref var, typ) in arguments {
                let (ty, mutbl, _) = self.convert_variable(ctx, None, typ)?;

                let pat = if var.is_empty() {
                    mk().wild_pat()
                } else {
                    // extern function declarations don't support/require mut patterns
                    let mutbl = if body.is_none() {
                        Mutability::Immutable
                    } else {
                        mutbl
                    };

                    let new_var = self
                        .renamer
                        .borrow_mut()
                        .insert(decl_id, var.as_str())
                        .expect(&format!(
                            "Failed to insert argument '{}' while converting '{}'",
                            var, name
                        ));

                    mk().set_mutbl(mutbl).ident_pat(new_var)
                };

                args.push(mk().arg(ty, pat))
            }

            // handle variadic arguments
            if is_variadic {
                if let Some(va_decl_id) = self.get_promoted_va_decl() {
                    // `register_va_arg` succeeded
                    let var = self
                        .renamer
                        .borrow_mut()
                        .get(&va_decl_id)
                        .expect(&format!("Failed to get name for variadic argument"));

                    // FIXME: detect mutability requirements
                    let pat = mk().set_mutbl(Mutability::Mutable).ident_pat(var);
                    args.push(mk().arg(mk().cvar_args_ty(), pat))
                } else {
                    args.push(mk().arg(mk().cvar_args_ty(), mk().wild_pat()))
                }
            }

            // handle return type
            let ret = match return_type {
                Some(return_type) => self.convert_type(return_type.ctype)?,
                None => mk().never_ty(),
            };
            let is_void_ret = return_type
                .map(|qty| self.ast_context[qty.ctype].kind == CTypeKind::Void)
                .unwrap_or(false);

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
                        let ret_type_id: CTypeId =
                            self.ast_context.resolve_type_id(return_type.ctype);
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
                    self.mk_cross_check(mk(), vec!["entry(djb2=\"main\")", "exit(djb2=\"main\")"])
                } else if is_global && !is_inline {
                    mk_linkage(false, new_name, name).abi("C").pub_()
                } else if is_inline && is_extern && !attrs.contains(&c_ast::Attribute::GnuInline) {
                    // c99 extern inline functions should be pub, but not gnu_inline attributed
                    // extern inlines, which become subject to their gnu89 visibility (private)

                    mk_linkage(false, new_name, name).abi("C").pub_()
                } else if self.cur_file.borrow().is_some() {
                    mk().abi("C").pub_()
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

                    // * In C99, a function defined inline will never, and a function defined extern
                    //   inline will always, emit an externally visible function.
                    // * If a non-static function is declared inline, then it must be defined in the
                    //   same translation unit. The inline definition that does not use extern is
                    //   not externally visible and does not prevent other translation units from
                    //   defining the same function. This makes the inline keyword an alternative to
                    //   static for defining functions inside header files, which may be included in
                    //   multiple translation units of the same program.
                    // * always_inline implies inline -
                    //   https://gcc.gnu.org/ml/gcc-help/2007-01/msg00051.html
                    //   even if the `inline` keyword isn't present
                    // * gnu_inline instead applies gnu89 rules. extern inline will not emit an
                    //   externally visible function.
                    if is_global && is_extern && !attrs.contains(&c_ast::Attribute::GnuInline) {
                        self.use_feature("linkage");
                        // ensures that public inlined rust function can be used in other modules
                        mk_ = mk_.single_attr("linkage = \"external\"");
                    }
                    // NOTE: it does not seem necessary to have an else branch here that
                    // specifies internal linkage in all other cases due to name mangling by rustc.
                }

                Ok(ConvertedDecl::Item(
                    mk_.span(span).unsafe_().fn_item(new_name, decl, block),
                ))
            } else {
                // Translating an extern function declaration

                // When putting extern fns into submodules, they need to be public to be accessible
                let visibility = if self.tcfg.reorganize_definitions {
                    "pub"
                } else {
                    ""
                };

                let mut mk_ = mk_linkage(true, new_name, name).span(span).vis(visibility);

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
    ) -> Result<Vec<Stmt>, TranslationError> {
        if self.tcfg.dump_function_cfgs {
            graph
                .dump_dot_graph(
                    &self.ast_context,
                    &store,
                    self.tcfg.dump_cfg_liveness,
                    self.tcfg.use_c_loop_info,
                    format!("{}_{}.dot", "cfg", name),
                )
                .expect("Failed to write CFG .dot file");
        }
        if self.tcfg.json_function_cfgs {
            graph
                .dump_json_graph(&store, format!("{}_{}.json", "cfg", name))
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

            let local = mk().local(
                mk().mutbl().ident_pat(current_block_ident),
                Some(current_block_ty),
                None as Option<P<Expr>>,
            );
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
    ) -> Result<Vec<Stmt>, TranslationError> {
        // Function body scope
        self.with_scope(|| {
            let (graph, store) = cfg::Cfg::from_stmts(self, ctx, body_ids, ret)?;
            self.convert_cfg(name, graph, store, IndexSet::new(), true)
        })
    }

    /// Convert a C expression to a rust boolean expression
    pub fn convert_condition(
        &self,
        ctx: ExprContext,
        target: bool,
        cond_id: CExprId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let ty_id = self.ast_context[cond_id]
            .kind
            .get_type()
            .ok_or_else(|| format_err!("bad condition type"))?;

        let null_pointer_case =
            |negated: bool, ptr: CExprId| -> Result<WithStmts<P<Expr>>, TranslationError> {
                let val = self.convert_expr(ctx.used().decay_ref(), ptr)?;
                let ptr_type = self.ast_context[ptr]
                    .kind
                    .get_type()
                    .ok_or_else(|| format_err!("bad pointer type for condition"))?;
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
                if self.ast_context.is_null_expr(null_expr) =>
            {
                null_pointer_case(!target, ptr)
            }

            CExprKind::Binary(_, c_ast::BinOp::EqualEqual, ptr, null_expr, _, _)
                if self.ast_context.is_null_expr(null_expr) =>
            {
                null_pointer_case(!target, ptr)
            }

            CExprKind::Binary(_, c_ast::BinOp::NotEqual, null_expr, ptr, _, _)
                if self.ast_context.is_null_expr(null_expr) =>
            {
                null_pointer_case(target, ptr)
            }

            CExprKind::Binary(_, c_ast::BinOp::NotEqual, ptr, null_expr, _, _)
                if self.ast_context.is_null_expr(null_expr) =>
            {
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

    /// Search for references to the given declaration in a value position
    /// inside the given expression. Uses of the declaration inside typeof
    /// operations are ignored because our translation will ignore them
    /// and use the computed types instead.
    fn has_decl_reference(&self, decl_id: CDeclId, expr_id: CExprId) -> bool {
        let mut iter = DFExpr::new(&self.ast_context, expr_id.into());
        while let Some(x) = iter.next() {
            match x {
                SomeId::Expr(e) => match self.ast_context[e].kind {
                    CExprKind::DeclRef(_, d, _) if d == decl_id => return true,
                    CExprKind::UnaryType(_, _, Some(_), _) => iter.prune(1),
                    _ => {}
                },
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

    pub fn convert_decl_stmt_info(
        &self,
        ctx: ExprContext,
        decl_id: CDeclId,
    ) -> Result<cfg::DeclStmtInfo, TranslationError> {
        if self.is_promoted_va_decl(decl_id) {
            // `va_list` decl was promoted to arg
            self.use_feature("c_variadic");
            return Ok(cfg::DeclStmtInfo::empty());
        }

        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable {
                ref ident,
                has_static_duration: true,
                is_externally_visible: false,
                is_defn: true,
                initializer,
                typ,
                ..
            } => {
                if self.static_initializer_is_uncompilable(initializer, typ) {
                    let ident2 = self
                        .renamer
                        .borrow_mut()
                        .insert_root(decl_id, ident)
                        .ok_or_else(|| {
                            TranslationError::generic(
                                "Unable to rename function scoped static initializer",
                            )
                        })?;
                    let (ty, _, init) = self.convert_variable(ctx.static_(), initializer, typ)?;
                    let default_init = self.implicit_default_expr(typ.ctype, true)?.to_expr();
                    let comment = String::from("// Initialized in run_static_initializers");
                    let span = self
                        .comment_store
                        .borrow_mut()
                        .add_comment_lines(&[comment])
                        .unwrap_or(DUMMY_SP);
                    let static_item =
                        mk().span(span)
                            .mutbl()
                            .static_item(&ident2, ty, default_init);
                    let mut init = init?;
                    init.set_unsafe();
                    let mut init = init.to_expr();

                    self.add_static_initializer_to_section(&ident2, typ, &mut init)?;
                    self.items.borrow_mut()[&self.main_file].add_item(static_item);

                    return Ok(cfg::DeclStmtInfo::empty());
                }
            }
            _ => {}
        };

        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable {
                has_static_duration: false,
                has_thread_duration: false,
                is_externally_visible: false,
                is_defn,
                ref ident,
                initializer,
                typ,
                ..
            } => {
                assert!(
                    is_defn,
                    "Only local variable definitions should be extracted"
                );

                let rust_name = self
                    .renamer
                    .borrow_mut()
                    .insert(decl_id, &ident)
                    .expect(&format!("Failed to insert variable '{}'", ident));

                if self.is_copied_va_decl(decl_id) {
                    // translate `va_list` declarations not promoted to an arg
                    // to `VaList` and do not emit an initializer.
                    let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name.clone());
                    let ty = {
                        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };
                        let path = vec!["", std_or_core, "ffi", "VaListImpl"];
                        mk().path_ty(path)
                    };
                    let local_mut = mk().local::<_, _, P<Expr>>(pat_mut, Some(ty), None);

                    return Ok(cfg::DeclStmtInfo::new(
                        vec![],                              // decl
                        vec![],                              // assign
                        vec![mk().local_stmt(P(local_mut))], // decl_and_assign
                    ));
                }

                let has_self_reference = if let Some(expr_id) = initializer {
                    self.has_decl_reference(decl_id, expr_id)
                } else {
                    false
                };

                let mut stmts = self.compute_variable_array_sizes(ctx, typ.ctype)?;

                let (ty, mutbl, init) = self.convert_variable(ctx, initializer, typ)?;
                let mut init = init?;

                stmts.append(init.stmts_mut());
                let init = init.into_value();

                let zeroed = self.implicit_default_expr(typ.ctype, false)?;
                let zeroed = if ctx.is_const {
                    zeroed.to_unsafe_pure_expr()
                } else {
                    zeroed.to_pure_expr()
                }.expect("Expected decl initializer to not have any statements");
                let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name.clone());
                let local_mut = mk().local(pat_mut, Some(ty.clone()), Some(zeroed));
                if has_self_reference {
                    let assign = mk().assign_expr(mk().ident_expr(rust_name), init);

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
                    let pat = mk().set_mutbl(mutbl).ident_pat(rust_name.clone());

                    let type_annotation = if self.tcfg.reduce_type_annotations
                        && !self.should_assign_type_annotation(typ.ctype, initializer)
                    {
                        None
                    } else {
                        Some(ty)
                    };

                    let local = mk().local(pat, type_annotation, Some(init.clone()));
                    let assign = mk().assign_expr(mk().ident_expr(rust_name), init);

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
                    self.renamer.borrow_mut().insert(decl_id, &ident).is_some()
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
                    Ok(cfg::DeclStmtInfo::new(vec![], vec![], vec![]))
                } else {
                    let item = match self.convert_decl(ctx, decl_id)? {
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
            }
        }
    }

    fn should_assign_type_annotation(
        &self,
        ctypeid: CTypeId,
        initializer: Option<CExprId>,
    ) -> bool {
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
                        if let Some(CExprKind::ImplicitCast(_, _, CastKind::NullToPointer, _, _)) =
                            initializer_kind
                        {
                            return true;
                        }

                        // We could set this to false and skip non null fn ptr annotations. This will work
                        // 99% of the time, however there is a strange case where fn ptr comparisons
                        // complain PartialEq is not implemented for the type inferred function type,
                        // but the identical type that is explicitly defined doesn't seem to have that issue
                        // Probably a rustc bug. See https://github.com/rust-lang/rust/issues/53861
                        true
                    }
                    _ => {
                        // Non function null ptrs provide enough information to skip
                        // type annotations; ie `= 0 as *const MyStruct;`
                        if initializer.is_none() {
                            return false;
                        }

                        if let Some(CExprKind::ImplicitCast(_, _, cast_kind, _, _)) =
                            initializer_kind
                        {
                            match cast_kind {
                                CastKind::NullToPointer => return false,
                                CastKind::ConstCast => return true,
                                _ => {}
                            };
                        }

                        // ref decayed ptrs generally need a type annotation
                        if let Some(CExprKind::Unary(_, c_ast::UnOp::AddressOf, _, _)) =
                            initializer_kind
                        {
                            return true;
                        }

                        false
                    }
                }
            }
            // For some reason we don't seem to apply type suffixes when 0-initializing
            // so type annotation is need for 0-init ints and floats at the moment, but
            // they could be simplified in favor of type suffixes
            CTypeKind::Bool
            | CTypeKind::Char
            | CTypeKind::SChar
            | CTypeKind::Short
            | CTypeKind::Int
            | CTypeKind::Long
            | CTypeKind::LongLong
            | CTypeKind::UChar
            | CTypeKind::UShort
            | CTypeKind::UInt
            | CTypeKind::ULong
            | CTypeKind::ULongLong
            | CTypeKind::LongDouble
            | CTypeKind::Int128
            | CTypeKind::UInt128 => initializer.is_none(),
            CTypeKind::Float | CTypeKind::Double => initializer.is_none(),
            CTypeKind::Struct(_) | CTypeKind::Union(_) | CTypeKind::Enum(_) => false,
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
    ) -> Result<
        (
            P<Ty>,
            Mutability,
            Result<WithStmts<P<Expr>>, TranslationError>,
        ),
        TranslationError,
    > {
        let init = match initializer {
            Some(x) => self.convert_expr(ctx.used(), x),
            None => self.implicit_default_expr(typ.ctype, ctx.is_static),
        };

        // Variable declarations for variable-length arrays use the type of a pointer to the
        // underlying array element
        let ty = if let CTypeKind::VariableArray(mut elt, _) =
            self.ast_context.resolve_type(typ.ctype).kind
        {
            elt = self.variable_array_base_type(elt);
            let ty = self.convert_type(elt)?;
            mk().path_ty(vec![
                mk().path_segment_with_args("Vec", mk().angle_bracketed_args(vec![ty]))
            ])
        } else {
            self.convert_type(typ.ctype)?
        };

        let mutbl = if typ.qualifiers.is_const {
            Mutability::Immutable
        } else {
            Mutability::Mutable
        };

        Ok((ty, mutbl, init))
    }

    fn convert_type(&self, type_id: CTypeId) -> Result<P<Ty>, TranslationError> {
        if let Some(cur_file) = *self.cur_file.borrow() {
            self.import_type(type_id, cur_file);
        }
        self.type_converter
            .borrow_mut()
            .convert(&self.ast_context, type_id)
    }

    /// Construct an expression for a NULL at any type, including forward declarations,
    /// function pointers, and normal pointers.
    fn null_ptr(&self, type_id: CTypeId, is_static: bool) -> Result<P<Expr>, TranslationError> {
        if self.ast_context.is_function_pointer(type_id) {
            return Ok(mk().path_expr(vec!["None"]));
        }

        let pointee = match self.ast_context.resolve_type(type_id).kind {
            CTypeKind::Pointer(pointee) => pointee,
            _ => return Err(TranslationError::generic("null_ptr requires a pointer")),
        };
        let ty = self.convert_type(type_id)?;
        let mut zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
        if is_static && !pointee.qualifiers.is_const {
            let mut qtype = pointee;
            qtype.qualifiers.is_const = true;
            self.use_feature("const_raw_ptr_to_usize_cast");
            let ty_ = self
                .type_converter
                .borrow_mut()
                .convert_pointer(&self.ast_context, qtype)?;
            zero = mk().cast_expr(zero, ty_);
        }
        Ok(mk().cast_expr(zero, ty))
    }

    /// Write to a `lhs` that is volatile
    pub fn volatile_write(
        &self,
        lhs: &P<Expr>,
        lhs_type: CQualTypeId,
        rhs: P<Expr>,
    ) -> Result<P<Expr>, TranslationError> {
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
            }
        };
        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };

        Ok(mk().call_expr(
            mk().path_expr(vec!["", std_or_core, "ptr", "write_volatile"]),
            vec![addr_lhs, rhs],
        ))
    }

    /// Read from a `lhs` that is volatile
    pub fn volatile_read(
        &self,
        lhs: &P<Expr>,
        lhs_type: CQualTypeId,
    ) -> Result<P<Expr>, TranslationError> {
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
        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };

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
                let csize_name = self
                    .renamer
                    .borrow()
                    .get(&CDeclId(counts.0))
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
    pub fn compute_variable_array_sizes(
        &self,
        ctx: ExprContext,
        mut type_id: CTypeId,
    ) -> Result<Vec<Stmt>, TranslationError> {
        let mut stmts = vec![];

        loop {
            match self.ast_context.resolve_type(type_id).kind {
                CTypeKind::Pointer(elt) => type_id = elt.ctype,
                CTypeKind::ConstantArray(elt, _) => type_id = elt,
                CTypeKind::VariableArray(elt, Some(expr_id)) => {
                    type_id = elt;

                    // Convert this expression
                    let expr = self.convert_expr(ctx.used(), expr_id)?
                        .and_then(|expr| {
                            let name = self
                                .renamer
                                .borrow_mut()
                                .insert(CDeclId(expr_id.0), "vla")
                                .unwrap(); // try using declref name?
                            // TODO: store the name corresponding to expr_id

                            let local = mk().local(
                                mk().ident_pat(name),
                                None as Option<P<Ty>>,
                                Some(mk().cast_expr(expr, mk().path_ty(vec!["usize"]))),
                            );

                            let res: Result<WithStmts<()>, TranslationError> = Ok(
                                WithStmts::new(vec![mk().local_stmt(P(local))], ())
                            );
                            res
                        })?;

                    stmts.extend(expr.into_stmts());
                }
                _ => break,
            }
        }

        Ok(stmts)
    }

    // Compute the size of a type
    // Rust type: usize
    pub fn compute_size_of_type(
        &self,
        ctx: ExprContext,
        type_id: CTypeId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        if let CTypeKind::VariableArray(elts, len) = self.ast_context.resolve_type(type_id).kind {
            let len = len.expect("Sizeof a VLA type with count expression omitted");

            let elts = self.compute_size_of_type(ctx, elts)?;
            return elts.and_then(|lhs| {
                let len = self.convert_expr(ctx.used().not_static(), len)?;
                Ok(len.map(|len| {
                    let rhs = cast_int(len, "usize");
                    mk().binary_expr(BinOpKind::Mul, lhs, rhs)
                }))
            });
        }
        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };
        let ty = self.convert_type(type_id)?;
        let name = "size_of";
        let params = mk().angle_bracketed_args(vec![ty]);
        let path = vec![
            mk().path_segment(""),
            mk().path_segment(std_or_core),
            mk().path_segment("mem"),
            mk().path_segment_with_args(name, params),
        ];
        let call = mk().call_expr(mk().path_expr(path), vec![] as Vec<P<Expr>>);

        Ok(WithStmts::new_val(call))
    }

    pub fn compute_align_of_type(
        &self,
        mut type_id: CTypeId,
        preferred: bool,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        type_id = self.variable_array_base_type(type_id);

        let ty = self.convert_type(type_id)?;
        let tys = vec![ty];
        let mut path = vec![mk().path_segment("")];
        if self.tcfg.emit_no_std {
            path.push(mk().path_segment("core"));
        } else {
            path.push(mk().path_segment("std"));
        }
        if preferred {
            self.use_feature("core_intrinsics");
            path.push(mk().path_segment("intrinsics"));
            path.push(mk().path_segment_with_args("pref_align_of", mk().angle_bracketed_args(tys)));
        } else {
            path.push(mk().path_segment("mem"));
            path.push(mk().path_segment_with_args("align_of", mk().angle_bracketed_args(tys)));
        }
        let call = mk().call_expr(mk().path_expr(path), vec![] as Vec<P<Expr>>);
        Ok(WithStmts::new_val(call))
    }

    fn convert_exprs(&self, ctx: ExprContext, exprs: &[CExprId])
                     -> Result<WithStmts<Vec<P<Expr>>>, TranslationError>
    {
        exprs
            .iter()
            .map(|arg| self.convert_expr(ctx, *arg))
            .collect()
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
    pub fn convert_expr(
        &self,
        mut ctx: ExprContext,
        expr_id: CExprId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let Located {
            loc: src_loc,
            kind: expr_kind,
        } = &self.ast_context[expr_id];

        trace!("Converting expr {:?}: {:?}", expr_id, self.ast_context[expr_id]);

        if self.tcfg.translate_const_macros {
            if let Some(converted) = self.convert_macro_expansion(ctx, expr_id)? {
                return Ok(converted);
            }
        }

        match *expr_kind {
            CExprKind::DesignatedInitExpr(..) => {
                Err(TranslationError::generic("Unexpected designated init expr"))
            }
            CExprKind::BadExpr => Err(TranslationError::generic(
                "convert_expr: expression kind not supported",
            )),
            CExprKind::ShuffleVector(_, ref child_expr_ids) => self
                .convert_shuffle_vector(ctx, child_expr_ids)
                .map_err(|e| {
                    TranslationError::new(self.ast_context.display_loc(src_loc), e.context(TranslationErrorKind::OldLLVMSimd))
                }),
            CExprKind::ConvertVector(..) => {
                Err(TranslationError::generic("convert vector not supported"))
            }

            CExprKind::UnaryType(_ty, kind, opt_expr, arg_ty) => {
                let result = match kind {
                    UnTypeOp::SizeOf => match opt_expr {
                        None => self.compute_size_of_type(ctx, arg_ty.ctype)?,
                        Some(_) => {
                            let inner = self.variable_array_base_type(arg_ty.ctype);
                            let inner_size = self.compute_size_of_type(ctx, inner)?;

                            if let Some(sz) = self.compute_size_of_expr(arg_ty.ctype) {
                                inner_size.map(|x| mk().binary_expr(BinOpKind::Mul, sz, x))
                            } else {
                                // Otherwise, use the pointer and make a deref of a pointer offset expression
                                inner_size
                            }
                        }
                    },
                    UnTypeOp::AlignOf => self.compute_align_of_type(arg_ty.ctype, false)?,
                    UnTypeOp::PreferredAlignOf => self.compute_align_of_type(arg_ty.ctype, true)?,
                };

                Ok(result.map(|x| mk().cast_expr(x, mk().path_ty(vec!["libc", "c_ulong"]))))
            }

            CExprKind::DeclRef(qual_ty, decl_id, lrvalue) => {
                let decl = &self
                    .ast_context
                    .get_decl(&decl_id)
                    .ok_or_else(|| format_err!("Missing declref {:?}", decl_id))?
                    .kind;
                if ctx.is_const {
                    if let CDeclKind::Variable { has_static_duration: true, .. } = decl {
                        return Err(format_translation_err!(
                            self.ast_context.display_loc(src_loc),
                            "Cannot refer to static duration variable in a const expression",
                        ));
                    }
                }

                let varname = decl.get_name().expect("expected variable name").to_owned();
                let rustname = self
                    .renamer
                    .borrow_mut()
                    .get(&decl_id)
                    .ok_or_else(|| format_err!("name not declared: '{}'", varname))?;

                // Import the referenced global decl into our submodule
                if self.tcfg.reorganize_definitions {
                    if let Some(cur_file) = self.cur_file.borrow().as_ref() {
                        self.add_import(*cur_file, decl_id, &rustname);
                        // match decl {
                        //     CDeclKind::Variable { is_defn: false, .. } => {}
                        //     _ => self.add_import(cur_file, decl_id, &rustname),
                        // }
                    }
                }

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

                // If we are referring to a function and need its address, we
                // need to cast it to fn() to ensure that it has a real address.
                if ctx.needs_address() {
                    if let &CDeclKind::Function { .. } = decl {
                        let ty = self.convert_type(qual_ty.ctype)?;
                        val = mk().cast_expr(val, ty);
                    }
                }

                if self.ast_context.is_va_list(qual_ty.ctype) {
                    val = mk().method_call_expr(val, "as_va_list", vec![] as Vec<P<Expr>>);
                } else if let CTypeKind::VariableArray(..) =
                    self.ast_context.resolve_type(qual_ty.ctype).kind
                {
                    val = mk().method_call_expr(val, "as_mut_ptr", vec![] as Vec<P<Expr>>);
                }

                Ok(WithStmts::new_val(val))
            }

            CExprKind::OffsetOf(ty, ref kind) => match kind {
                OffsetOfKind::Constant(val) => {
                    Ok(WithStmts::new_val(self.mk_int_lit(ty, *val, IntBase::Dec)))
                }
                OffsetOfKind::Variable(qty, field_id, expr_id) => {
                    self.extern_crates.borrow_mut().insert("memoffset");
                    self.items.borrow_mut()[&self.main_file].add_use(vec!["memoffset".into()], "offset_of");

                    // Struct Type
                    let decl_id = {
                        let kind = match self.ast_context[qty.ctype].kind {
                            CTypeKind::Elaborated(ty_id) => &self.ast_context[ty_id].kind,
                            ref kind => kind,
                        };

                        kind.as_decl_or_typedef()
                            .expect("Did not find decl_id for offsetof struct")
                    };
                    let name = self
                        .type_converter
                        .borrow()
                        .resolve_decl_name(decl_id)
                        .expect("Did not find name for offsetof struct");
                    let ty_ident = Nonterminal::NtIdent(mk().ident(name), false);

                    // Field name
                    let field_name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(None, *field_id)
                        .expect("Did not find name for offsetof struct field");
                    let field_ident = Nonterminal::NtIdent(mk().ident(field_name), false);

                    // Index Expr
                    let expr = self.convert_expr(ctx, *expr_id)?
                        .to_pure_expr()
                        .ok_or_else(|| {
                            format_err!("Expected Variable offsetof to be a side-effect free")
                        })?;
                    let expr = mk().cast_expr(expr, mk().ident_ty("usize"));
                    let index_expr = Nonterminal::NtExpr(expr);

                    // offset_of!(Struct, field[expr as usize]) as ty
                    let macro_body = vec![
                        TokenTree::token(token::Interpolated(Lrc::new(ty_ident)), DUMMY_SP),
                        TokenTree::token(token::Comma, DUMMY_SP),
                        TokenTree::token(token::Interpolated(Lrc::new(field_ident)), DUMMY_SP),
                        TokenTree::token(token::OpenDelim(DelimToken::Bracket), DUMMY_SP),
                        TokenTree::token(token::Interpolated(Lrc::new(index_expr)), DUMMY_SP),
                        TokenTree::token(token::CloseDelim(DelimToken::Bracket), DUMMY_SP),
                    ];
                    let path = mk().path("offset_of");
                    let mac = mk().mac_expr(mk().mac(path, macro_body, MacDelimiter::Parenthesis));

                    // Cast type
                    let cast_ty = self.convert_type(ty.ctype)?;
                    let cast_expr = mk().cast_expr(mac, cast_ty);

                    Ok(WithStmts::new_val(cast_expr))
                }
            },

            CExprKind::Literal(ty, ref kind) => self.convert_literal(ctx, ty, kind),

            CExprKind::ImplicitCast(ty, expr, kind, opt_field_id, _)
            | CExprKind::ExplicitCast(ty, expr, kind, opt_field_id, _) => {
                let is_explicit = if let CExprKind::ExplicitCast(..) = *expr_kind { true } else { false };
                // A reference must be decayed if a bitcast is required. Const casts in
                // LLVM 8 are now NoOp casts, so we need to include it as well.
                match kind {
                    CastKind::BitCast
                        | CastKind::PointerToIntegral
                        | CastKind::NoOp => ctx.decay_ref = DecayRef::Yes,
                    CastKind::FunctionToPointerDecay | CastKind::BuiltinFnToFnPtr => {
                        ctx.needs_address = true;
                    }
                    _ => {}
                }

                let source_ty = self.ast_context[expr]
                    .kind
                    .get_qual_type()
                    .ok_or_else(|| format_err!("bad source type"))?;

                let val = if is_explicit {
                    let stmts = self.compute_variable_array_sizes(ctx, ty.ctype)?;
                    let mut val = self.convert_expr(ctx, expr)?;
                    val.prepend_stmts(stmts);
                    val
                } else {
                    self.convert_expr(ctx, expr)?
                };
                // Shuffle Vector "function" builtins will add a cast to the output of the
                // builtin call which is unnecessary for translation purposes
                if self.casting_simd_builtin_call(expr, is_explicit, kind) {
                    return Ok(val);
                }
                self.convert_cast(ctx, source_ty, ty, val, Some(expr), Some(kind), opt_field_id)
            }

            CExprKind::Unary(type_id, op, arg, lrvalue) => {
                self.convert_unary_operator(ctx, op, type_id, arg, lrvalue)
            }

            CExprKind::Conditional(_, cond, lhs, rhs) => {
                if ctx.is_const {
                    return Err(format_translation_err!(
                        self.ast_context.display_loc(src_loc),
                        "Constants cannot contain ternary expressions in Rust",
                    ));
                }
                let cond = self.convert_condition(ctx, true, cond)?;

                let lhs = self.convert_expr(ctx, lhs)?;
                let rhs = self.convert_expr(ctx, rhs)?;

                if ctx.is_unused() {
                    let is_unsafe = lhs.is_unsafe() || rhs.is_unsafe();
                    let then: P<Block> = mk().block(lhs.into_stmts());
                    let els: P<Expr> = mk().block_expr(mk().block(rhs.into_stmts()));

                    let mut res = cond.and_then(|c| -> Result<_, TranslationError> {
                        Ok(WithStmts::new(
                            vec![mk().semi_stmt(mk().ifte_expr(c, then, Some(els)))],
                            self.panic_or_err("Conditional expression is not supposed to be used"),
                        ))
                    })?;
                    res.merge_unsafe(is_unsafe);
                    Ok(res)
                } else {
                    let then: P<Block> = lhs.to_block();
                    let els: P<Expr> = rhs.to_expr();

                    Ok(cond.map(|c| {
                        let ifte_expr = mk().ifte_expr(c, then, Some(els));

                        if ctx.ternary_needs_parens {
                            mk().paren_expr(ifte_expr)
                        } else {
                            ifte_expr
                        }
                    }))
                }
            }

            CExprKind::BinaryConditional(ty, lhs, rhs) => {
                if ctx.is_unused() {
                    let mut lhs = self.convert_condition(ctx, false, lhs)?;
                    let rhs = self.convert_expr(ctx, rhs)?;
                    lhs.merge_unsafe(rhs.is_unsafe());

                    lhs.and_then(|val| {
                        Ok(WithStmts::new(
                            vec![mk().semi_stmt(mk().ifte_expr(
                                val,
                                mk().block(rhs.into_stmts()),
                                None as Option<P<Expr>>,
                            ))],
                            self.panic_or_err(
                                "Binary conditional expression is not supposed to be used",
                            ),
                        ))
                    })
                } else {
                    self.name_reference_write_read(ctx, lhs)?
                        .result_map(|(_, lhs_val)| {
                            let cond = self.match_bool(true, ty.ctype, lhs_val.clone());
                            let ite = mk().ifte_expr(
                                cond,
                                mk().block(vec![mk().expr_stmt(lhs_val)]),
                                Some(self.convert_expr(ctx, rhs)?.to_expr()),
                            );
                            Ok(ite)
                        })
                }
            }

            CExprKind::Binary(type_id, op, lhs, rhs, opt_lhs_type_id, opt_res_type_id) => self
                .convert_binary_expr(ctx, type_id, op, lhs, rhs, opt_lhs_type_id, opt_res_type_id)
                .map_err(|e| e.add_loc(self.ast_context.display_loc(src_loc))),

            CExprKind::ArraySubscript(_, ref lhs, ref rhs, _) => {
                let lhs_node = &self.ast_context.index(*lhs).kind;
                let rhs_node = &self.ast_context.index(*rhs).kind;

                let lhs_node_type = lhs_node
                    .get_type()
                    .ok_or_else(|| format_err!("lhs node bad type"))?;
                let lhs_node_kind = &self.ast_context.resolve_type(lhs_node_type).kind;
                let lhs_is_indexable = lhs_node_kind.is_pointer() || lhs_node_kind.is_vector();

                // From here on in, the LHS is the pointer/array and the RHS the index
                let (lhs, rhs, lhs_node) = if lhs_is_indexable {
                    (lhs, rhs, lhs_node)
                } else {
                    (rhs, lhs, rhs_node)
                };

                let lhs_node_type = lhs_node
                    .get_type()
                    .ok_or_else(|| format_err!("lhs node bad type"))?;
                if self
                    .ast_context
                    .resolve_type(lhs_node_type)
                    .kind
                    .is_vector()
                {
                    return Err(TranslationError::new(
                        self.ast_context.display_loc(src_loc),
                        err_msg("Attempting to index a vector type")
                            .context(TranslationErrorKind::OldLLVMSimd),
                    ));
                }

                let rhs = self.convert_expr(ctx.used(), *rhs)?;
                rhs.and_then(|rhs| {
                    let simple_index_array = if ctx.needs_address() {
                        // We can't necessarily index into an array if we're using
                        // that element to compute an address.
                        None
                    } else {
                        match lhs_node {
                            &CExprKind::ImplicitCast(_, arr, CastKind::ArrayToPointerDecay, _, _) => {
                                match self.ast_context[arr].kind {
                                    CExprKind::Member(_, _, field_decl, _, _)
                                        if self.potential_flexible_array_members.borrow().contains(&field_decl) => None,
                                    ref kind => {
                                        let arr_type = kind.get_type()
                                            .ok_or_else(|| format_err!("bad arr type"))?;
                                        match self.ast_context.resolve_type(arr_type).kind {
                                            // These get translated to 0-element arrays, this avoids the bounds check
                                            // that using an array subscript in Rust would cause
                                            CTypeKind::IncompleteArray(_) => None,
                                            _ => Some(arr),
                                        }
                                    }
                                }
                            }
                            _ => None,
                        }
                    };

                    if let Some(arr) = simple_index_array {
                        // If the LHS just underwent an implicit cast from array to pointer, bypass that
                        // to make an actual Rust indexing operation

                        let t = self.ast_context[arr]
                            .kind
                            .get_type()
                            .ok_or_else(|| format_err!("bad arr type"))?;
                        let var_elt_type_id = match self.ast_context.resolve_type(t).kind {
                            CTypeKind::ConstantArray(..) => None,
                            CTypeKind::IncompleteArray(..) => None,
                            CTypeKind::VariableArray(elt, _) => Some(elt),
                            ref other => panic!("Unexpected array type {:?}", other),
                        };

                        let lhs = self.convert_expr(ctx.used(), arr)?;
                        Ok(lhs.map(|lhs| {
                            // stmts.extend(lhs.stmts_mut());
                            // is_unsafe = is_unsafe || lhs.is_unsafe();

                            // Don't dereference the offset if we're still within the variable portion
                            if let Some(elt_type_id) = var_elt_type_id {
                                match self.compute_size_of_expr(elt_type_id) {
                                    None => {
                                        mk().unary_expr(ast::UnOp::Deref, pointer_offset(lhs, rhs))
                                    }
                                    Some(sz) => pointer_offset(
                                        lhs,
                                        mk().binary_expr(BinOpKind::Mul, sz, cast_int(rhs, "usize")),
                                    ),
                                }
                            } else {
                                mk().index_expr(lhs, cast_int(rhs, "usize"))
                            }
                        }))
                    } else {
                        let lhs = self.convert_expr(ctx.used(), *lhs)?;
                        lhs.result_map(|lhs| {
                            // stmts.extend(lhs.stmts_mut());
                            // is_unsafe = is_unsafe || lhs.is_unsafe();

                            let lhs_type_id = lhs_node
                                .get_type()
                                .ok_or_else(|| format_err!("bad lhs type"))?;

                            // Determine the type of element being indexed
                            let pointee_type_id = match self.ast_context.resolve_type(lhs_type_id).kind {
                                CTypeKind::Pointer(pointee_id) => pointee_id,
                                _ => {
                                    return Err(format_err!(
                                        "Subscript applied to non-pointer: {:?}",
                                        lhs
                                    ).into())
                                }
                            };

                            if let Some(sz) = self.compute_size_of_expr(pointee_type_id.ctype) {
                                let offset =
                                    mk().binary_expr(BinOpKind::Mul, sz, cast_int(rhs, "usize"));
                                Ok(pointer_offset(lhs, offset))
                            } else {
                                // Otherwise, use the pointer and make a deref of a pointer offset expression
                                Ok(mk().unary_expr(ast::UnOp::Deref, pointer_offset(lhs, rhs)))
                            }
                        })
                    }
                })
            }

            CExprKind::Call(call_expr_ty, func, ref args) => {
                let fn_ty = self.ast_context.get_pointee_qual_type(
                    self.ast_context[func].kind.get_type()
                        .ok_or_else(|| format_err!("Invalid callee expression {:?}", func))?
                ).map(|ty| &self.ast_context.resolve_type(ty.ctype).kind);
                let is_variadic = match fn_ty {
                    Some(CTypeKind::Function(_, _, is_variadic, _, _)) => *is_variadic,
                    _ => false,
                };
                let func = match self.ast_context[func].kind {
                    // Direct function call
                    CExprKind::ImplicitCast(_, fexp, CastKind::FunctionToPointerDecay, _, _) => {
                        self.convert_expr(ctx.used(), fexp)?
                    }

                    // Builtin function call
                    CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) => {
                        return self.convert_builtin(ctx, fexp, args)
                    }

                    // Function pointer call
                    _ => {
                        let callee = self.convert_expr(ctx.used(), func)?;
                        let make_fn_ty = |ret_ty: P<Ty>| {
                            let ret_ty = match ret_ty.node {
                                TyKind::Tup(ref v) if v.is_empty() => FunctionRetTy::Default(DUMMY_SP),
                                _ => FunctionRetTy::Ty(ret_ty),
                            };
                            mk().barefn_ty(
                                mk().fn_decl(
                                    vec![
                                        mk().arg(
                                            mk().infer_ty(),
                                            mk().wild_pat(),
                                        );
                                        args.len()
                                    ],
                                    ret_ty,
                                    is_variadic,
                                )
                            )
                        };
                        match fn_ty {
                            Some(CTypeKind::Function(ret_ty, _, _, _, false)) => {
                                // K&R function pointer without arguments
                                if ctx.is_const { self.use_feature("const_transmute"); }
                                let ret_ty = self.convert_type(ret_ty.ctype)?;
                                let target_ty = make_fn_ty(ret_ty);
                                callee.map(|fn_ptr| {
                                    let fn_ptr = unwrap_function_pointer(fn_ptr);
                                    transmute_expr(mk().infer_ty(), target_ty, fn_ptr, self.tcfg.emit_no_std)
                                })
                            }
                            None => {
                                // We have to infer the return type from our expression type
                                if ctx.is_const { self.use_feature("const_transmute"); }
                                let ret_ty = self.convert_type(call_expr_ty.ctype)?;
                                let target_ty = make_fn_ty(ret_ty);
                                callee.map(|fn_ptr| {
                                    transmute_expr(mk().infer_ty(), target_ty, fn_ptr, self.tcfg.emit_no_std)
                                })
                            }
                            Some(_) => {
                                // Normal function pointer
                                callee.map(unwrap_function_pointer)
                            }
                        }
                    }
                };

                let call = func.and_then(|func| {
                    // We want to decay refs only when function is variadic
                    ctx.decay_ref = DecayRef::from(is_variadic);

                    let mut args = self.convert_exprs(ctx.used(), args)?;

                    // the C variadics feature requires us to call `as_va_list` on a `VaListImpl`
                    // when calling a function expecting to receive an object that is binary
                    // compatible with C's `va_list`.
                    if let Some(CTypeKind::Function(_, params, _, _, _)) = fn_ty {
                        // look for parameters of type `va_list`
                        let positions: Vec<usize> = params
                            .iter()
                            .enumerate()
                            .filter_map(|(pos, param)| {
                                if self.ast_context.is_pointer_to_va_list(param.ctype)
                                { Some(pos) } else { None }
                            })
                            .collect();

                        // ... call `as_va_list` for such parameters
                        if !positions.is_empty() {
                            args = args.map(|mut val| {
                                for pos in positions {
                                    val[pos] = mk().method_call_expr(
                                        val[pos].clone(),
                                        "as_va_list",
                                        Vec::<P<Expr>>::new());
                                }
                                val
                            });
                        }
                    }

                    let res: Result<_, TranslationError> = Ok(
                        args.map(|args| mk().call_expr(func, args))
                    );
                    res
                })?;

                self.convert_side_effects_expr(
                    ctx,
                    call,
                    "Function call expression is not supposed to be used",
                )
            }

            CExprKind::Member(_, expr, decl, kind, _) => {
                let is_bitfield = match &self.ast_context[decl].kind {
                    CDeclKind::Field { bitfield_width, .. } => bitfield_width.is_some(),
                    _ => unreachable!("Found a member which is not a field"),
                };

                if is_bitfield {
                    let field_name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(None, decl)
                        .unwrap();

                    self.convert_bitfield_member_expr(ctx, field_name, expr, kind)
                } else if ctx.is_unused() {
                    self.convert_expr(ctx, expr)
                } else {
                    let field_name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(None, decl)
                        .unwrap();
                    match kind {
                        MemberKind::Dot => {
                            let val = self.convert_expr(ctx, expr)?;
                            Ok(val.map(|v| mk().field_expr(v, field_name)))
                        }
                        MemberKind::Arrow => {
                            if let CExprKind::Unary(_, c_ast::UnOp::AddressOf, subexpr_id, _) =
                                self.ast_context[expr].kind
                            {
                                let val = self.convert_expr(ctx, subexpr_id)?;
                                Ok(val.map(|v| mk().field_expr(v, field_name)))
                            } else {
                                let val = self.convert_expr(ctx, expr)?;
                                Ok(val.map(|v| {
                                    mk().field_expr(
                                        mk().unary_expr(ast::UnOp::Deref, v),
                                        field_name,
                                    )
                                }))
                            }
                        }
                    }
                }
            }

            CExprKind::Paren(_, val) => self.convert_expr(ctx, val),

            CExprKind::CompoundLiteral(_, val) => self.convert_expr(ctx, val),

            CExprKind::InitList(ty, ref ids, opt_union_field_id, _) => {
                self.convert_init_list(ctx, ty, ids, opt_union_field_id)
            }

            CExprKind::ImplicitValueInit(ty) => {
                self.implicit_default_expr(ty.ctype, ctx.is_static)
            }

            CExprKind::Predefined(_, val_id) => self.convert_expr(ctx, val_id),

            CExprKind::Statements(_, compound_stmt_id) => {
                self.convert_statement_expression(ctx, compound_stmt_id)
            }

            CExprKind::VAArg(ty, val_id) => self.convert_vaarg(ctx, ty, val_id),

            CExprKind::Choose(_, _cond, lhs, rhs, is_cond_true) => {
                let chosen_expr = if is_cond_true {
                    self.convert_expr(ctx, lhs)?
                } else {
                    self.convert_expr(ctx, rhs)?
                };

                // TODO: Support compile-time choice between lhs and rhs based on cond.

                // From Clang Expr.h
                // ChooseExpr - GNU builtin-in function __builtin_choose_expr.
                // This AST node is similar to the conditional operator (?:) in C, with
                // the following exceptions:
                // - the test expression must be a integer constant expression.
                // - the expression returned acts like the chosen subexpression in every
                //   visible way: the type is the same as that of the chosen subexpression,
                //   and all predicates (whether it's an l-value, whether it's an integer
                //   constant expression, etc.) return the same result as for the chosen
                //   sub-expression.

                Ok(chosen_expr)
            }

        }
    }

    fn convert_macro_expansion(&self, ctx: ExprContext, expr_id: CExprId)
                               -> Result<Option<WithStmts<P<Expr>>>, TranslationError> {
        if let Some(macs) = self.ast_context.macro_expansions.get(&expr_id) {
            // Find the first macro after the macro we're currently
            // expanding, if any.
            if let Some(macro_id) = macs
                .splitn(2, |macro_id| ctx.expanding_macro(macro_id))
                .last()
                .unwrap()
                .first()
            {
                trace!("  found macro expansion: {:?}", macro_id);
                // Ensure that we've converted this macro and that it has a
                // valid definition
                if let ConvertedDecl::NoItem = self.convert_decl(ctx, *macro_id)? {
                    return Ok(None);
                }
                let macro_ty = self.macro_types.borrow()[macro_id];
                let rustname = self
                    .renamer
                    .borrow_mut()
                    .get(macro_id)
                    .ok_or_else(|| format_err!("Macro name not declared"))?;

                if let Some(cur_file) = self.cur_file.borrow().as_ref() {
                    self.add_import(*cur_file, *macro_id, &rustname);
                }

                let val = WithStmts::new_val(mk().path_expr(vec![rustname]));

                let expr_kind = &self.ast_context[expr_id].kind;
                if let Some(expr_ty) = expr_kind.get_qual_type() {
                    return self.convert_cast(ctx, CQualTypeId::new(macro_ty), expr_ty, val, None, None, None)
                        .map(Some);
                } else {
                    return Ok(Some(val));
                }

                // TODO: May need to handle volatile reads here, see
                // DeclRef below
            }
        }

        Ok(None)
    }

    fn convert_side_effects_expr(
        &self,
        ctx: ExprContext,
        expr: WithStmts<P<Expr>>,
        panic_msg: &str,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        if ctx.is_unused() {
            // Recall that if `used` is false, the `stmts` field of the output must contain
            // all side-effects (and a function call can always have side-effects)
            expr.and_then(|expr| {
                Ok(WithStmts::new(
                    vec![mk().semi_stmt(expr)],
                    self.panic_or_err(panic_msg),
                ))
            })
        } else {
            Ok(expr)
        }
    }

    fn convert_statement_expression(
        &self,
        ctx: ExprContext,
        compound_stmt_id: CStmtId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        fn as_semi_break_stmt(stmt: &ast::Stmt, lbl: &cfg::Label) -> Option<Option<P<ast::Expr>>> {
            if let ast::Stmt {
                node: ast::StmtKind::Semi(ref expr),
                ..
            } = *stmt
            {
                if let ast::Expr {
                    node: ast::ExprKind::Break(Some(ref blbl), ref ret_val),
                    ..
                } = **expr
                {
                    if blbl.ident == mk().label(lbl.pretty_print()).ident {
                        return Some(ret_val.clone());
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
                        self.convert_function_body(ctx, &name, &substmt_ids[0..(n - 1)], ret)?
                    }

                    _ => self.convert_function_body(
                        ctx,
                        &name,
                        &substmt_ids,
                        cfg::ImplicitReturnType::Void,
                    )?,
                };

                if let Some(stmt) = stmts.pop() {
                    match as_semi_break_stmt(&stmt, &lbl) {
                        Some(val) => {
                            let block = mk().block_expr({
                                match val {
                                    None => mk().block(stmts),
                                    Some(val) => WithStmts::new(stmts, val).to_block(),
                                }
                            });
                            // enclose block in parentheses to work around
                            // https://github.com/rust-lang/rust/issues/54482
                            return Ok(WithStmts::new_val(mk().paren_expr(block)));
                        }
                        _ => {
                            self.use_feature("label_break_value");
                            stmts.push(stmt)
                        }
                    }
                }

                let block_body = mk().block(stmts.clone());
                let val: P<Expr> = mk().labelled_block_expr(block_body, lbl.pretty_print());

                Ok(WithStmts::new(stmts, val))
            }
            _ => {
                if ctx.is_unused() {
                    let val =
                        self.panic_or_err("Empty statement expression is not supposed to be used");
                    Ok(WithStmts::new_val(val))
                } else {
                    Err(TranslationError::generic("Bad statement expression"))
                }
            }
        }
    }

    fn convert_cast(
        &self,
        ctx: ExprContext,
        source_ty: CQualTypeId,
        ty: CQualTypeId,
        val: WithStmts<P<Expr>>,
        expr: Option<CExprId>,
        kind: Option<CastKind>,
        opt_field_id: Option<CFieldId>,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let source_ty_kind = &self.ast_context.resolve_type(source_ty.ctype).kind;
        let target_ty_kind = &self.ast_context.resolve_type(ty.ctype).kind;

        if source_ty_kind == target_ty_kind {
            return Ok(val);
        }

        let kind = kind.unwrap_or_else(|| {
            match (source_ty_kind, target_ty_kind) {
                (CTypeKind::VariableArray(..), CTypeKind::Pointer(..))
                | (CTypeKind::ConstantArray(..), CTypeKind::Pointer(..))
                | (CTypeKind::IncompleteArray(..), CTypeKind::Pointer(..))
                    => CastKind::ArrayToPointerDecay,

                (CTypeKind::Function(..), CTypeKind::Pointer(..))
                    => CastKind::FunctionToPointerDecay,

                (_, CTypeKind::Pointer(..)) if source_ty_kind.is_integral_type()
                    => CastKind::IntegralToPointer,

                (CTypeKind::Pointer(..), CTypeKind::Bool)
                    => CastKind::PointerToBoolean,

                (CTypeKind::Pointer(..), _) if target_ty_kind.is_integral_type()
                    => CastKind::PointerToIntegral,

                (_, CTypeKind::Bool) if source_ty_kind.is_integral_type()
                    => CastKind::IntegralToBoolean,

                (CTypeKind::Bool, _) if target_ty_kind.is_signed_integral_type()
                    => CastKind::BooleanToSignedIntegral,

                (_, _) if source_ty_kind.is_integral_type() && target_ty_kind.is_integral_type()
                    => CastKind::IntegralCast,

                (_, _) if source_ty_kind.is_integral_type() && target_ty_kind.is_floating_type()
                    => CastKind::IntegralToFloating,

                (_, CTypeKind::Bool) if source_ty_kind.is_floating_type()
                    => CastKind::FloatingToBoolean,

                (_, _) if source_ty_kind.is_floating_type() && target_ty_kind.is_integral_type()
                    => CastKind::FloatingToIntegral,

                (_, _) if source_ty_kind.is_floating_type() && target_ty_kind.is_floating_type()
                    => CastKind::FloatingCast,

                (CTypeKind::Pointer(..), CTypeKind::Pointer(..))
                    => CastKind::BitCast,

                // Ignoring Complex casts for now

                _ => {
                    warn!(
                        "Unknown CastKind for {:?} to {:?} cast. Defaulting to BitCast",
                        source_ty_kind,
                        target_ty_kind,
                    );

                    CastKind::BitCast
                }
            }
        });

        match kind {
            CastKind::BitCast | CastKind::NoOp => {
                val.and_then(|x| {
                    if self.ast_context.is_function_pointer(ty.ctype)
                        || self.ast_context.is_function_pointer(source_ty.ctype)
                    {
                        if ctx.is_static || ctx.is_const {
                            self.use_feature("const_transmute");
                        }
                        let source_ty = self.convert_type(source_ty.ctype)?;
                        let target_ty = self.convert_type(ty.ctype)?;
                        Ok(WithStmts::new_unsafe_val(transmute_expr(
                            source_ty,
                            target_ty,
                            x,
                            self.tcfg.emit_no_std,
                        )))
                    } else {
                        // Normal case
                        let target_ty = self.convert_type(ty.ctype)?;
                        Ok(WithStmts::new_val(mk().cast_expr(x, target_ty)))
                    }
                })
            }

            CastKind::IntegralToPointer if self.ast_context.is_function_pointer(ty.ctype) => {
                if ctx.is_static || ctx.is_const {
                    self.use_feature("const_transmute");
                }
                let target_ty = self.convert_type(ty.ctype)?;
                val.and_then(|x| {
                    let intptr_t = mk().path_ty(vec!["libc", "intptr_t"]);
                    let intptr = mk().cast_expr(x, intptr_t.clone());
                    Ok(WithStmts::new_unsafe_val(
                        transmute_expr(intptr_t, target_ty, intptr, self.tcfg.emit_no_std)
                    ))
                })
            }

            CastKind::IntegralToPointer
            | CastKind::PointerToIntegral
            | CastKind::IntegralCast
            | CastKind::FloatingCast
            | CastKind::FloatingToIntegral
            | CastKind::IntegralToFloating => {
                let target_ty = self.convert_type(ty.ctype)?;
                let target_ty_ctype = &self.ast_context.resolve_type(ty.ctype).kind;

                let source_ty_ctype_id = source_ty.ctype;

                let source_ty = self.convert_type(source_ty_ctype_id)?;
                if let CTypeKind::LongDouble = target_ty_ctype {
                    self.extern_crates.borrow_mut().insert("f128");

                    let fn_path = mk().path_expr(vec!["f128", "f128", "new"]);
                    Ok(val.map(|val| mk().call_expr(fn_path, vec![val])))
                } else if let CTypeKind::LongDouble = self.ast_context[source_ty_ctype_id].kind {
                    self.extern_crates.borrow_mut().insert("num_traits");
                    self.items.borrow_mut()[&self.main_file].add_use(vec!["num_traits".into()], "ToPrimitive");

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
                        CTypeKind::LongLong => "to_i64",
                        CTypeKind::ULongLong => "to_u64",
                        CTypeKind::Int128 => "to_i128",
                        CTypeKind::UInt128 => "to_u128",
                        _ => {
                            return Err(format_err!(
                                "Tried casting long double to unsupported type: {:?}",
                                target_ty_ctype
                            )
                            .into())
                        }
                    };

                    Ok(val.map(|val| {
                        let to_call =
                            mk().method_call_expr(val, to_method_name, Vec::<P<Expr>>::new());

                        mk().method_call_expr(
                            to_call,
                            "unwrap",
                            Vec::<P<Expr>>::new(),
                        )
                    }))
                } else if let &CTypeKind::Enum(enum_decl_id) = target_ty_ctype {
                    // Casts targeting `enum` types...
                    let expr = expr.ok_or_else(|| format_err!("Casts to enums require a C ExprId"))?;
                    Ok(self.enum_cast(ty.ctype, enum_decl_id, expr, val, source_ty, target_ty))
                } else {
                    // Other numeric casts translate to Rust `as` casts,
                    // unless the cast is to a function pointer then use `transmute`.
                    val.and_then(|x| {
                        if self.ast_context.is_function_pointer(source_ty_ctype_id) {
                            if ctx.is_static || ctx.is_const {
                                self.use_feature("const_transmute");
                            }
                            Ok(WithStmts::new_unsafe_val(transmute_expr(source_ty, target_ty, x, self.tcfg.emit_no_std)))
                        } else {
                            Ok(WithStmts::new_val(mk().cast_expr(x, target_ty)))
                        }
                    })
                }
            }

            CastKind::LValueToRValue | CastKind::ToVoid | CastKind::ConstCast => Ok(val),

            CastKind::FunctionToPointerDecay | CastKind::BuiltinFnToFnPtr => {
                Ok(val.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x])))
            }

            CastKind::ArrayToPointerDecay => {
                let pointee = match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::Pointer(pointee) => pointee,
                    _ => panic!("Dereferencing a non-pointer"),
                };

                // Because va_list is defined as a single-element array in order for it to allocate
                // memory as a local variable and to be a pointer as a function argument we would
                // get spurious casts when trying to treat it like a VaList which has reference
                // semantics.
                if self.ast_context.is_va_list(pointee.ctype) {
                    return Ok(val)
                }

                let is_const = pointee.qualifiers.is_const;

                let expr_kind = expr.map(|e| &self.ast_context.index(e).kind);
                match expr_kind {
                    Some(&CExprKind::Literal(_, CLiteral::String(ref bytes, 1))) if is_const => {
                        let target_ty = self.convert_type(ty.ctype)?;

                        let mut bytes = bytes.to_owned();
                        bytes.push(0);
                        let byte_literal = mk().lit_expr(mk().bytestr_lit(bytes));
                        let val =
                            mk().cast_expr(byte_literal, mk().ptr_ty(mk().path_ty(vec!["u8"])));
                        let val = mk().cast_expr(val, target_ty);
                        Ok(WithStmts::new_val(val))
                    }
                    _ => {
                        // Variable length arrays are already represented as pointers.
                        if let CTypeKind::VariableArray(..) =
                            self.ast_context.resolve_type(source_ty.ctype).kind
                        {
                            Ok(val)
                        } else {
                            let method = if is_const || ctx.is_static {
                                "as_ptr"
                            } else {
                                "as_mut_ptr"
                            };

                            let call = val
                                .map(|x| mk().method_call_expr(x, method, vec![] as Vec<P<Expr>>));

                            // Static arrays can now use as_ptr. Can also cast that const ptr to a
                            // mutable pointer as we do here:
                            if ctx.is_static {
                                if !is_const {
                                    return Ok(call.map(|val| {
                                        let inferred_type = mk().infer_ty();
                                        let ptr_type = mk().mutbl().ptr_ty(inferred_type);
                                        mk().cast_expr(val, ptr_type)
                                    }))
                                }
                            }

                            Ok(call)
                        }
                    }
                }
            }

            CastKind::NullToPointer => {
                assert!(val.stmts().is_empty());
                Ok(WithStmts::new_val(self.null_ptr(ty.ctype, ctx.is_static)?))
            }

            CastKind::ToUnion => {
                let field_id = opt_field_id.expect("Missing field ID in union cast");
                let union_id = self.ast_context.parents[&field_id];

                let union_name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(union_id)
                    .expect("required union name");
                let field_name = self
                    .type_converter
                    .borrow()
                    .resolve_field_name(Some(union_id), field_id)
                    .expect("field name required");

                Ok(val.map(|x| {
                    mk().struct_expr(mk().path(vec![union_name]), vec![mk().field(field_name, x)])
                }))
            }

            CastKind::IntegralToBoolean
            | CastKind::FloatingToBoolean
            | CastKind::PointerToBoolean => {
                if let Some(expr) = expr {
                    self.convert_condition(ctx, true, expr)
                } else {
                    Ok(val.map(|e| self.match_bool(true, source_ty.ctype, e)))
                }
            }

            // I don't know how to actually cause clang to generate this
            CastKind::BooleanToSignedIntegral => Err(TranslationError::generic(
                "TODO boolean to signed integral not supported",
            )),

            CastKind::FloatingRealToComplex
            | CastKind::FloatingComplexToIntegralComplex
            | CastKind::FloatingComplexCast
            | CastKind::FloatingComplexToReal
            | CastKind::IntegralComplexToReal
            | CastKind::IntegralRealToComplex
            | CastKind::IntegralComplexCast
            | CastKind::IntegralComplexToFloatingComplex
            | CastKind::IntegralComplexToBoolean => Err(TranslationError::generic(
                "TODO casts with complex numbers not supported",
            )),

            CastKind::VectorSplat => Err(TranslationError::generic(
                "TODO vector splat casts not supported",
            )),
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
        enum_decl: CEnumId, // ID of the enum declaration corresponding to the target type
        expr: CExprId,      // ID of initial C argument to cast
        val: WithStmts<P<Expr>>, // translated Rust argument to cast
        _source_ty: P<Ty>,  // source type of cast
        target_ty: P<Ty>,   // target type of cast
    ) -> WithStmts<P<Expr>> {
        // Extract the IDs of the `EnumConstant` decls underlying the enum.
        let variants = match self.ast_context.index(enum_decl).kind {
            CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration"),
        };

        match self.ast_context.index(expr).kind {
            // This is the case of finding a variable which is an `EnumConstant` of the same enum
            // we are casting to. Here, we can just remove the extraneous cast instead of generating
            // a new one.
            CExprKind::DeclRef(_, decl_id, _) if variants.contains(&decl_id) => {
                return val.map(|x| match x.node {
                    ast::ExprKind::Cast(ref e, _) => e.clone(),
                    _ => panic!(format!(
                        "DeclRef {:?} of enum {:?} is not cast",
                        expr, enum_decl
                    )),
                })
            }

            CExprKind::Literal(_, CLiteral::Integer(i, _)) => {
                return val.map(|_| self.enum_for_i64(enum_type, i as i64));
            }

            CExprKind::Unary(_, c_ast::UnOp::Negate, subexpr_id, _) => {
                if let &CExprKind::Literal(_, CLiteral::Integer(i, _)) =
                    &self.ast_context[subexpr_id].kind
                {
                    return val.map(|_| self.enum_for_i64(enum_type, -(i as i64)));
                }
            }

            // In all other cases, a cast to an enum requires a `transmute` - Rust enums cannot be
            // converted into integral types as easily as C ones.
            _ => {}
        }

        val.map(|x| mk().cast_expr(x, target_ty))
    }

    pub fn implicit_default_expr(
        &self,
        ty_id: CTypeId,
        is_static: bool,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let resolved_ty_id = self.ast_context.resolve_type_id(ty_id);
        let resolved_ty = &self.ast_context.index(resolved_ty_id).kind;

        if resolved_ty.is_bool() {
            Ok(WithStmts::new_val(mk().lit_expr(mk().bool_lit(false))))
        } else if resolved_ty.is_integral_type() {
            Ok(WithStmts::new_val(mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed))))
        } else if resolved_ty.is_floating_type() {
            match self.ast_context[ty_id].kind {
                CTypeKind::LongDouble => Ok(WithStmts::new_val(mk().path_expr(vec!["f128", "f128", "ZERO"]))),
                _ => Ok(WithStmts::new_val(mk().lit_expr(mk().float_unsuffixed_lit("0.")))),
            }
        } else if let &CTypeKind::Pointer(_) = resolved_ty {
            self.null_ptr(resolved_ty_id, is_static)
                .map(WithStmts::new_val)
        } else if let &CTypeKind::ConstantArray(elt, sz) = resolved_ty {
            let sz = mk().lit_expr(mk().int_lit(sz as u128, LitIntType::Unsuffixed));
            Ok(self.implicit_default_expr(elt, is_static)?
                .map(|elt| mk().repeat_expr(elt, sz)))
        } else if let &CTypeKind::IncompleteArray(_) = resolved_ty {
            // Incomplete arrays are translated to zero length arrays
            Ok(WithStmts::new_val(mk().array_expr(vec![] as Vec<P<Expr>>)))
        } else if let Some(decl_id) = resolved_ty.as_underlying_decl() {
            self.zero_initializer(decl_id, ty_id, is_static)
        } else if let &CTypeKind::VariableArray(elt, _) = resolved_ty {
            // Variable length arrays unnested and implemented as a flat array of the underlying
            // element type.

            // Find base element type of potentially nested arrays
            let inner = self.variable_array_base_type(elt);
            let count = self.compute_size_of_expr(ty_id).unwrap();
            Ok(self.implicit_default_expr(inner, is_static)?
               .map(|val| vec_expr(val, count)))
        } else if let &CTypeKind::Vector(CQualTypeId { ctype, .. }, len) = resolved_ty {
            self.implicit_vector_default(ctype, len, is_static)
        } else {
            Err(format_err!("Unsupported default initializer: {:?}", resolved_ty).into())
        }
    }

    /// Produce zero-initializers for structs/unions/enums, looking them up when possible.
    fn zero_initializer(
        &self,
        decl_id: CDeclId,
        type_id: CTypeId,
        is_static: bool,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        // Look up the decl in the cache and return what we find (if we find anything)
        if let Some(init) = self.zero_inits.borrow().get(&decl_id) {
            return Ok(init.clone());
        }

        // Otherwise, construct the initializer
        let init = match self.ast_context.index(decl_id).kind {
            // Zero initialize all of the fields
            CDeclKind::Struct {
                ref fields,
                platform_byte_size,
                ..
            } => {
                let name_decl_id = match self.ast_context.index(type_id).kind {
                    CTypeKind::Typedef(decl_id) => decl_id,
                    _ => decl_id,
                };

                let name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(name_decl_id)
                    .unwrap();

                let fields = match *fields {
                    Some(ref fields) => fields,
                    None => {
                        return Err(TranslationError::generic(
                            "Attempted to zero-initialize forward-declared struct",
                        ))
                    }
                };

                let has_bitfields = fields
                    .iter()
                    .map(|field_id| match self.ast_context.index(*field_id).kind {
                        CDeclKind::Field { bitfield_width, .. } => bitfield_width.is_some(),
                        _ => unreachable!("Found non-field in record field list"),
                    })
                    .any(|x| x);

                if has_bitfields {
                    self.bitfield_zero_initializer(name, fields, platform_byte_size, is_static)?
                } else {
                    let fields: WithStmts<Vec<Field>> = fields
                        .into_iter()
                        .map(|field_id| {
                            let name = self
                                .type_converter
                                .borrow()
                                .resolve_field_name(Some(decl_id), *field_id)
                                .unwrap();

                            match self.ast_context.index(*field_id).kind {
                                CDeclKind::Field { typ, .. } => {
                                    Ok(self.implicit_default_expr(typ.ctype, is_static)?
                                       .map(|field_init| mk().field(name, field_init)))
                                }
                                _ => Err(TranslationError::generic(
                                    "Found non-field in record field list",
                                )),
                            }
                        })
                        .collect::<Result<_, TranslationError>>()?;

                    fields.map(|fields| mk().struct_expr(vec![name], fields))
                }
            }

            // Zero initialize the first field
            CDeclKind::Union { ref fields, .. } => {
                let name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .unwrap();

                let fields = match *fields {
                    Some(ref fields) => fields,
                    None => {
                        return Err(TranslationError::generic(
                            "Attempted to zero-initialize forward-declared struct",
                        ))
                    }
                };

                let &field_id = fields
                    .first()
                    .ok_or(format_err!("A union should have a field"))?;

                let field = match self.ast_context.index(field_id).kind {
                    CDeclKind::Field { typ, .. } => {
                        self.implicit_default_expr(typ.ctype, is_static)?
                            .map(|field_init| {
                                let name = self
                                    .type_converter
                                    .borrow()
                                    .resolve_field_name(Some(decl_id), field_id)
                                    .unwrap();

                                mk().field(name, field_init)
                            })
                    }
                    _ => return Err(TranslationError::generic(
                        "Found non-field in record field list"
                    )),
                };

                field.map(|field| mk().struct_expr(vec![name], vec![field]))
            }

            // Transmute the number `0` into the enum type
            CDeclKind::Enum { .. } => WithStmts::new_val(self.enum_for_i64(type_id, 0)),

            _ => return Err(TranslationError::generic(
                "Declaration is not associated with a type",
            )),
        };

        if init.is_pure() {
            // Insert the initializer into the cache, then return it
            self.zero_inits.borrow_mut().insert(decl_id, init.clone());
            Ok(init)
        } else {
            Err(TranslationError::generic("Expected no statements in zero initializer"))
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
                        BinOpKind::Or
                        | BinOpKind::And
                        | BinOpKind::Eq
                        | BinOpKind::Ne
                        | BinOpKind::Lt
                        | BinOpKind::Le
                        | BinOpKind::Gt
                        | BinOpKind::Ge => {
                            if target {
                                // If target == true, just return the argument
                                return arg.clone();
                            } else {
                                // If target == false, return !arg
                                return mk().unary_expr(ast::UnOp::Not, arg.clone());
                            }
                        }
                        _ => {}
                    }
                }
            }

            let val = if ty.is_enum() {
                mk().cast_expr(val, mk().path_ty(vec!["u64"]))
            } else {
                val
            };

            // The backup is to just compare against zero
            if target {
                mk().binary_expr(BinOpKind::Ne, zero, val)
            } else {
                mk().binary_expr(BinOpKind::Eq, zero, val)
            }
        }
    }

    pub fn with_scope<F, A>(&self, f: F) -> A
    where
        F: FnOnce() -> A,
    {
        self.renamer.borrow_mut().add_scope();
        let result = f();
        self.renamer.borrow_mut().drop_scope();
        result
    }

    /// If we're trying to organize item definitions into submodules, add them to a module
    /// scoped "namespace" if we have a path available, otherwise add it to the global "namespace"
    fn insert_item(&self, mut item: P<Item>, decl: &CDecl) {
        let decl_file_id = self.ast_context.file_id(decl);

        if self.tcfg.reorganize_definitions {
            add_src_loc_attr(&mut item.attrs, &decl.loc);
            let mut item_stores = self.items.borrow_mut();
            let items = item_stores
                .entry(decl_file_id.unwrap())
                .or_insert(ItemStore::new());

            items.add_item(item);
        } else {
            self.items.borrow_mut()[&self.main_file].add_item(item)
        }
    }

    /// If we're trying to organize foreign item definitions into submodules, add them to a module
    /// scoped "namespace" if we have a path available, otherwise add it to the global "namespace"
    fn insert_foreign_item(&self, mut item: ForeignItem, decl: &CDecl) {
        let decl_file_id = self.ast_context.file_id(decl);

        if self.tcfg.reorganize_definitions {
            add_src_loc_attr(&mut item.attrs, &decl.loc);
            let mut items = self.items.borrow_mut();
            let mod_block_items = items
                .entry(decl_file_id.unwrap())
                .or_insert(ItemStore::new());

            mod_block_items.add_foreign_item(item);
        } else {
            self.items.borrow_mut()[&self.main_file].add_foreign_item(item)
        }
    }

    fn add_import(&self, decl_file_id: FileId, decl_id: CDeclId, ident_name: &str) {
        let decl = &self.ast_context[decl_id];
        let import_file_id = self.ast_context.file_id(decl);

        // If the definition lives in the same header, there is no need to import it
        // in fact, this would be a hard rust error.
        // We should never import into the main module here, as that happens in make_submodule
        if import_file_id.map_or(false, |path| path == decl_file_id)
            || decl_file_id == self.main_file {
            return;
        }

        let mut module_path = vec!["super".into()];

        // If the decl does not live in the main module add the path to the sibling submodule
        if let Some(file_id) = import_file_id {
            if file_id != self.main_file {
                let file_name = clean_path(
                    &self.mod_names,
                    self.ast_context.get_file_path(file_id)
                );

                module_path.push(file_name);
            }
        }

        self.items
            .borrow_mut()
            .entry(decl_file_id)
            .or_insert(ItemStore::new())
            .add_use(module_path, ident_name);
    }

    fn add_lib_import(&self, decl_file_id: FileId, ident_name: &str, re_export: bool) {
        let attrs = if re_export {
            mk().pub_()
        } else {
            mk()
        };

        if decl_file_id == self.main_file {
            return;
        }

        let module_path = vec!["super".into()];

        let mut module_items = self.items.borrow_mut();
        module_items
            .entry(decl_file_id)
            .or_insert(ItemStore::new())
            .add_use_with_attr(module_path, ident_name, attrs);
    }

    fn import_type(
        &self,
        ctype: CTypeId,
        decl_file_id: FileId,
    ) {
        use self::CTypeKind::*;

        match self.ast_context[ctype].kind {
            Void | Char | SChar | UChar | Short | UShort | Int | UInt | Long | ULong | LongLong
            | ULongLong | Int128 | UInt128 | Half | Float | Double => {
                self.add_lib_import(decl_file_id, "libc", false);
            }
            LongDouble => {
                self.add_lib_import(decl_file_id, "f128", false);
            }
            // Bool uses the bool type, so no dependency on libc
            Bool => {}
            Paren(ctype)
            | Decayed(ctype)
            | IncompleteArray(ctype)
            | ConstantArray(ctype, _)
            | Elaborated(ctype)
            | Pointer(CQualTypeId { ctype, .. }) => {
                self.import_type(ctype, decl_file_id)
            }
            Enum(decl_id) | Typedef(decl_id) | Union(decl_id) | Struct(decl_id) => {
                let mut decl_id = decl_id.clone();
                // if the `decl` has been "squashed", get the corresponding `decl_id`
                if self.ast_context.prenamed_decls.contains_key(&decl_id) {
                    decl_id = *self.ast_context.prenamed_decls.get(&decl_id).unwrap();
                }

                let ident_name = &self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .expect("Expected decl name");
                self.add_import(decl_file_id, decl_id, ident_name);
            }
            Function(CQualTypeId { ctype, .. }, ref params, ..) => {
                // Return Type
                let type_kind = &self.ast_context[ctype].kind;

                // Rust doesn't use void for return type, so skip
                if *type_kind != Void {
                    self.import_type(ctype, decl_file_id);
                }

                // Param Types
                for param_id in params {
                    self.import_type(param_id.ctype, decl_file_id);
                }
            }
            Vector(CQualTypeId { ctype, .. }, len) => {
                // Since vector imports are global, we can find the correct type name in the parent scope
                let type_name = match (&self.ast_context[ctype].kind, len) {
                    (CTypeKind::Float, 4) => "__m128",
                    (CTypeKind::Float, 8) => "__m256",
                    (CTypeKind::Double, 2) => "__m128d",
                    (CTypeKind::Double, 4) => "__m256d",
                    (CTypeKind::LongLong, 4) => "__m256i",
                    (CTypeKind::LongLong, 2)
                    | (CTypeKind::Char, 16)
                    | (CTypeKind::Int, 4)
                    | (CTypeKind::Short, 8) => "__m128i",
                    (CTypeKind::LongLong, 1) | (CTypeKind::Int, 2) => "__m64",
                    (kind, len) => unimplemented!("Unknown vector type: {:?} x {}", kind, len),
                };

                self.add_lib_import(decl_file_id, type_name, true);
            }
            ref e => unimplemented!("{:?}", e),
        }
    }

    fn generate_submodule_imports(
        &self,
        decl_id: CDeclId,
        decl_file_id: Option<FileId>,
    ) {
        let decl_file_id = decl_file_id.expect("There should be a decl file path");
        let decl = self.ast_context.get_decl(&decl_id).unwrap();

        match decl.kind {
            CDeclKind::Struct { ref fields, .. } | CDeclKind::Union { ref fields, .. } => {
                let field_ids = fields.as_ref().map(|vec| vec.as_slice()).unwrap_or(&[]);

                for field_id in field_ids.iter() {
                    match self.ast_context[*field_id].kind {
                        CDeclKind::Field { typ, .. } => {
                            self.import_type(typ.ctype, decl_file_id)
                        }
                        _ => unreachable!("Found something in a struct other than a field"),
                    }
                }
            }
            CDeclKind::EnumConstant { .. } => {}
            // REVIEW: Enums can only be integer types? So libc is likely always required?
            CDeclKind::Enum { .. } => {
                self.add_lib_import(decl_file_id, "libc", false);
            }

            CDeclKind::Variable {
                has_static_duration: true,
                is_externally_visible: true,
                typ,
                ..
            }
            | CDeclKind::Variable {
                has_thread_duration: true,
                is_externally_visible: true,
                typ,
                ..
            }
            | CDeclKind::Typedef { typ, .. } => {
                self.import_type(typ.ctype, decl_file_id)
            }
            CDeclKind::Function {
                is_global: true,
                typ,
                ..
            } => self.import_type(typ, decl_file_id),

            CDeclKind::MacroObject { .. } => {
                if let Some(macro_ty) = self.macro_types.borrow().get(&decl_id) {
                    self.import_type(*macro_ty, decl_file_id)
                }
            }

            CDeclKind::Function { .. } => {
                // TODO: We may need to explicitly skip SIMD functions here when getting types for
                // a fn definition in a header since SIMD headers define functions but we're using imports
                // rather than translating the original definition
            }
            CDeclKind::Variable {
                has_static_duration,
                has_thread_duration,
                is_externally_visible: false,
                ..
            } if has_static_duration || has_thread_duration => {}
            ref e => unimplemented!("{:?}", e),
        }
    }
}
