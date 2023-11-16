use std::cell::RefCell;
use std::char;
use std::collections::HashMap;
use std::mem;
use std::ops::Index;
use std::path::{self, PathBuf};
use std::result::Result; // To override syn::Result from glob import

use dtoa;

use failure::{err_msg, format_err, Fail};
use indexmap::indexmap;
use indexmap::{IndexMap, IndexSet};
use log::{error, info, trace, warn};
use proc_macro2::{Punct, Spacing::*, Span, TokenStream, TokenTree};
use syn::spanned::Spanned as _;
use syn::*;
use syn::{BinOp, UnOp}; // To override c_ast::{BinOp,UnOp} from glob import

use crate::diagnostics::TranslationResult;
use crate::rust_ast::comment_store::CommentStore;
use crate::rust_ast::item_store::ItemStore;
use crate::rust_ast::set_span::SetSpan;
use crate::rust_ast::{pos_to_span, SpanExt};
use crate::translator::named_references::NamedReference;
use c2rust_ast_builder::{mk, properties::*, Builder};
use c2rust_ast_printer::pprust::{self};

use crate::c_ast::iterators::{DFExpr, SomeId};
use crate::c_ast::*;
use crate::cfg;
use crate::convert_type::TypeConverter;
use crate::renamer::Renamer;
use crate::with_stmts::WithStmts;
use crate::{c_ast, format_translation_err};
use crate::{ExternCrate, ExternCrateDetails, TranspilerConfig};
use c2rust_ast_exporter::clang_ast::LRValue;

mod assembly;
mod atomics;
mod builtins;
mod comments;
mod literals;
mod main_function;
mod named_references;
mod operators;
mod simd;
mod structs;
mod variadic;

pub use crate::diagnostics::{TranslationError, TranslationErrorKind};
use crate::CrateSet;
use crate::PragmaVec;

pub const INNER_SUFFIX: &str = "_Inner";
pub const PADDING_SUFFIX: &str = "_PADDING";

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

    // We will be referring to the expression by address. In this context we
    // can't index arrays because they may legally go out of bounds. We also
    // need to explicitly cast function references to fn() so we get their
    // address in function pointer literals.
    needs_address: bool,

    /// Set to false if we should decay VaListImpl to VaList or true if we are
    /// expect a VaListImpl in this context.
    expecting_valistimpl: bool,

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

    pub fn expect_valistimpl(self) -> Self {
        ExprContext {
            expecting_valistimpl: true,
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

#[derive(Clone, Debug, Default)]
pub struct FuncContext {
    /// The name of the function we're currently translating
    name: Option<String>,
    /// The name we give to the Rust function argument corresponding
    /// to the ellipsis in variadic C functions.
    va_list_arg_name: Option<String>,
    /// The va_list decls that are either `va_start`ed or `va_copy`ed.
    va_list_decl_ids: Option<IndexSet<CDeclId>>,
}

impl FuncContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enter_new(&mut self, fn_name: &str) {
        self.name = Some(fn_name.to_string());
        self.va_list_arg_name = None;
        self.va_list_decl_ids = None;
    }

    pub fn get_name(&self) -> &str {
        return self.name.as_ref().unwrap();
    }

    pub fn get_va_list_arg_name(&self) -> &str {
        return self.va_list_arg_name.as_ref().unwrap();
    }
}

#[derive(Clone)]
struct MacroExpansion {
    ty: CTypeId,
}

pub struct Translation<'c> {
    // Translation environment
    pub ast_context: TypedAstContext,
    pub tcfg: &'c TranspilerConfig,

    // Accumulated outputs
    pub features: RefCell<IndexSet<&'static str>>,
    sectioned_static_initializers: RefCell<Vec<Stmt>>,
    extern_crates: RefCell<CrateSet>,

    // Translation state and utilities
    type_converter: RefCell<TypeConverter>,
    renamer: RefCell<Renamer<CDeclId>>,
    zero_inits: RefCell<IndexMap<CDeclId, WithStmts<Box<Expr>>>>,
    function_context: RefCell<FuncContext>,
    potential_flexible_array_members: RefCell<IndexSet<CDeclId>>,
    macro_expansions: RefCell<IndexMap<CDeclId, Option<MacroExpansion>>>,

    // Comment support
    pub comment_context: CommentContext,      // Incoming comments
    pub comment_store: RefCell<CommentStore>, // Outgoing comments

    spans: HashMap<SomeId, Span>,

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

fn simple_metaitem(name: &str) -> NestedMeta {
    let meta_item = mk().meta_path(name);

    mk().nested_meta_item(NestedMeta::Meta(meta_item))
}

fn int_arg_metaitem(name: &str, arg: u128) -> NestedMeta {
    let lit = mk().int_unsuffixed_lit(arg);
    let inner = Meta::List(MetaList {
        path: mk().path(name),
        paren_token: Default::default(),
        nested: FromIterator::from_iter(
            vec![mk().nested_meta_item(NestedMeta::Lit(lit))].into_iter(),
        ),
    });
    NestedMeta::Meta(inner)
}

fn cast_int(val: Box<Expr>, name: &str, need_lit_suffix: bool) -> Box<Expr> {
    let opt_literal_val = match &*val {
        Expr::Lit(ref l) => match &l.lit {
            Lit::Int(i) => Some(i.base10_digits().parse().unwrap()),
            _ => None,
        },
        _ => None,
    };
    match opt_literal_val {
        Some(i) if !need_lit_suffix => mk().lit_expr(mk().int_unsuffixed_lit(i)),
        Some(i) => mk().lit_expr(mk().int_lit(i, name)),
        None => mk().cast_expr(val, mk().path_ty(vec![name])),
    }
}

/// Pointer offset that casts its argument to isize
fn pointer_offset(
    ptr: Box<Expr>,
    offset: Box<Expr>,
    multiply_by: Option<Box<Expr>>,
    neg: bool,
    mut deref: bool,
) -> Box<Expr> {
    let mut offset = cast_int(offset, "isize", false);

    if let Some(mul) = multiply_by {
        let mul = cast_int(mul, "isize", false);
        offset = mk().binary_expr(BinOp::Mul(Default::default()), offset, mul);
        deref = false;
    }

    if neg {
        offset = mk().unary_expr(UnOp::Neg(Default::default()), offset);
    }

    let res = mk().method_call_expr(ptr, "offset", vec![offset]);
    if deref {
        mk().unary_expr(UnOp::Deref(Default::default()), res)
    } else {
        res
    }
}

/// Given an expression with type Option<fn(...)->...>, unwrap
/// the Option and return the function.
fn unwrap_function_pointer(ptr: Box<Expr>) -> Box<Expr> {
    let err_msg = mk().lit_expr("non-null function pointer");
    mk().method_call_expr(ptr, "expect", vec![err_msg])
}

fn transmute_expr(source_ty: Box<Type>, target_ty: Box<Type>, expr: Box<Expr>) -> Box<Expr> {
    let type_args = match (&*source_ty, &*target_ty) {
        (Type::Infer(_), Type::Infer(_)) => Vec::new(),
        _ => vec![source_ty, target_ty],
    };
    let mut path = vec![mk().path_segment("core"), mk().path_segment("mem")];

    if type_args.is_empty() {
        path.push(mk().path_segment("transmute"));
    } else {
        path.push(mk().path_segment_with_args("transmute", mk().angle_bracketed_args(type_args)));
    }

    mk().call_expr(mk().abs_path_expr(path), vec![expr])
}

fn vec_expr(val: Box<Expr>, count: Box<Expr>) -> Box<Expr> {
    let from_elem = mk().abs_path_expr(vec!["std", "vec", "from_elem"]);
    mk().call_expr(from_elem, vec![val, count])
}

pub fn stmts_block(mut stmts: Vec<Stmt>) -> Block {
    match stmts.pop() {
        None => {}
        Some(Stmt::Expr(Expr::Block(ExprBlock {
            block, label: None, ..
        }))) if stmts.is_empty() => return block,
        Some(mut s) => {
            if let Stmt::Expr(e) = s {
                s = Stmt::Semi(e, Default::default());
            }
            stmts.push(s);
        }
    }
    mk().block(stmts)
}

/// Generate link attributes needed to ensure that the generated Rust libraries have the right symbol values.
fn mk_linkage(in_extern_block: bool, new_name: &str, old_name: &str) -> Builder {
    if new_name == old_name {
        if in_extern_block {
            mk() // There is no mangling by default in extern blocks anymore
        } else {
            mk().single_attr("no_mangle") // Don't touch my name Rust!
        }
    } else if in_extern_block {
        mk().str_attr("link_name", old_name) // Look for this name
    } else {
        mk().str_attr("export_name", old_name) // Make sure you actually name it this
    }
}

pub fn signed_int_expr(value: i64) -> Box<Expr> {
    if value < 0 {
        mk().unary_expr(
            UnOp::Neg(Default::default()),
            mk().lit_expr(mk().int_lit(u128::from(value.unsigned_abs()), "")),
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

                translation.renamer.borrow_mut().insert(decl_id, name);
            }
            CDeclKind::Variable {
                ref mut ident,
                has_static_duration,
                has_thread_duration,
                ..
            } if has_static_duration || has_thread_duration => ident.insert_str(0, prefix),
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

    let mut file_path: String = path.map_or("internal".to_string(), path_to_str);
    let path = path.unwrap_or_else(|| path::Path::new(""));
    let mut mod_names = mod_names.borrow_mut();
    if !mod_names.contains_key(&file_path.clone()) {
        mod_names.insert(file_path.clone(), path.to_path_buf());
    } else {
        let mod_path = mod_names.get(&file_path.clone()).unwrap();
        // A collision in the module names has occurred.
        // Ex: types.h can be included from
        // /usr/include/bits and /usr/include/sys
        if mod_path != path {
            let split_path: Vec<PathBuf> = path
                .to_path_buf()
                .parent()
                .unwrap()
                .iter()
                .map(PathBuf::from)
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
        panic!("Translation failed, see error above");
    }
}

pub fn translate(
    ast_context: TypedAstContext,
    tcfg: &TranspilerConfig,
    main_file: PathBuf,
) -> (String, PragmaVec, CrateSet) {
    let mut t = Translation::new(ast_context, tcfg, main_file.as_path());
    let ctx = ExprContext {
        used: true,
        is_static: false,
        is_const: false,
        decay_ref: DecayRef::Default,
        is_bitfield_write: false,
        needs_address: false,
        expecting_valistimpl: false,
        ternary_needs_parens: false,
        expanding_macro: None,
    };

    {
        t.use_crate(ExternCrate::Libc);

        // Sort the top-level declarations by file and source location so that we
        // preserve the ordering of all declarations in each file.
        t.ast_context.sort_top_decls();

        t.locate_comments();

        // Headers often pull in declarations that are unused;
        // we simplify the translator output by omitting those.
        t.ast_context
            .prune_unwanted_decls(tcfg.preserve_unused_functions);

        enum Name<'a> {
            Var(&'a str),
            Type(&'a str),
            Anonymous,
            None,
        }

        fn some_type_name(s: Option<&str>) -> Name {
            match s {
                None => Name::Anonymous,
                Some(r) => Name::Type(r),
            }
        }

        // Used for testing; so that we don't overlap with C function names
        if let Some(ref prefix) = t.tcfg.prefix_function_names {
            prefix_names(&mut t, prefix);
        }

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
                    use CDeclKind::*;
                    let is_unnamed = match t.ast_context[subdecl_id].kind {
                        Struct { name: None, .. }
                        | Union { name: None, .. }
                        | Enum { name: None, .. } => true,

                        // Detect case where typedef and struct share the same name.
                        // In this case the purpose of the typedef was simply to eliminate
                        // the need for the 'struct' tag when referring to the type name.
                        Struct {
                            name: Some(ref target_name),
                            ..
                        }
                        | Union {
                            name: Some(ref target_name),
                            ..
                        }
                        | Enum {
                            name: Some(ref target_name),
                            ..
                        } => name == target_name,

                        _ => false,
                    };

                    if is_unnamed
                        && !prenamed_decls
                            .values()
                            .any(|decl_id| *decl_id == subdecl_id)
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
            prenamed_decls.contains_key(decl_id)
                || prenamed_decls.values().any(|id| *id == *decl_id)
        }

        // Populate renamer with top-level names
        for (&decl_id, decl) in t.ast_context.iter_decls() {
            use CDeclKind::*;
            let decl_name = match decl.kind {
                _ if contains(&t.ast_context.prenamed_decls, &decl_id) => Name::None,
                Struct { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
                Enum { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
                Union { ref name, .. } => some_type_name(name.as_ref().map(String::as_str)),
                Typedef { ref name, .. } => Name::Type(name),
                Function { ref name, .. } => Name::Var(name),
                EnumConstant { ref name, .. } => Name::Var(name),
                Variable { ref ident, .. } if t.ast_context.c_decls_top.contains(&decl_id) => {
                    Name::Var(ident)
                }
                MacroObject { ref name, .. } => Name::Var(name),
                _ => Name::None,
            };
            match decl_name {
                Name::None => (),
                Name::Anonymous => {
                    t.type_converter
                        .borrow_mut()
                        .declare_decl_name(decl_id, "C2RustUnnamed");
                }
                Name::Type(name) => {
                    t.type_converter
                        .borrow_mut()
                        .declare_decl_name(decl_id, name);
                }
                Name::Var(name) => {
                    t.renamer.borrow_mut().insert(decl_id, name);
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
                    Err(e) => {
                        let k = &t.ast_context.get_decl(&decl_id).map(|x| &x.kind);
                        let msg = format!("Skipping declaration {:?} due to error: {}", k, e);
                        translate_failure(t.tcfg, &msg);
                    }
                    Ok(converted_decl) => {
                        use ConvertedDecl::*;
                        match converted_decl {
                            Item(item) => {
                                t.insert_item(item, decl);
                            }
                            ForeignItem(item) => {
                                t.insert_foreign_item(*item, decl);
                            }
                            Items(items) => {
                                for item in items {
                                    t.insert_item(item, decl);
                                }
                            }
                            NoItem => {}
                        }
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
                use CDeclKind::*;
                let needs_export = match decl.kind {
                    Struct { .. } => true,
                    Enum { .. } => true,
                    EnumConstant { .. } => true,
                    Union { .. } => true,
                    Typedef { .. } => {
                        // Only check the key as opposed to `contains`
                        // because the key should be the typedef id
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
            use CDeclKind::*;
            let needs_export = match t.ast_context[*top_id].kind {
                Function { is_implicit, .. } => !is_implicit,
                Variable { .. } => true,
                MacroObject { .. } => tcfg.translate_const_macros,
                MacroFunction { .. } => tcfg.translate_fn_macros,
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
                    Err(e) => {
                        let decl = &t.ast_context.get_decl(top_id);
                        let msg = match decl {
                            Some(decl) => {
                                let decl_identifier = decl.kind.get_name().map_or_else(
                                    || {
                                        t.ast_context
                                            .display_loc(&decl.loc)
                                            .map_or("Unknown".to_string(), |l| format!("at {}", l))
                                    },
                                    |name| name.clone(),
                                );
                                format!("Failed to translate {}: {}", decl_identifier, e)
                            }
                            _ => format!("Failed to translate declaration: {}", e,),
                        };
                        translate_failure(t.tcfg, &msg);
                    }
                    Ok(converted_decl) => {
                        use ConvertedDecl::*;
                        match converted_decl {
                            Item(item) => {
                                t.insert_item(item, decl);
                            }
                            ForeignItem(item) => {
                                t.insert_foreign_item(*item, decl);
                            }
                            Items(items) => {
                                for item in items {
                                    t.insert_item(item, decl);
                                }
                            }
                            NoItem => {}
                        }
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
                    translate_failure(t.tcfg, &msg)
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

        let mut mod_items: Vec<Box<Item>> = Vec::new();

        // Keep track of new uses we need while building header submodules
        let mut new_uses = ItemStore::new();

        // Header Reorganization: Submodule Item Stores
        for (file_id, ref mut mod_item_store) in t.items.borrow_mut().iter_mut() {
            if *file_id != t.main_file {
                if tcfg.reorganize_definitions {
                    t.use_feature("register_tool");
                }
                let mut submodule = make_submodule(
                    &t.ast_context,
                    mod_item_store,
                    *file_id,
                    &mut new_uses,
                    &t.mod_names,
                    tcfg.reorganize_definitions,
                );
                let comments = t.comment_context.get_remaining_comments(*file_id);
                submodule.set_span(match t.comment_store.borrow_mut().add_comments(&comments) {
                    Some(pos) => submodule.span().with_hi(pos),
                    None => submodule.span(),
                });
                mod_items.push(submodule);
            }
        }

        // Main file item store
        let (items, foreign_items, uses) = t.items.borrow_mut()[&t.main_file].drain();

        // Re-order comments
        // FIXME: We shouldn't have to replace with an empty comment store here, that's bad design
        let traverser = t
            .comment_store
            .replace(CommentStore::new())
            .into_comment_traverser();

        /*
        // Add a comment mapping span to each node that should have a
        // comment printed before it. The pretty printer picks up these
        // spans and uses them to decide when to emit comments.
        mod_items = mod_items
            .into_iter()
            .map(|i| traverser.traverse_item(*i)).map(Box::new)
            .collect();
        let foreign_items: Vec<ForeignItem> = foreign_items
            .into_iter()
            .map(|fi| traverser.traverse_foreign_item(fi))
            .collect();
        let items: Vec<Box<Item>> = items
            .into_iter()
            .map(|i| traverser.traverse_item(*i)).map(Box::new)
            .collect();
        */

        let mut reordered_comment_store = traverser.into_comment_store();
        let remaining_comments = t.comment_context.get_remaining_comments(t.main_file);
        reordered_comment_store.add_comments(&remaining_comments);

        // We need a dummy SourceMap with a dummy file so that pprust can try to
        // look up source line numbers for Spans. This is needed to be able to
        // print trailing comments after exprs/stmts/etc. on the same line. The
        // SourceMap will think that all Spans are invalid, but will return line
        // 0 for all of them.

        // FIXME: Use or delete this code
        // let comments = Comments::new(reordered_comment_store.into_comments());

        // pass all converted items to the Rust pretty printer
        let translation = pprust::to_string(|| {
            let (attrs, mut all_items) = arrange_header(&t, t.tcfg.is_binary(main_file.as_path()));

            all_items.extend(mod_items);

            // This could have been merged in with items below; however, it's more idiomatic to have
            // imports near the top of the file than randomly scattered about. Also, there is probably
            // no reason to have comments associated with imports so it doesn't need to go through
            // the above comment store process
            all_items.extend(uses.into_items());

            // Print new uses from submodules
            let (_, _, new_uses) = new_uses.drain();
            all_items.extend(new_uses.into_items());

            if !foreign_items.is_empty() {
                all_items.push(mk().extern_("C").foreign_items(foreign_items));
            }

            // Add the items accumulated
            all_items.extend(items);

            //s.print_remaining_comments();
            syn::File {
                shebang: None,
                attrs,
                items: all_items.into_iter().map(|x| *x).collect(),
            }
        });
        (translation, pragmas, crates)
    }
}

fn item_ident(i: &Item) -> Option<&Ident> {
    use Item::*;
    Some(match i {
        Const(ic) => &ic.ident,
        Enum(ie) => &ie.ident,
        ExternCrate(iec) => &iec.ident,
        Fn(ifn) => &ifn.sig.ident,
        ForeignMod(_ifm) => return None,
        Impl(_ii) => return None,
        Macro(im) => return im.ident.as_ref(),
        Macro2(im2) => &im2.ident,
        Mod(im) => &im.ident,
        Static(is) => &is.ident,
        Struct(is) => &is.ident,
        Trait(it) => &it.ident,
        TraitAlias(ita) => &ita.ident,
        Type(it) => &it.ident,
        Union(iu) => &iu.ident,
        Use(ItemUse { tree: _, .. }) => unimplemented!(),
        Verbatim(_tokenstream) => {
            warn!("cannot determine name of tokenstream item");
            return None;
        }
        _ => {
            warn!("cannot determine name of unknown item kind");
            return None;
        }
    })
}

fn item_vis(i: &Item) -> Option<Visibility> {
    use Item::*;
    Some(
        match i {
            Const(ic) => &ic.vis,
            Enum(ie) => &ie.vis,
            ExternCrate(iec) => &iec.vis,
            Fn(ifn) => &ifn.vis,
            ForeignMod(_ifm) => return None,
            Impl(_ii) => return None,
            Macro(_im) => return None,
            Macro2(im2) => &im2.vis,
            Mod(im) => &im.vis,
            Static(is) => &is.vis,
            Struct(is) => &is.vis,
            Trait(it) => &it.vis,
            TraitAlias(ita) => &ita.vis,
            Type(it) => &it.vis,
            Union(iu) => &iu.vis,
            Use(ItemUse { vis, .. }) => vis,
            Verbatim(_tokenstream) => {
                warn!("cannot determine visibility of tokenstream item");
                return None;
            }
            _ => {
                warn!("cannot determine visibility of unknown item kind");
                return None;
            }
        }
        .clone(),
    )
}

fn foreign_item_ident_vis(fi: &ForeignItem) -> Option<(&Ident, Visibility)> {
    use ForeignItem::*;
    Some(match fi {
        Fn(ifn) => (&ifn.sig.ident, ifn.vis.clone()),
        Static(is) => (&is.ident, is.vis.clone()),
        Type(it) => (&it.ident, it.vis.clone()),
        Macro(_im) => return None,
        Verbatim(_tokenstream) => {
            warn!("cannot determine name and visibility of tokenstream foreign item");
            return None;
        }
        _ => {
            warn!("cannot determine name and visibility of unknown foreign item kind");
            return None;
        }
    })
}

fn make_submodule(
    ast_context: &TypedAstContext,
    item_store: &mut ItemStore,
    file_id: FileId,
    use_item_store: &mut ItemStore,
    mod_names: &RefCell<IndexMap<String, PathBuf>>,
    reorganize_definitions: bool,
) -> Box<Item> {
    let (mut items, foreign_items, uses) = item_store.drain();
    let file_path = ast_context.get_file_path(file_id);
    let include_line_number = ast_context
        .get_file_include_line_number(file_id)
        .unwrap_or(0);
    let mod_name = clean_path(mod_names, file_path);

    for item in items.iter() {
        let ident_name = match item_ident(item) {
            Some(i) => i.to_string(),
            None => continue,
        };
        let use_path = vec!["self".into(), mod_name.clone()];

        let vis = match item_vis(item) {
            Some(Visibility::Public(_)) => mk().pub_(),
            Some(_) => mk(),
            None => continue,
        };

        use_item_store.add_use_with_attr(use_path, &ident_name, vis);
    }

    for foreign_item in foreign_items.iter() {
        let ident_name = match foreign_item_ident_vis(foreign_item) {
            Some((ident, _vis)) => ident.to_string(),
            None => continue,
        };
        let use_path = vec!["self".into(), mod_name.clone()];

        use_item_store.add_use(use_path, &ident_name);
    }

    for item in uses.into_items() {
        items.push(item);
    }

    if !foreign_items.is_empty() {
        items.push(mk().extern_("C").foreign_items(foreign_items));
    }

    let module_builder = mk().vis("pub");
    let module_builder = if reorganize_definitions {
        let file_path_str = file_path.map_or(mod_name.as_str(), |path| {
            path.to_str().expect("Found invalid unicode")
        });
        module_builder.str_attr(
            vec!["c2rust", "header_src"],
            format!("{}:{}", file_path_str, include_line_number),
        )
    } else {
        module_builder
    };
    module_builder.mod_item(mod_name, Some(mk().mod_(items)))
}

// TODO(kkysen) shouldn't need `extern crate`
/// Pretty-print the leading pragmas and extern crate declarations
// Fixing this would require major refactors for marginal benefit.
#[allow(clippy::vec_box)]
fn arrange_header(t: &Translation, is_binary: bool) -> (Vec<syn::Attribute>, Vec<Box<Item>>) {
    let mut out_attrs = vec![];
    let mut out_items = vec![];
    if t.tcfg.emit_modules && !is_binary {
        for c in t.extern_crates.borrow().iter() {
            out_items.push(mk().use_simple_item(
                mk().abs_path(vec![ExternCrateDetails::from(*c).ident]),
                None::<Ident>,
            ))
        }
    } else {
        let pragmas = t.get_pragmas();
        for (key, mut values) in pragmas {
            values.sort_unstable();
            // generate #[key(values)]
            let value_attr_vec = values
                .into_iter()
                .map(|value| mk().nested_meta_item(mk().meta_path(value)))
                .collect::<Vec<_>>();
            let item = mk().meta_list(vec![key], value_attr_vec);
            for attr in mk()
                .meta_item_attr(AttrStyle::Inner(Default::default()), item)
                .as_inner_attrs()
            {
                out_attrs.push(attr);
            }
        }

        if t.tcfg.emit_no_std {
            out_attrs.push(mk().single_attr("no_std").as_inner_attrs()[0].clone());
        }

        if is_binary {
            // TODO(kkysen) shouldn't need `extern crate`
            // Add `extern crate X;` to the top of the file
            for extern_crate in t.extern_crates.borrow().iter() {
                let extern_crate = ExternCrateDetails::from(*extern_crate);
                if extern_crate.macro_use {
                    out_items.push(
                        mk().single_attr("macro_use")
                            .extern_crate_item(extern_crate.ident.clone(), None),
                    );
                }
            }

            out_items.push(mk().use_glob_item(mk().abs_path(vec![&t.tcfg.crate_name()])));
        }
    }
    (out_attrs, out_items)
}

/// Convert a boolean expression to a c_int
fn bool_to_int(val: Box<Expr>) -> Box<Expr> {
    mk().cast_expr(val, mk().path_ty(vec!["libc", "c_int"]))
}

/// Add a src_loc = "line:col" attribute to an item/foreign_item
fn add_src_loc_attr(attrs: &mut Vec<syn::Attribute>, src_loc: &Option<SrcLoc>) {
    if let Some(src_loc) = src_loc.as_ref() {
        let loc_str = format!("{}:{}", src_loc.line, src_loc.column);
        let meta = mk().meta_namevalue(vec!["c2rust", "src_loc"], loc_str);
        let prepared = mk().prepare_meta(meta);
        let attr = mk().attribute(AttrStyle::Outer, prepared.path, prepared.tokens);
        attrs.push(attr);
    }
}

/// Get a mutable reference to the attributes of a ForeignItem
fn foreign_item_attrs(item: &mut ForeignItem) -> Option<&mut Vec<syn::Attribute>> {
    use ForeignItem::*;
    Some(match item {
        Fn(ForeignItemFn { ref mut attrs, .. }) => attrs,
        Static(ForeignItemStatic { ref mut attrs, .. }) => attrs,
        Type(ForeignItemType { ref mut attrs, .. }) => attrs,
        Macro(ForeignItemMacro { ref mut attrs, .. }) => attrs,
        Verbatim(TokenStream { .. }) => return None,
        _ => return None,
    })
}

/// Get a mutable reference to the attributes of an Item
fn item_attrs(item: &mut Item) -> Option<&mut Vec<syn::Attribute>> {
    use Item::*;
    Some(match item {
        Const(ItemConst { ref mut attrs, .. }) => attrs,
        Enum(ItemEnum { ref mut attrs, .. }) => attrs,
        ExternCrate(ItemExternCrate { ref mut attrs, .. }) => attrs,
        Fn(ItemFn { ref mut attrs, .. }) => attrs,
        ForeignMod(ItemForeignMod { ref mut attrs, .. }) => attrs,
        Impl(ItemImpl { ref mut attrs, .. }) => attrs,
        Macro(ItemMacro { ref mut attrs, .. }) => attrs,
        Macro2(ItemMacro2 { ref mut attrs, .. }) => attrs,
        Mod(ItemMod { ref mut attrs, .. }) => attrs,
        Static(ItemStatic { ref mut attrs, .. }) => attrs,
        Struct(ItemStruct { ref mut attrs, .. }) => attrs,
        Trait(ItemTrait { ref mut attrs, .. }) => attrs,
        TraitAlias(ItemTraitAlias { ref mut attrs, .. }) => attrs,
        Type(ItemType { ref mut attrs, .. }) => attrs,
        Union(ItemUnion { ref mut attrs, .. }) => attrs,
        Use(ItemUse { ref mut attrs, .. }) => attrs,
        Verbatim(TokenStream { .. }) => return None,
        _ => return None,
    })
}

/// Unwrap a layer of parenthesization from an Expr, if present
pub(crate) fn unparen(expr: &Expr) -> &Expr {
    match *expr {
        Expr::Paren(ExprParen { ref expr, .. }) => expr,
        _ => expr,
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
    /// [`ForeignItem`] is large (472 bytes), so [`Box`] it.
    ForeignItem(Box<ForeignItem>), // would be 472 bytes
    Item(Box<Item>),       // 24 bytes
    Items(Vec<Box<Item>>), // 24 bytes
    NoItem,
}

struct ConvertedVariable {
    pub ty: Box<Type>,
    pub mutbl: Mutability,
    pub init: TranslationResult<WithStmts<Box<Expr>>>,
}

impl<'c> Translation<'c> {
    pub fn new(
        mut ast_context: TypedAstContext,
        tcfg: &'c TranspilerConfig,
        main_file: &path::Path,
    ) -> Self {
        let comment_context = CommentContext::new(&mut ast_context);
        let mut type_converter = TypeConverter::new();

        if tcfg.translate_valist {
            type_converter.translate_valist = true
        }

        let main_file = ast_context.find_file_id(main_file).unwrap_or(0);
        let items = indexmap! {main_file => ItemStore::new()};

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
            function_context: RefCell::new(FuncContext::new()),
            potential_flexible_array_members: RefCell::new(IndexSet::new()),
            macro_expansions: RefCell::new(IndexMap::new()),
            comment_context,
            comment_store: RefCell::new(CommentStore::new()),
            spans: HashMap::new(),
            sectioned_static_initializers: RefCell::new(Vec::new()),
            items: RefCell::new(items),
            mod_names: RefCell::new(IndexMap::new()),
            main_file,
            extern_crates: RefCell::new(IndexSet::new()),
            cur_file: RefCell::new(None),
        }
    }

    fn use_crate(&self, extern_crate: ExternCrate) {
        self.extern_crates.borrow_mut().insert(extern_crate);
    }

    pub fn cur_file(&self) -> FileId {
        if let Some(cur_file) = *self.cur_file.borrow() {
            cur_file
        } else {
            self.main_file
        }
    }

    fn with_cur_file_item_store<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut ItemStore) -> T,
    {
        let mut item_stores = self.items.borrow_mut();
        let item_store = item_stores
            .entry(Self::cur_file(self))
            .or_insert_with(ItemStore::new);
        f(item_store)
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

        if self.features.borrow().contains("register_tool") {
            pragmas.push(("register_tool", vec!["c2rust"]));
        }

        if !features.is_empty() {
            pragmas.push(("feature", features));
        }
        pragmas
    }

    // This node should _never_ show up in the final generated code. This is an easy way to notice
    // if it does.
    pub fn panic_or_err(&self, msg: &str) -> Box<Expr> {
        self.panic_or_err_helper(msg, self.tcfg.panic_on_translator_failure)
    }

    pub fn panic(&self, msg: &str) -> Box<Expr> {
        self.panic_or_err_helper(msg, true)
    }

    fn panic_or_err_helper(&self, msg: &str, panic: bool) -> Box<Expr> {
        let macro_name = if panic { "panic" } else { "compile_error" };
        let macro_msg = vec![TokenTree::Literal(proc_macro2::Literal::string(msg))]
            .into_iter()
            .collect::<TokenStream>();
        mk().mac_expr(mk().mac(
            mk().path(vec![macro_name]),
            macro_msg,
            MacroDelimiter::Paren(Default::default()),
        ))
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

            use CExprKind::*;
            match self.ast_context[expr_id].kind {
                DeclRef(_, _, LRValue::LValue) => return true,
                ImplicitCast(_, _, cast_kind, _, _) | ExplicitCast(_, _, cast_kind, _, _) => {
                    use CastKind::*;
                    match cast_kind {
                        IntegralToPointer | FunctionToPointerDecay | PointerToIntegral => {
                            return true;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        false
    }

    /// The purpose of this function is to decide on whether or not a static initializer's
    /// translation is able to be compiled as a valid rust static initializer
    fn static_initializer_is_uncompilable(
        &self,
        expr_id: Option<CExprId>,
        qtype: CQualTypeId,
    ) -> bool {
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

            use CExprKind::*;
            match self.ast_context[expr_id].kind {
                // Technically we're being conservative here, but it's only the most
                // contrived array indexing initializers that would be accepted
                ArraySubscript(..) => return true,
                Member(..) => return true,

                Conditional(..) => return true,
                Unary(typ, Negate, _, _) => {
                    if self
                        .ast_context
                        .resolve_type(typ.ctype)
                        .kind
                        .is_unsigned_integral_type()
                    {
                        return true;
                    }
                }

                // PointerToIntegral is no longer allowed, const-eval throws an
                // error: "pointer-to-integer cast" needs an rfc before being
                // allowed inside constants
                ImplicitCast(_, _, PointerToIntegral, _, _)
                | ExplicitCast(_, _, PointerToIntegral, _, _) => return true,

                Binary(typ, op, _, _, _, _) => {
                    let problematic_op = matches!(op, Add | Subtract | Multiply | Divide | Modulus);

                    if problematic_op {
                        let k = &self.ast_context.resolve_type(typ.ctype).kind;
                        if k.is_unsigned_integral_type() || k.is_pointer() {
                            return true;
                        }
                    }
                }
                Unary(_, AddressOf, expr_id, _) => {
                    if let Member(_, expr_id, _, _, _) = self.ast_context[expr_id].kind {
                        if let DeclRef(..) = self.ast_context[expr_id].kind {
                            return true;
                        }
                    }
                }
                InitList(qtype, _, _, _) => {
                    let ty = &self.ast_context.resolve_type(qtype.ctype).kind;

                    if let &CTypeKind::Struct(decl_id) = ty {
                        let decl = &self.ast_context[decl_id].kind;

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
                }
                ImplicitCast(qtype, _, IntegralToPointer, _, _)
                | ExplicitCast(qtype, _, IntegralToPointer, _, _) => {
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
        init: &mut Box<Expr>,
    ) -> TranslationResult<()> {
        let mut default_init = self.implicit_default_expr(typ.ctype, true)?.to_expr();

        std::mem::swap(init, &mut default_init);

        let root_lhs_expr = mk().path_expr(vec![name]);
        let assign_expr = mk().assign_expr(root_lhs_expr, default_init);
        let stmt = mk().expr_stmt(assign_expr);

        self.sectioned_static_initializers.borrow_mut().push(stmt);

        Ok(())
    }

    fn generate_global_static_init(&mut self) -> (Box<Item>, Box<Item>) {
        // If we don't want to consume self.sectioned_static_initializers for some reason, we could clone the vec
        let sectioned_static_initializers = self.sectioned_static_initializers.replace(Vec::new());

        let fn_name = self
            .renamer
            .borrow_mut()
            .pick_name("run_static_initializers");
        let fn_ty = ReturnType::Default;
        let fn_decl = mk().fn_decl(fn_name.clone(), vec![], None, fn_ty.clone());
        let fn_bare_decl = (vec![], None, fn_ty);
        let fn_block = mk().block(sectioned_static_initializers);
        let fn_item = mk().unsafe_().extern_("C").fn_item(fn_decl, fn_block);

        let static_attributes = mk()
            .single_attr("used")
            .meta_item_attr(
                AttrStyle::Outer,
                mk().meta_list(
                    "cfg_attr",
                    vec![
                        mk().nested_meta_item(mk().meta_namevalue("target_os", "linux")),
                        mk().nested_meta_item(mk().meta_namevalue("link_section", ".init_array")),
                    ],
                ),
            )
            .meta_item_attr(
                AttrStyle::Outer,
                mk().meta_list(
                    "cfg_attr",
                    vec![
                        mk().nested_meta_item(mk().meta_namevalue("target_os", "windows")),
                        mk().nested_meta_item(mk().meta_namevalue("link_section", ".CRT$XIB")),
                    ],
                ),
            )
            .meta_item_attr(
                AttrStyle::Outer,
                mk().meta_list(
                    "cfg_attr",
                    vec![
                        mk().nested_meta_item(mk().meta_namevalue("target_os", "macos")),
                        mk().nested_meta_item(
                            mk().meta_namevalue("link_section", "__DATA,__mod_init_func"),
                        ),
                    ],
                ),
            );
        let static_array_size = mk().lit_expr(mk().int_unsuffixed_lit(1));
        let static_ty = mk().array_ty(
            mk().unsafe_().extern_("C").barefn_ty(fn_bare_decl),
            static_array_size,
        );
        let static_val = mk().array_expr(vec![mk().path_expr(vec![fn_name])]);
        let static_item = static_attributes.static_item("INIT_ARRAY", static_ty, static_val);

        (fn_item, static_item)
    }

    fn convert_decl(&self, ctx: ExprContext, decl_id: CDeclId) -> TranslationResult<ConvertedDecl> {
        let decl = self
            .ast_context
            .get_decl(&decl_id)
            .ok_or_else(|| format_err!("Missing decl {:?}", decl_id))?;

        let mut span = self
            .get_span(SomeId::Decl(decl_id))
            .unwrap_or_else(Span::call_site);

        use CDeclKind::*;
        match decl.kind {
            Struct { fields: None, .. }
            | Union { fields: None, .. }
            | Enum {
                integral_type: None,
                ..
            } => {
                self.use_feature("extern_types");
                let name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .unwrap();

                let extern_item = mk().span(span).pub_().ty_foreign_item(name);
                Ok(ConvertedDecl::ForeignItem(extern_item))
            }

            Struct {
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

                // Pre-declare all the field names, checking for duplicates
                for &x in fields {
                    if let CDeclKind::Field { ref name, .. } = self.ast_context.index(x).kind {
                        self.type_converter
                            .borrow_mut()
                            .declare_field_name(decl_id, x, name);
                    }
                }

                // Gather up all the field names and field types
                let (field_entries, contains_va_list) =
                    self.convert_struct_fields(decl_id, fields, platform_byte_size)?;

                let mut derives = vec![];
                if !contains_va_list {
                    derives.push("Copy");
                    derives.push("Clone");
                };
                let has_bitfields =
                    fields
                        .iter()
                        .any(|field_id| match self.ast_context.index(*field_id).kind {
                            CDeclKind::Field { bitfield_width, .. } => bitfield_width.is_some(),
                            _ => unreachable!("Found non-field in record field list"),
                        });
                if has_bitfields {
                    derives.push("BitfieldStruct");
                    self.use_crate(ExternCrate::C2RustBitfields);
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
                    Some(mf) if mf > 1 => reprs.push(int_arg_metaitem("packed", mf as u128)),
                    _ => {}
                }

                if let Some(alignment) = manual_alignment {
                    // This is the most complicated case: we have `align(N)` which
                    // might be mixed with or included into a `packed` structure,
                    // which Rust doesn't currently support; instead, we split
                    // the structure into 2 structures like this:
                    //   #[align(N)]
                    //   pub struct Foo(pub Foo_Inner);
                    //   #[packed(M)]
                    //   pub struct Foo_Inner {
                    //     ...fields...
                    //   }
                    //
                    // TODO: right now, we always emit the pair of structures
                    // instead, we should only split when needed, but that
                    // would significantly complicate the implementation
                    assert!(self.ast_context.has_inner_struct_decl(decl_id));
                    let inner_name = self.resolve_decl_inner_name(decl_id);
                    let inner_ty = mk().path_ty(vec![inner_name.clone()]);
                    let inner_repr_attr = mk().meta_list("repr", reprs);
                    let inner_struct = mk()
                        .span(span)
                        .pub_()
                        .call_attr("derive", derives)
                        .meta_item_attr(AttrStyle::Outer, inner_repr_attr)
                        .struct_item(inner_name.clone(), field_entries, false);

                    // https://github.com/rust-lang/rust/issues/33626
                    let outer_ty = mk().path_ty(vec![name.clone()]);
                    let outer_reprs = vec![
                        simple_metaitem("C"),
                        int_arg_metaitem("align", alignment as u128),
                        // TODO: copy others from `reprs` above
                    ];
                    let repr_attr = mk().meta_list("repr", outer_reprs);
                    let outer_field = mk().pub_().enum_field(mk().ident_ty(inner_name));
                    let outer_struct = mk()
                        .span(span)
                        .pub_()
                        .call_attr("derive", vec!["Copy", "Clone"])
                        .meta_item_attr(AttrStyle::Outer, repr_attr)
                        .struct_item(name, vec![outer_field], true);

                    // Emit `const X_PADDING: usize = size_of(Outer) - size_of(Inner);`
                    let padding_name = self
                        .type_converter
                        .borrow_mut()
                        .resolve_decl_suffix_name(decl_id, PADDING_SUFFIX)
                        .to_owned();
                    let padding_ty = mk().path_ty(vec!["usize"]);
                    let outer_size = self.compute_size_of_ty(outer_ty)?.to_expr();
                    let inner_size = self.compute_size_of_ty(inner_ty)?.to_expr();
                    let padding_value =
                        mk().binary_expr(BinOp::Sub(Default::default()), outer_size, inner_size);
                    let padding_const = mk()
                        .span(span)
                        .call_attr("allow", vec!["dead_code", "non_upper_case_globals"])
                        .const_item(padding_name, padding_ty, padding_value);

                    let structs = vec![outer_struct, inner_struct, padding_const];
                    Ok(ConvertedDecl::Items(structs))
                } else {
                    assert!(!self.ast_context.has_inner_struct_decl(decl_id));
                    let repr_attr = mk().meta_list("repr", reprs);

                    let mut mk_ = mk()
                        .span(span)
                        .pub_()
                        .call_attr("derive", derives)
                        .meta_item_attr(AttrStyle::Outer, repr_attr);

                    if contains_va_list {
                        mk_ = mk_.generic_over(mk().lt_param(mk().ident("a")))
                    }

                    Ok(ConvertedDecl::Item(mk_.struct_item(
                        name,
                        field_entries,
                        false,
                    )))
                }
            }

            Union {
                fields: Some(ref fields),
                is_packed,
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
                            ));
                        }
                    }
                }

                let mut repr = vec!["C"];
                if is_packed {
                    repr.push("packed");
                }

                Ok(if field_syns.is_empty() {
                    // Empty unions are a GNU extension, but Rust doesn't allow empty unions.
                    ConvertedDecl::Item(
                        mk().span(span)
                            .pub_()
                            .call_attr("derive", vec!["Copy", "Clone"])
                            .call_attr("repr", repr)
                            .struct_item(name, vec![], false),
                    )
                } else {
                    ConvertedDecl::Item(
                        mk().span(span)
                            .pub_()
                            .call_attr("derive", vec!["Copy", "Clone"])
                            .call_attr("repr", repr)
                            .union_item(name, field_syns),
                    )
                })
            }

            Field { .. } => Err(TranslationError::generic(
                "Field declarations should be handled inside structs/unions",
            )),

            Enum {
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
                    mk().span(span).pub_().type_item(enum_name, ty),
                ))
            }

            EnumConstant { value, .. } => {
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
                if let Some(cur_file) = *self.cur_file.borrow() {
                    self.add_import(cur_file, enum_id, &enum_name);
                }
                let ty = mk().path_ty(mk().path(vec![enum_name]));
                let val = match value {
                    ConstIntExpr::I(value) => signed_int_expr(value),
                    ConstIntExpr::U(value) => mk().lit_expr(mk().int_unsuffixed_lit(value as u128)),
                };

                Ok(ConvertedDecl::Item(
                    mk().span(span).pub_().const_item(name, ty, val),
                ))
            }

            // We can allow non top level function declarations (i.e. extern
            // declarations) without any problem. Clang doesn't support nested
            // functions, so we will never see nested function definitions.
            Function {
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

                let (ret, is_variadic): (Option<CQualTypeId>, bool) =
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
                            .into());
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
                    ctx,
                    span,
                    is_global,
                    is_inline,
                    is_main,
                    is_variadic,
                    is_extern,
                    new_name,
                    name,
                    &args,
                    ret,
                    body,
                    attrs,
                );

                converted_function.or_else(|e| match self.tcfg.replace_unsupported_decls {
                    ReplaceMode::Extern if body.is_none() => self.convert_function(
                        ctx,
                        span,
                        is_global,
                        false,
                        is_main,
                        is_variadic,
                        is_extern,
                        new_name,
                        name,
                        &args,
                        ret,
                        None,
                        attrs,
                    ),
                    _ => Err(e),
                })
            }

            Typedef { ref typ, .. } => {
                let new_name = &self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .unwrap();

                if self.import_simd_typedef(new_name)? {
                    return Ok(ConvertedDecl::NoItem);
                }

                // We can't typedef to std::ffi::VaList, since the typedef won't
                // have explicit lifetime params which VaList
                // requires. Temporarily disable translation of valist to Rust
                // native VaList.
                let translate_valist = mem::replace(
                    &mut self.type_converter.borrow_mut().translate_valist,
                    false,
                );
                let ty = self.convert_type(typ.ctype)?;
                self.type_converter.borrow_mut().translate_valist = translate_valist;

                Ok(ConvertedDecl::Item(
                    mk().span(span).pub_().type_item(new_name, ty),
                ))
            }

            // Externally-visible variable without initializer (definition elsewhere)
            Variable {
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
                let ConvertedVariable { ty, mutbl, init: _ } =
                    self.convert_variable(ctx.static_(), None, typ)?;
                // When putting extern statics into submodules, they need to be public to be accessible
                let visibility = if self.tcfg.reorganize_definitions {
                    "pub"
                } else {
                    ""
                };
                let mut extern_item = mk_linkage(true, &new_name, ident)
                    .span(span)
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
            Variable {
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
                    let ConvertedVariable { ty, mutbl: _, init } =
                        self.convert_variable(ctx.not_static(), initializer, typ)?;

                    let mut init = init?.to_expr();

                    let comment = String::from("// Initialized in run_static_initializers");
                    let comment_pos = if span.is_dummy() {
                        None
                    } else {
                        Some(span.lo())
                    };
                    span = self
                        .comment_store
                        .borrow_mut()
                        .extend_existing_comments(
                            &[comment],
                            comment_pos,
                            //CommentStyle::Isolated,
                        )
                        .map(pos_to_span)
                        .unwrap_or(span);

                    self.add_static_initializer_to_section(new_name, typ, &mut init)?;

                    (ty, init)
                } else {
                    let ConvertedVariable { ty, mutbl: _, init } =
                        self.convert_variable(ctx.static_(), initializer, typ)?;
                    let mut init = init?;
                    // TODO: Replace this by relying entirely on
                    // WithStmts.is_unsafe() of the translated variable
                    if self.static_initializer_is_unsafe(initializer, typ) {
                        init.set_unsafe()
                    }
                    let init = init.to_unsafe_pure_expr().ok_or_else(|| {
                        format_err!("Expected no side-effects in static initializer")
                    })?;

                    (ty, init)
                };

                let static_def = if is_externally_visible {
                    mk_linkage(false, new_name, ident).pub_().extern_("C")
                } else if self.cur_file.borrow().is_some() {
                    mk().pub_()
                } else {
                    mk()
                };

                // Force mutability due to the potential for raw pointers occurring in the type
                // and because we may be assigning to these variables in the external initializer
                let mut static_def = static_def.span(span).mutbl();
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

            Variable { .. } => Err(TranslationError::generic(
                "This should be handled in 'convert_decl_stmt'",
            )),

            MacroObject { .. } => {
                let name = self
                    .renamer
                    .borrow_mut()
                    .get(&decl_id)
                    .expect("Macro object not named");

                trace!(
                    "Expanding macro {:?}: {:?}",
                    decl_id,
                    self.ast_context[decl_id]
                );

                let maybe_replacement = self.canonical_macro_replacement(
                    ctx.set_const(true).set_expanding_macro(decl_id),
                    &self.ast_context.macro_expansions[&decl_id],
                );

                match maybe_replacement {
                    Ok((replacement, ty)) => {
                        trace!("  to {:?}", replacement);

                        let expansion = MacroExpansion { ty };
                        self.macro_expansions
                            .borrow_mut()
                            .insert(decl_id, Some(expansion));
                        let ty = self.convert_type(ty)?;

                        Ok(ConvertedDecl::Item(mk().span(span).pub_().const_item(
                            name,
                            ty,
                            replacement,
                        )))
                    }
                    Err(e) => {
                        self.macro_expansions.borrow_mut().insert(decl_id, None);
                        info!("Could not expand macro {}: {}", name, e);
                        Ok(ConvertedDecl::NoItem)
                    }
                }
            }

            // We aren't doing anything with the definitions of function-like
            // macros yet.
            MacroFunction { .. } => Ok(ConvertedDecl::NoItem),

            // Do not translate non-canonical decls. They will be translated at
            // their canonical declaration.
            NonCanonicalDecl { .. } => Ok(ConvertedDecl::NoItem),

            StaticAssert { .. } => {
                warn!("ignoring static assert during translation");
                Ok(ConvertedDecl::NoItem)
            }
        }
    }

    fn canonical_macro_replacement(
        &self,
        ctx: ExprContext,
        replacements: &[CExprId],
    ) -> TranslationResult<(Box<Expr>, CTypeId)> {
        let (val, ty) = replacements
            .iter()
            .try_fold::<Option<(WithStmts<Box<Expr>>, CTypeId)>, _, _>(None, |canonical, id| {
                let ty = self.ast_context[*id]
                    .kind
                    .get_type()
                    .ok_or_else(|| format_err!("Invalid expression type"))?;
                let (expr_id, ty) = self
                    .ast_context
                    .resolve_expr_type_id(*id)
                    .unwrap_or((*id, ty));
                let expr = self.convert_expr(ctx, expr_id)?;

                // Join ty and cur_ty to the smaller of the two types. If the
                // types are not cast-compatible, abort the fold.
                let ty_kind = self.ast_context.resolve_type(ty).kind.clone();
                if let Some((canon_val, canon_ty)) = canonical {
                    let canon_ty_kind = self.ast_context.resolve_type(canon_ty).kind.clone();
                    if let Some(smaller_ty) =
                        CTypeKind::smaller_compatible_type(canon_ty_kind.clone(), ty_kind)
                    {
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
            })?
            .ok_or_else(|| format_err!("Could not find a valid type for macro"))?;

        val.to_unsafe_pure_expr()
            .map(|val| (val, ty))
            .ok_or_else(|| TranslationError::generic("Macro expansion is not a pure expression"))

        // TODO: Validate that all replacements are equivalent and pick the most
        // common type to minimize casts.
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
    ) -> TranslationResult<ConvertedDecl> {
        self.function_context.borrow_mut().enter_new(name);

        self.with_scope(|| {
            let mut args: Vec<FnArg> = vec![];

            // handle regular (non-variadic) arguments
            for &(decl_id, ref var, typ) in arguments {
                let ConvertedVariable { ty, mutbl, init: _ } =
                    self.convert_variable(ctx, None, typ)?;

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
                        .unwrap_or_else(|| {
                            panic!(
                                "Failed to insert argument '{}' while converting '{}'",
                                var, name
                            )
                        });

                    mk().set_mutbl(mutbl).ident_pat(new_var)
                };

                args.push(mk().arg(ty, pat))
            }

            if is_variadic {
                // function definitions
                if let Some(body_id) = body {
                    let arg_va_list_name = self.register_va_decls(body_id);

                    // FIXME: detect mutability requirements.
                    let pat = mk()
                        .set_mutbl(Mutability::Mutable)
                        .ident_pat(arg_va_list_name);
                    args.push(mk().arg(mk().cvar_args_ty(), pat));
                } else {
                    // function declarations
                    args.push(mk().arg(mk().cvar_args_ty(), mk().wild_pat()));
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
                ReturnType::Default
            } else {
                ReturnType::Type(Default::default(), ret)
            };

            let decl = mk().fn_decl(
                new_name,
                args,
                is_variadic.then(|| mk().variadic_arg(vec![])),
                ret,
            );

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
                let mut block = stmts_block(body_stmts);
                if let Some(span) = self.get_span(SomeId::Stmt(body)) {
                    block.set_span(span);
                }

                // c99 extern inline functions should be pub, but not gnu_inline attributed
                // extern inlines, which become subject to their gnu89 visibility (private)
                let is_extern_inline =
                    is_inline && is_extern && !attrs.contains(&c_ast::Attribute::GnuInline);

                // Only add linkage attributes if the function is `extern`
                let mut mk_ = if is_main {
                    mk()
                } else if (is_global && !is_inline) || is_extern_inline {
                    mk_linkage(false, new_name, name).extern_("C").pub_()
                } else if self.cur_file.borrow().is_some() {
                    mk().extern_("C").pub_()
                } else {
                    mk().extern_("C")
                };

                for attr in attrs {
                    mk_ = match attr {
                        c_ast::Attribute::AlwaysInline => mk_.call_attr("inline", vec!["always"]),
                        c_ast::Attribute::Cold => mk_.single_attr("cold"),
                        c_ast::Attribute::NoInline => mk_.call_attr("inline", vec!["never"]),
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
                        mk_ = mk_.str_attr("linkage", "external");
                    }
                    // NOTE: it does not seem necessary to have an else branch here that
                    // specifies internal linkage in all other cases due to name mangling by rustc.
                }

                Ok(ConvertedDecl::Item(
                    mk_.span(span).unsafe_().fn_item(decl, block),
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

                let function_decl = mk_.fn_foreign_item(decl);

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
    ) -> TranslationResult<Vec<Stmt>> {
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
                mk().ref_lt_ty("static", mk().path_ty(vec!["str"]))
            } else {
                mk().path_ty(vec!["u64"])
            };

            let local = mk().local(
                mk().mutbl().ident_pat(current_block_ident),
                Some(current_block_ty),
                None,
            );
            stmts.push(mk().local_stmt(Box::new(local)))
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
    ) -> TranslationResult<Vec<Stmt>> {
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
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let ty_id = self.ast_context[cond_id]
            .kind
            .get_type()
            .ok_or_else(|| format_err!("bad condition type"))?;

        let null_pointer_case =
            |negated: bool, ptr: CExprId| -> TranslationResult<WithStmts<Box<Expr>>> {
                let val = self.convert_expr(ctx.used().decay_ref(), ptr)?;
                let ptr_type = self.ast_context[ptr]
                    .kind
                    .get_type()
                    .ok_or_else(|| format_err!("bad pointer type for condition"))?;
                Ok(val.map(|e| {
                    if self.ast_context.is_function_pointer(ptr_type) {
                        if negated {
                            mk().method_call_expr(e, "is_some", vec![])
                        } else {
                            mk().method_call_expr(e, "is_none", vec![])
                        }
                    } else {
                        let is_null = mk().method_call_expr(e, "is_null", vec![]);
                        if negated {
                            mk().unary_expr(UnOp::Not(Default::default()), is_null)
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
    ) -> TranslationResult<cfg::DeclStmtInfo> {
        if let CDeclKind::Variable {
            ref ident,
            has_static_duration: true,
            is_externally_visible: false,
            is_defn: true,
            initializer,
            typ,
            ..
        } = self.ast_context.index(decl_id).kind
        {
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
                let ConvertedVariable { ty, mutbl: _, init } =
                    self.convert_variable(ctx.static_(), initializer, typ)?;
                let default_init = self.implicit_default_expr(typ.ctype, true)?.to_expr();
                let comment = String::from("// Initialized in run_static_initializers");
                let span = self
                    .comment_store
                    .borrow_mut()
                    .add_comments(&[comment])
                    .map(pos_to_span)
                    .unwrap_or_else(Span::call_site);
                let static_item = mk()
                    .span(span)
                    .mutbl()
                    .static_item(&ident2, ty, default_init);
                let mut init = init?;
                init.set_unsafe();
                let mut init = init.to_expr();

                self.add_static_initializer_to_section(&ident2, typ, &mut init)?;
                self.items.borrow_mut()[&self.main_file].add_item(static_item);

                return Ok(cfg::DeclStmtInfo::empty());
            }
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
                    .insert(decl_id, ident)
                    .unwrap_or_else(|| panic!("Failed to insert variable '{}'", ident));

                if self.ast_context.is_va_list(typ.ctype) {
                    // translate `va_list` variables to `VaListImpl`s and omit the initializer.
                    let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name);
                    let ty = {
                        let path = vec!["core", "ffi", "VaListImpl"];
                        mk().path_ty(mk().abs_path(path))
                    };
                    let local_mut = mk().local(pat_mut, Some(ty), None);

                    return Ok(cfg::DeclStmtInfo::new(
                        vec![],                                     // decl
                        vec![],                                     // assign
                        vec![mk().local_stmt(Box::new(local_mut))], // decl_and_assign
                    ));
                }

                let has_self_reference = if let Some(expr_id) = initializer {
                    self.has_decl_reference(decl_id, expr_id)
                } else {
                    false
                };

                let mut stmts = self.compute_variable_array_sizes(ctx, typ.ctype)?;

                let ConvertedVariable { ty, mutbl, init } =
                    self.convert_variable(ctx, initializer, typ)?;
                let mut init = init?;

                stmts.append(init.stmts_mut());
                let init = init.into_value();

                let zeroed = self.implicit_default_expr(typ.ctype, false)?;
                let zeroed = if ctx.is_const {
                    zeroed.to_unsafe_pure_expr()
                } else {
                    zeroed.to_pure_expr()
                }
                .expect("Expected decl initializer to not have any statements");
                let pat_mut = mk().set_mutbl("mut").ident_pat(rust_name.clone());
                let local_mut = mk().local(pat_mut, Some(ty.clone()), Some(zeroed));
                if has_self_reference {
                    let assign = mk().assign_expr(mk().ident_expr(rust_name), init);

                    let mut assign_stmts = stmts.clone();
                    assign_stmts.push(mk().semi_stmt(assign.clone()));

                    let mut decl_and_assign = vec![mk().local_stmt(Box::new(local_mut.clone()))];
                    decl_and_assign.append(&mut stmts);
                    decl_and_assign.push(mk().expr_stmt(assign));

                    Ok(cfg::DeclStmtInfo::new(
                        vec![mk().local_stmt(Box::new(local_mut))],
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
                    decl_and_assign.push(mk().local_stmt(Box::new(local)));

                    Ok(cfg::DeclStmtInfo::new(
                        vec![mk().local_stmt(Box::new(local_mut))],
                        assign_stmts,
                        decl_and_assign,
                    ))
                }
            }

            ref decl => {
                let inserted = if let Some(ident) = decl.get_name() {
                    self.renamer.borrow_mut().insert(decl_id, ident).is_some()
                } else {
                    false
                };

                // TODO: We need this because we can have multiple 'extern' decls of the same variable.
                //       When we do, we must make sure to insert into the renamer the first time, and
                //       then skip subsequent times.
                use CDeclKind::*;
                let skip = match decl {
                    Variable { .. } => !inserted,
                    Struct { .. } => true,
                    Union { .. } => true,
                    Enum { .. } => true,
                    Typedef { .. } => true,
                    _ => false,
                };

                if skip {
                    Ok(cfg::DeclStmtInfo::new(vec![], vec![], vec![]))
                } else {
                    use ConvertedDecl::*;
                    let items = match self.convert_decl(ctx, decl_id)? {
                        Item(item) => vec![item],
                        ForeignItem(item) => {
                            vec![mk().extern_("C").foreign_items(vec![*item])]
                        }
                        Items(items) => items,
                        NoItem => return Ok(cfg::DeclStmtInfo::empty()),
                    };

                    let item_stmt = |item| mk().item_stmt(item);
                    Ok(cfg::DeclStmtInfo::new(
                        items.iter().cloned().map(item_stmt).collect(),
                        vec![],
                        items.into_iter().map(item_stmt).collect(),
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

        use CTypeKind::*;
        match self.ast_context.resolve_type(ctypeid).kind {
            Pointer(CQualTypeId { ctype, .. }) => {
                match self.ast_context.resolve_type(ctype).kind {
                    Function(..) => {
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
            Bool | Char | SChar | Short | Int | Long | LongLong | UChar | UShort | UInt | ULong
            | ULongLong | LongDouble | Int128 | UInt128 => initializer.is_none(),
            Float | Double => initializer.is_none(),
            Struct(_) | Union(_) | Enum(_) => false,
            Function(..) => unreachable!("Can't have a function directly as a type"),
            Typedef(_) => unreachable!("Typedef should be expanded though resolve_type"),
            _ => true,
        }
    }

    fn convert_variable(
        &self,
        ctx: ExprContext,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
    ) -> TranslationResult<ConvertedVariable> {
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

        Ok(ConvertedVariable { ty, mutbl, init })
    }

    fn convert_type(&self, type_id: CTypeId) -> TranslationResult<Box<Type>> {
        if let Some(cur_file) = *self.cur_file.borrow() {
            self.import_type(type_id, cur_file);
        }
        self.type_converter
            .borrow_mut()
            .convert(&self.ast_context, type_id)
    }

    /// Construct an expression for a NULL at any type, including forward declarations,
    /// function pointers, and normal pointers.
    fn null_ptr(&self, type_id: CTypeId, is_static: bool) -> TranslationResult<Box<Expr>> {
        if self.ast_context.is_function_pointer(type_id) {
            return Ok(mk().path_expr(vec!["None"]));
        }

        let pointee = match self.ast_context.resolve_type(type_id).kind {
            CTypeKind::Pointer(pointee) => pointee,
            _ => return Err(TranslationError::generic("null_ptr requires a pointer")),
        };
        let ty = self.convert_type(type_id)?;
        let mut zero = mk().lit_expr(mk().int_unsuffixed_lit(0));
        if is_static && !pointee.qualifiers.is_const {
            let mut qtype = pointee;
            qtype.qualifiers.is_const = true;
            let ty_ = self
                .type_converter
                .borrow_mut()
                .convert_pointer(&self.ast_context, qtype)?;
            zero = mk().cast_expr(zero, ty_);
        }
        Ok(mk().cast_expr(zero, ty))
    }

    fn addr_lhs(
        &self,
        lhs: Box<Expr>,
        lhs_type: CQualTypeId,
        write: bool,
    ) -> TranslationResult<Box<Expr>> {
        let mutbl = if write {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        let addr_lhs = match *lhs {
            Expr::Unary(ExprUnary {
                op: UnOp::Deref(_),
                expr: e,
                ..
            }) => {
                if write == lhs_type.qualifiers.is_const {
                    let lhs_type = self.convert_type(lhs_type.ctype)?;
                    let ty = mk().set_mutbl(mutbl).ptr_ty(lhs_type);

                    mk().cast_expr(e, ty)
                } else {
                    e
                }
            }
            _ => {
                let addr_lhs = mk().set_mutbl(mutbl).addr_of_expr(lhs);

                let lhs_type = self.convert_type(lhs_type.ctype)?;
                let ty = mk().set_mutbl(mutbl).ptr_ty(lhs_type);

                mk().cast_expr(addr_lhs, ty)
            }
        };
        Ok(addr_lhs)
    }

    /// Write to a `lhs` that is volatile
    pub fn volatile_write(
        &self,
        lhs: Box<Expr>,
        lhs_type: CQualTypeId,
        rhs: Box<Expr>,
    ) -> TranslationResult<Box<Expr>> {
        let addr_lhs = self.addr_lhs(lhs, lhs_type, true)?;

        Ok(mk().call_expr(
            mk().abs_path_expr(vec!["core", "ptr", "write_volatile"]),
            vec![addr_lhs, rhs],
        ))
    }

    /// Read from a `lhs` that is volatile
    pub fn volatile_read(
        &self,
        lhs: Box<Expr>,
        lhs_type: CQualTypeId,
    ) -> TranslationResult<Box<Expr>> {
        let addr_lhs = self.addr_lhs(lhs, lhs_type, false)?;

        // We explicitly annotate the type of pointer we're reading from
        // in order to avoid omitted bit-casts to const from causing the
        // wrong type to be inferred via the result of the pointer.
        let mut path_parts: Vec<PathSegment> = vec![];
        for elt in ["core", "ptr"] {
            path_parts.push(mk().path_segment(elt))
        }
        let elt_ty = self.convert_type(lhs_type.ctype)?;
        let ty_params = mk().angle_bracketed_args(vec![elt_ty]);
        let elt = mk().path_segment_with_args("read_volatile", ty_params);
        path_parts.push(elt);

        let read_volatile_expr = mk().abs_path_expr(path_parts);
        Ok(mk().call_expr(read_volatile_expr, vec![addr_lhs]))
    }

    // Compute the offset multiplier for variable length array indexing
    // Rust type: usize
    pub fn compute_size_of_expr(&self, type_id: CTypeId) -> Option<Box<Expr>> {
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
                    Some(esize) => mk().binary_expr(BinOp::Mul(Default::default()), csize, esize),
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
    ) -> TranslationResult<Vec<Stmt>> {
        let mut stmts = vec![];

        loop {
            match self.ast_context.resolve_type(type_id).kind {
                CTypeKind::Pointer(elt) => type_id = elt.ctype,
                CTypeKind::ConstantArray(elt, _) => type_id = elt,
                CTypeKind::VariableArray(elt, Some(expr_id)) => {
                    type_id = elt;

                    // Convert this expression
                    let expr = self.convert_expr(ctx.used(), expr_id)?.and_then(|expr| {
                        let name = self
                            .renamer
                            .borrow_mut()
                            .insert(CDeclId(expr_id.0), "vla")
                            .unwrap(); // try using declref name?
                                       // TODO: store the name corresponding to expr_id

                        let local = mk().local(
                            mk().ident_pat(name),
                            None,
                            Some(mk().cast_expr(expr, mk().path_ty(vec!["usize"]))),
                        );

                        let res: TranslationResult<WithStmts<()>> =
                            Ok(WithStmts::new(vec![mk().local_stmt(Box::new(local))], ()));
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
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if let CTypeKind::VariableArray(elts, len) = self.ast_context.resolve_type(type_id).kind {
            let len = len.expect("Sizeof a VLA type with count expression omitted");

            let elts = self.compute_size_of_type(ctx, elts)?;
            return elts.and_then(|lhs| {
                let len = self.convert_expr(ctx.used().not_static(), len)?;
                Ok(len.map(|len| {
                    let rhs = cast_int(len, "usize", true);
                    mk().binary_expr(BinOp::Mul(Default::default()), lhs, rhs)
                }))
            });
        }
        let ty = self.convert_type(type_id)?;
        self.compute_size_of_ty(ty)
    }

    fn compute_size_of_ty(&self, ty: Box<Type>) -> TranslationResult<WithStmts<Box<Expr>>> {
        let name = "size_of";
        let params = mk().angle_bracketed_args(vec![ty]);
        let path = vec![
            mk().path_segment("core"),
            mk().path_segment("mem"),
            mk().path_segment_with_args(name, params),
        ];
        let call = mk().call_expr(mk().abs_path_expr(path), vec![]);

        Ok(WithStmts::new_val(call))
    }

    pub fn compute_align_of_type(
        &self,
        mut type_id: CTypeId,
        preferred: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        type_id = self.variable_array_base_type(type_id);

        let ty = self.convert_type(type_id)?;
        let tys = vec![ty];
        let mut path = vec![mk().path_segment("core")];
        if preferred {
            self.use_feature("core_intrinsics");
            path.push(mk().path_segment("intrinsics"));
            path.push(mk().path_segment_with_args("pref_align_of", mk().angle_bracketed_args(tys)));
        } else {
            path.push(mk().path_segment("mem"));
            path.push(mk().path_segment_with_args("align_of", mk().angle_bracketed_args(tys)));
        }
        let call = mk().call_expr(mk().abs_path_expr(path), vec![]);
        Ok(WithStmts::new_val(call))
    }

    // Fixing this would require major refactors for marginal benefit.
    #[allow(clippy::vec_box)]
    fn convert_exprs(
        &self,
        ctx: ExprContext,
        exprs: &[CExprId],
    ) -> TranslationResult<WithStmts<Vec<Box<Expr>>>> {
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
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let Located {
            loc: src_loc,
            kind: expr_kind,
        } = &self.ast_context[expr_id];

        trace!(
            "Converting expr {:?}: {:?}",
            expr_id,
            self.ast_context[expr_id]
        );

        if self.tcfg.translate_const_macros {
            if let Some(converted) = self.convert_macro_expansion(ctx, expr_id)? {
                return Ok(converted);
            }
        }

        if self.tcfg.translate_fn_macros {
            let text = self.ast_context.macro_expansion_text.get(&expr_id);
            if let Some(converted) = text.and_then(|text| self.convert_macro_invocation(ctx, text))
            {
                return Ok(converted);
            }
        }

        use CExprKind::*;
        match *expr_kind {
            DesignatedInitExpr(..) => {
                Err(TranslationError::generic("Unexpected designated init expr"))
            }
            BadExpr => Err(TranslationError::generic(
                "convert_expr: expression kind not supported",
            )),
            ShuffleVector(_, ref child_expr_ids) => self
                .convert_shuffle_vector(ctx, child_expr_ids)
                .map_err(|e| {
                    TranslationError::new(
                        self.ast_context.display_loc(src_loc),
                        e.context(TranslationErrorKind::OldLLVMSimd),
                    )
                }),
            ConvertVector(..) => Err(TranslationError::generic("convert vector not supported")),

            UnaryType(_ty, kind, opt_expr, arg_ty) => {
                let result = match kind {
                    UnTypeOp::SizeOf => match opt_expr {
                        None => self.compute_size_of_type(ctx, arg_ty.ctype)?,
                        Some(_) => {
                            let inner = self.variable_array_base_type(arg_ty.ctype);
                            let inner_size = self.compute_size_of_type(ctx, inner)?;

                            if let Some(sz) = self.compute_size_of_expr(arg_ty.ctype) {
                                inner_size.map(|x| {
                                    mk().binary_expr(BinOp::Mul(Default::default()), sz, x)
                                })
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

            ConstantExpr(_ty, child, value) => {
                if let Some(constant) = value {
                    self.convert_constant(constant).map(WithStmts::new_val)
                } else {
                    self.convert_expr(ctx, child)
                }
            }

            DeclRef(qual_ty, decl_id, lrvalue) => {
                let decl = &self
                    .ast_context
                    .get_decl(&decl_id)
                    .ok_or_else(|| format_err!("Missing declref {:?}", decl_id))?
                    .kind;
                if ctx.is_const {
                    if let CDeclKind::Variable {
                        has_static_duration: true,
                        ..
                    } = decl
                    {
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
                    val = self.volatile_read(val, qual_ty)?;
                }

                // If the variable is actually an `EnumConstant`, we need to add a cast to the
                // expected integral type. When modifying this, look at `Translation::enum_cast` -
                // this function assumes `DeclRef`'s to `EnumConstants`'s will translate to casts.
                if let &CDeclKind::EnumConstant { .. } = decl {
                    let ty = self.convert_type(qual_ty.ctype)?;
                    val = mk().cast_expr(val, ty);
                }

                // Most references to the va_list should refer to the VaList
                // type, not VaListImpl
                if !ctx.expecting_valistimpl && self.ast_context.is_va_list(qual_ty.ctype) {
                    val = mk().method_call_expr(val, "as_va_list", Vec::new());
                }

                // If we are referring to a function and need its address, we
                // need to cast it to fn() to ensure that it has a real address.
                let mut set_unsafe = false;
                if ctx.needs_address() {
                    if let &CDeclKind::Function { ref parameters, .. } = decl {
                        let ty = self.convert_type(qual_ty.ctype)?;
                        let actual_ty = self
                            .type_converter
                            .borrow_mut()
                            .knr_function_type_with_parameters(
                                &self.ast_context,
                                qual_ty.ctype,
                                parameters,
                            )?;
                        if let Some(actual_ty) = actual_ty {
                            // If we're casting a concrete function to
                            // a K&R function pointer type, use transmute
                            if let Some(cur_file) = *self.cur_file.borrow() {
                                self.import_type(qual_ty.ctype, cur_file);
                            }
                            val = transmute_expr(actual_ty, ty, val);
                            set_unsafe = true;
                        } else {
                            val = mk().cast_expr(val, ty);
                        }
                    }
                }

                if let CTypeKind::VariableArray(..) =
                    self.ast_context.resolve_type(qual_ty.ctype).kind
                {
                    val = mk().method_call_expr(val, "as_mut_ptr", vec![]);
                }

                let mut res = WithStmts::new_val(val);
                res.merge_unsafe(set_unsafe);
                Ok(res)
            }

            OffsetOf(ty, ref kind) => match kind {
                OffsetOfKind::Constant(val) => Ok(WithStmts::new_val(self.mk_int_lit(
                    ty,
                    *val,
                    IntBase::Dec,
                )?)),
                OffsetOfKind::Variable(qty, field_id, expr_id) => {
                    self.use_crate(ExternCrate::Memoffset);

                    // Struct Type
                    let decl_id = {
                        let kind = match self.ast_context[qty.ctype].kind {
                            CTypeKind::Elaborated(ty_id) => &self.ast_context[ty_id].kind,
                            ref kind => kind,
                        };

                        kind.as_decl_or_typedef()
                            .expect("Did not find decl_id for offsetof struct")
                    };
                    let name = self.resolve_decl_inner_name(decl_id);
                    let ty_ident = mk().ident(name);

                    // Field name
                    let field_name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(None, *field_id)
                        .expect("Did not find name for offsetof struct field");
                    let field_ident = mk().ident(field_name);

                    // Index Expr
                    let expr = self
                        .convert_expr(ctx, *expr_id)?
                        .to_pure_expr()
                        .ok_or_else(|| {
                            format_err!("Expected Variable offsetof to be a side-effect free")
                        })?;
                    let expr = mk().cast_expr(expr, mk().ident_ty("usize"));
                    use syn::__private::ToTokens;
                    let index_expr = expr.to_token_stream();

                    // offset_of!(Struct, field[expr as usize]) as ty
                    let macro_body = vec![
                        TokenTree::Ident(ty_ident),
                        TokenTree::Punct(Punct::new(',', Alone)),
                        TokenTree::Ident(field_ident),
                        TokenTree::Group(proc_macro2::Group::new(
                            proc_macro2::Delimiter::Bracket,
                            index_expr,
                        )),
                    ];
                    let path = mk().path("offset_of");
                    let mac = mk().mac_expr(mk().mac(
                        path,
                        macro_body,
                        MacroDelimiter::Paren(Default::default()),
                    ));

                    // Cast type
                    let cast_ty = self.convert_type(ty.ctype)?;
                    let cast_expr = mk().cast_expr(mac, cast_ty);

                    Ok(WithStmts::new_val(cast_expr))
                }
            },

            Literal(ty, ref kind) => self.convert_literal(ctx, ty, kind),

            ImplicitCast(ty, expr, kind, opt_field_id, _)
            | ExplicitCast(ty, expr, kind, opt_field_id, _) => {
                let is_explicit = matches!(expr_kind, CExprKind::ExplicitCast(..));
                // A reference must be decayed if a bitcast is required. Const casts in
                // LLVM 8 are now NoOp casts, so we need to include it as well.
                match kind {
                    CastKind::BitCast | CastKind::PointerToIntegral | CastKind::NoOp => {
                        ctx.decay_ref = DecayRef::Yes
                    }
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
                self.convert_cast(
                    ctx,
                    source_ty,
                    ty,
                    val,
                    Some(expr),
                    Some(kind),
                    opt_field_id,
                )
            }

            Unary(type_id, op, arg, lrvalue) => {
                self.convert_unary_operator(ctx, op, type_id, arg, lrvalue)
            }

            Conditional(_, cond, lhs, rhs) => {
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
                    let then = mk().block(lhs.into_stmts());
                    let else_ = mk().block_expr(mk().block(rhs.into_stmts()));

                    let mut res = cond.and_then(|c| -> TranslationResult<_> {
                        Ok(WithStmts::new(
                            vec![mk().semi_stmt(mk().ifte_expr(c, then, Some(else_)))],
                            self.panic_or_err("Conditional expression is not supposed to be used"),
                        ))
                    })?;
                    res.merge_unsafe(is_unsafe);
                    Ok(res)
                } else {
                    let then = lhs.to_block();
                    let else_ = rhs.to_expr();

                    Ok(cond.map(|c| {
                        let ifte_expr = mk().ifte_expr(c, then, Some(else_));

                        if ctx.ternary_needs_parens {
                            mk().paren_expr(ifte_expr)
                        } else {
                            ifte_expr
                        }
                    }))
                }
            }

            BinaryConditional(ty, lhs, rhs) => {
                if ctx.is_unused() {
                    let mut lhs = self.convert_condition(ctx, false, lhs)?;
                    let rhs = self.convert_expr(ctx, rhs)?;
                    lhs.merge_unsafe(rhs.is_unsafe());

                    lhs.and_then(|val| {
                        Ok(WithStmts::new(
                            vec![mk().semi_stmt(mk().ifte_expr(
                                val,
                                mk().block(rhs.into_stmts()),
                                None,
                            ))],
                            self.panic_or_err(
                                "Binary conditional expression is not supposed to be used",
                            ),
                        ))
                    })
                } else {
                    self.name_reference_write_read(ctx, lhs)?.result_map(
                        |NamedReference {
                             rvalue: lhs_val, ..
                         }| {
                            let cond = self.match_bool(true, ty.ctype, lhs_val.clone());
                            let ite = mk().ifte_expr(
                                cond,
                                mk().block(vec![mk().expr_stmt(lhs_val)]),
                                Some(self.convert_expr(ctx, rhs)?.to_expr()),
                            );
                            Ok(ite)
                        },
                    )
                }
            }

            Binary(type_id, op, lhs, rhs, opt_lhs_type_id, opt_res_type_id) => self
                .convert_binary_expr(ctx, type_id, op, lhs, rhs, opt_lhs_type_id, opt_res_type_id)
                .map_err(|e| e.add_loc(self.ast_context.display_loc(src_loc))),

            ArraySubscript(_, ref lhs, ref rhs, _) => {
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
                            &CExprKind::ImplicitCast(
                                _,
                                arr,
                                CastKind::ArrayToPointerDecay,
                                _,
                                _,
                            ) => {
                                match self.ast_context[arr].kind {
                                    CExprKind::Member(_, _, field_decl, _, _)
                                        if self
                                            .potential_flexible_array_members
                                            .borrow()
                                            .contains(&field_decl) =>
                                    {
                                        None
                                    }
                                    ref kind => {
                                        let arr_type = kind
                                            .get_type()
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
                                let mul = self.compute_size_of_expr(elt_type_id);
                                pointer_offset(lhs, rhs, mul, false, true)
                            } else {
                                mk().index_expr(lhs, cast_int(rhs, "usize", false))
                            }
                        }))
                    } else {
                        // LHS must be ref decayed for the offset method call's self param
                        let lhs = self.convert_expr(ctx.used().decay_ref(), *lhs)?;
                        lhs.result_map(|lhs| {
                            // stmts.extend(lhs.stmts_mut());
                            // is_unsafe = is_unsafe || lhs.is_unsafe();

                            let lhs_type_id = lhs_node
                                .get_type()
                                .ok_or_else(|| format_err!("bad lhs type"))?;

                            // Determine the type of element being indexed
                            let pointee_type_id =
                                match self.ast_context.resolve_type(lhs_type_id).kind {
                                    CTypeKind::Pointer(pointee_id) => pointee_id,
                                    _ => {
                                        return Err(format_err!(
                                            "Subscript applied to non-pointer: {:?}",
                                            lhs
                                        )
                                        .into());
                                    }
                                };

                            let mul = self.compute_size_of_expr(pointee_type_id.ctype);
                            Ok(pointer_offset(lhs, rhs, mul, false, true))
                        })
                    }
                })
            }

            Call(call_expr_ty, func, ref args) => {
                let fn_ty =
                    self.ast_context
                        .get_pointee_qual_type(
                            self.ast_context[func].kind.get_type().ok_or_else(|| {
                                format_err!("Invalid callee expression {:?}", func)
                            })?,
                        )
                        .map(|ty| &self.ast_context.resolve_type(ty.ctype).kind);
                let is_variadic = match fn_ty {
                    Some(CTypeKind::Function(_, _, is_variadic, _, _)) => *is_variadic,
                    _ => false,
                };
                let func = match self.ast_context[func].kind {
                    // Direct function call
                    CExprKind::ImplicitCast(_, fexp, CastKind::FunctionToPointerDecay, _, _)
                    // Only a direct function call with pointer decay if the
                    // callee is a declref
                    if matches!(self.ast_context[fexp].kind, CExprKind::DeclRef(..)) =>
                        {
                            self.convert_expr(ctx.used(), fexp)?
                        }

                    // Builtin function call
                    CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) => {
                        return self.convert_builtin(ctx, fexp, args);
                    }

                    // Function pointer call
                    _ => {
                        let callee = self.convert_expr(ctx.used(), func)?;
                        let make_fn_ty = |ret_ty: Box<Type>| {
                            let ret_ty = match *ret_ty {
                                Type::Tuple(TypeTuple { elems: ref v, .. }) if v.is_empty() => ReturnType::Default,
                                _ => ReturnType::Type(Default::default(), ret_ty),
                            };
                            let bare_ty = (
                                vec![mk().bare_arg(mk().infer_ty(), None::<Box<Ident>>); args.len()],
                                None::<Variadic>,
                                ret_ty
                            );
                            mk().barefn_ty(bare_ty)
                        };
                        match fn_ty {
                            Some(CTypeKind::Function(ret_ty, _, _, _, false)) => {
                                // K&R function pointer without arguments
                                let ret_ty = self.convert_type(ret_ty.ctype)?;
                                let target_ty = make_fn_ty(ret_ty);
                                callee.map(|fn_ptr| {
                                    let fn_ptr = unwrap_function_pointer(fn_ptr);
                                    transmute_expr(mk().infer_ty(), target_ty, fn_ptr)
                                })
                            }
                            None => {
                                // We have to infer the return type from our expression type
                                let ret_ty = self.convert_type(call_expr_ty.ctype)?;
                                let target_ty = make_fn_ty(ret_ty);
                                callee.map(|fn_ptr| {
                                    transmute_expr(mk().infer_ty(), target_ty, fn_ptr)
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

                    let args = self.convert_exprs(ctx.used(), args)?;

                    let res: TranslationResult<_> = Ok(args.map(|args| mk().call_expr(func, args)));
                    res
                })?;

                self.convert_side_effects_expr(
                    ctx,
                    call,
                    "Function call expression is not supposed to be used",
                )
            }

            Member(qual_ty, expr, decl, kind, _) => {
                if ctx.is_unused() {
                    self.convert_expr(ctx, expr)
                } else {
                    let mut val = match kind {
                        MemberKind::Dot => self.convert_expr(ctx, expr)?,
                        MemberKind::Arrow => {
                            if let CExprKind::Unary(_, c_ast::UnOp::AddressOf, subexpr_id, _) =
                                self.ast_context[expr].kind
                            {
                                // Special-case the `(&x)->field` pattern
                                // Convert it directly into `x.field`
                                self.convert_expr(ctx, subexpr_id)?
                            } else {
                                let val = self.convert_expr(ctx, expr)?;
                                val.map(|v| mk().unary_expr(UnOp::Deref(Default::default()), v))
                            }
                        }
                    };

                    let record_id = self.ast_context.parents[&decl];
                    if self.ast_context.has_inner_struct_decl(record_id) {
                        // The structure is split into an outer and an inner,
                        // so we need to go through the outer structure to the inner one
                        val = val.map(|v| mk().anon_field_expr(v, 0));
                    };

                    let field_name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(None, decl)
                        .unwrap();
                    let is_bitfield = match &self.ast_context[decl].kind {
                        CDeclKind::Field { bitfield_width, .. } => bitfield_width.is_some(),
                        _ => unreachable!("Found a member which is not a field"),
                    };
                    if is_bitfield {
                        // Convert a bitfield member one of four ways:
                        // A) bf.a()
                        // B) (*bf).a()
                        // C) bf
                        // D) (*bf)
                        //
                        // The first two are when we know this bitfield member is going to be read
                        // from (default), possibly requiring a dereference first. The latter two
                        // are generated when we are expecting to require a write, which will need
                        // to make a method call with some input which we do not yet have access
                        // to and will have to be handled elsewhere, IE `bf.set_a(1)`
                        if !ctx.is_bitfield_write {
                            // Cases A and B above
                            val = val.map(|v| mk().method_call_expr(v, field_name, vec![]));
                        }
                    } else {
                        val = val.map(|v| mk().field_expr(v, field_name));
                    };

                    // Most references to the va_list should refer to the VaList
                    // type, not VaListImpl
                    if !ctx.expecting_valistimpl && self.ast_context.is_va_list(qual_ty.ctype) {
                        val = val.map(|v| {
                            mk().method_call_expr(v, "as_va_list", Vec::<Box<Expr>>::new())
                        });
                    }

                    Ok(val)
                }
            }

            Paren(_, val) => self.convert_expr(ctx, val),

            CompoundLiteral(_, val) => self.convert_expr(ctx, val),

            InitList(ty, ref ids, opt_union_field_id, _) => {
                self.convert_init_list(ctx, ty, ids, opt_union_field_id)
            }

            ImplicitValueInit(ty) => self.implicit_default_expr(ty.ctype, ctx.is_static),

            Predefined(_, val_id) => self.convert_expr(ctx, val_id),

            Statements(_, compound_stmt_id) => {
                self.convert_statement_expression(ctx, compound_stmt_id)
            }

            VAArg(ty, val_id) => self.convert_vaarg(ctx, ty, val_id),

            Choose(_, _cond, lhs, rhs, is_cond_true) => {
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

            Atomic {
                ref name,
                ptr,
                order,
                val1,
                order_fail,
                val2,
                weak,
                ..
            } => self.convert_atomic(ctx, name, ptr, order, val1, order_fail, val2, weak),
        }
    }

    pub fn convert_constant(&self, constant: ConstIntExpr) -> TranslationResult<Box<Expr>> {
        let expr = match constant {
            ConstIntExpr::U(n) => mk().lit_expr(mk().int_unsuffixed_lit(n as u128)),

            ConstIntExpr::I(n) if n >= 0 => mk().lit_expr(mk().int_unsuffixed_lit(n as u128)),

            ConstIntExpr::I(n) => mk().unary_expr(
                UnOp::Neg(Default::default()),
                mk().lit_expr(mk().int_unsuffixed_lit(n.unsigned_abs() as u128)),
            ),
        };
        Ok(expr)
    }

    fn convert_macro_expansion(
        &self,
        ctx: ExprContext,
        expr_id: CExprId,
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        if let Some(macs) = self.ast_context.macro_invocations.get(&expr_id) {
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
                let expansion = self.macro_expansions.borrow().get(macro_id).cloned();
                let macro_ty = match expansion {
                    // expansion exists
                    Some(Some(expansion)) => expansion.ty,

                    // expansion wasn't possible
                    Some(None) => return Ok(None),

                    // We haven't tried to expand it yet
                    None => {
                        self.convert_decl(ctx, *macro_id)?;
                        if let Some(Some(expansion)) = self.macro_expansions.borrow().get(macro_id)
                        {
                            expansion.ty
                        } else {
                            return Ok(None);
                        }
                    }
                };
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
                    return self
                        .convert_cast(
                            ctx,
                            CQualTypeId::new(macro_ty),
                            expr_ty,
                            val,
                            None,
                            None,
                            None,
                        )
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

    fn convert_macro_invocation(
        &self,
        _ctx: ExprContext,
        text: &str,
    ) -> Option<WithStmts<Box<Expr>>> {
        let mut split = text.splitn(2, '(');
        let ident = split.next()?;
        let args = split.next()?.trim_end_matches(')');

        let ts: TokenStream = syn::parse_str(args).ok()?;
        Some(WithStmts::new_val(mk().mac_expr(mk().mac(
            mk().path(ident),
            ts,
            MacroDelimiter::Paren(Default::default()),
        ))))
    }

    /// If `ctx` is unused, convert `expr` to a semi statement, otherwise return
    /// `expr`.
    fn convert_side_effects_expr(
        &self,
        ctx: ExprContext,
        expr: WithStmts<Box<Expr>>,
        panic_msg: &str,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
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
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        fn as_semi_break_stmt(stmt: &Stmt, lbl: &cfg::Label) -> Option<Option<Box<Expr>>> {
            if let Stmt::Semi(
                Expr::Break(ExprBreak {
                    label: Some(blbl),
                    expr: ret_val,
                    ..
                }),
                _,
            ) = stmt
            {
                if blbl.ident == mk().label(lbl.pretty_print()).name.ident {
                    return Some(ret_val.clone());
                }
            }
            None
        }

        match self.ast_context[compound_stmt_id].kind {
            CStmtKind::Compound(ref substmt_ids) if !substmt_ids.is_empty() => {
                let n = substmt_ids.len();
                let result_id = substmt_ids[n - 1];

                let name = format!("<stmt-expr_{:?}>", compound_stmt_id);
                let lbl = cfg::Label::FromC(compound_stmt_id, None);

                let mut stmts = match self.ast_context[result_id].kind {
                    CStmtKind::Expr(expr_id) => {
                        let ret = cfg::ImplicitReturnType::StmtExpr(ctx, expr_id, lbl.clone());
                        self.convert_function_body(ctx, &name, &substmt_ids[0..(n - 1)], ret)?
                    }

                    _ => self.convert_function_body(
                        ctx,
                        &name,
                        substmt_ids,
                        cfg::ImplicitReturnType::Void,
                    )?,
                };

                if let Some(stmt) = stmts.pop() {
                    match as_semi_break_stmt(&stmt, &lbl) {
                        Some(val) => {
                            let block = mk().block_expr(match val {
                                None => mk().block(stmts),
                                Some(val) => WithStmts::new(stmts, val).to_block(),
                            });

                            // enclose block in parentheses to work around
                            // https://github.com/rust-lang/rust/issues/54482
                            let val = mk().paren_expr(block);
                            let stmts = if ctx.is_unused() {
                                vec![mk().semi_stmt(val.clone())]
                            } else {
                                Vec::new()
                            };

                            return Ok(WithStmts::new(stmts, val));
                        }
                        _ => {
                            self.use_feature("label_break_value");
                            stmts.push(stmt)
                        }
                    }
                }

                let block_body = mk().block(stmts.clone());
                let val: Box<Expr> = mk().labelled_block_expr(block_body, lbl.pretty_print());

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
        val: WithStmts<Box<Expr>>,
        expr: Option<CExprId>,
        kind: Option<CastKind>,
        opt_field_id: Option<CFieldId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let source_ty_kind = &self.ast_context.resolve_type(source_ty.ctype).kind;
        let target_ty_kind = &self.ast_context.resolve_type(ty.ctype).kind;

        if source_ty_kind == target_ty_kind {
            return Ok(val);
        }

        let kind = kind.unwrap_or_else(|| {
            match (source_ty_kind, target_ty_kind) {
                (CTypeKind::VariableArray(..), CTypeKind::Pointer(..))
                | (CTypeKind::ConstantArray(..), CTypeKind::Pointer(..))
                | (CTypeKind::IncompleteArray(..), CTypeKind::Pointer(..)) => {
                    CastKind::ArrayToPointerDecay
                }

                (CTypeKind::Function(..), CTypeKind::Pointer(..)) => {
                    CastKind::FunctionToPointerDecay
                }

                (_, CTypeKind::Pointer(..)) if source_ty_kind.is_integral_type() => {
                    CastKind::IntegralToPointer
                }

                (CTypeKind::Pointer(..), CTypeKind::Bool) => CastKind::PointerToBoolean,

                (CTypeKind::Pointer(..), _) if target_ty_kind.is_integral_type() => {
                    CastKind::PointerToIntegral
                }

                (_, CTypeKind::Bool) if source_ty_kind.is_integral_type() => {
                    CastKind::IntegralToBoolean
                }

                (CTypeKind::Bool, _) if target_ty_kind.is_signed_integral_type() => {
                    CastKind::BooleanToSignedIntegral
                }

                (_, _)
                    if source_ty_kind.is_integral_type() && target_ty_kind.is_integral_type() =>
                {
                    CastKind::IntegralCast
                }

                (_, _)
                    if source_ty_kind.is_integral_type() && target_ty_kind.is_floating_type() =>
                {
                    CastKind::IntegralToFloating
                }

                (_, CTypeKind::Bool) if source_ty_kind.is_floating_type() => {
                    CastKind::FloatingToBoolean
                }

                (_, _)
                    if source_ty_kind.is_floating_type() && target_ty_kind.is_integral_type() =>
                {
                    CastKind::FloatingToIntegral
                }

                (_, _)
                    if source_ty_kind.is_floating_type() && target_ty_kind.is_floating_type() =>
                {
                    CastKind::FloatingCast
                }

                (CTypeKind::Pointer(..), CTypeKind::Pointer(..)) => CastKind::BitCast,

                // Ignoring Complex casts for now
                _ => {
                    warn!(
                        "Unknown CastKind for {:?} to {:?} cast. Defaulting to BitCast",
                        source_ty_kind, target_ty_kind,
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
                        let source_ty = self.convert_type(source_ty.ctype)?;
                        let target_ty = self.convert_type(ty.ctype)?;
                        Ok(WithStmts::new_unsafe_val(transmute_expr(
                            source_ty, target_ty, x,
                        )))
                    } else {
                        // Normal case
                        let target_ty = self.convert_type(ty.ctype)?;
                        Ok(WithStmts::new_val(mk().cast_expr(x, target_ty)))
                    }
                })
            }

            CastKind::IntegralToPointer if self.ast_context.is_function_pointer(ty.ctype) => {
                let target_ty = self.convert_type(ty.ctype)?;
                val.and_then(|x| {
                    let intptr_t = mk().path_ty(vec!["libc", "intptr_t"]);
                    let intptr = mk().cast_expr(x, intptr_t.clone());
                    Ok(WithStmts::new_unsafe_val(transmute_expr(
                        intptr_t, target_ty, intptr,
                    )))
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
                    self.use_crate(ExternCrate::F128);

                    let fn_path = mk().path_expr(vec!["f128", "f128", "new"]);
                    Ok(val.map(|val| mk().call_expr(fn_path, vec![val])))
                } else if let CTypeKind::LongDouble = self.ast_context[source_ty_ctype_id].kind {
                    self.f128_cast_to(val, target_ty_ctype)
                } else if let &CTypeKind::Enum(enum_decl_id) = target_ty_ctype {
                    // Casts targeting `enum` types...
                    let expr =
                        expr.ok_or_else(|| format_err!("Casts to enums require a C ExprId"))?;
                    Ok(self.enum_cast(ty.ctype, enum_decl_id, expr, val, source_ty, target_ty))
                } else if target_ty_ctype.is_floating_type() && source_ty_kind.is_bool() {
                    val.and_then(|x| {
                        Ok(WithStmts::new_val(mk().cast_expr(
                            mk().cast_expr(x, mk().path_ty(vec!["u8"])),
                            target_ty,
                        )))
                    })
                } else {
                    // Other numeric casts translate to Rust `as` casts,
                    // unless the cast is to a function pointer then use `transmute`.
                    val.and_then(|x| {
                        if self.ast_context.is_function_pointer(source_ty_ctype_id) {
                            Ok(WithStmts::new_unsafe_val(transmute_expr(
                                source_ty, target_ty, x,
                            )))
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
                // Because va_list is sometimes defined as a single-element
                // array in order for it to allocate memory as a local variable
                // and to be a pointer as a function argument we would get
                // spurious casts when trying to treat it like a VaList which
                // has reference semantics.
                if self.ast_context.is_va_list(ty.ctype) {
                    return Ok(val);
                }

                let pointee = match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::Pointer(pointee) => pointee,
                    _ => panic!("Dereferencing a non-pointer"),
                };

                let is_const = pointee.qualifiers.is_const;

                let expr_kind = expr.map(|e| &self.ast_context.index(e).kind);
                match expr_kind {
                    Some(&CExprKind::Literal(_, CLiteral::String(ref bytes, 1))) if is_const => {
                        let target_ty = self.convert_type(ty.ctype)?;

                        let mut bytes = bytes.to_owned();
                        bytes.push(0);
                        let byte_literal = mk().lit_expr(bytes);
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

                            let call = val.map(|x| mk().method_call_expr(x, method, vec![]));

                            // Static arrays can now use as_ptr. Can also cast that const ptr to a
                            // mutable pointer as we do here:
                            if ctx.is_static && !is_const {
                                return Ok(call.map(|val| {
                                    let inferred_type = mk().infer_ty();
                                    let ptr_type = mk().mutbl().ptr_ty(inferred_type);
                                    mk().cast_expr(val, ptr_type)
                                }));
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

    /// Cast a f128 to some other int or float type
    fn f128_cast_to(
        &self,
        val: WithStmts<Box<Expr>>,
        target_ty_ctype: &CTypeKind,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        self.use_crate(ExternCrate::NumTraits);

        self.with_cur_file_item_store(|item_store| {
            item_store.add_use(vec!["num_traits".into()], "ToPrimitive");
        });
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
                .into());
            }
        };

        Ok(val.map(|val| {
            let to_call = mk().method_call_expr(val, to_method_name, Vec::new());

            mk().method_call_expr(to_call, "unwrap", Vec::new())
        }))
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
        val: WithStmts<Box<Expr>>, // translated Rust argument to cast
        _source_ty: Box<Type>, // source type of cast
        target_ty: Box<Type>, // target type of cast
    ) -> WithStmts<Box<Expr>> {
        // Extract the IDs of the `EnumConstant` decls underlying the enum.
        let variants = match self.ast_context.index(enum_decl).kind {
            CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration", enum_decl),
        };

        match self.ast_context.index(expr).kind {
            // This is the case of finding a variable which is an `EnumConstant` of the same enum
            // we are casting to. Here, we can just remove the extraneous cast instead of generating
            // a new one.
            CExprKind::DeclRef(_, decl_id, _) if variants.contains(&decl_id) => {
                return val.map(|x| match *unparen(&x) {
                    Expr::Cast(ExprCast { ref expr, .. }) => expr.clone(),
                    _ => panic!("DeclRef {:?} of enum {:?} is not cast", expr, enum_decl),
                });
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
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let resolved_ty_id = self.ast_context.resolve_type_id(ty_id);
        let resolved_ty = &self.ast_context.index(resolved_ty_id).kind;

        if self.ast_context.is_va_list(resolved_ty_id) {
            // generate MaybeUninit::uninit().assume_init()
            let path = vec!["core", "mem", "MaybeUninit", "uninit"];
            let call = mk().call_expr(mk().abs_path_expr(path), vec![]);
            let call = mk().method_call_expr(call, "assume_init", vec![]);
            return Ok(WithStmts::new_val(call));
        }

        if resolved_ty.is_bool() {
            Ok(WithStmts::new_val(mk().lit_expr(mk().bool_lit(false))))
        } else if resolved_ty.is_integral_type() {
            Ok(WithStmts::new_val(
                mk().lit_expr(mk().int_unsuffixed_lit(0)),
            ))
        } else if resolved_ty.is_floating_type() {
            match self.ast_context[ty_id].kind {
                CTypeKind::LongDouble => Ok(WithStmts::new_val(
                    mk().path_expr(vec!["f128", "f128", "ZERO"]),
                )),
                _ => Ok(WithStmts::new_val(
                    mk().lit_expr(mk().float_unsuffixed_lit("0.")),
                )),
            }
        } else if let &CTypeKind::Pointer(_) = resolved_ty {
            self.null_ptr(resolved_ty_id, is_static)
                .map(WithStmts::new_val)
        } else if let &CTypeKind::ConstantArray(elt, sz) = resolved_ty {
            let sz = mk().lit_expr(mk().int_unsuffixed_lit(sz as u128));
            Ok(self
                .implicit_default_expr(elt, is_static)?
                .map(|elt| mk().repeat_expr(elt, sz)))
        } else if let &CTypeKind::IncompleteArray(_) = resolved_ty {
            // Incomplete arrays are translated to zero length arrays
            Ok(WithStmts::new_val(mk().array_expr(vec![])))
        } else if let Some(decl_id) = resolved_ty.as_underlying_decl() {
            self.zero_initializer(decl_id, ty_id, is_static)
        } else if let &CTypeKind::VariableArray(elt, _) = resolved_ty {
            // Variable length arrays unnested and implemented as a flat array of the underlying
            // element type.

            // Find base element type of potentially nested arrays
            let inner = self.variable_array_base_type(elt);
            let count = self.compute_size_of_expr(ty_id).unwrap();
            Ok(self
                .implicit_default_expr(inner, is_static)?
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
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if let Some(file_id) = self.cur_file.borrow().as_ref() {
            self.import_type(type_id, *file_id);
        }

        // Look up the decl in the cache and return what we find (if we find anything)
        if let Some(init) = self.zero_inits.borrow().get(&decl_id) {
            return Ok(init.clone());
        }

        let name_decl_id = match self.ast_context.index(type_id).kind {
            CTypeKind::Typedef(decl_id) => decl_id,
            _ => decl_id,
        };

        // Otherwise, construct the initializer
        let mut init = match self.ast_context.index(decl_id).kind {
            // Zero initialize all of the fields
            CDeclKind::Struct {
                fields: Some(ref fields),
                platform_byte_size,
                ..
            } => {
                let name = self.resolve_decl_inner_name(name_decl_id);
                self.convert_struct_zero_initializer(
                    name,
                    decl_id,
                    fields,
                    platform_byte_size,
                    is_static,
                )?
            }

            CDeclKind::Struct { fields: None, .. } => {
                return Err(TranslationError::generic(
                    "Attempted to zero-initialize forward-declared struct",
                ));
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
                        ));
                    }
                };

                let &field_id = fields
                    .first()
                    .ok_or_else(|| format_err!("A union should have a field"))?;

                let field = match self.ast_context.index(field_id).kind {
                    CDeclKind::Field { typ, .. } => self
                        .implicit_default_expr(typ.ctype, is_static)?
                        .map(|field_init| {
                            let name = self
                                .type_converter
                                .borrow()
                                .resolve_field_name(Some(decl_id), field_id)
                                .unwrap();

                            mk().field(name, field_init)
                        }),
                    _ => {
                        return Err(TranslationError::generic(
                            "Found non-field in record field list",
                        ));
                    }
                };

                field.map(|field| mk().struct_expr(vec![name], vec![field]))
            }

            // Transmute the number `0` into the enum type
            CDeclKind::Enum { .. } => WithStmts::new_val(self.enum_for_i64(type_id, 0)),

            _ => {
                return Err(TranslationError::generic(
                    "Declaration is not associated with a type",
                ));
            }
        };

        if self.ast_context.has_inner_struct_decl(name_decl_id) {
            // If the structure is split into an outer/inner,
            // wrap the inner initializer using the outer structure
            let outer_name = self
                .type_converter
                .borrow()
                .resolve_decl_name(name_decl_id)
                .unwrap();

            let outer_path = mk().path_expr(vec![outer_name]);
            init = init.map(|i| mk().call_expr(outer_path, vec![i]));
        };

        if init.is_pure() {
            // Insert the initializer into the cache, then return it
            self.zero_inits.borrow_mut().insert(decl_id, init.clone());
            Ok(init)
        } else {
            Err(TranslationError::generic(
                "Expected no statements in zero initializer",
            ))
        }
    }

    /// Resolve the inner name of a structure declaration
    /// if there is one (if the structure was split),
    /// otherwise just return the normal name
    fn resolve_decl_inner_name(&self, decl_id: CDeclId) -> String {
        if self.ast_context.has_inner_struct_decl(decl_id) {
            self.type_converter
                .borrow_mut()
                .resolve_decl_suffix_name(decl_id, INNER_SUFFIX)
                .to_owned()
        } else {
            self.type_converter
                .borrow()
                .resolve_decl_name(decl_id)
                .unwrap()
        }
    }

    /// Convert a boolean expression to a boolean for use in && or || or if
    fn match_bool(&self, target: bool, ty_id: CTypeId, val: Box<Expr>) -> Box<Expr> {
        let ty = &self.ast_context.resolve_type(ty_id).kind;

        if self.ast_context.is_function_pointer(ty_id) {
            if target {
                mk().method_call_expr(val, "is_some", vec![])
            } else {
                mk().method_call_expr(val, "is_none", vec![])
            }
        } else if ty.is_pointer() {
            let mut res = mk().method_call_expr(val, "is_null", vec![]);
            if target {
                res = mk().unary_expr(UnOp::Not(Default::default()), res)
            }
            res
        } else if ty.is_bool() {
            if target {
                val
            } else {
                mk().unary_expr(UnOp::Not(Default::default()), val)
            }
        } else {
            // One simplification we can make at the cost of inspecting `val` more closely: if `val`
            // is already in the form `(x <op> y) as <ty>` where `<op>` is a Rust operator
            // that returns a boolean, we can simple output `x <op> y` or `!(x <op> y)`.
            if let Expr::Cast(ExprCast { expr: ref arg, .. }) = *unparen(&val) {
                if let Expr::Binary(ExprBinary { op, .. }) = *unparen(arg) {
                    match op {
                        BinOp::Or(_)
                        | BinOp::And(_)
                        | BinOp::Eq(_)
                        | BinOp::Ne(_)
                        | BinOp::Lt(_)
                        | BinOp::Le(_)
                        | BinOp::Gt(_)
                        | BinOp::Ge(_) => {
                            if target {
                                // If target == true, just return the argument
                                return Box::new(unparen(arg).clone());
                            } else {
                                // If target == false, return !arg
                                return mk().unary_expr(
                                    UnOp::Not(Default::default()),
                                    Box::new(unparen(arg).clone()),
                                );
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
            let zero = if ty.is_floating_type() {
                mk().lit_expr(mk().float_unsuffixed_lit("0."))
            } else {
                mk().lit_expr(mk().int_unsuffixed_lit(0))
            };

            if target {
                mk().binary_expr(BinOp::Ne(Default::default()), val, zero)
            } else {
                mk().binary_expr(BinOp::Eq(Default::default()), val, zero)
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
    fn insert_item(&self, mut item: Box<Item>, decl: &CDecl) {
        let decl_file_id = self.ast_context.file_id(decl);

        if self.tcfg.reorganize_definitions {
            self.use_feature("register_tool");
            let attrs = item_attrs(&mut item).expect("no attrs field on unexpected item variant");
            add_src_loc_attr(attrs, &decl.loc.as_ref().map(|x| x.begin()));
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
            self.use_feature("register_tool");
            let attrs = foreign_item_attrs(&mut item)
                .expect("no attrs field on unexpected foreign item variant");
            add_src_loc_attr(attrs, &decl.loc.as_ref().map(|x| x.begin()));
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
            || decl_file_id == self.main_file
        {
            return;
        }

        // TODO: get rid of this, not compatible with nested modules
        let mut module_path = vec!["super".into()];

        // If the decl does not live in the main module add the path to the sibling submodule
        if let Some(file_id) = import_file_id {
            if file_id != self.main_file {
                let file_name =
                    clean_path(&self.mod_names, self.ast_context.get_file_path(file_id));

                module_path.push(file_name);
            }
        }

        self.items
            .borrow_mut()
            .entry(decl_file_id)
            .or_insert(ItemStore::new())
            .add_use(module_path, ident_name);
    }

    fn import_type(&self, ctype: CTypeId, decl_file_id: FileId) {
        use self::CTypeKind::*;

        let type_kind = &self.ast_context[ctype].kind;
        match type_kind {
            // libc can be accessed from anywhere as of Rust 2019 by full path
            Void | Char | SChar | UChar | Short | UShort | Int | UInt | Long | ULong | LongLong
            | ULongLong | Int128 | UInt128 | Half | BFloat16 | Float | Double | LongDouble => {}
            // Bool uses the bool type, so no dependency on libc
            Bool => {}
            Paren(ctype)
            | Decayed(ctype)
            | IncompleteArray(ctype)
            | ConstantArray(ctype, _)
            | Elaborated(ctype)
            | Pointer(CQualTypeId { ctype, .. })
            | Attributed(CQualTypeId { ctype, .. }, _)
            | VariableArray(ctype, _)
            | Reference(CQualTypeId { ctype, .. })
            | BlockPointer(CQualTypeId { ctype, .. })
            | TypeOf(ctype)
            | Complex(ctype) => self.import_type(*ctype, decl_file_id),
            Enum(decl_id) | Typedef(decl_id) | Union(decl_id) | Struct(decl_id) => {
                let mut decl_id = *decl_id;
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
                let type_kind = &self.ast_context[*ctype].kind;

                // Rust doesn't use void for return type, so skip
                if *type_kind != Void {
                    self.import_type(*ctype, decl_file_id);
                }

                // Param Types
                for param_id in params {
                    self.import_type(param_id.ctype, decl_file_id);
                }
            }
            Vector(..) => {
                // Handled in `import_simd_typedef`
            }
            TypeOfExpr(_) | BuiltinFn => {}
            UnhandledSveType => {
                // TODO: handle SVE types
            }
        }
    }

    fn generate_submodule_imports(&self, decl_id: CDeclId, decl_file_id: Option<FileId>) {
        let decl_file_id = decl_file_id.expect("There should be a decl file path");
        let decl = self.ast_context.get_decl(&decl_id).unwrap();

        match decl.kind {
            CDeclKind::Struct { ref fields, .. } | CDeclKind::Union { ref fields, .. } => {
                let field_ids = fields.as_ref().map(|vec| vec.as_slice()).unwrap_or(&[]);

                for field_id in field_ids.iter() {
                    match self.ast_context[*field_id].kind {
                        CDeclKind::Field { typ, .. } => self.import_type(typ.ctype, decl_file_id),
                        _ => unreachable!("Found something in a struct other than a field"),
                    }
                }
            }
            CDeclKind::EnumConstant { .. } | CDeclKind::Enum { .. } => {}

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
            | CDeclKind::Typedef { typ, .. } => self.import_type(typ.ctype, decl_file_id),
            CDeclKind::Function {
                is_global: true,
                typ,
                ..
            } => self.import_type(typ, decl_file_id),

            CDeclKind::MacroObject { .. } => {
                if let Some(Some(expansion)) = self.macro_expansions.borrow().get(&decl_id) {
                    self.import_type(expansion.ty, decl_file_id)
                }
            }

            CDeclKind::Function { .. } | CDeclKind::MacroFunction { .. } => {
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
