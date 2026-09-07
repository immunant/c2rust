//! Frontend logic for parsing and expanding ASTs.  This code largely mimics the behavior of
//! `rustc_driver::run_compiler`.

use rustc_ast::ast;
use rustc_ast::node_id::NodeMap;
use rustc_ast::ptr::P;
use rustc_ast::token;
use rustc_ast::tokenstream::TokenTree;
use rustc_ast::DUMMY_NODE_ID;
use rustc_ast::{
    AssocItem, Block, BlockCheckMode, Expr, ForeignItem, Item, ItemKind, NodeId, Param, Pat, Stmt,
    Ty, UnsafeSource,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_data_structures::unord::UnordMap;
use rustc_driver;
use rustc_errors::PResult;
use rustc_errors::{Diag, ErrorGuaranteed};
use rustc_hir::def::{PartialRes, PerNS, Res};
use rustc_index::IndexVec;
use rustc_interface::interface;
use rustc_middle::ty;
use rustc_parse::exp;
use rustc_parse::parser::attr::InnerAttrPolicy;
use rustc_parse::parser::{AttemptLocalParseRecovery, ForceCollect, Parser};
use rustc_session::config::Input;
use rustc_session::config::Options as SessionOptions;
use rustc_session::{self, Session};
use rustc_span::def_id::LocalDefId;
use rustc_span::source_map::FileLoader;
use rustc_span::source_map::SourceMap;
use rustc_span::symbol::Symbol;
use rustc_span::SyntaxContext;
use rustc_span::{FileName, Span, DUMMY_SP};
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::ast_manip::{remove_paren, AstSpanMaps};
use crate::command::{GenerationalTyCtxt, RefactorState, Registry};
use crate::file_io::{ArcFileIO, FileIO};
// TODO: don't forget to call span_fix after parsing
// use crate::span_fix;
use crate::context::HirMap;
use crate::util::Lone;
use crate::RefactorCtxt;

/// Compiler phase selection.  Later phases have more analysis results available, but are less
/// robust against broken code.  (For example, phase 3 provides typechecking results, but can't be
/// used on code that doesn't typecheck.)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Phase {
    /// Phase 1: Runs on the source code immediately after parsing, before macro expansion.
    Phase1,
    /// Phase 2: Runs after macro expansion and name resolution have finished.
    Phase2,
    /// Phase 3: Runs after typechecking has finished.
    Phase3,
}

impl<'a, 'tcx: 'a> RefactorCtxt<'a, 'tcx> {
    pub fn new_phase_1(sess: &'a Session) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(sess, None, None)
    }

    pub fn new_phase_2_3(
        sess: &'a Session,
        max_node_id: NodeId,
        tcx_raw: ty::TyCtxt<'tcx>,
        partial_res_map: UnordMap<NodeId, PartialRes>,
        node_id_to_def_id: FxHashMap<NodeId, LocalDefId>,
        def_id_to_node_id: IndexVec<LocalDefId, NodeId>,
        import_res_map: NodeMap<PerNS<Option<Res<NodeId>>>>,
        tcx: GenerationalTyCtxt<'tcx>,
        span_maps: AstSpanMaps,
    ) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(
            sess,
            Some(HirMap::new(
                max_node_id,
                tcx_raw,
                partial_res_map,
                node_id_to_def_id,
                def_id_to_node_id,
                import_res_map,
                span_maps,
            )),
            Some(tcx),
        )
    }
}

/// Sysroot adjustment: if the sysroot is unset, and `args[0]` is an absolute path, use `args[0]` to
/// infer a sysroot.  Rustc's own sysroot detection (filesearch::get_or_default_sysroot) uses
/// env::current_exe, which will point to c2rust-refactor, not rustc.
fn maybe_set_sysroot(mut sopts: SessionOptions, args: &[String]) -> SessionOptions {
    if sopts.maybe_sysroot.is_none() && !args.is_empty() {
        let p = Path::new(&args[0]);
        if p.is_absolute() {
            if let Some(sysroot) = p.parent().and_then(|p| p.parent()) {
                sopts.maybe_sysroot = Some(sysroot.to_owned());
            }
        }
    }
    sopts
}

pub fn clone_config(config: &interface::Config) -> interface::Config {
    let input = match &config.input {
        Input::File(f) => Input::File(f.clone()),
        Input::Str { name, input } => Input::Str {
            name: name.clone(),
            input: input.clone(),
        },
    };
    interface::Config {
        opts: config.opts.clone(),
        crate_cfg: config.crate_cfg.clone(),
        crate_check_cfg: config.crate_check_cfg.clone(),
        input,
        output_file: config.output_file.clone(),
        output_dir: config.output_dir.clone(),
        file_loader: None,
        locale_resources: rustc_driver::DEFAULT_LOCALE_RESOURCES.to_vec(),
        lint_caps: config.lint_caps.clone(),
        psess_created: None,
        hash_untracked_state: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: config.registry.clone(),
        ice_file: config.ice_file.clone(),
        using_internal_features: config.using_internal_features.clone(),
        expanded_args: config.expanded_args.clone(),
    }
}

pub fn create_config(args: &[String]) -> interface::Config {
    let mut early_dcx = rustc_session::EarlyDiagCtxt::new(Default::default());
    // Both direct and Cargo-derived invocations include the executable. The
    // target handle_options API now expects only arguments; the old API
    // stripped argv[0] internally.
    let matches =
        rustc_driver::handle_options(&early_dcx, &args[1..]).expect("rustc arg parsing failed");
    let mut sopts = rustc_session::config::build_session_options(&mut early_dcx, &matches);
    // Print human readable error (the default).
    sopts.error_format = Default::default();
    let cfg = matches.opt_strs("cfg");
    let check_cfg = matches.opt_strs("check-cfg");
    let sopts = maybe_set_sysroot(sopts, args);
    let output_dir = matches.opt_str("out-dir").map(|o| PathBuf::from(&o));
    let output_file = matches
        .opt_str("o")
        .map(|o| rustc_session::config::OutFileName::Real(PathBuf::from(o)));

    assert!(
        matches.free.len() == 1,
        "expected exactly one input file, but found: {:?}",
        matches.free
    );
    let input = Input::File(Path::new(&matches.free[0]).to_owned());

    interface::Config {
        opts: sopts,
        crate_cfg: cfg,
        crate_check_cfg: check_cfg,
        input,
        output_file,
        output_dir,
        file_loader: None,
        locale_resources: rustc_driver::DEFAULT_LOCALE_RESOURCES.to_vec(),
        lint_caps: Default::default(),
        psess_created: None,
        hash_untracked_state: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: rustc_driver::diagnostics_registry(),
        ice_file: None,
        using_internal_features: Arc::new(std::sync::atomic::AtomicBool::new(false)),
        expanded_args: args.to_vec(),
    }
}

pub fn run_compiler<F, R>(
    mut config: interface::Config,
    file_loader: Option<Box<dyn FileLoader + Send + Sync>>,
    f: F,
) -> R
where
    F: FnOnce(&interface::Compiler) -> R + Send,
    R: Send,
{
    // Force disable incremental compilation.  It causes panics with multiple typechecking.
    config.opts.incremental = None;
    config.file_loader = file_loader;

    interface::run_compiler(config, f)
}

pub fn run_refactoring<F, R>(
    config: interface::Config,
    cmd_reg: Registry,
    file_io: Arc<dyn FileIO + Sync + Send>,
    marks: HashSet<(NodeId, String)>,
    f: F,
) -> R
where
    F: FnOnce(RefactorState<'_>) -> R + Send,
    R: Send,
{
    let compiler_config = clone_config(&config);
    let loader = Some(Box::new(ArcFileIO(file_io.clone())) as Box<dyn FileLoader + Send + Sync>);
    run_compiler(compiler_config, loader, move |compiler| {
        // Reserve byte zero so no real source span can collide with DUMMY_SP.
        compiler
            .sess
            .source_map()
            .new_source_file(FileName::Custom("<dummy>".to_string()), " ".to_string());
        // Symbol indices belong to these session globals. Only strings cross
        // compiler-session boundaries, including checkpoint/reload boundaries.
        let marks = marks
            .into_iter()
            .map(|(id, label)| (id, Symbol::intern(&label)))
            .collect();
        let state = RefactorState::new(config, compiler, cmd_reg, file_io, marks);
        let result = f(state);
        // Refactoring commands decide which source errors they can tolerate
        // (for example, autoretype accepts unchanged preexisting errors). The
        // old manually owned Compiler did not abort on those errors at drop.
        // Preserve that contract across run_compiler's new abort_if_errors at
        // callback exit, while still surfacing delayed compiler bugs. Fatal
        // errors and failed commands unwind before reaching this point.
        let _ = compiler.sess.dcx().emit_stashed_diagnostics();
        compiler.sess.dcx().flush_delayed();
        compiler.sess.dcx().reset_err_count();
        result
    })
}

fn make_parser<'a>(sess: &'a Session, src: &str) -> Parser<'a> {
    rustc_parse::unwrap_or_emit_fatal(rustc_parse::new_parser_from_source_str(
        &sess.psess,
        FileName::anon_source_code(src),
        src.to_owned(),
    ))
}

pub fn emit_and_panic(db: Diag<'_, ErrorGuaranteed>, what: &str) -> ! {
    db.emit();
    panic!("error parsing {}", what);
}

// Helper functions for parsing source code in an existing `Session`.
pub fn parse_expr(sess: &Session, src: &str) -> P<Expr> {
    let mut p = make_parser(sess, src);
    match p.parse_expr() {
        Ok(mut expr) => {
            remove_paren(&mut expr);
            expr
        }
        Err(db) => emit_and_panic(db, "expr"),
    }
}

pub fn parse_pat(sess: &Session, src: &str) -> P<Pat> {
    let mut p = make_parser(sess, src);
    // TODO: do we want to allow top-level or-patterns here?
    match p.parse_pat_no_top_alt(None, None) {
        Ok(mut pat) => {
            remove_paren(&mut pat);
            pat
        }
        Err(db) => emit_and_panic(db, "pat"),
    }
}

pub fn parse_ty(sess: &Session, src: &str) -> P<Ty> {
    let mut p = make_parser(sess, src);
    match p.parse_ty() {
        Ok(mut ty) => {
            remove_paren(&mut ty);
            ty
        }
        Err(db) => emit_and_panic(db, "ty"),
    }
}

pub fn parse_stmts(sess: &Session, src: &str) -> Vec<Stmt> {
    let mut p = make_parser(sess, src);
    let mut stmts = Vec::new();
    while p.token != token::Eof {
        match p.parse_full_stmt(AttemptLocalParseRecovery::Yes) {
            Ok(Some(mut stmt)) => {
                remove_paren(&mut stmt);
                stmts.push(stmt);
            }
            Ok(None) => break,
            Err(db) => emit_and_panic(db, "stmts"),
        }
    }
    stmts
}

pub fn parse_items(sess: &Session, src: &str) -> Vec<P<Item>> {
    let mut p = make_parser(sess, src);
    let mut items = Vec::new();
    loop {
        match p.parse_item(ForceCollect::Yes) {
            Ok(Some(mut item)) => {
                remove_paren(&mut item);
                items.push(item.lone());
            }
            Ok(None) => break,
            Err(db) => emit_and_panic(db, "items"),
        }
    }
    items
}

pub fn parse_impl_items(sess: &Session, src: &str) -> Vec<P<AssocItem>> {
    // TODO: rustc no longer exposes `parse_impl_item_`. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("impl ! {{ {} }}", src));
    match p.parse_item(ForceCollect::No) {
        Ok(item) => match item.expect("expected to find an item").into_inner().kind {
            ItemKind::Impl(box ast::Impl { items, .. }) => items.into_iter().collect(),
            _ => panic!("expected to find an impl item"),
        },
        Err(db) => emit_and_panic(db, "impl items"),
    }
}

pub fn parse_foreign_items(sess: &Session, src: &str) -> Vec<P<ForeignItem>> {
    // TODO: rustc no longer exposes a method for parsing ForeignItems. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("extern {{ {} }}", src));
    match p.parse_item(ForceCollect::No) {
        Ok(item) => match item.expect("expected to find an item").into_inner().kind {
            ItemKind::ForeignMod(fm) => fm.items.into_iter().collect(),
            _ => panic!("expected to find a foreignmod item"),
        },
        Err(db) => emit_and_panic(db, "foreign items"),
    }
}

pub fn parse_block(sess: &Session, src: &str) -> P<Block> {
    let mut p = make_parser(sess, src);

    let rules = if p.eat_keyword(exp!(Unsafe)) {
        BlockCheckMode::Unsafe(UnsafeSource::UserProvided)
    } else {
        BlockCheckMode::Default
    };

    match p.parse_expr().map(|e| e.into_inner().kind) {
        Ok(ast::ExprKind::Block(mut block, _)) => {
            remove_paren(&mut block);
            block.rules = rules;
            block
        }
        Ok(_) => panic!("expected to find a block item"),
        Err(db) => emit_and_panic(db, "block"),
    }
}

fn parse_arg_inner<'a>(p: &mut Parser<'a>) -> PResult<'a, Param> {
    // `parse_arg` is private, so we make do with `parse_attribute`,
    // `parse_pat`, & `parse_ty`.
    let mut attrs = ast::AttrVec::new();
    while let token::Pound = p.token.kind {
        attrs.push(p.parse_attribute(InnerAttrPolicy::Forbidden(None)).unwrap());
    }
    let pat = p.parse_pat_no_top_alt(None, None)?;
    p.expect(exp!(Colon))?;
    let ty = p.parse_ty()?;
    Ok(Param {
        attrs,
        pat,
        ty,
        id: DUMMY_NODE_ID,
        span: DUMMY_SP,
        is_placeholder: false,
    })
}

pub fn parse_arg(sess: &Session, src: &str) -> Param {
    let mut p = make_parser(sess, src);
    match parse_arg_inner(&mut p) {
        Ok(mut arg) => {
            remove_paren(&mut arg);
            arg
        }
        Err(db) => emit_and_panic(db, "arg"),
    }
}

pub fn run_parser<F, R>(sess: &Session, src: &str, f: F) -> R
where
    F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R>,
{
    let mut p = make_parser(sess, src);
    match f(&mut p) {
        Ok(x) => x,
        Err(db) => emit_and_panic(db, "src"),
    }
}

pub fn run_parser_tts<F, R>(sess: &Session, tts: Vec<TokenTree>, f: F) -> R
where
    F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R>,
{
    let mut p = Parser::new(
        &sess.psess,
        tts.into_iter().collect(),
        Some("c2rust-refactor parser"),
    );
    match f(&mut p) {
        Ok(x) => x,
        Err(db) => emit_and_panic(db, "tts"),
    }
}

pub fn try_run_parser<F, R>(sess: &Session, src: &str, f: F) -> Option<R>
where
    F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R>,
{
    let mut p = make_parser(sess, src);
    match f(&mut p) {
        Ok(x) => Some(x),
        Err(db) => {
            db.cancel();
            None
        }
    }
}

pub fn try_run_parser_tts<F, R>(sess: &Session, tts: Vec<TokenTree>, f: F) -> Option<R>
where
    F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R>,
{
    let mut p = Parser::new(
        &sess.psess,
        tts.into_iter().collect(),
        Some("c2rust-refactor parser"),
    );
    match f(&mut p) {
        Ok(x) => Some(x),
        Err(db) => {
            db.cancel();
            None
        }
    }
}

/// Create a span whose text is `s`.  Note this is somewhat expensive, as it adds a new dummy file
/// to the `SourceMap` on every call.
pub fn make_span_for_text(cm: &SourceMap, s: &str) -> Span {
    let fm = cm.new_source_file(FileName::anon_source_code(s), s.to_string());
    Span::new(fm.start_pos, fm.end_position(), SyntaxContext::root(), None)
}
