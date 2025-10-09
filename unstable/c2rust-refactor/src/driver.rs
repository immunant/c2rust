//! Frontend logic for parsing and expanding ASTs.  This code largely mimics the behavior of
//! `rustc_driver::run_compiler`.

use rustc_ast::ast;
use rustc_ast::ptr::P;
use rustc_ast::token::{self, TokenKind};
use rustc_ast::tokenstream::TokenTree;
use rustc_ast::DUMMY_NODE_ID;
use rustc_ast::{
    AssocItem, Block, BlockCheckMode, Expr, ForeignItem, Item, ItemKind, NodeId, Param, Pat, Stmt,
    Ty, UnsafeSource,
};
use rustc_codegen_ssa::traits::CodegenBackend;
use rustc_data_structures::fx::FxHashMap;
use rustc_data_structures::sync::Lrc;
use rustc_driver;
use rustc_errors::PResult;
use rustc_errors::{DiagnosticBuilder, ErrorGuaranteed};
use rustc_index::vec::IndexVec;
use rustc_interface::interface;
use rustc_interface::util::{get_codegen_backend, run_in_thread_pool_with_globals};
use rustc_interface::{util, Config};
use rustc_lint::LintStore;
use rustc_middle::hir::map as hir_map;
use rustc_middle::ty;
use rustc_parse::parser::attr::InnerAttrPolicy;
use rustc_parse::parser::{AttemptLocalParseRecovery, ForceCollect, Parser};
use rustc_session::config::Input;
use rustc_session::config::Options as SessionOptions;
use rustc_session::{self, DiagnosticOutput, Session};
use rustc_span::def_id::LocalDefId;
use rustc_span::edition::Edition;
use rustc_span::hygiene::SyntaxContext;
use rustc_span::source_map::SourceMap;
use rustc_span::source_map::{FileLoader, RealFileLoader};
use rustc_span::symbol::{kw, Symbol};
use rustc_span::SourceFileHashAlgorithm;
use rustc_span::{FileName, Span, DUMMY_SP};
use std::collections::HashSet;
use std::mem;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use crate::ast_manip::remove_paren;
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
        map: hir_map::Map<'tcx>,
        node_id_to_def_id: FxHashMap<NodeId, LocalDefId>,
        def_id_to_node_id: IndexVec<LocalDefId, NodeId>,
        tcx: GenerationalTyCtxt<'tcx>,
    ) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(
            sess,
            Some(HirMap::new(
                max_node_id,
                map,
                node_id_to_def_id,
                def_id_to_node_id,
            )),
            Some(tcx),
        )
    }
}

// /// Various driver bits that we have lying around at the end of `phase_1_parse_input`.  This is
// /// everything we need to (re-)run the compiler from phase 1 onward.
// pub struct Phase1Bits {
//     session: Session,
//     cstore: CStore,
//     codegen_backend: Box<CodegenBackend>,
//     input: Input,
//     output: Option<PathBuf>,
//     out_dir: Option<PathBuf>,
//     control: CompileController<'static>,
//     krate: Crate,
// }

// impl Phase1Bits {
//     /// Set up the compiler again, using a previously-constructed `Session`.
//     ///
//     /// A `Crate` is mostly self-contained, but its `Span`s are really indexes into external
//     /// tables.  So if you actually plan to run the compiler after calling `reset()`, the new
//     /// `krate` passed here should satisfy a few properties:
//     ///
//     ///  1. The crate must have been parsed under the same `SourceMap` used by `session`.  Spans'
//     ///     `hi` and `lo` byte positions are indices into the `SourceMap` used for parsing, so
//     ///     transferring those spans to a different `SourceMap` produces nonsensical results.
//     ///
//     ///  2. The crate must not contain any paths starting with `$crate` from a non-empty
//     ///     `SyntaxCtxt`.  These types of paths appear during macro expansion, and can only be
//     ///     resolved using tables populated by the macro expander.
//     ///
//     ///  3. All `NodeId`s in the crate must be DUMMY_NODE_ID.
//     ///
//     ///  4. The crate must not contain automatically-injected `extern crate` declarations.  The
//     ///     compilation process will inject new copies of these, and then fail due to the name
//     ///     collision.
//     ///
//     /// A crate that has only been compiled to `Phase1` already satisfies points 2-4.  If you want
//     /// to re-compile a crate from `Phase2` or later, use `recheck::prepare_recheck` to fix things
//     /// up first.
//     pub fn from_session_and_crate(old_session: &Session, krate: Crate) -> Phase1Bits {
//         let (session, cstore, codegen_backend) = rebuild_session(old_session);

//         let in_path = old_session.local_crate_source_file.clone();
//         let input = Input::File(in_path.unwrap());

//         let mut control = CompileController::basic();
//         control.provide = Box::new(move |providers| {
//             use rustc_hir::def_id::CrateNum;
//             use rustc_middle::privacy::AccessLevels;
//             use rustc_data_structures::sync::Lrc;
//             use rustc_privacy;

//             fn privacy_access_levels<'tcx>(tcx: TyCtxt<'_, 'tcx, 'tcx>,
//                                            krate: CrateNum) -> Lrc<AccessLevels> {
//                 // Get and call the original implementation, resetting the error count before
//                 // returning so that `abort_if_errors` won't abort.
//                 // NOTE: It's possible in theory for the codegen_backend to override the
//                 // implementation, since `codegen_backend.provide` runs after `default_provide`.
//                 // We wouldn't handle that since we call only `rustc_privacy::provide` here.
//                 // Privacy checking would be a weird thing for a backend to override, though.
//                 let mut p = Providers::default();
//                 rustc_privacy::provide(&mut p);
//                 let r = (p.privacy_access_levels)(tcx, krate);
//                 tcx.sess.diagnostic().reset_err_count();
//                 r
//             }
//             providers.privacy_access_levels = privacy_access_levels;

//             // TODO: provide error-resetting versions of other "query + `abort_if_errors`" passes
//             // in `phase_3_run_analysis_passes`.
//         });

//         Phase1Bits {
//             session, cstore, codegen_backend,

//             input,
//             output: None,
//             out_dir: None,

//             control, krate,
//         }
//     }

//     /// Set up the compiler using a previously-created session, repeating phase 1 (input parsing).
//     pub fn from_session_reparse(old_session: &Session) -> Phase1Bits {
//         let (session, cstore, codegen_backend) = rebuild_session(old_session);

//         let in_path = old_session.local_crate_source_file.clone();
//         let input = Input::File(in_path.unwrap());

//         let control = CompileController::basic();

//         // Start of `compile_input` code
//         let krate = driver::phase_1_parse_input(&control, &session, &input).unwrap();

//         Phase1Bits {
//             session, cstore, codegen_backend,

//             input,
//             output: None,
//             out_dir: None,

//             control, krate,
//         }
//     }

//     pub fn into_crate(self) -> Crate {
//         self.krate
//     }
// }

/// Sysroot adjustment: if the sysroot is unset, and args[0] is an absolute path, use args[0] to
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
        // TODO: do we need more than the defaults here?
        // CheckCfg does not implement Default
        crate_check_cfg: Default::default(),
        input,
        input_path: config.input_path.clone(),
        output_file: config.output_file.clone(),
        output_dir: config.output_dir.clone(),
        file_loader: None,
        diagnostic_output: DiagnosticOutput::Default,
        lint_caps: config.lint_caps.clone(),
        parse_sess_created: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: config.registry.clone(),
    }
}

pub fn create_config(args: &[String]) -> interface::Config {
    let matches = rustc_driver::handle_options(args).expect("rustc arg parsing failed");
    let sopts = rustc_session::config::build_session_options(&matches);
    let cfg = interface::parse_cfgspecs(matches.opt_strs("cfg"));
    let check_cfg = interface::parse_check_cfg(matches.opt_strs("check-cfg"));
    let sopts = maybe_set_sysroot(sopts, args);
    let output_dir = matches.opt_str("out-dir").map(|o| PathBuf::from(&o));
    let output_file = matches.opt_str("o").map(|o| PathBuf::from(&o));

    assert!(matches.free.len() == 1, "expected exactly one input file");
    let input_path = Some(Path::new(&matches.free[0]).to_owned());
    let input = Input::File(input_path.as_ref().unwrap().clone());

    interface::Config {
        opts: sopts,
        crate_cfg: cfg,
        crate_check_cfg: check_cfg,
        input,
        input_path,
        output_file,
        output_dir,
        file_loader: None,
        diagnostic_output: DiagnosticOutput::Default,
        lint_caps: Default::default(),
        parse_sess_created: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: rustc_driver::diagnostics_registry(),
    }
}

#[cfg_attr(feature = "profile", flame)]
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
    config.opts.edition = Edition::Edition2018;

    interface::run_compiler(config, f)
}

#[cfg_attr(feature = "profile", flame)]
pub fn run_refactoring<F, R>(
    mut config: interface::Config,
    cmd_reg: Registry,
    file_io: Arc<dyn FileIO + Sync + Send>,
    marks: HashSet<(NodeId, Symbol)>,
    f: F,
) -> R
where
    F: FnOnce(RefactorState) -> R + Send,
    R: Send,
{
    // Force disable incremental compilation.  It causes panics with multiple typechecking.
    config.opts.incremental = None;

    run_in_thread_pool_with_globals(Edition::Edition2018, 1, move || {
        let state = RefactorState::new(config, cmd_reg, file_io, marks);
        f(state)
    })
}

#[allow(dead_code)]
pub struct Compiler {
    pub sess: Lrc<Session>,
    pub codegen_backend: Lrc<Box<dyn CodegenBackend>>,
    input: Input,
    input_path: Option<PathBuf>,
    output_dir: Option<PathBuf>,
    output_file: Option<PathBuf>,
    temps_dir: Option<PathBuf>,
    register_lints: Option<Box<dyn Fn(&Session, &mut LintStore) + Send + Sync>>,
    override_queries:
        Option<fn(&Session, &mut ty::query::Providers, &mut ty::query::ExternProviders)>,
}

pub fn make_compiler(
    config: &Config,
    file_io: Arc<dyn FileIO + Sync + Send>,
) -> interface::Compiler {
    let mut config = clone_config(config);
    config.file_loader = Some(Box::new(ArcFileIO(file_io)));
    let (sess, codegen_backend) = util::create_session(
        config.opts,
        config.crate_cfg,
        config.crate_check_cfg,
        config.diagnostic_output,
        config.file_loader,
        config.input_path.clone(),
        config.lint_caps,
        config.make_codegen_backend,
        config.registry,
    );

    // Put a dummy file at the beginning of the source_map, so that no real `Span` will accidentally
    // collide with `DUMMY_SP` (which is `0 .. 0`).
    sess.source_map()
        .new_source_file(FileName::Custom("<dummy>".to_string()), " ".to_string());

    let temps_dir = sess
        .opts
        .unstable_opts
        .temps_dir
        .as_ref()
        .map(|o| PathBuf::from(&o));

    let compiler = Compiler {
        sess,
        codegen_backend,
        input: config.input,
        input_path: config.input_path,
        output_dir: config.output_dir,
        output_file: config.output_file,
        override_queries: config.override_queries,
        temps_dir,
        register_lints: config.register_lints,
    };

    let compiler = unsafe { mem::transmute(compiler) };
    compiler
}

// pub fn run_compiler_to_phase1(args: &[String],
//                               file_loader: Option<Box<FileLoader+Sync+Send>>) -> Phase1Bits {
//     let matches = rustc_driver::handle_options(args)
//         .expect("rustc arg parsing failed");
//     let (sopts, _cfg) = session::config::build_session_options_and_crate_config(&matches);
//     let sopts = maybe_set_sysroot(sopts, args);
//     let out_dir = matches.opt_str("out-dir").map(|o| PathBuf::from(&o));
//     let output = matches.opt_str("o").map(|o| PathBuf::from(&o));

//     assert!(matches.free.len() == 1,
//            "expected exactly one input file");
//     let in_path = Some(Path::new(&matches.free[0]).to_owned());
//     let input = Input::File(in_path.as_ref().unwrap().clone());

//     let (session, cstore, codegen_backend) = build_session(sopts, in_path, file_loader);

//     // It might seem tempting to set up a custom CompileController and invoke `compile_input` here,
//     // in order to avoid duplicating a bunch of `compile_input`'s logic.  Unfortunately, that
//     // doesn't work well with the current API.  The `CompileState`s provided to the PhaseController
//     // callbacks only contain the data relevant to th ecurrent  phase - for example, in the
//     // after_analysis callback, `tcx` is available but `krate`, `arena`, and `hir_map` are not.
//     // Furthermore, the callback type is such that the `CompileState`s for separate callbacks have
//     // unrelated lifetimes, so we can't (safely) collect up the relevant pieces ourselves from
//     // multiple callback invocations.

//     let control = CompileController::basic();

//     // Start of `compile_input` code
//     let krate = driver::phase_1_parse_input(&control, &session, &input).unwrap();

//     Phase1Bits {
//         session, cstore, codegen_backend,
//         input, output, out_dir,
//         control, krate,
//     }
// }

/// Run the compiler with some command line `args`.  Stops compiling and invokes the callback
/// `func` after the indicated `phase`.
///
/// `file_loader` can be `None` to read source code from the file system.  Otherwise, the provided
/// loader will be used within the compiler.  For example, editor integration uses a custom file
/// loader to provide the compiler with buffer contents for currently open files.
// pub fn run_compiler<F, R>(args: &[String],
//                           file_loader: Option<Box<FileLoader+Sync+Send>>,
//                           phase: Phase,
//                           func: F) -> R
//         where F: FnOnce(Crate, RefactorCtxt) -> R {
//     let bits = run_compiler_to_phase1(args, file_loader);
//     run_compiler_from_phase1(bits, phase, func)
// }

pub fn build_session_from_args(
    args: &[String],
    file_loader: Option<Box<dyn FileLoader + Sync + Send>>,
) -> Session {
    let matches = rustc_driver::handle_options(args).expect("rustc arg parsing failed");

    let sopts = rustc_session::config::build_session_options(&matches);
    let sopts = maybe_set_sysroot(sopts, args);

    assert!(matches.free.len() == 1, "expected exactly one input file");
    let in_path = Some(Path::new(&matches.free[0]).to_owned());

    let (session, _codegen_backend) = build_session(sopts, in_path, file_loader);
    session
}

fn build_session(
    sopts: SessionOptions,
    in_path: Option<PathBuf>,
    file_loader: Option<Box<dyn FileLoader + Sync + Send>>,
) -> (Session, Box<dyn CodegenBackend>) {
    // Corresponds roughly to `run_compiler`.
    let descriptions = rustc_driver::diagnostics_registry();
    let file_loader = file_loader.unwrap_or_else(|| Box::new(RealFileLoader));
    let hash_kind = sopts
        .unstable_opts
        .src_hash_algorithm
        .unwrap_or(SourceFileHashAlgorithm::Md5);
    // Note: `source_map` is expected to be an `Lrc<SourceMap>`, which is an alias for `Rc<SourceMap>`.
    // If this ever changes, we'll need a new trick to obtain the `SourceMap` in `rebuild_session`.
    let source_map = Rc::new(SourceMap::with_file_loader_and_hash_kind(
        file_loader,
        sopts.file_path_mapping(),
        hash_kind,
    ));
    // Put a dummy file at the beginning of the source_map, so that no real `Span` will accidentally
    // collide with `DUMMY_SP` (which is `0 .. 0`).
    source_map.new_source_file(FileName::Custom("<dummy>".to_string()), " ".to_string());

    let codegen_backend = get_codegen_backend(
        &sopts.maybe_sysroot,
        sopts
            .unstable_opts
            .codegen_backend
            .as_ref()
            .map(|name| &name[..]),
    );
    let target_override = codegen_backend.target_override(&sopts);
    let sess = rustc_session::build_session(
        sopts,
        in_path,
        None,
        descriptions,
        DiagnosticOutput::Default,
        Default::default(),
        None,
        target_override,
    );
    codegen_backend.init(&sess);

    (sess, codegen_backend)
}

fn make_parser<'a>(sess: &'a Session, src: &str) -> Parser<'a> {
    rustc_parse::new_parser_from_source_str(
        &sess.parse_sess,
        FileName::anon_source_code(src),
        src.to_owned(),
    )
}

pub fn emit_and_panic(mut db: DiagnosticBuilder<ErrorGuaranteed>, what: &str) -> ! {
    db.emit();
    panic!("error parsing {}", what);
}

// Helper functions for parsing source code in an existing `Session`.
#[cfg_attr(feature = "profile", flame)]
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

#[cfg_attr(feature = "profile", flame)]
pub fn parse_pat(sess: &Session, src: &str) -> P<Pat> {
    let mut p = make_parser(sess, src);
    // TODO: do we want to allow top-level or-patterns here?
    match p.parse_pat_no_top_alt(None) {
        Ok(mut pat) => {
            remove_paren(&mut pat);
            pat
        }
        Err(db) => emit_and_panic(db, "pat"),
    }
}

#[cfg_attr(feature = "profile", flame)]
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

#[cfg_attr(feature = "profile", flame)]
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

#[cfg_attr(feature = "profile", flame)]
pub fn parse_items(sess: &Session, src: &str) -> Vec<P<Item>> {
    let mut p = make_parser(sess, src);
    let mut items = Vec::new();
    loop {
        match p.parse_item(ForceCollect::No) {
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

#[cfg_attr(feature = "profile", flame)]
pub fn parse_impl_items(sess: &Session, src: &str) -> Vec<P<AssocItem>> {
    // TODO: rustc no longer exposes `parse_impl_item_`. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("impl ! {{ {} }}", src));
    match p.parse_item(ForceCollect::No) {
        Ok(item) => match item.expect("expected to find an item").into_inner().kind {
            ItemKind::Impl(box ast::Impl { items, .. }) => items,
            _ => panic!("expected to find an impl item"),
        },
        Err(db) => emit_and_panic(db, "impl items"),
    }
}

#[cfg_attr(feature = "profile", flame)]
pub fn parse_foreign_items(sess: &Session, src: &str) -> Vec<P<ForeignItem>> {
    // TODO: rustc no longer exposes a method for parsing ForeignItems. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("extern {{ {} }}", src));
    match p.parse_item(ForceCollect::No) {
        Ok(item) => match item.expect("expected to find an item").into_inner().kind {
            ItemKind::ForeignMod(fm) => fm.items,
            _ => panic!("expected to find a foreignmod item"),
        },
        Err(db) => emit_and_panic(db, "foreign items"),
    }
}

#[cfg_attr(feature = "profile", flame)]
pub fn parse_block(sess: &Session, src: &str) -> P<Block> {
    let mut p = make_parser(sess, src);

    let rules = if p.eat(&TokenKind::Ident(kw::Unsafe, false)) {
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
    const INNER_ATTR_FORBIDDEN: InnerAttrPolicy<'_> = InnerAttrPolicy::Forbidden {
        reason: "inner attributes not allowed in function arguments",
        saw_doc_comment: false,
        prev_outer_attr_sp: None,
    };

    let mut attrs: Vec<ast::Attribute> = Vec::new();
    while let token::Pound = p.token.kind {
        attrs.push(p.parse_attribute(INNER_ATTR_FORBIDDEN).unwrap());
    }
    let pat = p.parse_pat_no_top_alt(None)?;
    p.expect(&TokenKind::Colon)?;
    let ty = p.parse_ty()?;
    Ok(Param {
        attrs: attrs.into(),
        pat,
        ty,
        id: DUMMY_NODE_ID,
        span: DUMMY_SP,
        is_placeholder: false,
    })
}

#[cfg_attr(feature = "profile", flame)]
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

#[cfg_attr(feature = "profile", flame)]
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

#[cfg_attr(feature = "profile", flame)]
pub fn run_parser_tts<F, R>(sess: &Session, tts: Vec<TokenTree>, f: F) -> R
where
    F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R>,
{
    let mut p = rustc_parse::stream_to_parser(
        &sess.parse_sess,
        tts.into_iter().collect(),
        Some("c2rust-refactor parser"),
    );
    match f(&mut p) {
        Ok(x) => x,
        Err(db) => emit_and_panic(db, "tts"),
    }
}

#[cfg_attr(feature = "profile", flame)]
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

#[cfg_attr(feature = "profile", flame)]
pub fn try_run_parser_tts<F, R>(sess: &Session, tts: Vec<TokenTree>, f: F) -> Option<R>
where
    F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R>,
{
    let mut p = rustc_parse::stream_to_parser(
        &sess.parse_sess,
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
    Span::new(fm.start_pos, fm.end_pos, SyntaxContext::root(), None)
}
