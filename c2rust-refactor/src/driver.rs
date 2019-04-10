//! Frontend logic for parsing and expanding ASTs.  This code largely mimics the behavior of
//! `rustc_driver::driver::compile_input`.

use rustc::dep_graph::DepGraph;
use rustc::hir::map as hir_map;
use rustc::session::config::Options as SessionOptions;
use rustc::session::config::{Input, OutputFilenames};
use rustc::session::{self, DiagnosticOutput, Session};
use rustc::ty::steal::Steal;
use rustc::ty::{self, GlobalCtxt, Resolutions, TyCtxt};
use rustc::util::common::ErrorReported;
use rustc_codegen_utils::codegen_backend::CodegenBackend;
use rustc_data_structures::declare_box_region_type;
use rustc_data_structures::sync::{Lock, Lrc};
use rustc_driver;
use rustc_errors::DiagnosticBuilder;
use rustc_incremental::DepGraphFuture;
use rustc_interface::interface;
use rustc_interface::interface::BoxedResolver;
use rustc_interface::util::get_codegen_backend;
use rustc_interface::{util, Config};
use rustc_metadata::cstore::CStore;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashSet;
use std::mem;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::mpsc;
use std::sync::Arc;
use syntax::ast;
use syntax::ast::DUMMY_NODE_ID;
use syntax::ast::{
    Arg, Block, BlockCheckMode, Expr, ForeignItem, ImplItem, Item, ItemKind, NodeId, Pat, Stmt, Ty,
    UnsafeSource,
};
use syntax::ext::base::NamedSyntaxExtension;
use syntax::ext::hygiene::SyntaxContext;
use syntax::feature_gate::AttributeType;
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::parse::{self, PResult};
use syntax::ptr::P;
use syntax::source_map::SourceMap;
use syntax::source_map::{FileLoader, RealFileLoader};
use syntax::symbol::{keywords, Symbol};
use syntax::tokenstream::TokenTree;
use syntax_pos::FileName;
use syntax_pos::Span;

use crate::ast_manip::remove_paren;
use crate::command::{RefactorState, Registry};
use crate::file_io::{ArcFileIO, FileIO};
// TODO: don't forget to call span_fix after parsing
// use crate::span_fix;
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
    pub fn new_phase_1(sess: &'a Session, cstore: &'a CStore) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(sess, cstore, None, None)
    }

    pub fn new_phase_2(
        sess: &'a Session,
        cstore: &'a CStore,
        map: &'a hir_map::Map<'tcx>,
    ) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(sess, cstore, Some(map), None)
    }

    pub fn new_phase_3(
        sess: &'a Session,
        cstore: &'a CStore,
        map: &'a hir_map::Map<'tcx>,
        tcx: TyCtxt<'a, 'tcx, 'tcx>,
    ) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(sess, cstore, Some(map), Some(tcx))
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
//             use rustc::hir::def_id::CrateNum;
//             use rustc::middle::privacy::AccessLevels;
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
    if sopts.maybe_sysroot.is_none() && args.len() > 0 {
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
        input,
        input_path: config.input_path.clone(),
        output_file: config.output_file.clone(),
        output_dir: config.output_dir.clone(),
        file_loader: None,
        diagnostic_output: DiagnosticOutput::Default,
        stderr: config.stderr.clone(),
        crate_name: config.crate_name.clone(),
        lint_caps: config.lint_caps.clone(),
    }
}

pub fn create_config(args: &[String]) -> interface::Config {
    let matches = rustc_driver::handle_options(args).expect("rustc arg parsing failed");
    let (sopts, cfg) = session::config::build_session_options_and_crate_config(&matches);
    let sopts = maybe_set_sysroot(sopts, args);
    let output_dir = matches.opt_str("out-dir").map(|o| PathBuf::from(&o));
    let output_file = matches.opt_str("o").map(|o| PathBuf::from(&o));

    assert!(matches.free.len() == 1, "expected exactly one input file");
    let input_path = Some(Path::new(&matches.free[0]).to_owned());
    let input = Input::File(input_path.as_ref().unwrap().clone());

    interface::Config {
        opts: sopts,
        crate_cfg: cfg,
        input,
        input_path,
        output_file,
        output_dir,
        file_loader: None,
        diagnostic_output: DiagnosticOutput::Default,
        stderr: None,
        crate_name: None,
        lint_caps: Default::default(),
    }
}

pub fn run_compiler<F, R>(
    mut config: interface::Config,
    file_loader: Option<Box<dyn FileLoader + Send + Sync>>,
    f: F,
) -> R
where
    F: FnOnce(&interface::Compiler) -> R,
    R: Send,
{
    // Force disable incremental compilation.  It causes panics with multiple typechecking.
    config.opts.incremental = None;
    config.file_loader = file_loader;

    syntax::with_globals(move || {
        ty::tls::GCX_PTR.set(&Lock::new(0), || {
            ty::tls::with_thread_locals(|| {
                interface::run_compiler_in_existing_thread_pool(config, f)
            })
        })
    })
}

pub fn run_refactoring<F, R>(
    mut config: interface::Config,
    cmd_reg: Registry,
    file_io: Arc<FileIO + Sync + Send>,
    marks: HashSet<(NodeId, Symbol)>,
    f: F,
) -> R
where
    F: FnOnce(RefactorState) -> R,
    R: Send,
{
    // Force disable incremental compilation.  It causes panics with multiple typechecking.
    config.opts.incremental = None;

    syntax::with_globals(move || {
        ty::tls::GCX_PTR.set(&Lock::new(0), || {
            ty::tls::with_thread_locals(|| {
                let state = RefactorState::new(config, cmd_reg, file_io, marks);
                f(state)
            })
        })
    })
}

#[allow(dead_code)]
pub struct Compiler {
    pub sess: Lrc<Session>,
    pub codegen_backend: Lrc<Box<dyn CodegenBackend>>,
    source_map: Lrc<SourceMap>,
    input: Input,
    input_path: Option<PathBuf>,
    output_dir: Option<PathBuf>,
    output_file: Option<PathBuf>,
    queries: Queries,
    pub cstore: Lrc<CStore>,
    crate_name: Option<String>,
}

#[allow(dead_code)]
#[derive(Default)]
struct Queries {
    dep_graph_future: Query<Option<DepGraphFuture>>,
    parse: Query<ast::Crate>,
    crate_name: Query<String>,
    register_plugins: Query<(ast::Crate, PluginInfo)>,
    expansion: Query<(ast::Crate, Rc<Option<RefCell<BoxedResolver>>>)>,
    dep_graph: Query<DepGraph>,
    lower_to_hir: Query<(Steal<hir_map::Forest>, ExpansionResult)>,
    prepare_outputs: Query<OutputFilenames>,
    codegen_channel: Query<(
        Steal<mpsc::Sender<Box<dyn Any + Send>>>,
        Steal<mpsc::Receiver<Box<dyn Any + Send>>>,
    )>,
    global_ctxt: Query<BoxedGlobalCtxt>,
    ongoing_codegen: Query<Box<dyn Any>>,
    link: Query<()>,
}

#[allow(dead_code)]
struct Query<T> {
    result: RefCell<Option<Result<T, ErrorReported>>>,
}

impl<T> Default for Query<T> {
    fn default() -> Self {
        Query {
            result: RefCell::new(None),
        }
    }
}

#[allow(dead_code)]
struct PluginInfo {
    syntax_exts: Vec<NamedSyntaxExtension>,
    attributes: Vec<(String, AttributeType)>,
}

struct ExpansionResult {
    pub defs: Steal<hir_map::Definitions>,
    pub resolutions: Steal<Resolutions>,
}

declare_box_region_type!(
    pub BoxedGlobalCtxt,
    for('gcx),
    (&'gcx GlobalCtxt<'gcx>) -> ((), ())
);

pub fn make_compiler(config: &Config, file_io: Arc<FileIO + Sync + Send>) -> interface::Compiler {
    let mut config = clone_config(config);
    config.file_loader = Some(Box::new(ArcFileIO(file_io)));
    let (sess, codegen_backend, source_map) = util::create_session(
        config.opts,
        config.crate_cfg,
        config.diagnostic_output,
        config.file_loader,
        config.input_path.clone(),
        config.lint_caps,
    );

    // Put a dummy file at the beginning of the source_map, so that no real `Span` will accidentally
    // collide with `DUMMY_SP` (which is `0 .. 0`).
    source_map.new_source_file(FileName::Custom("<dummy>".to_string()), " ".to_string());

    let cstore = Lrc::new(CStore::new(codegen_backend.metadata_loader()));

    let compiler = Compiler {
        sess,
        codegen_backend,
        source_map,
        cstore,
        input: config.input,
        input_path: config.input_path,
        output_dir: config.output_dir,
        output_file: config.output_file,
        queries: Default::default(),
        crate_name: config.crate_name,
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
    file_loader: Option<Box<FileLoader + Sync + Send>>,
) -> Session {
    let matches = rustc_driver::handle_options(args).expect("rustc arg parsing failed");
    let (sopts, _cfg) = session::config::build_session_options_and_crate_config(&matches);
    let sopts = maybe_set_sysroot(sopts, args);

    assert!(matches.free.len() == 1, "expected exactly one input file");
    let in_path = Some(Path::new(&matches.free[0]).to_owned());

    let (session, _cstore, _codegen_backend) = build_session(sopts, in_path, file_loader);
    session
}

fn build_session(
    sopts: SessionOptions,
    in_path: Option<PathBuf>,
    file_loader: Option<Box<FileLoader + Sync + Send>>,
) -> (Session, CStore, Box<CodegenBackend>) {
    // Corresponds roughly to `run_compiler`.
    let descriptions = rustc_interface::util::diagnostics_registry();
    let file_loader = file_loader.unwrap_or_else(|| Box::new(RealFileLoader));
    // Note: `source_map` is expected to be an `Lrc<SourceMap>`, which is an alias for `Rc<SourceMap>`.
    // If this ever changes, we'll need a new trick to obtain the `SourceMap` in `rebuild_session`.
    let source_map = Rc::new(SourceMap::with_file_loader(
        file_loader,
        sopts.file_path_mapping(),
    ));
    // Put a dummy file at the beginning of the source_map, so that no real `Span` will accidentally
    // collide with `DUMMY_SP` (which is `0 .. 0`).
    source_map.new_source_file(FileName::Custom("<dummy>".to_string()), " ".to_string());

    let sess = session::build_session_with_source_map(
        sopts,
        in_path,
        descriptions,
        source_map,
        DiagnosticOutput::Default,
        Default::default(),
    );

    let codegen_backend = get_codegen_backend(&sess);
    let cstore = CStore::new(codegen_backend.metadata_loader());

    (sess, cstore, codegen_backend)
}

fn make_parser<'a>(sess: &'a Session, src: &str) -> Parser<'a> {
    parse::new_parser_from_source_str(
        &sess.parse_sess,
        FileName::anon_source_code(src),
        src.to_owned(),
    )
}

pub fn emit_and_panic(mut db: DiagnosticBuilder, what: &str) -> ! {
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
    match p.parse_pat(None) {
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
    // TODO: rustc no longer exposes `parse_full_stmt`. `parse_block` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("{{ {} }}", src));
    match p.parse_block() {
        Ok(blk) => blk
            .into_inner()
            .stmts
            .into_iter()
            .map(|mut s| {
                remove_paren(&mut s);
                s.lone()
            })
            .collect(),
        Err(db) => emit_and_panic(db, "stmts"),
    }
}

pub fn parse_items(sess: &Session, src: &str) -> Vec<P<Item>> {
    let mut p = make_parser(sess, src);
    let mut items = Vec::new();
    loop {
        match p.parse_item() {
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

pub fn parse_impl_items(sess: &Session, src: &str) -> Vec<ImplItem> {
    // TODO: rustc no longer exposes `parse_impl_item_`. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("impl ! {{ {} }}", src));
    match p.parse_item() {
        Ok(item) => match item.expect("expected to find an item").into_inner().node {
            ItemKind::Impl(_, _, _, _, _, _, items) => items,
            _ => panic!("expected to find an impl item"),
        },
        Err(db) => emit_and_panic(db, "impl items"),
    }
}

pub fn parse_foreign_items(sess: &Session, src: &str) -> Vec<ForeignItem> {
    // TODO: rustc no longer exposes a method for parsing ForeignItems. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("extern {{ {} }}", src));
    match p.parse_item() {
        Ok(item) => match item.expect("expected to find an item").into_inner().node {
            ItemKind::ForeignMod(fm) => fm.items,
            _ => panic!("expected to find a foreignmod item"),
        },
        Err(db) => emit_and_panic(db, "foreign items"),
    }
}

pub fn parse_block(sess: &Session, src: &str) -> P<Block> {
    let mut p = make_parser(sess, src);

    let rules = if p.eat_keyword(keywords::Unsafe) {
        BlockCheckMode::Unsafe(UnsafeSource::UserProvided)
    } else {
        BlockCheckMode::Default
    };

    match p.parse_block() {
        Ok(mut block) => {
            remove_paren(&mut block);
            block.rules = rules;
            block
        }
        Err(db) => emit_and_panic(db, "block"),
    }
}

fn parse_arg_inner<'a>(p: &mut Parser<'a>) -> PResult<'a, Arg> {
    // `parse_arg` is private, so we make do with `parse_pat` + `parse_ty`.
    let pat = p.parse_pat(None)?;
    p.expect(&Token::Colon)?;
    let ty = p.parse_ty()?;
    Ok(Arg {
        pat,
        ty,
        id: DUMMY_NODE_ID,
    })
}

pub fn parse_arg(sess: &Session, src: &str) -> Arg {
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
    let mut p = parse::new_parser_from_tts(&sess.parse_sess, tts);
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
        Err(mut db) => {
            db.cancel();
            None
        }
    }
}

pub fn try_run_parser_tts<F, R>(sess: &Session, tts: Vec<TokenTree>, f: F) -> Option<R>
where
    F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R>,
{
    let mut p = parse::new_parser_from_tts(&sess.parse_sess, tts);
    match f(&mut p) {
        Ok(x) => Some(x),
        Err(mut db) => {
            db.cancel();
            None
        }
    }
}

/// Create a span whose text is `s`.  Note this is somewhat expensive, as it adds a new dummy file
/// to the `SourceMap` on every call.
pub fn make_span_for_text(cm: &SourceMap, s: &str) -> Span {
    let fm = cm.new_source_file(FileName::anon_source_code(s), s.to_string());
    Span::new(fm.start_pos, fm.end_pos, SyntaxContext::empty())
}
