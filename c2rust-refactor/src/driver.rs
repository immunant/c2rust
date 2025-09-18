//! Frontend logic for parsing and expanding ASTs.  This code largely mimics the behavior of
//! `rustc_driver::run_compiler`.

use rustc_middle::dep_graph::DepGraph;
use rustc_middle::hir::map as hir_map;
use rustc_lint::LintStore;
use rustc_hir as hir;
use rustc_session::config::Options as SessionOptions;
use rustc_session::config::{Input, OutputFilenames};
use rustc_session::{self, DiagnosticOutput, Session};
use rustc_data_structures::steal::Steal;
use rustc_middle::ty::{self, ResolverOutputs};
use rustc_codegen_ssa::traits::CodegenBackend;
use rustc_data_structures::declare_box_region_type;
use rustc_data_structures::sync::{Lock, Lrc};
use rustc_driver;
use rustc_errors::{DiagnosticBuilder, ErrorGuaranteed};
use rustc_incremental::DepGraphFuture;
use rustc_interface::interface;
use rustc_interface::interface::BoxedResolver;
use rustc_interface::util::get_codegen_backend;
use rustc_interface::{util, Config};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashSet;
use std::mem;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;
use rustc_ast::ast;
use rustc_ast::DUMMY_NODE_ID;
use rustc_ast::{
    AssocItem, Block, BlockCheckMode, Expr, ForeignItem, Item, ItemKind, NodeId, Param, Pat, Stmt,
    Ty, UnsafeSource,
};
use rustc_span::hygiene::SyntaxContext;
use rustc_parse::parser::Parser;
use rustc_ast::token::{self, TokenKind};
use rustc_errors::PResult;
use rustc_ast::ptr::P;
use rustc_span::source_map::SourceMap;
use rustc_span::source_map::{FileLoader, RealFileLoader};
use rustc_span::symbol::{kw, Symbol};
use rustc_ast::tokenstream::TokenTree;
use rustc_span::{FileName, Span, DUMMY_SP};
use rustc_span::edition::Edition;

use crate::ast_manip::remove_paren;
use crate::command::{GenerationalTyCtxt, RefactorState, Registry};
use crate::file_io::{ArcFileIO, FileIO};
// TODO: don't forget to call span_fix after parsing
// use crate::span_fix;
use crate::util::Lone;
use crate::RefactorCtxt;
use crate::context::HirMap;

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
        RefactorCtxt::new(sess, None, None, None)
    }

    pub fn new_phase_2(
        sess: &'a Session,
        max_node_id: NodeId,
        map: &'a hir_map::Map<'tcx>,
    ) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(sess, None, Some(HirMap::new(max_node_id, map)), None)
    }

    pub fn new_phase_3(
        sess: &'a Session,
        max_node_id: NodeId,
        map: &'a hir_map::Map<'tcx>,
        tcx: GenerationalTyCtxt<'tcx>,
    ) -> RefactorCtxt<'a, 'tcx> {
        RefactorCtxt::new(sess, None, Some(HirMap::new(max_node_id, map)), Some(tcx))
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
        input,
        input_path: config.input_path.clone(),
        output_file: config.output_file.clone(),
        output_dir: config.output_dir.clone(),
        file_loader: None,
        diagnostic_output: DiagnosticOutput::Default,
        stderr: config.stderr.clone(),
        crate_name: config.crate_name.clone(),
        lint_caps: config.lint_caps.clone(),
        register_lints: None,
        override_queries: None,
        registry: config.registry.clone(),
    }
}

pub fn create_config(args: &[String]) -> interface::Config {
    let matches = rustc_driver::handle_options(args).expect("rustc arg parsing failed");
    let sopts = session::config::build_session_options(&matches);
    let cfg = interface::parse_cfgspecs(matches.opt_strs("cfg"));
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
        register_lints: None,
        override_queries: None,
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
    F: FnOnce(&interface::Compiler) -> R,
    R: Send,
{
    // Force disable incremental compilation.  It causes panics with multiple typechecking.
    config.opts.incremental = None;
    config.file_loader = file_loader;

    syntax::with_globals(Edition::Edition2018, move || {
        ty::tls::GCX_PTR.set(&Lock::new(0), || {
            ty::tls::with_thread_locals(|| {
                interface::run_compiler_in_existing_thread_pool(config, f)
            })
        })
    })
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
    F: FnOnce(RefactorState) -> R,
    R: Send,
{
    // Force disable incremental compilation.  It causes panics with multiple typechecking.
    config.opts.incremental = None;

    syntax::with_globals(Edition::Edition2018, move || {
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
    crate_name: Option<String>,
    register_lints: Option<Box<dyn Fn(&Session, &mut LintStore) + Send + Sync>>,
    override_queries:
        Option<fn(&Session, &mut ty::query::Providers<'_>, &mut ty::query::Providers<'_>)>,
}

#[allow(dead_code)]
#[derive(Default)]
struct Queries<'tcx> {
    dep_graph_future: Query<Option<DepGraphFuture>>,
    parse: Query<ast::Crate>,
    crate_name: Query<String>,
    register_plugins: Query<(ast::Crate, Lrc<LintStore>)>,
    expansion: Query<(ast::Crate, Steal<Rc<RefCell<BoxedResolver>>>)>,
    dep_graph: Query<DepGraph>,
    lower_to_hir: Query<(&'tcx hir::map::Forest, Steal<ResolverOutputs>)>,
    prepare_outputs: Query<OutputFilenames>,
    global_ctxt: Query<BoxedGlobalCtxt>,
    ongoing_codegen: Query<Box<dyn Any>>,
    link: Query<()>,
}

#[allow(dead_code)]
struct Query<T> {
    // TODO: fix the Err type
    result: RefCell<Option<Result<T, ()>>>,
}

impl<T> Default for Query<T> {
    fn default() -> Self {
        Query {
            result: RefCell::new(None),
        }
    }
}

declare_box_region_type!(
    pub BoxedGlobalCtxt,
    for('gcx),
    (&'gcx GlobalCtxt<'gcx>) -> ((), ())
);

pub fn make_compiler(config: &Config, file_io: Arc<dyn FileIO + Sync + Send>) -> interface::Compiler {
    let mut config = clone_config(config);
    config.file_loader = Some(Box::new(ArcFileIO(file_io)));
    let (sess, codegen_backend, source_map) = util::create_session(
        config.opts,
        config.crate_cfg,
        config.diagnostic_output,
        config.file_loader,
        config.input_path.clone(),
        config.lint_caps,
        config.registry,
    );

    // Put a dummy file at the beginning of the source_map, so that no real `Span` will accidentally
    // collide with `DUMMY_SP` (which is `0 .. 0`).
    source_map.new_source_file(FileName::Custom("<dummy>".to_string()), " ".to_string());

    let compiler = Compiler {
        sess,
        codegen_backend,
        source_map,
        input: config.input,
        input_path: config.input_path,
        output_dir: config.output_dir,
        output_file: config.output_file,
        crate_name: config.crate_name,
        override_queries: config.override_queries,
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

    let sopts = session::config::build_session_options(&matches);
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
    match p.parse_pat(None) {
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

#[cfg_attr(feature = "profile", flame)]
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

#[cfg_attr(feature = "profile", flame)]
pub fn parse_impl_items(sess: &Session, src: &str) -> Vec<AssocItem> {
    // TODO: rustc no longer exposes `parse_impl_item_`. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("impl ! {{ {} }}", src));
    match p.parse_item() {
        Ok(item) => match item.expect("expected to find an item").into_inner().kind {
            ItemKind::Impl(_, _, _, _, _, _, items) => items,
            _ => panic!("expected to find an impl item"),
        },
        Err(db) => emit_and_panic(db, "impl items"),
    }
}

#[cfg_attr(feature = "profile", flame)]
pub fn parse_foreign_items(sess: &Session, src: &str) -> Vec<ForeignItem> {
    // TODO: rustc no longer exposes a method for parsing ForeignItems. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, &format!("extern {{ {} }}", src));
    match p.parse_item() {
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

    match p.parse_block() {
        Ok(mut block) => {
            remove_paren(&mut block);
            block.rules = rules;
            block
        }
        Err(db) => emit_and_panic(db, "block"),
    }
}

fn parse_arg_inner<'a>(p: &mut Parser<'a>) -> PResult<'a, Param> {
    // `parse_arg` is private, so we make do with `parse_attribute`,
    // `parse_pat`, & `parse_ty`.
    let mut attrs: Vec<ast::Attribute> = Vec::new();
    while let token::Pound = p.token.kind {
        attrs.push(p.parse_attribute(false).unwrap());
    }
    let pat = p.parse_pat(None)?;
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
    let mut p = rustc_parse::new_parser_from_tts(&sess.parse_sess, tts);
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
        Err(mut db) => {
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
    let mut p = rustc_parse::new_parser_from_tts(&sess.parse_sess, tts);
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
    Span::new(fm.start_pos, fm.end_pos, SyntaxContext::root())
}
