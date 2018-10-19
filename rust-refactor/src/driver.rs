//! Frontend logic for parsing and expanding ASTs.  This code largely mimics the behavior of
//! `rustc_driver::driver::compile_input`.

use std::mem::{self, ManuallyDrop};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use rustc::hir::map as hir_map;
use rustc::ty::{TyCtxt, AllArenas};
use rustc::session::{self, Session};
use rustc::session::config::{Input, Options};
use rustc_driver;
use rustc_driver::driver::{self, build_output_filenames, CompileController};
use rustc_errors::DiagnosticBuilder;
use rustc_metadata::cstore::CStore;
use rustc_resolve::MakeGlobMap;
use rustc_codegen_utils::link;
use rustc_codegen_utils::codegen_backend::CodegenBackend;
use syntax::ast::{
    Crate, Expr, Pat, Ty, Stmt, Item, ImplItem, ForeignItem, ItemKind, Block, Arg, BlockCheckMode,
    UnsafeSource,
};
use syntax::ast::DUMMY_NODE_ID;
use syntax::source_map::SourceMap;
use syntax::source_map::{FileLoader, RealFileLoader};
use syntax::ext::hygiene::SyntaxContext;
use syntax::parse::{self, PResult};
use syntax::parse::token::Token;
use syntax::parse::parser::Parser;
use syntax::ptr::P;
use syntax::symbol::keywords;
use syntax::tokenstream::TokenTree;
use syntax_pos::FileName;
use syntax_pos::Span;
use arena::SyncDroplessArena;

use ast_manip::remove_paren;
use span_fix;
use util::Lone;


/// Driver context.  Contains all available analysis results as of the current compiler phase.
///
/// Accessor methods will panic if the requested results are not available.
#[derive(Clone)]
pub struct Ctxt<'a, 'tcx: 'a> {
    sess: &'a Session,
    map: Option<&'a hir_map::Map<'tcx>>,
    tcx: Option<TyCtxt<'a, 'tcx, 'tcx>>,

    /// This is a reference to the same `DroplessArena` used in `tcx`.  Analyses working with types
    /// use this to allocate extra values with the same lifetime `'tcx` as the types themselves.
    /// This way `Ty` wrappers don't need two lifetime parameters everywhere.
    tcx_arena: Option<&'tcx SyncDroplessArena>,

    cstore: &'a CStore,
}

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

impl<'a, 'tcx: 'a> Ctxt<'a, 'tcx> {
    fn new_phase_1(sess: &'a Session, cstore: &'a CStore) -> Ctxt<'a, 'tcx> {
        Ctxt {
            sess,
            cstore,
            map: None,
            tcx: None,
            tcx_arena: None,
        }
    }

    fn new_phase_2(sess: &'a Session,
                   cstore: &'a CStore,
                   map: &'a hir_map::Map<'tcx>) -> Ctxt<'a, 'tcx> {
        Ctxt {
            sess,
            cstore,
            map: Some(map),
            tcx: None,
            tcx_arena: None,
        }
    }

    fn new_phase_3(sess: &'a Session,
                   cstore: &'a CStore,
                   map: &'a hir_map::Map<'tcx>,
                   tcx: TyCtxt<'a, 'tcx, 'tcx>,
                   tcx_arena: &'tcx SyncDroplessArena) -> Ctxt<'a, 'tcx> {
        Ctxt {
            sess,
            cstore,
            map: Some(map),
            tcx: Some(tcx),
            tcx_arena: Some(tcx_arena),
        }
    }

    pub fn session(&self) -> &'a Session {
        self.sess
    }

    pub fn cstore(&self) -> &'a CStore { self.cstore }

    pub fn hir_map(&self) -> &'a hir_map::Map<'tcx> {
        self.map
            .expect("hir map is not available in this context (requires phase 2)")
    }

    pub fn ty_ctxt(&self) -> TyCtxt<'a, 'tcx, 'tcx> {
        self.tcx
            .expect("ty ctxt is not available in this context (requires phase 3)")
    }

    pub fn ty_arena(&self) -> &'tcx SyncDroplessArena {
        self.tcx_arena
            .expect("ty ctxt is not available in this context (requires phase 3)")
    }
}




/// Various driver bits that we have lying around at the end of `phase_1_parse_input`.  This is
/// everything we need to (re-)run the compiler from phase 1 onward.
pub struct Phase1Bits {
    session: Session,
    cstore: CStore,
    codegen_backend: Box<CodegenBackend>,
    input: Input,
    output: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    control: CompileController<'static>,
    krate: Crate,
}

impl Phase1Bits {
    /// Set up the compiler again, using a previously-constructed `Session`.
    ///
    /// A `Crate` is mostly self-contained, but its `Span`s are really indexes into external
    /// tables.  So if you actually plan to run the compiler after calling `reset()`, the new
    /// `krate` passed here should satisfy a few properties:
    ///
    ///  1. The crate must have been parsed under the same `SourceMap` used by `session`.  Spans'
    ///     `hi` and `lo` byte positions are indices into the `SourceMap` used for parsing, so
    ///     transferring those spans to a different `SourceMap` produces nonsensical results.
    ///
    ///  2. The crate must not contain any paths starting with `$crate` from a non-empty
    ///     `SyntaxCtxt`.  These types of paths appear during macro expansion, and can only be
    ///     resolved using tables populated by the macro expander.
    ///
    ///  3. All `NodeId`s in the crate must be DUMMY_NODE_ID.
    ///
    ///  4. The crate must not contain automatically-injected `extern crate` declarations.  The
    ///     compilation process will inject new copies of these, and then fail due to the name
    ///     collision.
    ///
    /// A crate that has only been compiled to `Phase1` already satisfies points 2-4.  If you want
    /// to re-compile a crate from `Phase2` or later, use `recheck::prepare_recheck` to fix things
    /// up first.
    pub fn from_session_and_crate(old_session: &Session, krate: Crate) -> Phase1Bits {
        let (session, cstore, codegen_backend) = rebuild_session(old_session);

        let in_path = old_session.local_crate_source_file.clone();
        let input = Input::File(in_path.unwrap());

        let control = CompileController::basic();

        Phase1Bits {
            session, cstore, codegen_backend,

            input,
            output: None,
            out_dir: None,

            control, krate,
        }
    }

    /// Set up the compiler using a previously-created session, repeating phase 1 (input parsing).
    pub fn from_session_reparse(old_session: &Session) -> Phase1Bits {
        let (session, cstore, codegen_backend) = rebuild_session(old_session);

        let in_path = old_session.local_crate_source_file.clone();
        let input = Input::File(in_path.unwrap());

        let control = CompileController::basic();

        // Start of `compile_input` code
        let krate = driver::phase_1_parse_input(&control, &session, &input).unwrap();

        Phase1Bits {
            session, cstore, codegen_backend,

            input,
            output: None,
            out_dir: None,

            control, krate,
        }
    }

    pub fn into_crate(self) -> Crate {
        self.krate
    }
}


pub fn run_compiler_to_phase1(args: &[String],
                              file_loader: Option<Box<FileLoader+Sync+Send>>) -> Phase1Bits {
    let matches = rustc_driver::handle_options(args)
        .expect("rustc arg parsing failed");
    let (sopts, _cfg) = session::config::build_session_options_and_crate_config(&matches);
    let out_dir = matches.opt_str("out-dir").map(|o| PathBuf::from(&o));
    let output = matches.opt_str("o").map(|o| PathBuf::from(&o));

    assert!(matches.free.len() == 1,
           "expected exactly one input file");
    let in_path = Some(Path::new(&matches.free[0]).to_owned());
    let input = Input::File(in_path.as_ref().unwrap().clone());

    let (session, cstore, codegen_backend) = build_session(sopts, in_path, file_loader);

    // It might seem tempting to set up a custom CompileController and invoke `compile_input` here,
    // in order to avoid duplicating a bunch of `compile_input`'s logic.  Unfortunately, that
    // doesn't work well with the current API.  The `CompileState`s provided to the PhaseController
    // callbacks only contain the data relevant to th ecurrent  phase - for example, in the
    // after_analysis callback, `tcx` is available but `krate`, `arena`, and `hir_map` are not.
    // Furthermore, the callback type is such that the `CompileState`s for separate callbacks have
    // unrelated lifetimes, so we can't (safely) collect up the relevant pieces ourselves from
    // multiple callback invocations.

    let control = CompileController::basic();

    // Start of `compile_input` code
    let krate = driver::phase_1_parse_input(&control, &session, &input).unwrap();

    Phase1Bits {
        session, cstore, codegen_backend,
        input, output, out_dir,
        control, krate,
    }
}

pub fn run_compiler_from_phase1<F, R>(bits: Phase1Bits,
                                      phase: Phase,
                                      func: F) -> R
        where F: FnOnce(Crate, Ctxt) -> R {
    let Phase1Bits {
        session, cstore, codegen_backend, input, output, out_dir, control, krate,
    } = bits;

    // Immediately fix up the attr spans, since during expansion, any `derive` attrs will be
    // removed.
    let krate = span_fix::fix_attr_spans(krate);

    if phase == Phase::Phase1 {
        let cx = Ctxt::new_phase_1(&session, &cstore);
        return func(krate, cx);
    }

    let outputs = build_output_filenames(&input, &out_dir, &output, &krate.attrs, &session);
    let crate_name = link::find_crate_name(Some(&session), &krate.attrs, &input);
    let mut expand_result = driver::phase_2_configure_and_expand(
        &session, &cstore, krate, /*registry*/ None, &crate_name,
        /*addl_plugins*/ None, MakeGlobMap::No, |_| Ok(())).unwrap();
    let krate = expand_result.expanded_crate;

    let arenas = AllArenas::new();

    let hir_map = hir_map::map_crate(&session, &cstore, &mut expand_result.hir_forest, &expand_result.defs);

    if phase == Phase::Phase2 {
        let cx = Ctxt::new_phase_2(&session, &cstore, &hir_map);
        return func(krate, cx);
    }

    let mut result = None;
    let _ = driver::phase_3_run_analysis_passes(
        &*codegen_backend,
        &control,
        &session, &cstore, hir_map, expand_result.analysis, expand_result.resolutions,
        &arenas, &crate_name, &outputs,
        |tcx, _analysis, _incremental_hashes_map, _result| {
            if phase == Phase::Phase3 {
                let cx = Ctxt::new_phase_3(&session, &cstore, &tcx.hir, tcx, &arenas.interner);
                result = Some(func(krate, cx));
                return;
            }
            unreachable!();
        });
    result.unwrap()
}

/// Run the compiler with some command line `args`.  Stops compiling and invokes the callback
/// `func` after the indicated `phase`.
///
/// `file_loader` can be `None` to read source code from the file system.  Otherwise, the provided
/// loader will be used within the compiler.  For example, editor integration uses a custom file
/// loader to provide the compiler with buffer contents for currently open files.
pub fn run_compiler<F, R>(args: &[String],
                          file_loader: Option<Box<FileLoader+Sync+Send>>,
                          phase: Phase,
                          func: F) -> R
        where F: FnOnce(Crate, Ctxt) -> R {
    let bits = run_compiler_to_phase1(args, file_loader);
    run_compiler_from_phase1(bits, phase, func)
}

pub fn build_session_from_args(args: &[String],
                               file_loader: Option<Box<FileLoader+Sync+Send>>) -> Session {
    let matches = rustc_driver::handle_options(args)
        .expect("rustc arg parsing failed");
    let (sopts, _cfg) = session::config::build_session_options_and_crate_config(&matches);

    assert!(matches.free.len() == 1,
           "expected exactly one input file");
    let in_path = Some(Path::new(&matches.free[0]).to_owned());

    let (session, _cstore, _codegen_backend) = build_session(sopts, in_path, file_loader);
    session
}

fn build_session(sopts: Options,
                 in_path: Option<PathBuf>,
                 file_loader: Option<Box<FileLoader+Sync+Send>>) -> (Session, CStore, Box<CodegenBackend>) {
    // Corresponds roughly to `run_compiler`.
    let descriptions = rustc_driver::diagnostics_registry();
    let file_loader = file_loader.unwrap_or_else(|| Box::new(RealFileLoader));
    // Note: `codemap` is expected to be an `Lrc<SourceMap>`, which is an alias for `Rc<SourceMap>`.
    // If this ever changes, we'll need a new trick to obtain the `SourceMap` in `rebuild_session`.
    let codemap = Rc::new(SourceMap::with_file_loader(file_loader, sopts.file_path_mapping()));
    // Put a dummy file at the beginning of the codemap, so that no real `Span` will accidentally
    // collide with `DUMMY_SP` (which is `0 .. 0`).
    {
        let fm = codemap.new_filemap(FileName::Custom("<dummy>".to_string()), " ".to_string());
        fm.next_line(fm.start_pos);
    }

    let emitter_dest = None;

    let sess = session::build_session_with_codemap(
        sopts, in_path, descriptions, codemap, emitter_dest
    );

    let codegen_backend = rustc_driver::get_codegen_backend(&sess);
    let cstore = CStore::new(codegen_backend.metadata_loader());

    (sess, cstore, codegen_backend)
}

/// Build a new session from an existing one.  This uses the same `SourceMap`, so spans will be
/// compatible across both sessions.
fn rebuild_session(old_session: &Session) -> (Session, CStore, Box<CodegenBackend>) {
    let descriptions = rustc_driver::diagnostics_registry();

    // We happen to know that the `&SourceMap` we get from `old_session.codemap()` is inside an `Rc`
    // pointer, so we can clone that `Rc` with a little unsafe code.
    let codemap = unsafe {
        let temp_rc = ManuallyDrop::new(Rc::from_raw(old_session.codemap()));
        let codemap = (*temp_rc).clone();
        mem::forget(temp_rc);
        codemap
    };

    let emitter_dest = None;

    let session = session::build_session_with_codemap(
        old_session.opts.clone(),
        old_session.local_crate_source_file.clone(),
        descriptions,
        codemap,
        emitter_dest,
    );

    let codegen_backend = rustc_driver::get_codegen_backend(&session);
    let cstore = CStore::new(codegen_backend.metadata_loader());

    (session, cstore, codegen_backend)
}


fn make_parser<'a>(sess: &'a Session, name: &str, src: &str) -> Parser<'a> {
    parse::new_parser_from_source_str(&sess.parse_sess,
                                      FileName::Real(PathBuf::from(name)),
                                      src.to_owned())
}

fn emit_and_panic(mut db: DiagnosticBuilder, what: &str) -> ! {
    db.emit();
    panic!("error parsing {}", what);
}

// Helper functions for parsing source code in an existing `Session`.
pub fn parse_expr(sess: &Session, src: &str) -> P<Expr> {
    let mut p = make_parser(sess, "<expr>", src);
    match p.parse_expr() {
        Ok(expr) => remove_paren(expr),
        Err(db) => emit_and_panic(db, "expr"),
    }
}

pub fn parse_pat(sess: &Session, src: &str) -> P<Pat> {
    let mut p = make_parser(sess, "<pat>", src);
    match p.parse_pat() {
        Ok(pat) => remove_paren(pat),
        Err(db) => emit_and_panic(db, "pat"),
    }
}

pub fn parse_ty(sess: &Session, src: &str) -> P<Ty> {
    let mut ty = make_parser(sess, "<ty>", src);
    match ty.parse_ty() {
        Ok(ty) => remove_paren(ty),
        Err(db) => emit_and_panic(db, "ty"),
    }
}

pub fn parse_stmts(sess: &Session, src: &str) -> Vec<Stmt> {
    // TODO: rustc no longer exposes `parse_full_stmt`. `parse_block` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, "<stmt>", &format!("{{ {} }}", src));
    match p.parse_block() {
        Ok(blk) => blk.into_inner().stmts.into_iter().map(|s| remove_paren(s).lone()).collect(),
        Err(db) => emit_and_panic(db, "stmts"),
    }
}

pub fn parse_items(sess: &Session, src: &str) -> Vec<P<Item>> {
    let mut p = make_parser(sess, "<item>", src);
    let mut items = Vec::new();
    loop {
        match p.parse_item() {
            Ok(Some(item)) => items.push(remove_paren(item).lone()),
            Ok(None) => break,
            Err(db) => emit_and_panic(db, "items"),
        }
    }
    items
}

pub fn parse_impl_items(sess: &Session, src: &str) -> Vec<ImplItem> {
    // TODO: rustc no longer exposes `parse_impl_item_`. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, "<impl>", &format!("impl ! {{ {} }}", src));
    match p.parse_item() {
        Ok(item) => {
            match item.expect("expected to find an item").into_inner().node {
                ItemKind::Impl(_, _, _, _, _, _, items) => items,
                _ => panic!("expected to find an impl item"),
            }
        }
        Err(db) => emit_and_panic(db, "impl items"),
    }
}

pub fn parse_foreign_items(sess: &Session, src: &str) -> Vec<ForeignItem> {
    // TODO: rustc no longer exposes a method for parsing ForeignItems. `parse_item` is a hacky
    // workaround that may cause suboptimal error messages.
    let mut p = make_parser(sess, "<foreign_item>", &format!("extern {{ {} }}", src));
    match p.parse_item() {
        Ok(item) => {
            match item.expect("expected to find an item").into_inner().node {
                ItemKind::ForeignMod(fm) => fm.items,
                _ => panic!("expected to find a foreignmod item"),
            }
        }
        Err(db) => emit_and_panic(db, "foreign items"),
    }
}

pub fn parse_block(sess: &Session, src: &str) -> P<Block> {
    let mut p = make_parser(sess, "<block>", src);

    let rules = if p.eat_keyword(keywords::Unsafe) {
        BlockCheckMode::Unsafe(UnsafeSource::UserProvided)
    } else {
        BlockCheckMode::Default
    };

    match p.parse_block() {
        Ok(block) => {
            let block = remove_paren(block);
            block.map(|b| Block { rules, ..b })
        },
        Err(db) => emit_and_panic(db, "block"),
    }
}

fn parse_arg_inner<'a>(p: &mut Parser<'a>) -> PResult<'a, Arg> {
    // `parse_arg` is private, so we make do with `parse_pat` + `parse_ty`.
    let pat = p.parse_pat()?;
    p.expect(&Token::Colon)?;
    let ty = p.parse_ty()?;
    Ok(Arg { pat, ty, id: DUMMY_NODE_ID })
}

pub fn parse_arg(sess: &Session, src: &str) -> Arg {
    let mut p = make_parser(sess, "<arg>", src);
    match parse_arg_inner(&mut p) {
        Ok(arg) => remove_paren(arg),
        Err(db) => emit_and_panic(db, "arg"),
    }
}


pub fn run_parser<F, R>(sess: &Session, src: &str, f: F) -> R
        where F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R> {
    let mut p = make_parser(sess, "<src>", src);
    match f(&mut p) {
        Ok(x) => x,
        Err(db) => emit_and_panic(db, "src"),
    }
}

pub fn run_parser_tts<F, R>(sess: &Session, tts: Vec<TokenTree>, f: F) -> R
        where F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R> {
    let mut p = parse::new_parser_from_tts(&sess.parse_sess, tts);
    match f(&mut p) {
        Ok(x) => x,
        Err(db) => emit_and_panic(db, "tts"),
    }
}

pub fn try_run_parser<F, R>(sess: &Session, src: &str, f: F) -> Option<R>
        where F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R> {
    let mut p = make_parser(sess, "<src>", src);
    match f(&mut p) {
        Ok(x) => Some(x),
        Err(mut db) => {
            db.cancel();
            None
        },
    }
}

pub fn try_run_parser_tts<F, R>(sess: &Session, tts: Vec<TokenTree>, f: F) -> Option<R>
        where F: for<'a> FnOnce(&mut Parser<'a>) -> PResult<'a, R> {
    let mut p = parse::new_parser_from_tts(&sess.parse_sess, tts);
    match f(&mut p) {
        Ok(x) => Some(x),
        Err(mut db) => {
            db.cancel();
            None
        },
    }
}


/// Create a span whose text is `s`.  Note this is somewhat expensive, as it adds a new dummy file
/// to the `SourceMap` on every call.
pub fn make_span_for_text(cm: &SourceMap, s: &str) -> Span {
    let fm = cm.new_filemap(FileName::Custom("<text>".to_string()), s.to_string());
    fm.next_line(fm.start_pos);
    Span::new(fm.start_pos, fm.end_pos, SyntaxContext::empty())
}
