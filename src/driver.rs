//! Frontend logic for parsing and expanding ASTs.  This code largely mimics the behavior of
//! `librustc_driver::driver::compile_input`.

use std::path::{Path, PathBuf};
use std::rc::Rc;
use arena::DroplessArena;
use rustc::dep_graph::DepGraph;
use rustc::hir::map as hir_map;
use rustc::ty::{TyCtxt, GlobalArenas};
use rustc::session::{self, Session};
use rustc::session::config::{Input, Options};
use rustc_driver;
use rustc_driver::driver;
use rustc_metadata::cstore::CStore;
use rustc_resolve::MakeGlobMap;
use rustc_trans;
use rustc_trans::back::link;
use syntax::ast::{Crate, Expr, Pat, Ty, Stmt, Item};
use syntax::codemap::CodeMap;
use syntax::codemap::{FileLoader, RealFileLoader};
use syntax::parse;
use syntax::parse::parser::Parser;
use syntax::ptr::P;

use remove_paren::remove_paren;
use span_fix;
use util::Lone;


#[derive(Clone)]
pub struct Ctxt<'a, 'hir: 'a, 'gcx: 'a + 'tcx, 'tcx: 'a> {
    sess: &'a Session,
    map: Option<&'a hir_map::Map<'hir>>,
    tcx: Option<TyCtxt<'a, 'gcx, 'tcx>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Phase {
    Phase1,
    Phase2,
    Phase3,
}

impl<'a, 'hir, 'gcx: 'a + 'tcx, 'tcx: 'a> Ctxt<'a, 'hir, 'gcx, 'tcx> {
    fn new_phase_1(sess: &'a Session) -> Ctxt<'a, 'hir, 'gcx, 'tcx> {
        Ctxt {
            sess: sess,
            map: None,
            tcx: None,
        }
    }

    fn new_phase_2(sess: &'a Session,
                   map: &'a hir_map::Map<'hir>) -> Ctxt<'a, 'hir, 'gcx, 'tcx> {
        Ctxt {
            sess: sess,
            map: Some(map),
            tcx: None,
        }
    }

    fn new_phase_3(sess: &'a Session,
                   map: &'a hir_map::Map<'hir>,
                   tcx: TyCtxt<'a, 'gcx, 'tcx>) -> Ctxt<'a, 'hir, 'gcx, 'tcx> {
        Ctxt {
            sess: sess,
            map: Some(map),
            tcx: Some(tcx),
        }
    }

    pub fn session(&self) -> &'a Session {
        self.sess
    }

    pub fn hir_map(&self) -> &'a hir_map::Map<'hir> {
        self.map
            .expect("hir map is not available in this context (requires phase 2)")
    }

    pub fn ty_ctxt(&self) -> TyCtxt<'a, 'gcx, 'tcx> {
        self.tcx
            .expect("ty ctxt is not available in this context (requires phase 3)")
    }
}


pub fn run_compiler<F, R>(args: &[String],
                          file_loader: Option<Box<FileLoader>>,
                          phase: Phase,
                          func: F) -> R
        where F: FnOnce(Crate, Ctxt) -> R {
    let matches = rustc_driver::handle_options(args)
        .expect("rustc arg parsing failed");
    let (sopts, _cfg) = session::config::build_session_options_and_crate_config(&matches);

    assert!(matches.free.len() == 1,
           "expected exactly one input file");
    let in_path = Some(Path::new(&matches.free[0]).to_owned());
    let input = Input::File(in_path.as_ref().unwrap().clone());

    let (sess, cstore) = build_session(sopts, in_path, file_loader);

    // Start of `compile_input` code
    let krate = driver::phase_1_parse_input(&sess, &input).unwrap();
    // Leave parens in place until after expansion, unless we're stopping at phase 1.  But
    // immediately fix up the attr spans, since during expansion, any `derive` attrs will be
    // removed.
    let krate = span_fix::fix_attr_spans(krate);

    if phase == Phase::Phase1 {
        let krate = remove_paren(krate);
        let cx = Ctxt::new_phase_1(&sess);
        return func(krate, cx);
    }

    let crate_name = link::find_crate_name(Some(&sess), &krate.attrs, &input);
    let mut expand_result = driver::phase_2_configure_and_expand(
        &sess, &cstore, krate, /*registry*/ None, &crate_name,
        /*addl_plugins*/ None, MakeGlobMap::No, |_| Ok(())).unwrap();
    let krate = expand_result.expanded_crate;
    let krate = remove_paren(krate);

    let arena = DroplessArena::new();
    let arenas = GlobalArenas::new();

    let hir_map = hir_map::map_crate(&mut expand_result.hir_forest, expand_result.defs);

    if phase == Phase::Phase2 {
        let cx = Ctxt::new_phase_2(&sess, &hir_map);
        return func(krate, cx);
    }

    driver::phase_3_run_analysis_passes(
        // TODO: probably shouldn't be cloning hir_map... it ought to be accessible through the tcx
        // somehow
        &sess, hir_map.clone(), expand_result.analysis, expand_result.resolutions,
        &arena, &arenas, &crate_name,
        |tcx, _analysis, _incremental_hashes_map, _result| {
            if phase == Phase::Phase3 {
                let cx = Ctxt::new_phase_3(&sess, &hir_map, tcx);
                return func(krate, cx);
            }
            unreachable!();
        }).unwrap()
}

pub fn with_crate_and_context<F, R>(args: &[String],
                                    phase: Phase,
                                    func: F) -> R
        where F: FnOnce(Crate, Ctxt) -> R {
    run_compiler(args, None, phase, func)
}

fn build_session(sopts: Options,
                 in_path: Option<PathBuf>,
                 file_loader: Option<Box<FileLoader>>) -> (Session, Rc<CStore>) {
    // Corresponds roughly to `run_compiler`.
    let descriptions = rustc_driver::diagnostics_registry();
    let dep_graph = DepGraph::new(sopts.build_dep_graph());
    let cstore = Rc::new(CStore::new(&dep_graph, Box::new(rustc_trans::LlvmMetadataLoader)));
    let file_loader = file_loader.unwrap_or_else(|| Box::new(RealFileLoader));
    let codemap = Rc::new(CodeMap::with_file_loader(file_loader, sopts.file_path_mapping()));
    // Put a dummy file at the beginning of the codemap, so that no real `Span` will accidentally
    // collide with `DUMMY_SP` (which is `0 .. 0`).
    codemap.new_filemap_and_lines("<dummy>", " ");
    let emitter_dest = None;

    let sess = session::build_session_with_codemap(
        sopts, &dep_graph, in_path, descriptions, cstore.clone(), codemap, emitter_dest,
    );

    (sess, cstore)
}


fn make_parser<'a>(sess: &'a Session, name: &str, src: &str) -> Parser<'a> {
    parse::new_parser_from_source_str(&sess.parse_sess,
                                      name.to_owned(),
                                      src.to_owned())
}

// Helper functions for parsing source code in an existing `Session`.
pub fn parse_expr(sess: &Session, src: &str) -> P<Expr> {
    let mut p = make_parser(sess, "<expr>", src);
    match p.parse_expr() {
        Ok(expr) => remove_paren(expr),
        Err(e) => panic!("error parsing expr: {:?}", e.into_diagnostic()),
    }
}

pub fn parse_pat(sess: &Session, src: &str) -> P<Pat> {
    let mut p = make_parser(sess, "<pat>", src);
    match p.parse_pat() {
        Ok(pat) => remove_paren(pat),
        Err(e) => panic!("error parsing pat: {:?}", e.into_diagnostic()),
    }
}

pub fn parse_ty(sess: &Session, src: &str) -> P<Ty> {
    let mut ty = make_parser(sess, "<ty>", src);
    match ty.parse_ty() {
        Ok(ty) => remove_paren(ty),
        Err(e) => panic!("error parsing ty: {:?}", e.into_diagnostic()),
    }
}

pub fn parse_stmts(sess: &Session, src: &str) -> Vec<Stmt> {
    let mut p = make_parser(sess, "<stmt>", src);
    let mut stmts = Vec::new();
    loop {
        match p.parse_full_stmt(false) {
            Ok(Some(stmt)) => stmts.push(remove_paren(stmt).lone()),
            Ok(None) => break,
            Err(e) => panic!("error parsing stmts: {:?}", e.into_diagnostic()),
        }
    }
    stmts
}

pub fn parse_items(sess: &Session, src: &str) -> Vec<P<Item>> {
    let mut p = make_parser(sess, "<stmt>", src);
    let mut items = Vec::new();
    loop {
        match p.parse_item() {
            Ok(Some(item)) => items.push(remove_paren(item).lone()),
            Ok(None) => break,
            Err(e) => panic!("error parsing items: {:?}", e.into_diagnostic()),
        }
    }
    items
}
