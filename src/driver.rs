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
use rustc_errors::Diagnostic;
use rustc_metadata::creader::CrateLoader;
use rustc_metadata::cstore::CStore;
use rustc_resolve::{Resolver, MakeGlobMap};
use rustc_trans::back::link;
use syntax;
use syntax::ast::{Crate, Expr, Pat, Ty, Stmt, Item};
use syntax::codemap::{CodeMap, RealFileLoader};
use syntax::ext::base::ExtCtxt;
use syntax::parse;
use syntax::parse::parser::Parser;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax_ext;
use syntax_pos::{BytePos, Span};

use get_span::GetSpan;
use remove_paren::remove_paren;
use util::Lone;


pub struct Ctxt<'a, 'hir: 'a, 'gcx: 'a + 'tcx, 'tcx: 'a> {
    sess: &'a Session,
    map: Option<&'a hir_map::Map<'hir>>,
    tcx: Option<TyCtxt<'a, 'gcx, 'tcx>>,
    cursors: Vec<BytePos>,
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

            cursors: Vec::new(),
        }
    }

    fn new_phase_2(sess: &'a Session,
                   map: &'a hir_map::Map<'hir>) -> Ctxt<'a, 'hir, 'gcx, 'tcx> {
        Ctxt {
            sess: sess,
            map: Some(map),
            tcx: None,

            cursors: Vec::new(),
        }
    }

    fn new_phase_3(sess: &'a Session,
                   map: &'a hir_map::Map<'hir>,
                   tcx: TyCtxt<'a, 'gcx, 'tcx>) -> Ctxt<'a, 'hir, 'gcx, 'tcx> {
        Ctxt {
            sess: sess,
            map: Some(map),
            tcx: Some(tcx),

            cursors: Vec::new(),
        }
    }

    pub fn session(&self) -> &'a Session {
        self.sess
    }

    pub fn hir_map(&self) -> &'a hir_map::Map<'hir> {
        self.map.unwrap()
    }

    pub fn ty_ctxt(&self) -> TyCtxt<'a, 'gcx, 'tcx> {
        self.tcx.unwrap()
    }

    pub fn add_cursor(&mut self, file: &str, line: u32, col: u32) {
        let fm = match self.sess.codemap().get_filemap(file) {
            Some(x) => x,
            None => {
                println!("warning: cursor lies in nonexistent file {:?}", file);
                return;
            },
        };

        if line == 0 || line as usize - 1 >= fm.lines.borrow().len() {
            println!("warning: line {} is outside the bounds of {}", line, file);
            return;
        };
        let (lo, hi) = fm.line_bounds(line as usize - 1);

        let line_len = hi.0 - lo.0;
        if col == 0 || col - 1 >= line_len {
            println!("warning: column {} is outside the bounds of {} line {}",
                     col, file, line);
            return;
        }

        self.cursors.push(lo + BytePos(col - 1));
    }

    pub fn span_has_cursor(&self, sp: Span) -> bool {
        for &c in &self.cursors {
            if sp.lo <= c && c < sp.hi {
                return true;
            }
        }
        false
    }

    pub fn has_cursor<T: GetSpan>(&self, x: &T) -> bool {
        self.span_has_cursor(x.get_span())
    }
}


pub fn with_crate_and_context<F>(args: &[String],
                                 phase: Phase,
                                 func: F)
        where F: FnOnce(Crate, Ctxt) {
    let matches = rustc_driver::handle_options(args)
        .expect("rustc arg parsing failed");
    let (sopts, _cfg) = session::config::build_session_options_and_crate_config(&matches);

    assert!(matches.free.len() == 1,
           "expected exactly one input file");
    let in_path = Some(Path::new(&matches.free[0]).to_owned());
    let input = Input::File(in_path.as_ref().unwrap().clone());

    let (sess, cstore) = build_session(sopts, in_path);

    // Start of `compile_input` code
    let krate = driver::phase_1_parse_input(&sess, &input).unwrap();
    // Leave parens in place until after expansion, unless we're stopping at phase 1.

    if phase == Phase::Phase1 {
        let krate = remove_paren(krate);
        let cx = Ctxt::new_phase_1(&sess);
        func(krate, cx);
        return;
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
        func(krate, cx);
        return;
    }

    driver::phase_3_run_analysis_passes(
        // TODO: probably shouldn't be cloning hir_map... it ought to be accessible through the tcx
        // somehow
        &sess, hir_map.clone(), expand_result.analysis, expand_result.resolutions,
        &arena, &arenas, &crate_name,
        |tcx, analysis, incremental_hashes_map, result| {
            if phase == Phase::Phase3 {
                let cx = Ctxt::new_phase_3(&sess, &hir_map, tcx);
                func(krate, cx);
                return;
            }
        }).unwrap();
}

fn build_session(sopts: Options,
                 in_path: Option<PathBuf>) -> (Session, Rc<CStore>) {
    // Corresponds roughly to `run_compiler`.
    let descriptions = rustc_driver::diagnostics_registry();
    let dep_graph = DepGraph::new(sopts.build_dep_graph());
    let cstore = Rc::new(CStore::new(&dep_graph));
    let codemap = Rc::new(CodeMap::with_file_loader(Box::new(RealFileLoader)));
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
pub fn parse_expr(sess: &Session, src: &str) -> Result<P<Expr>, Diagnostic> {
    let mut p = make_parser(sess, "<expr>", src);
    match p.parse_expr() {
        Ok(expr) => Ok(remove_paren(expr)),
        Err(e) => Err(e.into_diagnostic()),
    }
}

pub fn parse_pat(sess: &Session, src: &str) -> Result<P<Pat>, Diagnostic> {
    let mut p = make_parser(sess, "<pat>", src);
    match p.parse_pat() {
        Ok(pat) => Ok(remove_paren(pat)),
        Err(e) => Err(e.into_diagnostic()),
    }
}

pub fn parse_ty(sess: &Session, src: &str) -> Result<P<Ty>, Diagnostic> {
    let mut ty = make_parser(sess, "<ty>", src);
    match ty.parse_ty() {
        Ok(ty) => Ok(remove_paren(ty)),
        Err(e) => Err(e.into_diagnostic()),
    }
}

pub fn parse_stmts(sess: &Session, src: &str) -> Result<Vec<Stmt>, Diagnostic> {
    let mut p = make_parser(sess, "<stmt>", src);
    let mut stmts = Vec::new();
    loop {
        match p.parse_full_stmt(false) {
            Ok(Some(stmt)) => stmts.push(remove_paren(stmt).lone()),
            Ok(None) => break,
            Err(e) => return Err(e.into_diagnostic()),
        }
    }
    Ok(stmts)
}

pub fn parse_items(sess: &Session, src: &str) -> Result<Vec<P<Item>>, Diagnostic> {
    let mut p = make_parser(sess, "<stmt>", src);
    let mut items = Vec::new();
    loop {
        match p.parse_item() {
            Ok(Some(item)) => items.push(remove_paren(item).lone()),
            Ok(None) => break,
            Err(e) => return Err(e.into_diagnostic()),
        }
    }
    Ok(items)
}
