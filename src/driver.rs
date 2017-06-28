//! Frontend logic for parsing and expanding ASTs.  This code largely mimics the behavior of
//! `librustc_driver::driver::compile_input`.

use std::path::Path;
use std::rc::Rc;
use rustc::dep_graph::DepGraph;
use rustc::session::{self, Session};
use rustc::session::config::Input;
use rustc_driver;
use rustc_errors::{Diagnostic, ColorConfig, Handler};
use rustc_metadata::creader::CrateLoader;
use rustc_metadata::cstore::CStore;
use rustc_resolve::{Resolver, MakeGlobMap};
use rustc_trans::back::link;
use syntax;
use syntax::ast::{Crate, Expr, Pat, Stmt, Item};
use syntax::codemap::{CodeMap, RealFileLoader};
use syntax::ext::base::ExtCtxt;
use syntax::parse;
use syntax::parse::parser::Parser;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax_ext;

fn build_session(args: &[String]) -> (Session, Rc<CStore>) {
    let matches = rustc_driver::handle_options(args)
        .expect("rustc arg parsing failed");

    let (sopts, cfg) = session::config::build_session_options_and_crate_config(&matches);

    let descriptions = rustc_driver::diagnostics_registry();

    let dep_graph = DepGraph::new(sopts.build_dep_graph());
    let cstore = Rc::new(CStore::new(&dep_graph));

    let codemap = Rc::new(CodeMap::with_file_loader(Box::new(RealFileLoader)));

    assert!(matches.free.len() == 1,
            "expected exactly 1 input file");
    let in_path = Some(Path::new(&matches.free[0]).to_owned());

    let emitter_dest = None;

    let sess = session::build_session_with_codemap(
        sopts, &dep_graph, in_path, descriptions, cstore.clone(), codemap, emitter_dest,
    );

    (sess, cstore)
}


fn parse_crate_for_session(sess: &Session, cstore: Rc<CStore>) -> Crate {
    let in_path = sess.local_crate_source_file.as_ref().unwrap().clone();

    let krate = parse::parse_crate_from_file(&in_path, &sess.parse_sess).unwrap();

    let crate_name = link::find_crate_name(Some(&sess), &krate.attrs, &Input::File(in_path.clone()));

    let (mut krate, features) = syntax::config::features(krate, &sess.parse_sess, sess.opts.test);

    // these need to be set "early" so that expansion sees `quote` if enabled.
    *sess.features.borrow_mut() = features;

    *sess.crate_types.borrow_mut() = rustc_driver::driver::collect_crate_types(sess, &krate.attrs);
    *sess.crate_disambiguator.borrow_mut() = Symbol::intern(&rustc_driver::driver::compute_crate_disambiguator(sess));

    let alt_std_name = sess.opts.alt_std_name.clone();
    let krate = syntax::std_inject::maybe_inject_crates_ref(krate, alt_std_name);

    // TODO: if we ever want to support #[plugin(..)], this is the place to do it.
    // Look for the "plugin loading" timed pass in rustc_driver::driver.
    let syntax_exts = Vec::new();

    let _ignore = sess.dep_graph.in_ignore();
    let mut crate_loader = CrateLoader::new(sess, &cstore, &crate_name);
    crate_loader.preprocess(&krate);
    let resolver_arenas = Resolver::arenas();
    let mut resolver = Resolver::new(sess,
                                     &krate,
                                     &crate_name,
                                     MakeGlobMap::No,
                                     &mut crate_loader,
                                     &resolver_arenas);
    syntax_ext::register_builtins(&mut resolver, syntax_exts, sess.features.borrow().quote);

    let features = sess.features.borrow();
    let cfg = syntax::ext::expand::ExpansionConfig {
        features: Some(&features),
        recursion_limit: sess.recursion_limit.get(),
        trace_mac: sess.opts.debugging_opts.trace_macros,
        should_test: sess.opts.test,
        ..syntax::ext::expand::ExpansionConfig::default(crate_name.to_string())
    };

    let mut ecx = ExtCtxt::new(&sess.parse_sess, cfg, &mut resolver);
    let err_count = ecx.parse_sess.span_diagnostic.err_count();

    let krate = ecx.monotonic_expander().expand_crate(krate);

    krate
}



pub fn parse_crate(args: &[String]) -> (Crate, Session) {
    let (sess, cstore) = build_session(args);
    let krate = parse_crate_for_session(&sess, cstore);
    (krate, sess)
}

fn make_parser<'a>(sess: &'a Session, name: &str, src: &str) -> Parser<'a> {
    parse::new_parser_from_source_str(&sess.parse_sess,
                                      name.to_owned(),
                                      src.to_owned())
}

pub fn parse_expr(sess: &Session, src: &str) -> Result<P<Expr>, Diagnostic> {
    let mut p = make_parser(sess, "<expr>", src);
    match p.parse_expr() {
        Ok(expr) => Ok(expr),
        Err(e) => Err(e.into_diagnostic()),
    }
}

pub fn parse_pat(sess: &Session, src: &str) -> Result<P<Pat>, Diagnostic> {
    let mut p = make_parser(sess, "<pat>", src);
    match p.parse_pat() {
        Ok(pat) => Ok(pat),
        Err(e) => Err(e.into_diagnostic()),
    }
}

fn mk_diagnostic(msg: &str) -> Diagnostic {
    let h = Handler::with_tty_emitter(ColorConfig::Auto, true, false, None);
    let diag = h.struct_err(msg).into_diagnostic();
    diag
}

pub fn parse_stmts(sess: &Session, src: &str) -> Result<Vec<Stmt>, Diagnostic> {
    let mut p = make_parser(sess, "<stmt>", src);
    let mut stmts = Vec::new();
    loop {
        match p.parse_full_stmt(false) {
            Ok(Some(stmt)) => stmts.push(stmt),
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
            Ok(Some(item)) => items.push(item),
            Ok(None) => break,
            Err(e) => return Err(e.into_diagnostic()),
        }
    }
    Ok(items)
}
