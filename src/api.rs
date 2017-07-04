use rustc::hir::def_id::DefId;
use rustc::session::Session;
use rustc::ty::Ty;
use rustc::ty::item_path::{ItemPathBuffer, RootMode};
use syntax::ast::NodeId;
use syntax::ast::{Path, PathSegment, Ident};
use syntax::codemap::DUMMY_SP;
use syntax::symbol::Symbol;
use syntax::symbol::keywords;

pub use matcher::MatchCtxt;
pub use matcher::{fold_match, fold_match_with};
pub use driver::{parse_expr, parse_pat, parse_stmts, parse_items};
pub use subst::Subst;
pub use bindings::Type as BindingType;

use bindings::Bindings;
use bindings::IntoSymbol;
use driver;
use fold::Fold;
use matcher::Pattern;

pub fn replace_expr<T: Fold>(sess: &Session,
                             ast: T,
                             pat: &str,
                             repl: &str) -> <T as Fold>::Result {
    let pat = parse_expr(sess, pat).unwrap();
    let repl = parse_expr(sess, repl).unwrap();
    fold_match(pat, ast, |_, bnd| repl.clone().subst(&bnd))
}

pub fn replace_stmts<T: Fold>(sess: &Session,
                              ast: T,
                              pat: &str,
                              repl: &str) -> <T as Fold>::Result {
    let pat = parse_stmts(sess, pat).unwrap();
    let repl = parse_stmts(sess, repl).unwrap();
    fold_match(pat, ast, |_, bnd| repl.clone().subst(&bnd))
}


pub fn find_first_with<P, T>(init_mcx: MatchCtxt,
                             pattern: P,
                             target: T) -> Option<Bindings>
        where P: Pattern, T: Fold {
    let mut result = None;
    fold_match_with(init_mcx, pattern, target, |p, bnd| {
        if result.is_none() {
            result = Some(bnd);
        }
        p
    });
    result
}

pub fn find_first<P, T>(pattern: P,
                        target: T) -> Option<Bindings>
        where P: Pattern, T: Fold {
    find_first_with(MatchCtxt::new(), pattern, target)
}


pub trait DriverCtxtExt<'gcx> {
    fn node_type(&self, id: NodeId) -> Ty<'gcx>;
    fn def_path(&self, id: DefId) -> Path;
}

impl<'a, 'hir, 'gcx, 'tcx> DriverCtxtExt<'gcx> for driver::Ctxt<'a, 'hir, 'gcx, 'tcx> {
    fn node_type(&self, id: NodeId) -> Ty<'gcx> {
        let parent = self.hir_map().get_parent(id);
        let parent_body = self.hir_map().body_owned_by(parent);
        let tables = self.ty_ctxt().body_tables(parent_body);
        tables.node_id_to_type(id)
    }

    fn def_path(&self, id: DefId) -> Path {
        let root = PathSegment {
            identifier: keywords::CrateRoot.ident(),
            span: DUMMY_SP,
            parameters: None,
        };
        let mut buf = ItemPathVec(RootMode::Local, vec![root]);
        self.ty_ctxt().push_item_path(&mut buf, id);

        Path {
            span: DUMMY_SP,
            segments: buf.1,
        }
    }
}


struct ItemPathVec(RootMode, Vec<PathSegment>);

impl ItemPathBuffer for ItemPathVec {
    fn root_mode(&self) -> &RootMode {
        &self.0
    }

    fn push(&mut self, text: &str) {
        self.1.push(PathSegment {
            identifier: Ident::with_empty_ctxt(text.into_symbol()),
            span: DUMMY_SP,
            parameters: None,
        });
    }
}
