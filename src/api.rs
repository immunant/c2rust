use rustc::session::Session;

pub use matcher::MatchCtxt;
pub use matcher::{fold_match, fold_match_with};
pub use driver::{parse_expr, parse_pat, parse_stmts, parse_items};
pub use subst::Subst;
pub use bindings::Type as BindingType;

use fold::Fold;

pub fn replace_expr<T: Fold>(sess: &Session,
                             ast: T,
                             pat: &str,
                             repl: &str) -> <T as Fold>::Result {
    let pat = parse_expr(sess, pat).unwrap();
    let repl = parse_expr(sess, repl).unwrap();
    fold_match(pat, ast, |_, bnd| repl.clone().subst(&bnd))
}
