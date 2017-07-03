use rustc::session::Session;

pub use matcher::MatchCtxt;
pub use matcher::{fold_match, fold_match_with};
pub use driver::{parse_expr, parse_pat, parse_stmts, parse_items};
pub use subst::Subst;
pub use bindings::Type as BindingType;

use bindings::Bindings;
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
