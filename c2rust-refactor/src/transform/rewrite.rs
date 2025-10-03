use rustc_ast::Crate;
use rustc_span::symbol::Symbol;

use crate::command::{CommandState, Registry};
use crate::contains_mark::contains_mark;
use crate::driver::Phase;
use crate::matcher::{MatchCtxt, Subst, mut_visit_match_with};
use crate::transform::Transform;
use crate::ast_builder::IntoSymbol;
use crate::RefactorCtxt;


/// # `rewrite_expr` Command
/// 
/// Usage: `rewrite_expr PAT REPL [FILTER]`
/// 
/// Marks: reads `FILTER`, if set; may read other marks depending on `PAT`
/// 
/// For every expression in the crate matching `PAT`, replace it with `REPL`.
/// `PAT` and `REPL` are both Rust expressions.  `PAT` can use placeholders to
/// capture nodes from the matched AST, and `REPL` can refer to those same
/// placeholders to substitute in the captured nodes.  See the `matcher` module for
/// details on AST pattern matching.
/// 
/// If `FILTER` is provided, only expressions marked `FILTER` will be rewritten.
/// This usage is obsolete - change `PAT` to `marked!(PAT, FILTER)` to get the same
/// behavior.
/// 
/// Example:
/// 
/// ```ignore
///     fn double(x: i32) -> i32 {
///         x * 2
///     }
/// ```
/// 
/// After running `rewrite_expr '$e * 2' '$e + $e'`:
/// 
/// ```ignore
///     fn double(x: i32) -> i32 {
///         x + x
///     }
/// ```
/// 
/// Here `$e * 2` matches `x * 2`, capturing `x` as `$e`.  Then `x` is
/// substituted for `$e` in `$e + $e`, producing the final expression `x + x`.
pub struct RewriteExpr {
    pub pat: String,
    pub repl: String,
    pub filter: Option<Symbol>,
}

impl Transform for RewriteExpr {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let mut mcx = MatchCtxt::new(st, cx);
        let pat = mcx.parse_expr(&self.pat);
        let repl = mcx.parse_expr(&self.repl);
        mut_visit_match_with(mcx, pat, krate, |ast, mcx| {
            if let Some(filter) = self.filter {
                if !contains_mark(&**ast, filter, st) {
                    return;
                }
            }

            *ast = repl.clone().subst(st, cx, &mcx.bindings);
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `rewrite_ty` Command
/// 
/// Usage: `rewrite_ty PAT REPL [FILTER]`
/// 
/// Marks: reads `FILTER`, if set; may read other marks depending on `PAT`
/// 
/// For every type in the crate matching `PAT`, replace it with `REPL`.  `PAT` and
/// `REPL` are both Rust types.  `PAT` can use placeholders to capture nodes from
/// the matched AST, and `REPL` can refer to those same placeholders to substitute
/// in the captured nodes.  See the `matcher` module for details on AST pattern
/// matching.
/// 
/// If `FILTER` is provided, only expressions marked `FILTER` will be rewritten.
/// This usage is obsolete - change `PAT` to `marked!(PAT, FILTER)` to get the same
/// behavior.
/// 
/// See the documentation for `rewrite_expr` for an example of this style of
/// rewriting.
pub struct RewriteTy {
    pub pat: String,
    pub repl: String,
    pub filter: Option<Symbol>,
}

impl Transform for RewriteTy {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let mut mcx = MatchCtxt::new(st, cx);
        let pat = mcx.parse_ty(&self.pat);
        let repl = mcx.parse_ty(&self.repl);
        mut_visit_match_with(mcx, pat, krate, |ast, mcx| {
            if let Some(filter) = self.filter {
                if !contains_mark(&**ast, filter, st) {
                    return;
                }
            }

            *ast = repl.clone().subst(st, cx, &mcx.bindings);
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `rewrite_stmts` Command
///
/// Usage: `rewrite_stmts PAT REPL`
///
/// For every statement sequence in the crate matching `PAT`, replace it with `REPL`.  `PAT` and
/// `REPL` are both Rust statement sequences.  `PAT` can use placeholders to capture nodes from
/// the matched AST, and `REPL` can refer to those same placeholders to substitute
/// in the captured nodes.  See the `matcher` module for details on AST pattern
/// matching.
///
/// See the documentation for `rewrite_expr` for an example of this style of
/// rewriting.
pub struct RewriteStmts {
    pub pat: String,
    pub repl: String,
}

impl Transform for RewriteStmts {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let mut mcx = MatchCtxt::new(st, cx);
        let pat = mcx.parse_stmts(&self.pat);
        let repl = mcx.parse_stmts(&self.repl);
        mut_visit_match_with(mcx, pat, krate, |ast, mcx| {
            *ast = repl.clone().subst(st, cx, &mcx.bindings);
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


pub struct DebugMatchExpr {
    pub pat: String,
}

impl Transform for DebugMatchExpr {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {

        let mut init_mcx = MatchCtxt::new(st, cx);
        init_mcx.debug = true;
        let pat = init_mcx.parse_expr(&self.pat);
        mut_visit_match_with(init_mcx, pat, krate, |ast, _mcx| {
            eprintln!("matched node {:?}", ast);
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}



pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("rewrite_expr", |args| mk(RewriteExpr {
        pat: args[0].clone(),
        repl: args[1].clone(),
        filter: if args.len() >= 3 { Some((&args[2]).into_symbol()) } else { None },
    }));
    reg.register("rewrite_ty", |args| mk(RewriteTy {
        pat: args[0].clone(),
        repl: args[1].clone(),
        filter: if args.len() >= 3 { Some((&args[2]).into_symbol()) } else { None },
    }));
    reg.register("rewrite_stmts", |args| mk(RewriteStmts {
        pat: args[0].clone(),
        repl: args[1].clone(),
    }));

    reg.register("debug_match_expr", |args| mk(DebugMatchExpr {
        pat: args[0].clone(),
    }));
}
