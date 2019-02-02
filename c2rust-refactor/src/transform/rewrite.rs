use syntax::ast::Crate;
use syntax::symbol::Symbol;

use crate::api::*;
use crate::command::{CommandState, Registry};
use crate::contains_mark::contains_mark;
use crate::driver::{self, Phase};
use crate::transform::Transform;
use c2rust_ast_builder::IntoSymbol;


fn make_init_mcx<'a, 'tcx>(st: &'a CommandState,
                           cx: &'a driver::Ctxt<'a, 'tcx>)
                           -> MatchCtxt<'a, 'tcx> {
    let mut init_mcx = MatchCtxt::new(st, cx);
    init_mcx.set_type("__i", BindingType::Ident);
    init_mcx.set_type("__j", BindingType::Ident);

    init_mcx.set_type("__e", BindingType::Expr);
    init_mcx.set_type("__f", BindingType::Expr);

    init_mcx.set_type("__p", BindingType::Pat);
    init_mcx.set_type("__q", BindingType::Pat);

    init_mcx.set_type("__t", BindingType::Ty);
    init_mcx.set_type("__u", BindingType::Ty);

    init_mcx.set_type("__s", BindingType::Stmt);
    init_mcx.set_type("__r", BindingType::Stmt);

    init_mcx.set_type("__m", BindingType::MultiStmt);
    init_mcx.set_type("__n", BindingType::MultiStmt);

    // "d" for "definition"
    init_mcx.set_type("__d", BindingType::Item);
    init_mcx.set_type("__c", BindingType::Item);

    init_mcx
}


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
///     fn double(x: i32) -> i32 {
///         x * 2
///     }
/// 
/// After running `rewrite_expr '__e * 2' '__e + __e'`:
/// 
///     fn double(x: i32) -> i32 {
///         x + x
///     }
/// 
/// Here `__e * 2` matches `x * 2`, capturing `x` as `__e`.  Then `x` is
/// substituted for `__e` in `__e + __e`, producing the final expression `x + x`.
pub struct RewriteExpr {
    pub pat: String,
    pub repl: String,
    pub filter: Option<Symbol>,
}

impl Transform for RewriteExpr {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let pat = parse_expr(cx.session(), &self.pat);
        let repl = parse_expr(cx.session(), &self.repl);

        fold_match_with(make_init_mcx(st, cx), pat, krate, |ast, bnd| {
            if let Some(filter) = self.filter {
                if !contains_mark(&*ast, filter, st) {
                    return ast;
                }
            }

            repl.clone().subst(st, cx, &bnd)
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
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let pat = parse_ty(cx.session(), &self.pat);
        let repl = parse_ty(cx.session(), &self.repl);

        fold_match_with(make_init_mcx(st, cx), pat, krate, |ast, bnd| {
            if let Some(filter) = self.filter {
                if !contains_mark(&*ast, filter, st) {
                    return ast;
                }
            }

            repl.clone().subst(st, cx, &bnd)
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
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let pat = parse_stmts(cx.session(), &self.pat);
        let repl = parse_stmts(cx.session(), &self.repl);
        fold_match_with(make_init_mcx(st, cx), pat, krate, |_, bnd| {
            repl.clone().subst(st, cx, &bnd)
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
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let pat = parse_expr(cx.session(), &self.pat);

        let mut init_mcx = make_init_mcx(st, cx);
        init_mcx.debug = true;
        fold_match_with(init_mcx, pat, krate, |ast, _bnd| {
            eprintln!("matched node {:?}", ast);
            ast
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
