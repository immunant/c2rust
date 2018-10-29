use syntax::ast::Crate;
use syntax::symbol::Symbol;

use api::*;
use command::{CommandState, Registry};
use contains_mark::contains_mark;
use driver::{self, Phase};
use transform::Transform;
use rust_ast_builder::IntoSymbol;


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

    // "d" for "definition"
    init_mcx.set_type("__d", BindingType::Item);
    init_mcx.set_type("__c", BindingType::Item);

    init_mcx
}


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
}
