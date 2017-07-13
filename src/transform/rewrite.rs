use syntax::ast::Crate;

use api::*;
use command::CommandState;
use contains_mark::contains_mark;
use driver;
use transform::Transform;


pub struct RewriteExpr {
    pub pat: String,
    pub repl: String,
}

impl Transform for RewriteExpr {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let pat = parse_expr(cx.session(), &self.pat);
        let repl = parse_expr(cx.session(), &self.repl);

        let mut init_mcx = MatchCtxt::new();
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

        fold_match_with(init_mcx, pat, krate, |ast, bnd| {
            if !contains_mark(&*ast, "target", st) {
                return ast;
            }

            repl.clone().subst(&bnd)
        })
    }
}
