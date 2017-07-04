use syntax::ast::*;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;
use syntax::util::ThinVec;

use api::*;
use driver::{self, Phase};
use transform::Transform;


pub struct AssignToUpdate;

impl Transform for AssignToUpdate {
    fn transform(&self, krate: Crate, cx: &driver::Ctxt) -> Crate {
        let pat = parse_expr(cx.session(), "__x.__f = __y").unwrap();
        let repl = parse_expr(cx.session(), "__x = __rhs").unwrap();

        fold_match(pat, krate, |orig, mut bnd| {
            let x = bnd.expr("__x").clone();

            let struct_def_id = match cx.node_type(x.id).ty_to_def_id() {
                Some(x) => x,
                None => return orig,
            };
            let struct_path = cx.def_path(struct_def_id);

            // TODO: adding a Path binding type should let us simplify this
            let field = Field {
                ident: Spanned {
                    node: bnd.ident("__f").clone(),
                    span: DUMMY_SP,
                },
                expr: bnd.expr("__y").clone(),
                span: DUMMY_SP,
                is_shorthand: false,
                attrs: ThinVec::new(),
            };

            let struct_expr = Expr {
                id: DUMMY_NODE_ID,
                node: ExprKind::Struct(struct_path, vec![field], Some(x)),
                span: DUMMY_SP,
                attrs: ThinVec::new(),
            };

            bnd.add_expr("__rhs", P(struct_expr));
            repl.clone().subst(&bnd)
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}
