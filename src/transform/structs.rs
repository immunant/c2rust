use syntax::ast::*;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;
use syntax::util::ThinVec;

use api::*;
use ast_equiv::AstEquiv;
use driver::{self, Phase};
use transform::Transform;
use util::Lone;


pub struct AssignToUpdate;

impl Transform for AssignToUpdate {
    fn transform(&self, krate: Crate, cx: &driver::Ctxt) -> Crate {
        let pat = parse_expr(cx.session(), "__x.__f = __y");
        let repl = parse_expr(cx.session(), "__x = __s { __f: __y, .. __x }");

        fold_match(pat, krate, |orig, mut bnd| {
            let x = bnd.expr("__x").clone();

            let struct_def_id = match cx.node_type(x.id).ty_to_def_id() {
                Some(x) => x,
                None => return orig,
            };
            let struct_path = cx.def_path(struct_def_id);

            bnd.add_path("__s", struct_path);
            repl.clone().subst(&bnd)
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


pub struct MergeUpdates;

impl Transform for MergeUpdates {
    fn transform(&self, krate: Crate, cx: &driver::Ctxt) -> Crate {
        fold_blocks(krate, |curs| {
            loop {
                // Find a struct update.
                curs.advance_until(|s| is_struct_update(s));
                if curs.eof() {
                    break;
                }
                let (path, mut fields, base) = unpack_struct_update(curs.remove());

                // Collect additional updates to the same struct.
                while !curs.eof() && is_struct_update_for(curs.next(), &base) {
                    let (_, mut more_fields, _) = unpack_struct_update(curs.remove());
                    fields.append(&mut more_fields)
                }

                // Build a new struct update and store it.
                curs.insert(build_struct_update(path, fields, base))
            }
        })
    }
}

fn is_struct_update(s: &Stmt) -> bool {
    let e = match_or!([s.node] StmtKind::Semi(ref e) => e; return false);
    let (lhs, rhs) = match_or!([e.node] ExprKind::Assign(ref lhs, ref rhs) => (lhs, rhs);
                               return false);
    match_or!([rhs.node] ExprKind::Struct(_, _, Some(ref base)) => lhs.ast_equiv(base);
              return false)
}

fn is_struct_update_for(s: &Stmt, base1: &Expr) -> bool {
    let e = match_or!([s.node] StmtKind::Semi(ref e) => e; return false);
    let (lhs, rhs) = match_or!([e.node] ExprKind::Assign(ref lhs, ref rhs) => (lhs, rhs);
                               return false);
    match_or!([rhs.node] ExprKind::Struct(_, _, Some(ref base)) => base1.ast_equiv(base);
              return false)
}

fn unpack_struct_update(s: Stmt) -> (Path, Vec<Field>, P<Expr>) {
    let e = expect!([s.node] StmtKind::Semi(e) => e);
    let (lhs, rhs) = expect!([e.unwrap().node] ExprKind::Assign(lhs, rhs) => (lhs, rhs));
    expect!([rhs.unwrap().node]
            ExprKind::Struct(path, fields, Some(base)) => (path, fields, base))
}

fn build_struct_update(path: Path, fields: Vec<Field>, base: P<Expr>) -> Stmt {
    mk().semi_stmt(
        mk().assign_expr(
            &base,
            mk().struct_expr_base(path, fields, Some(&base))))
}
