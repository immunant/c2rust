use syntax::ast::*;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;


use api::*;
use ast_equiv::AstEquiv;
use command::{CommandState, Registry};
use driver::{self, Phase};
use transform::Transform;
use util::IntoSymbol;


pub struct AssignToUpdate;

impl Transform for AssignToUpdate {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
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
    fn transform(&self, krate: Crate, _st: &CommandState, _cx: &driver::Ctxt) -> Crate {
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
    let rhs = match_or!([e.node] ExprKind::Assign(_, ref rhs) => rhs;
                        return false);
    match_or!([rhs.node] ExprKind::Struct(_, _, Some(ref base)) => base1.ast_equiv(base);
              return false)
}

fn unpack_struct_update(s: Stmt) -> (Path, Vec<Field>, P<Expr>) {
    let e = expect!([s.node] StmtKind::Semi(e) => e);
    let rhs = expect!([e.unwrap().node] ExprKind::Assign(_, rhs) => rhs);
    expect!([rhs.unwrap().node]
            ExprKind::Struct(path, fields, Some(base)) => (path, fields, base))
}

fn build_struct_update(path: Path, fields: Vec<Field>, base: P<Expr>) -> Stmt {
    mk().semi_stmt(
        mk().assign_expr(
            &base,
            mk().struct_expr_base(path, fields, Some(&base))))
}


pub struct Rename(pub String);

impl Transform for Rename {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let new_ident = Ident::with_empty_ctxt((&self.0 as &str).into_symbol());
        let mut target_def_id = None;

        // Find the struct definition and rename it.
        let krate = fold_nodes(krate, |i: P<Item>| {
            if target_def_id.is_some() || !st.marked(i.id, "target") {
                return SmallVector::one(i);
            }

            // Make sure this is actually a struct declaration, and not, say, the target
            // declaration's containing module.
            match_or!([struct_item_id(&i)] Some(x) => x; return SmallVector::one(i));
            target_def_id = Some(cx.node_def_id(i.id));

            SmallVector::one(i.map(|i| {
                Item {
                    ident: new_ident.clone(),
                    .. i
                }
            }))
        });

        // Find uses of the struct and rewrite them.  We need to check everywhere a Path may
        // appear, since the struct name may be used as a scope for methods or other associated
        // items.

        let target_def_id = target_def_id
            .expect("found no struct to rename");

        let krate = fold_resolved_paths(krate, cx, |qself, mut path, def_id| {
            if def_id == target_def_id {
                path.segments.last_mut().unwrap().identifier = new_ident;
            }
            (qself, path)
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn struct_item_id(i: &Item) -> Option<NodeId> {
    let vd = match_or!([i.node] ItemKind::Struct(ref vd, _) => vd; return None);
    let id = match_or!([*vd] VariantData::Struct(_, id) => id; return None);
    Some(id)
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("struct_assign_to_update", |_args| mk(AssignToUpdate));
    reg.register("struct_merge_updates", |_args| mk(MergeUpdates));
    reg.register("rename_struct", |args| mk(Rename(args[0].clone())));
}
