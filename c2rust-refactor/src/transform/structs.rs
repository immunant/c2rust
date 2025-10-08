use rustc_middle::ty;
use rustc_ast::*;
use rustc_ast::ptr::P;
use rustc_span::symbol::Ident;

use smallvec::smallvec;

use crate::ast_manip::{fold_blocks, FlatMapNodes, AstEquiv};
use crate::command::{CommandState, Registry};
use crate::driver::{Phase, parse_expr};
use crate::{expect, match_or};
use crate::matcher::{mut_visit_match, Subst};
use crate::path_edit::fold_resolved_paths;
use crate::transform::Transform;
use crate::ast_builder::{mk, IntoSymbol};
use crate::RefactorCtxt;


/// # `struct_assign_to_update` Command
///
/// Usage: `struct_assign_to_update`
///
/// Replace all struct field assignments with functional update expressions.
///
/// Example:
///
/// ```ignore
///     let mut x: S = ...;
///     x.f = 1;
///     x.g = 2;
/// ```
///
/// After running `struct_assign_to_update`:
///
/// ```ignore
///     let mut x: S = ...;
///     x = S { f: 1, ..x };
///     x = S { g: 2, ..x };
/// ```
pub struct AssignToUpdate;

impl Transform for AssignToUpdate {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let pat = parse_expr(cx.session(), "__x.__f = __y");
        let repl = parse_expr(cx.session(), "__x = __s { __f: __y, .. __x }");

        mut_visit_match(st, cx, pat, krate, |orig, mut mcx| {
            let x = mcx.bindings.get::<_, P<Expr>>("__x").unwrap().clone();

            let struct_def_id = match cx.node_type(x.id).kind() {
                ty::TyKind::Adt(ref def, _) => def.did(),
                _ => return,
            };
            let struct_path = cx.def_path(struct_def_id);

            mcx.bindings.add("__s", struct_path);
            *orig = repl.clone().subst(st, cx, &mcx.bindings);
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `struct_merge_updates` Command
///
/// Usage: `struct_merge_updates`
///
/// Merge consecutive struct updates into a single update.
///
/// Example:
///
/// ```ignore
///     let mut x: S = ...;
///     x = S { f: 1, ..x };
///     x = S { g: 2, ..x };
/// ```
///
/// After running `struct_assign_to_update`:
///
/// ```ignore
///     let mut x: S = ...;
///     x = S { f: 1, g: 2, ..x };
/// ```
pub struct MergeUpdates;

impl Transform for MergeUpdates {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, _cx: &RefactorCtxt) {
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
    let e = match_or!([s.kind] StmtKind::Semi(ref e) => e; return false);
    let (lhs, rhs) = match_or!([e.kind] ExprKind::Assign(ref lhs, ref rhs, _) => (lhs, rhs);
                               return false);
    if let ExprKind::Struct(se) = &rhs.kind {
        if let StructRest::Base(ref base) = se.rest {
            return lhs.ast_equiv(base);
        }
    }
    false
}

fn is_struct_update_for(s: &Stmt, base1: &Expr) -> bool {
    let e = match_or!([s.kind] StmtKind::Semi(ref e) => e; return false);
    let rhs = match_or!([e.kind] ExprKind::Assign(_, ref rhs, _) => rhs;
                        return false);
    if let ExprKind::Struct(se) = &rhs.kind {
        if let StructRest::Base(ref base) = se.rest {
            return base1.ast_equiv(base);
        }
    }
    false
}

fn unpack_struct_update(s: Stmt) -> (Path, Vec<ExprField>, P<Expr>) {
    let e = expect!([s.kind] StmtKind::Semi(e) => e);
    let rhs = expect!([e.into_inner().kind] ExprKind::Assign(_, rhs, _) => rhs);
    let se = expect!([rhs.into_inner().kind] ExprKind::Struct(se) => se);
    let StructExpr { path, fields, rest, .. } = se.into_inner();
    let base = expect!([rest] StructRest::Base(base) => base);
    (path, fields, base)
}

fn build_struct_update(path: Path, fields: Vec<ExprField>, base: P<Expr>) -> Stmt {
    mk().semi_stmt(
        mk().assign_expr(
            &base,
            mk().struct_expr_base(path, fields, Some(&base))))
}


/// # `rename_struct` Command
///
/// Obsolete - use `rename_items_regex` instead.
///
/// Usage: `rename_struct NAME`
///
/// Marks: `target`
///
/// Rename the struct marked `target` to `NAME`.  Only supports renaming a single
/// struct at a time.
pub struct Rename(pub String);

impl Transform for Rename {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let new_ident = Ident::with_dummy_span((&self.0 as &str).into_symbol());
        let mut target_def_id = None;

        // Find the struct definition and rename it.
        FlatMapNodes::visit(krate, |i: P<Item>| {
            if target_def_id.is_some() || !st.marked(i.id, "target") {
                return smallvec![i];
            }

            // Make sure this is actually a struct declaration, and not, say, the target
            // declaration's containing module.
            if !is_struct(&i) {
                return smallvec![i];
            }
            target_def_id = Some(cx.node_def_id(i.id));

            smallvec![i.map(|i| {
                Item {
                    ident: new_ident,
                    .. i
                }
            })]
        });

        // Find uses of the struct and rewrite them.  We need to check everywhere a Path may
        // appear, since the struct name may be used as a scope for methods or other associated
        // items.

        let target_def_id = target_def_id
            .expect("found no struct to rename");

        fold_resolved_paths(krate, cx, |qself, mut path, def| {
            if let Some(def_id) = def[0].opt_def_id() {
                if def_id == target_def_id {
                    path.segments.last_mut().unwrap().ident = new_ident;
                }
            }
            (qself, path)
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn is_struct(i: &Item) -> bool {
    if let ItemKind::Struct(ref vd, _) = i.kind {
        if let VariantData::Struct(..) = *vd {
            return true;
        }
    }
    false
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("struct_assign_to_update", |_args| mk(AssignToUpdate));
    reg.register("struct_merge_updates", |_args| mk(MergeUpdates));
    reg.register("rename_struct", |args| mk(Rename(args[0].clone())));
}
