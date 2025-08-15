use rustc_hir::def_id::DefId;
use rustc_type_ir::sty::TyKind;
use std::collections::HashSet;
use std::fmt::Display;
use rustc_ast::*;
use rustc_ast::mut_visit::MutVisitor;
use rustc_ast::ptr::P;
use rustc_span::symbol::Ident;
use smallvec::smallvec;

use crate::ast_builder::mk;
use crate::ast_manip::{FlatMapNodes, MutVisit, visit_nodes};
use crate::ast_manip::lr_expr::{self, fold_expr_with_context};
use crate::command::{CommandState, Registry};
use crate::driver::{Phase, parse_impl_items, parse_stmts, parse_expr};
use crate::reflect::reflect_def_path;
use crate::matcher::{Bindings, BindingType, MatchCtxt, Subst, mut_visit_match_with};
use crate::transform::Transform;
use crate::RefactorCtxt;

/// # `ionize` Command
/// 
/// Usage: `ionize`
/// 
/// Marks: `target`
/// 
/// Convert each union marked `target` to a type-safe Rust enum.  The generated
/// enums will have `as_variant` and `as_variant_mut` methods for each union field,
/// which panic if the enum is not the named variant.  Also updates assignments to
/// union variables to assign one of the new enum variants, and updates uses of
/// union fields to call the new methods instead.
pub struct Ionize {

}

struct ExprFolder<F> {
    callback: F,
}

impl<F: FnMut(&mut P<Expr>)> MutVisitor for ExprFolder<F> {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        (self.callback)(e)
    }
}

fn fold_top_exprs<T, F>(x: &mut T, callback: F)
    where T: MutVisit, F: FnMut(&mut P<Expr>) {
    let mut f = ExprFolder { callback: callback };
    x.visit(&mut f)
}

fn accessor_name<T: Display>(fieldname: T) -> Ident {
    mk().ident(format!("as_{}", fieldname))
}

fn mut_accessor_name<T: Display>(fieldname: T) -> Ident {
    mk().ident(format!("as_{}_mut", fieldname))
}

fn generate_enum_accessors(cx: &RefactorCtxt) -> Vec<P<AssocItem>> {
    parse_impl_items(cx.session(), r#"

    fn __as_variant(&self) -> &__type {
        match *self {
            __enum::__constructor(ref x) => x,
            _ => panic!("wrong variant"),
        }
    }

    fn __as_variant_mut(&mut self) -> &mut __type {
        match *self {
            __enum::__constructor(ref mut x) => x,
            _ => panic!("wrong variant"),
        }
    }

    "#)
}

impl Transform for Ionize {
    fn min_phase(&self) -> Phase { Phase::Phase3 }
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {

        let _as_variant_methods = generate_enum_accessors(cx);
        let outer_assignment_pat = parse_stmts(cx.session(), "__val.__field = __expr;");
        let outer_assignment_repl = parse_stmts(cx.session(), "__val = __con(__expr);");

        // Definition Ids of all marked unions
        let mut targets: HashSet<DefId> = HashSet::new();

        // Find marked unions
        visit_nodes(krate, |i: &Item| {
            if st.marked(i.id, "target") {
                if let ItemKind::Union(VariantData::Struct(ref _fields, _), _) = i.kind {
                    if let Some(def_id) = cx.hir_map().opt_local_def_id_from_node_id(i.id) {
                        targets.insert(def_id.to_def_id());
                    } else {
                        panic!("Bad target, no def id")
                    }
                } else {
                    panic!("Bad target, expected union")
                }
            }
       });

        let mut mcx = MatchCtxt::new(st, cx);
        mcx.set_type("__field", BindingType::Ident);
        mcx.set_type("__expr", BindingType::Expr);
        mcx.set_type("__val", BindingType::Expr);

        // Replace union assignment with enum assignment
        mut_visit_match_with(mcx, outer_assignment_pat, krate, |e, mcx| {
            let field = mcx.bindings.get::<_, Ident>("__field").unwrap();
            let _expr = mcx.bindings.get::<_, P<Expr>>("__expr").unwrap();
            let val = mcx.bindings.get::<_, P<Expr>>("__val").unwrap();


            let ty0 = cx.adjusted_node_type(val.id);
            match ty0.kind() {
                TyKind::Adt(ref adt, _) if targets.contains(&adt.did()) => {

                    let (_qself, mut path) = reflect_def_path(cx.ty_ctxt(), adt.did());
                    path.segments.push(mk().path_segment(field));
                    let mut bnd1 = mcx.bindings.clone();
                    bnd1.add("__con", mk().path_expr(path));

                    *e = outer_assignment_repl.clone().subst(st, cx, &bnd1);
                }
                _ => {}
            }
        });

        let outer_access_pat = parse_expr(cx.session(), "__val.__field");
        let outer_access_repl = parse_expr(cx.session(), "__val.__accessor()");
        let mut mcx = MatchCtxt::new(st, cx);
        mcx.set_type("__field", BindingType::Ident);
        mcx.set_type("__val", BindingType::Expr);

        fold_top_exprs(krate, |e: &mut P<Expr>| {
            fold_expr_with_context(e, lr_expr::Context::Rvalue, |e, context| {
                if lr_expr::Context::Rvalue == context {
                    match mcx.clone_match(&*outer_access_pat, &*e) {
                        Ok(mcx1) => {
                            let mut bnd = mcx1.bindings.clone();
                            let accessor = accessor_name(bnd.get::<_, Ident>("__field").unwrap());
                            bnd.add("__accessor", accessor);
                            *e = outer_access_repl.clone().subst(st, cx, &bnd);
                        }
                        Err(_) => {}
                    }
                }
            });
        });

        // Replace union with enum
        FlatMapNodes::visit(krate, |i: P<Item>| {
            match cx.hir_map().opt_local_def_id_from_node_id(i.id) {
                Some(ref def_id) if targets.contains(&def_id.to_def_id()) => {}
                _ => return smallvec![i]
            }

            if let ItemKind::Union(VariantData::Struct(ref fields, _), _) = i.kind {
                let impl_items = fields.iter().flat_map(|x| {
                    let mut bnd = Bindings::new();
                    let fieldname = x.ident.expect("missing union field");
                    let accessor = accessor_name(fieldname);
                    let accessor_mut = mut_accessor_name(fieldname);
                    bnd.add("__enum", i.ident);
                    bnd.add("__constructor", fieldname);
                    bnd.add("__type", x.ty.clone());
                    bnd.add("__as_variant", accessor);
                    bnd.add("__as_variant_mut", accessor_mut);
                    generate_enum_accessors(cx).subst(st, cx, &bnd)
                }).collect();

                let enum_variants = fields.iter().map(|x| {
                    let enum_field = mk().enum_field(x.ty.clone());
                    mk().variant(x.ident.expect("expected field name to be populated"),
                                 VariantData::Tuple(vec![enum_field], DUMMY_NODE_ID))
                }).collect();

                let _ty = mk().ident_ty(i.ident);
                let impl_ = mk().impl_item(mk().ident_ty(i.ident), impl_items);
                let enum_ = mk().enum_item(i.ident, enum_variants);


                smallvec![impl_, enum_]
            } else {
                panic!("ionize: Marked target not a union")
            }
        });
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("ionize", |_args| mk(Ionize{}))
}
