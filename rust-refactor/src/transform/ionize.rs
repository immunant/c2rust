use api::*;
use ast_manip::lr_expr::*;
use command::{CommandState, Registry};
use driver::{self, Phase, parse_impl_items};
use reflect::reflect_def_path;
use rustc::hir::def_id::DefId;
use rustc::ty::TyKind;
use std::collections::HashSet;
use std::fmt::Display;
use syntax::ast::*;
use syntax::fold::Folder;
use syntax::ptr::P;
use transform::Transform;

pub struct Ionize {

}

struct ExprFolder<F> {
    callback: F,
}

impl<F: FnMut(P<Expr>) -> P<Expr>> Folder for ExprFolder<F> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        (self.callback)(e)
    }
}

fn fold_top_exprs<T, F>(x: T, callback: F) -> <T as Fold>::Result
    where T: Fold, F: FnMut(P<Expr>) -> P<Expr> {
    let mut f = ExprFolder { callback: callback };
    x.fold(&mut f)
}

fn accessor_name<T: Display>(fieldname: T) -> Ident {
    mk().ident(format!("as_{}", fieldname))
}

fn mut_accessor_name<T: Display>(fieldname: T) -> Ident {
    mk().ident(format!("as_{}_mut", fieldname))
}

fn generate_enum_accessors(cx: &driver::Ctxt) -> Vec<ImplItem> {
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
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {

        let _as_variant_methods = generate_enum_accessors(cx);
        let outer_assignment_pat = parse_stmts(cx.session(), "__val.__field = __expr;");
        let outer_assignment_repl = parse_stmts(cx.session(), "__val = __con(__expr);");

        // Definition Ids of all marked unions
        let mut targets: HashSet<DefId> = HashSet::new();

        // Find marked unions
        visit_nodes(&krate, |i: &Item| {
            if st.marked(i.id, "target") {
                if let ItemKind::Union(VariantData::Struct(ref _fields, _), _) = i.node {
                    if let Some(def_id) = cx.hir_map().opt_local_def_id(i.id) {
                        targets.insert(def_id);
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
        let krate = fold_match_with(mcx, outer_assignment_pat, krate, |e, bnd| {
            let field = bnd.ident("__field");
            let _expr = bnd.expr("__expr");
            let val = bnd.expr("__val");


            let ty0 = cx.adjusted_node_type(val.id);
            match ty0.sty {
                TyKind::TyAdt(ref adt, _) if targets.contains(&adt.did) => {

                    let (_qself, mut path) = reflect_def_path(cx.ty_ctxt(), adt.did);
                    path.segments.push(mk().path_segment(field));
                    let mut bnd1 = bnd.clone();
                    bnd1.add_expr("__con", mk().path_expr(path));

                    outer_assignment_repl.clone().subst(st, cx, &bnd1)
                }
                _ => e
            }
        });

        let outer_access_pat = parse_expr(cx.session(), "__val.__field");
        let outer_access_repl = parse_expr(cx.session(), "__val.__accessor()");
        let mut mcx = MatchCtxt::new(st, cx);
        mcx.set_type("__field", BindingType::Ident);
        mcx.set_type("__val", BindingType::Expr);

        let krate = fold_top_exprs(krate, |e: P<Expr>| {
            fold_expr_with_context(e, lr_expr::Context::Rvalue, |e, context| {
                if Context::Rvalue == context {
                    match mcx.clone_match(&*outer_access_pat, &*e) {
                        Ok(mcx1) => {
                            let mut bnd = mcx1.bindings.clone();
                            let accessor = accessor_name(bnd.ident("__field"));
                            bnd.add_ident("__accessor", accessor);
                            outer_access_repl.clone().subst(st, cx, &bnd)
                        }
                        Err(_) => e,
                    }
                } else {
                    e
                }
            })
        });

        // Replace union with enum
        let krate = fold_nodes(krate, |i: P<Item>| {
            match cx.hir_map().opt_local_def_id(i.id) {
                Some(ref def_id) if targets.contains(def_id) => {}
                _ => return smallvec![i]
            }

            if let ItemKind::Union(VariantData::Struct(ref fields, _), _) = i.node {
                let impl_items = fields.iter().flat_map(|x| {
                    let mut bnd = Bindings::new();
                    let fieldname = x.ident.expect("missing union field");
                    let accessor = accessor_name(fieldname);
                    let accessor_mut = mut_accessor_name(fieldname);
                    bnd.add_ident("__enum", i.ident);
                    bnd.add_ident("__constructor", fieldname);
                    bnd.add_ty("__type", x.ty.clone());
                    bnd.add_ident("__as_variant", accessor);
                    bnd.add_ident("__as_variant_mut", accessor_mut);
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

        krate
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("ionize", |_args| mk(Ionize{}))
}
