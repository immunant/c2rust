use std::collections::HashMap;
use syntax::ast::*;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;
use syntax::util::ThinVec;
use rustc::hir;
use rustc::hir::def::Def;

use api::*;
use ast_equiv::AstEquiv;
use bindings::Bindings;
use bindings::IntoSymbol;
use driver::{self, Phase};
use transform::Transform;
use util::Lone;


pub struct CollectToStruct {
    pub struct_name: String,
    pub instance_name: String,
}

impl Transform for CollectToStruct {
    fn transform(&self, krate: Crate, cx: &driver::Ctxt) -> Crate {
        let static_pat: P<Item> = parse_items(cx.session(), "static __x: __t = __init;").lone();


        // Map from Symbol (the name) to the DefId of the old `static`.
        let mut old_statics = HashMap::new();

        let krate = fold_modules(krate, |curs| {
            let mut matches = Vec::new();
            let mut insert_point = None;

            while let Some(bnd) = curs.advance_until_match(
                    |i| MatchCtxt::from_match(&static_pat, &i)
                            .ok().map(|mcx| mcx.bindings)) {
                if !cx.has_cursor(curs.next()) {
                    curs.advance();
                    continue;
                }
                println!("found {:?}: {:?}", bnd.ident("__x"), bnd.ty("__t"));

                // Record this static
                let node_id = curs.next().id;
                let def_id = cx.hir_map().local_def_id(node_id);
                old_statics.insert(bnd.ident("__x").name, def_id);

                if insert_point.is_none() {
                    insert_point = Some(curs.mark());
                }
                curs.remove();
                matches.push(bnd);
            }

            println!("collected {} matching statics", matches.len());

            if let Some(insert_point) = insert_point {
                curs.seek(insert_point);
                curs.insert(build_collected_struct(&self.struct_name, &matches));
                curs.insert(build_struct_instance(&self.struct_name,
                                                  &self.instance_name,
                                                  &matches));
            }
        });

        let ident_pat = parse_expr(cx.session(), "__x");
        let ident_repl = parse_expr(cx.session(), "__s.__x");
        let mut init_mcx = MatchCtxt::new();
        init_mcx.set_type("__x", BindingType::Ident);
        init_mcx.bindings.add_ident(
            "__s", Ident::with_empty_ctxt((&self.instance_name as &str).into_symbol()));

        let krate = fold_match_with(init_mcx, ident_pat, krate, |orig, bnd| {
            let static_id = match old_statics.get(&bnd.ident("__x").name) {
                Some(&x) => x,
                None => return orig,
            };

            let def = match cx.hir_map().expect_expr(orig.id).node {
                hir::ExprPath(hir::QPath::Resolved(_, ref path)) => &path.def,
                _ => return orig,
            };

            if def.def_id() != static_id {
                return orig;
            }

            // This really is a reference to one of the collected statics.  Replace it with a
            // reference to the generated struct.
            ident_repl.clone().subst(&bnd)
        });

        krate
    }
}

fn build_collected_struct(name: &str, matches: &[Bindings]) -> P<Item> {
    let fields = matches.iter().map(|bnd| StructField {
        span: DUMMY_SP,
        ident: Some(bnd.ident("__x").clone()),
        vis: Visibility::Inherited,
        id: DUMMY_NODE_ID,
        ty: bnd.ty("__t").clone(),
        attrs: Vec::new(),
    }).collect::<Vec<_>>();

    let var_data = VariantData::Struct(fields, DUMMY_NODE_ID);

    P(Item {
        ident: Ident::with_empty_ctxt(name.into_symbol()),
        attrs: Vec::new(),
        id: DUMMY_NODE_ID,
        node: ItemKind::Struct(var_data, Generics::default()),
        vis: Visibility::Inherited,
        span: DUMMY_SP,
    })
}

fn build_struct_instance(struct_name: &str,
                         instance_name: &str,
                         matches: &[Bindings]) -> P<Item> {
    let path = Path {
        span: DUMMY_SP,
        segments: vec![
            PathSegment {
                identifier: Ident::with_empty_ctxt(struct_name.into_symbol()),
                span: DUMMY_SP,
                parameters: None,
            },
        ],
    };

    let ty = P(Ty {
        id: DUMMY_NODE_ID,
        node: TyKind::Path(None, path.clone()),
        span: DUMMY_SP,
    });

    let fields = matches.iter().map(|bnd| Field {
        ident: Spanned {
            node: bnd.ident("__x").clone(),
            span: DUMMY_SP,
        },
        expr: bnd.expr("__init").clone(),
        span: DUMMY_SP,
        is_shorthand: false,
        attrs: ThinVec::new(),
    }).collect::<Vec<_>>();

    let init = P(Expr {
        id: DUMMY_NODE_ID,
        node: ExprKind::Struct(path, fields, None),
        span: DUMMY_SP,
        attrs: ThinVec::new(),
    });

    P(Item {
        ident: Ident::with_empty_ctxt(instance_name.into_symbol()),
        attrs: Vec::new(),
        id: DUMMY_NODE_ID,
        node: ItemKind::Static(ty, Mutability::Mutable, init),
        vis: Visibility::Inherited,
        span: DUMMY_SP,
    })
}
