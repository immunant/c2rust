use syntax::ast::*;
use transform::Transform;
use command::{CommandState, Registry};
use driver::{self, Phase, parse_impl_items};
use api::*;
use syntax::ptr::P;
use bindings::Bindings;
use syntax::util::small_vector::SmallVector;
use transform;
use fold_node;
use syntax::symbol::keywords;

pub struct Ionize {

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
    fn transform(&self, mut krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {

        let as_variant_methods = generate_enum_accessors(cx);

        krate = fold_nodes(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return SmallVector::one(i)
            }

            if let ItemKind::Union(VariantData::Struct(ref fields, _), _) = i.node {
                let impl_items = fields.iter().flat_map(|x| {
                    let mut bnd = Bindings::new();
                    bnd.add_ident("__enum", i.ident);
                    bnd.add_ident("__constructor", x.ident.expect("missing struct field"));
                    bnd.add_ident("__type", i.ident); // XXX: PLACEHOLDER
                    bnd.add_ident("__as_variant", i.ident); // XXX: PLACEHOLDER
                    bnd.add_ident("__as_variant_mut", i.ident); // XXX: PLACEHOLDER
                    generate_enum_accessors(cx).subst(st, cx, &bnd)
                }).collect();

                let enum_variants = fields.iter().map(|x| {
                    let enum_field = mk().struct_field(keywords::Invalid.ident(), x.ty.clone());
                    mk().variant(x.ident.expect("expected field name to be populated"), VariantData::Tuple(vec![enum_field], DUMMY_NODE_ID))
                }).collect();

                let ty = mk().ident_ty(i.ident);
                let impl_ = mk().impl_item(mk().ident_ty(i.ident), impl_items);
                let enum_ = mk().enum_item(i.ident, enum_variants);


                SmallVector::many(vec![impl_, enum_])
            } else {
                panic!("ionize: Marked target not a union")
            }
        });

        krate
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("ionize", |args| mk(Ionize{}))
}