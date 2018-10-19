use std::collections::HashSet;
use syntax::ast::*;
use syntax::source_map::DUMMY_SP;
use syntax::ptr::P;
use syntax::symbol::Symbol;

use api::*;
use command::{CommandState, Registry};
use driver;
use transform::Transform;
use util::IntoSymbol;
use util::HirDefExt;


/// Add a type variable with the given name (default: `T`) to each `target` item, and replace each
/// `target` type annotation within a `target` item with the new type variable.  This makes all
/// items with a `target` mark or containing a `target` type annotation generic over the new type
/// variable, and adjusts references between them appropriately.  References from unmarked items to
/// rewritten ones will set the type argument to the provided replacement type (default: use the
/// first replaced type annotation).
pub struct GeneralizeItems {
    ty_var_name: Symbol,
    replacement_ty: Option<String>,
}

impl Transform for GeneralizeItems {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find marked types and replace with the named type variable.

        // Map from item NodeId to the concrete type that was replaced with the type variable.
        // These types are used later as the actual parameters in references to rewritten items.
        // If more than one type was replaced, only the first will be kept in this map.
        let mut replacement_ty = self.replacement_ty.as_ref()
            .map(|s| parse_ty(cx.session(), s));

        let krate = fold_nodes(krate, |ty: P<Ty>| {
            if !st.marked(ty.id, "target") {
                return ty;
            }

            let parent_id = cx.hir_map().get_parent(ty.id);
            if !st.marked(parent_id, "target") {
                return ty;
            }

            if replacement_ty.is_none() {
                replacement_ty = Some(ty.clone());
            }
            mk().ident_ty(self.ty_var_name)
        });

        // (2) Add parameters to rewritten items.

        let mut item_def_ids = HashSet::new();
        let krate = fold_nodes(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return smallvec![i];
            }
            item_def_ids.insert(cx.node_def_id(i.id));
            smallvec![i.map(|mut i| {
                {
                    let gen = match i.node {
                        ItemKind::Fn(_, _, _, _, ref mut gen, _) => gen,
                        ItemKind::Enum(_, ref mut gen) => gen,
                        ItemKind::Struct(_, ref mut gen) => gen,
                        ItemKind::Union(_, ref mut gen) => gen,
                        ItemKind::Trait(_, _, ref mut gen, _, _) => gen,
                        ItemKind::Impl(_, _, _, ref mut gen, _, _, _) => gen,
                        _ => panic!("item has no room for generics"),
                    };
                    gen.params.push(GenericParam::Type(mk().ty_param(self.ty_var_name)));
                }
                i
            })]
        });

        // (3) Rewrite references to each item, replacing `X` with `X<ty1>`.  If the reference to
        // rewritten item `X` appears inside another rewritten item `Y`, we instead replace `X`
        // with `X<T>`, referring to `Y`'s instance of the type parameter.

        let replacement_ty = replacement_ty
            .expect("must provide a replacement type argument or mark");

        let krate = fold_resolved_paths_with_id(krate, cx, |path_id, qself, mut path, def| {
            match def.opt_def_id() {
                Some(def_id) if item_def_ids.contains(&def_id) => (),
                _ => return (qself, path),
            };

            let parent_id = cx.hir_map().get_parent(path_id);
            let arg = if st.marked(parent_id, "target") {
                mk().ident_ty(self.ty_var_name)
            } else {
                replacement_ty.clone()
            };

            {
                let seg = path.segments.last_mut().unwrap();
                if let Some(ref mut params) = seg.parameters {
                    *params = params.clone().map(|mut params| {
                        match params {
                            GenericArgs::AngleBracketed(ref mut abpd) =>
                                abpd.types.push(arg),
                            GenericArgs::Parenthesized(..) =>
                                panic!("expected angle bracketed params, but found parenthesized"),
                        }
                        params
                    });
                } else {
                    let abpd = AngleBracketedArgs {
                        span: DUMMY_SP,
                        lifetimes: vec![],
                        types: vec![arg],
                        bindings: vec![],
                    };
                    seg.parameters = Some(P(GenericArgs::AngleBracketed(abpd)));
                }
            }

            (qself, path)
        });

        krate
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("generalize_items", |args| mk(GeneralizeItems {
        ty_var_name: args.get(0).map_or("T", |x| x).into_symbol(),
        replacement_ty: args.get(1).cloned(),
    }));
}
