use std::borrow::Cow;
use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use regex::Regex;
use rustc::hir::def_id::DefId;
use rustc::ty::TypeVariants;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::attr;
use syntax::codemap::Spanned;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::util::small_vector::SmallVector;

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};
use fold::Fold;
use transform::Transform;
use util::IntoSymbol;


pub struct GeneralizeItems {
    ty_var_name: Symbol,
}

impl Transform for GeneralizeItems {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find marked types and replace with the named type variable.

        // Map from item NodeId to the concrete type that was replaced with the type variable.
        // These types are used later as the actual parameters in references to rewritten items.
        // If more than one type was replaced, only the first will be kept in this map.
        let mut item_ty_arg = HashMap::new();

        let krate = fold_nodes(krate, |ty: P<Ty>| {
            if !st.marked(ty.id, "target") {
                return ty;
            }

            let item_id = cx.hir_map().get_parent(ty.id);
            if let Entry::Vacant(e) = item_ty_arg.entry(item_id) {
                e.insert(ty);
            }
            mk().ident_ty(self.ty_var_name)
        });

        // (2) Add parameters to rewritten items.

        let krate = fold_nodes(krate, |i: P<Item>| {
            if !item_ty_arg.contains_key(&i.id) {
                return SmallVector::one(i);
            }
            SmallVector::one(i.map(|mut i| {
                {
                    let gen = match i.node {
                        ItemKind::Fn(_, _, _, _, ref mut gen, _) => gen,
                        ItemKind::Enum(_, ref mut gen) => gen,
                        ItemKind::Struct(_, ref mut gen) => gen,
                        ItemKind::Union(_, ref mut gen) => gen,
                        ItemKind::Trait(_, ref mut gen, _, _) => gen,
                        ItemKind::Impl(_, _, _, ref mut gen, _, _, _) => gen,
                        _ => panic!("item has no room for generics"),
                    };
                    gen.ty_params.push(mk().ty_param(self.ty_var_name));
                }
                i
            }))
        });

        // (3) Rewrite references to each item, replacing `X` with `X<ty1>`.  If the reference to
        // rewritten item `X` appears inside another rewritten item `Y`, we instead replace `X`
        // with `X<T>`, referring to `Y`'s instance of the type parameter.

        let item_ids = item_ty_arg.keys().cloned().collect::<HashSet<_>>();
        let def_ty_arg = item_ty_arg.into_iter()
            .map(|(id, arg)| (cx.node_def_id(id), arg)).collect::<HashMap<_, _>>();

        let krate = fold_resolved_paths_with_id(krate, cx, |path_id, qself, mut path, def_id| {
            if !def_ty_arg.contains_key(&def_id) {
                return (qself, path);
            }

            let parent_id = cx.hir_map().get_parent(path_id);
            let arg = if item_ids.contains(&parent_id) || st.marked(parent_id, "target") {
                mk().ident_ty(self.ty_var_name)
            } else {
                def_ty_arg[&def_id].clone()
            };

            {
                let seg = path.segments.last_mut().unwrap();
                if let Some(ref mut params) = seg.parameters {
                    *params = params.clone().map(|mut params| {
                        match params {
                            PathParameters::AngleBracketed(ref mut abpd) =>
                                abpd.types.push(arg),
                            PathParameters::Parenthesized(..) =>
                                panic!("expected angle bracketed params, but found parenthesized"),
                        }
                        params
                    });
                } else {
                    let abpd = AngleBracketedParameterData {
                        lifetimes: vec![],
                        types: vec![arg],
                        bindings: vec![],
                    };
                    seg.parameters = Some(P(PathParameters::AngleBracketed(abpd)));
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
    }));
}
