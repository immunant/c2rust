use std::borrow::Cow;
use std::collections::hash_map::{HashMap, Entry};
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


fn ty_var_name(idx: usize) -> Symbol {
    if idx < 7 {
        format!("{}", (b'T' + idx as u8) as char).into_symbol()
    } else {
        format!("T{}", idx).into_symbol()
    }
}

pub struct GeneralizeItems;

impl Transform for GeneralizeItems {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find marked types and replace with type variables.  As a side effect, also records
        // the number type variables introduced in each item.

        // Map from item NodeId to a list of types, one per introduced variable.  These types are
        // used later as the actual parameters in references to rewritten items.
        let mut item_ty_args = HashMap::new();

        let krate = fold_nodes(krate, |ty: P<Ty>| {
            if !st.marked(ty.id, "target") {
                return ty;
            }

            let item_id = cx.hir_map().get_parent(ty.id);
            let var_idx = match item_ty_args.entry(item_id) {
                Entry::Vacant(e) => {
                    e.insert(vec![ty]);
                    0
                },
                Entry::Occupied(mut e) => {
                    let n = e.get().len();
                    e.get_mut().push(ty);
                    n
                },
            };
            let var_name = ty_var_name(var_idx);
            mk().ident_ty(var_name)
        });

        // (2) Add parameters to rewritten items.

        let krate = fold_nodes(krate, |i: P<Item>| {
            if !item_ty_args.contains_key(&i.id) {
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
                    for idx in 0 .. item_ty_args[&i.id].len() {
                        gen.ty_params.push(mk().ty_param(ty_var_name(idx)));
                    }
                }
                i
            }))
        });

        // (3) Rewrite references to each item, replacing `X` with `X<ty1, ty2, ...>`.

        let item_ty_args = item_ty_args.into_iter()
            .map(|(id, args)| (cx.node_def_id(id), args)).collect::<HashMap<_, _>>();

        let krate = fold_resolved_paths(krate, cx, |qself, mut path, def_id| {
            if !item_ty_args.contains_key(&def_id) {
                return (qself, path);
            }

            let args = &item_ty_args[&def_id];
            {
                let seg = path.segments.last_mut().unwrap();
                if let Some(ref mut params) = seg.parameters {
                    *params = params.clone().map(|mut params| {
                        match params {
                            PathParameters::AngleBracketed(ref mut abpd) =>
                                abpd.types.extend(args.iter().cloned()),
                            PathParameters::Parenthesized(..) =>
                                panic!("expected angle bracketed params, but found parenthesized"),
                        }
                        params
                    });
                } else {
                    let abpd = AngleBracketedParameterData {
                        lifetimes: vec![],
                        types: args.clone(),
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

    reg.register("generalize_items", |_args| mk(GeneralizeItems));
}
