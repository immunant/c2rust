use std::collections::HashSet;
use rustc_ast::*;
use rustc_ast::ptr::P;
use rustc_span::symbol::Symbol;
use smallvec::smallvec;

use crate::ast_manip::{FlatMapNodes, MutVisitNodes};
use crate::command::{CommandState, Registry};
use crate::driver::{parse_ty};
use crate::path_edit::fold_resolved_paths_with_id;
use crate::transform::Transform;
use crate::RefactorCtxt;
use crate::ast_builder::{mk, IntoSymbol};


/// # `generalize_items` Command
/// 
/// Usage: `generalize_items VAR [TY]`
/// 
/// Marks: `target`
/// 
/// Replace marked types with generic type parameters.
/// 
/// Specifically: add a new type parameter called `VAR` to each item marked
/// `target`, replacing type annotations inside that item that are marked `target`
/// with uses of the type parameter.  Also update all uses of `target` items,
/// passing `TY` as the new type argument when used inside a non-`target` item, and
/// passing the type variable `VAR` when used inside a `target` item.
/// 
/// If `TY` is not provided, it defaults to a copy of the first type annotation
/// that was replaced with `VAR`.
/// 
/// Example:
///
/// ```ignore
///     struct Foo {    // Foo: target
///         x: i32,     // i32: target
///         y: i32,
///     }
/// 
///     fn f(foo: Foo) { ... }  // f: target
/// 
///     fn main() {
///         f(...);
///     }
/// ```
/// 
/// After running `generalize_items T`:
///
/// ```ignore
///     // 1. Foo gains a new type parameter `T`
///     struct Foo<T> {
///         // 2. Marked type annotations become `T`
///         x: T,
///         y: i32,
///     }
/// 
///     // 3. `f` gains a new type parameter `T`, and passes
///     // it through to uses of `Foo`
///     fn f<T>(foo: Foo<T>) { ... }
///     struct Bar<T> {
///         foo: Foo<T>,
///     }
/// 
///     fn main() {
///         // 4. Uses outside target items use `i32`, the
///         // first type that was replaced with `T`.
///         f::<i32>(...);
///     }
/// ```
pub struct GeneralizeItems {
    ty_var_name: Symbol,
    replacement_ty: Option<String>,
}

impl Transform for GeneralizeItems {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Find marked types and replace with the named type variable.

        // Map from item NodeId to the concrete type that was replaced with the type variable.
        // These types are used later as the actual parameters in references to rewritten items.
        // If more than one type was replaced, only the first will be kept in this map.
        let mut replacement_ty = self.replacement_ty.as_ref()
            .map(|s| parse_ty(cx.session(), s));

        MutVisitNodes::visit(krate, |ty: &mut P<Ty>| {
            if !st.marked(ty.id, "target") {
                return;
            }

            let hir_id = cx.hir_map().node_to_hir_id(ty.id);
            let parent_id = cx.hir_map().get_parent_item(hir_id);
            let parent_id = cx.hir_map().local_def_id_to_node_id(parent_id);
            if !st.marked(parent_id, "target") {
                return;
            }

            if replacement_ty.is_none() {
                replacement_ty = Some(ty.clone());
            }
            *ty = mk().ident_ty(self.ty_var_name)
        });

        // (2) Add parameters to rewritten items.

        let mut item_def_ids = HashSet::new();
        FlatMapNodes::visit(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return smallvec![i];
            }
            item_def_ids.insert(cx.node_def_id(i.id));
            smallvec![i.map(|mut i| {
                {
                    let gen = match i.kind {
                        ItemKind::Fn(box Fn { generics: ref mut gen, .. }) => gen,
                        ItemKind::Enum(_, ref mut gen) => gen,
                        ItemKind::Struct(_, ref mut gen) => gen,
                        ItemKind::Union(_, ref mut gen) => gen,
                        ItemKind::Trait(box Trait { ref mut generics, .. }) => generics,
                        ItemKind::Impl(box Impl { ref mut generics, .. }) => generics,
                        _ => panic!("item has no room for generics"),
                    };
                    gen.params.push(mk().ty_param(self.ty_var_name));
                }
                i
            })]
        });

        // (3) Rewrite references to each item, replacing `X` with `X<ty1>`.  If the reference to
        // rewritten item `X` appears inside another rewritten item `Y`, we instead replace `X`
        // with `X<T>`, referring to `Y`'s instance of the type parameter.

        let replacement_ty = replacement_ty
            .expect("must provide a replacement type argument or mark");

        fold_resolved_paths_with_id(krate, cx, |path_id, qself, mut path, def| {
            match def[0].opt_def_id() {
                Some(def_id) if item_def_ids.contains(&def_id) => (),
                _ => return (qself, path),
            };

            let hir_id = cx.hir_map().node_to_hir_id(path_id);
            let parent_id = cx.hir_map().get_parent_item(hir_id);
            let parent_id = cx.hir_map().local_def_id_to_node_id(parent_id);
            let arg = if st.marked(parent_id, "target") {
                mk().ident_ty(self.ty_var_name)
            } else {
                replacement_ty.clone()
            };

            {
                let seg = path.segments.last_mut().unwrap();
                if let Some(ref mut args) = seg.args {
                    *args = args.clone().map(|mut args| {
                        match args {
                            GenericArgs::AngleBracketed(ref mut abpd) =>
                                abpd.args.push(AngleBracketedArg::Arg(mk().generic_arg(arg))),
                            GenericArgs::Parenthesized(..) =>
                                panic!("expected angle bracketed params, but found parenthesized"),
                        }
                        args
                    });
                } else {
                    let abpd = mk().angle_bracketed_args(vec![arg]);
                    seg.args = Some(P(GenericArgs::AngleBracketed(abpd)));
                }
            }

            (qself, path)
        });
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("generalize_items", |args| mk(GeneralizeItems {
        ty_var_name: args.get(0).map_or("T", |x| x).into_symbol(),
        replacement_ty: args.get(1).cloned(),
    }));
}
