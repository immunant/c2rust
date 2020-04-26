use std::sync::atomic::Ordering;

use rustc::ty;

use rlua::prelude::{LuaMetaMethod, LuaUserData, LuaUserDataMethods};

use crate::command::TyCtxtGeneration;
use crate::context::RefactorCtxt;

/// Refactoring module
// @module Refactor

/// Lua encoding of a `Ty<'tcx>` type.
// @type LuaTy

/// Lua encoding of a `Ty<'tcx>` type.
/// Each `Ty<'tcx>` is actually a reference to a `TyS<'tcx>` object
/// which lives inside the `all_arenas` object in the `BoxedGlobalCtxt`
/// generator. When the `BoxedGlobalCtxt` is dropped after
/// `BoxedGlobalCtxt::enter` return, we get a bunch of dangling pointers.
/// To ensure safety, we keep a generation counter inside `RefactorState`
/// and increment it after dropping each `BoxedGlobalCtxt`. On each
/// access to a `LuaTy` object, we check that it's valid for the current
/// generation, and panic otherwise.
#[derive(Debug, Clone)]
pub struct LuaTy {
    /// Arc-reference to the original TyCtxtGeneration.
    /// We need to keep this around so we can perform the check
    /// without having access to the `RefactorCtxt`.
    tcx_gen_ref: TyCtxtGeneration,

    /// The `TyCtxt` generation of this pointer.
    tcx_gen: usize,

    /// The pointer itself.
    ty_ptr: *const (),
}

/// `Ty<'tcx>` points into a `SyncDroplessArena` which is not thread-local,
/// so pointers into it are safe to send across threads
unsafe impl Send for LuaTy {}

impl LuaTy {
    #[inline]
    pub fn from_ty<'a, 'tcx>(ty: ty::Ty<'tcx>, cx: &'a RefactorCtxt<'a, 'tcx>) -> Self {
        let tcx_gen_ref = cx.tcx_gen();
        let tcx_gen = tcx_gen_ref.load(Ordering::Relaxed);
        LuaTy {
            tcx_gen_ref,
            tcx_gen,
            ty_ptr: ty as *const ty::TyS<'tcx> as *const (),
        }
    }
}

impl<'a> From<&LuaTy> for ty::Ty<'a> {
    #[inline]
    fn from(lua_ty: &LuaTy) -> Self {
        let tcx_gen = lua_ty.tcx_gen_ref.load(Ordering::Relaxed);
        if lua_ty.tcx_gen != tcx_gen {
            panic!("Dangling LuaTy from generation {}, current is {}",
                   lua_ty.tcx_gen, tcx_gen);
        }
        unsafe { &*(lua_ty.ty_ptr as *const ty::TyS) }
    }
}

#[allow(unused_doc_comments)]
impl LuaUserData for LuaTy {
    fn add_methods<'lua, M: LuaUserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(LuaMetaMethod::ToString, |_lua_ctx, this, ()| {
            let ty: ty::Ty = this.into();
            Ok(format!("kind:{:?} flags:{}", ty.kind, ty.flags.bits()))
        });

        /// Return the kind of this type
        // @function kind_name
        // @treturn string the kind as a string
        methods.add_method("kind_name", |_lua_ctx, this, ()| {
            let ty: ty::Ty = this.into();
            macro_rules! match_kinds {
                {[$($unit_kind:ident),*], [$($tuple_kind:ident),*]} => {
                    match ty.kind {
                        $(ty::TyKind::$unit_kind => Ok(stringify!($unit_kind)),)*
                        $(ty::TyKind::$tuple_kind(..) => Ok(stringify!($kind)),)*
                    }
                }
            };
            match_kinds!{
                [Bool, Char, Str, Never, Error],
                [Int, Uint, Float, Adt, Foreign, Array, Slice, RawPtr,
                 Ref, FnDef, FnPtr, Dynamic, Closure, Generator,
                 GeneratorWitness, Tuple, Projection, UnnormalizedProjection,
                 Opaque, Param, Bound, Placeholder, Infer]
            }
        });

        // TODO: more accessors
    }
}
