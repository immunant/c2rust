use rlua::prelude::{LuaFunction, LuaResult};
use rlua::{UserData, UserDataMethods};
use syntax::source_map::symbol::Symbol;

use crate::ast_manip::fn_edit::{FnKind, FnLike, mut_visit_fns};
use crate::command::CommandState;

pub struct AstVisitor<'a> {
    st: &'a CommandState,
}

impl<'cs> AstVisitor<'cs> {
    pub fn new(st: &'cs CommandState) -> Self {
        AstVisitor {
            st,
        }
    }
}

#[allow(unused_doc_comments)]
impl UserData for AstVisitor<'_> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        /// Visits all function like items
        // @function visit_fn_like
        // @tparam function() callback Function called for each function like item.
        methods.add_method_mut("visit_fn_like", |lua_ctx, this, callback: LuaFunction| {
            let mut found_err = Ok(());

            mut_visit_fns(&mut *this.st.krate_mut(), |fn_like| {
                if found_err.is_err() {
                    return;
                }

                let res: LuaResult<()> = lua_ctx.scope(|scope| {
                    let fn_like = scope.create_nonstatic_userdata(fn_like)?;

                    callback.call(fn_like)
                });

                found_err = res;
            });

            found_err
        });
    }
}

#[allow(unused_doc_comments)]
impl UserData for &mut FnLike {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        /// Determines whether or not this function-like type is a normal function
        // @function is_normal
        // @treturn bool true if a normal function
        methods.add_method_mut("is_normal", |_, this, _: ()| Ok(this.kind == FnKind::Normal));

        /// Determines whether or not this function-like type is an impl method
        // @function is_impl_method
        // @treturn bool true if an impl method
        methods.add_method_mut("is_impl_method", |_, this, _: ()| Ok(this.kind == FnKind::ImplMethod));

        /// Determines whether or not this function-like type is a trait method
        // @function is_trait_method
        // @treturn bool true if a trait method
        methods.add_method_mut("is_trait_method", |_, this, _: ()| Ok(this.kind == FnKind::TraitMethod));

        /// Determines whether or not this function-like type is a foreign function
        // @function is_foreign
        // @treturn bool true if a foreign function
        methods.add_method_mut("is_foreign", |_, this, _: ()| Ok(this.kind == FnKind::Foreign));

        /// Gets the ident of this function-like
        // @function get_ident
        // @treturn string
        methods.add_method_mut("get_ident", |_, this, _: ()| Ok(this.ident.to_string()));

        /// Sets the ident of this function-like
        // @function set_ident
        // @tparam string ident New ident to use
        methods.add_method_mut("set_ident", |_, this, ident: String| {
            this.ident.name = Symbol::intern(&ident);

            Ok(())
        });


    }
}
