use rlua::prelude::{LuaFunction, LuaResult};
use rlua::{UserData, UserDataMethods};
use syntax::ast::{Arg, PatKind};
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

        /// Gets the name of this function-like type
        // @function get_name
        // @treturn string
        methods.add_method_mut("get_name", |_, this, _: ()| Ok(this.ident.to_string()));

        /// Sets the name of this function-like type
        // @function set_name
        // @tparam string name New name to use
        methods.add_method_mut("set_name", |_, this, ident: String| {
            this.ident.name = Symbol::intern(&ident);

            Ok(())
        });

        /// Gets the arguments of this function
        // @function get_args
        // @treturn array List of arguments
        methods.add_method_mut("get_args", |lua_ctx, this, _: ()| {
            let lua_table = lua_ctx.create_table()?;

            lua_ctx.scope(|scope| {
                let arg_iter = this.decl.inputs.iter_mut().map(|a| ArgWrapper(a));

                // TODO: Node id rather than i
                for (i, arg) in arg_iter.enumerate() {
                    let arg = scope.create_nonstatic_userdata(arg)?;

                    lua_table.set(i, arg)?;
                }

                // let x = this.decl.inputs.iter_mut().map(|a| ArgWrapper(a)).next().unwrap();
                // let x = scope.create_nonstatic_userdata(x)?;
                // Ok(x)
                Ok(())
            })?;

            Ok(lua_table)
        });
    }
}

struct ArgWrapper<'a>(&'a mut Arg);

#[allow(unused_doc_comments)]
impl UserData for ArgWrapper<'_> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        /// Gets the name of this function-like type
        // @function get_name
        // @treturn string
        methods.add_method_mut("get_name", |_, this, _: ()| {
            match this.0.pat.node {
                PatKind::Ident(_, ident, _) => Ok(ident.to_string()),
                ref e => unreachable!("Found {:?}", e),
            }
        });

        /// Sets the name of this function-like type
        // @function set_name
        // @tparam string name New name to use
        methods.add_method_mut("set_name", |_, this, new_ident: String| {
            match this.0.pat.node {
                PatKind::Ident(_, mut ident, _) => ident.name = Symbol::intern(&new_ident),
                ref e => unreachable!("Found {:?}", e),
            }

            Ok(())
        });
    }
}
