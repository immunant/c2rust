//! AST transformation implementations.  Most `c2rust-refactor` commands are transforms implemented in the
//! submodules of this module.

use rustc_ast::Crate;

use crate::command::{Command, CommandState, RefactorState, Registry};
use crate::driver::Phase;
use crate::RefactorCtxt;

/// An AST transformation that can be applied to a crate.
pub trait Transform {
    /// Apply the transformation.
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt);

    /// Return the minimum phase at which this transform can operate.  See the `Phase` docs for
    /// details.  The default is `Phase2`.
    fn min_phase(&self) -> Phase {
        // Most transforms should run on expanded code.
        Phase::Phase2
    }
}

/// Adapter for turning a `Transform` into a `Command`.
pub struct TransformCommand<T: Transform>(pub T);

impl<T: Transform> Command for TransformCommand<T> {
    fn run(&mut self, state: &mut RefactorState) {
        state
            .transform_crate(self.0.min_phase(), |st, cx| {
                self.0.transform(&mut *st.krate_mut(), st, cx)
            })
            .expect("Failed to run compiler");
    }
}

/// Wrap a `Transform` to produce a `Box<Command>`.
fn mk<T: Transform + 'static>(t: T) -> Box<dyn Command> {
    Box::new(TransformCommand(t))
}

macro_rules! transform_modules {
    ($($name:ident,)*) => {
        $( pub mod $name; )*

        pub fn register_commands(reg: &mut Registry) {
            $( $name::register_commands(reg); )*
        }
    };
}

transform_modules! {
    canonicalize_refs,
    casts,
    char_literals,
    control_flow,
    externs,
    format,
    funcs,
    generics,
    //TODO: this is disabled because it uses Subst for AssocItem
    //ionize,
    items,
    // TODO: this is disabled for now because it depends on analysis/runtime
    //lifetime_analysis,
    linkage,
    literals,
    reorganize_definitions,
    ownership,
    retype,
    rewrite,
    statics,
    structs,
    test,
    vars,
}
