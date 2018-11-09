//! AST transformation implementations.  Most `c2rust-refactor` commands are transforms implemented in the
//! submodules of this module.

use syntax::ast::Crate;

use command::{Command, RefactorState, CommandState, Registry};
use driver::{self, Phase};


/// An AST transformation that can be applied to a crate.
pub trait Transform {
    /// Apply the transformation.
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate;

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
        state.transform_crate(self.0.min_phase(), |st, cx| {
            st.map_krate(|krate| {
                self.0.transform(krate, st, cx)
            });
        });
    }
}

/// Wrap a `Transform` to produce a `Box<Command>`.
fn mk<T: Transform + 'static>(t: T) -> Box<Command> {
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
    char_literals,
    control_flow,
    externs,
    format,
    funcs,
    generics,
    ionize,
    items,
    linkage,
    literals,
    ownership,
    retype,
    rewrite,
    statics,
    structs,
    test,
    vars,
    wrapping_arith,
}
