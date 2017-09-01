use syntax::ast::Crate;

use command::{Command, CommandState, Registry};
use driver::{self, Phase};
use script::RefactorState;
use util::IntoSymbol;


pub trait Transform {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate;

    fn min_phase(&self) -> Phase {
        // Most transforms should run on expanded code.
        Phase::Phase2
    }
}


pub struct TransformCommand<T: Transform>(pub T);

impl<T: Transform> Command for TransformCommand<T> {
    fn run(&mut self, state: &mut RefactorState) {
        state.with_context_at_phase(self.0.min_phase(), |st, cx| {
            st.map_krate(|krate| {
                self.0.transform(krate, st, cx)
            });
        });
    }
}

fn mk<T: Transform + 'static>(t: T) -> Box<Command> {
    Box::new(TransformCommand(t))
}



macro_rules! transform_modules {
    ($($name:ident,)*) => {
        $( mod $name; )*

        pub fn register_transform_commands(reg: &mut Registry) {
            $( $name::register_commands(reg); )*
        }
    };
}

transform_modules! {
    control_flow,
    format,
    funcs,
    generics,
    ionize,
    items,
    linkage,
    ownership,
    retype,
    rewrite,
    statics,
    structs,
    test,
    vars,
    wrapping_arith,

}
