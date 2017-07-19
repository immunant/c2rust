use syntax::ast::Crate;

use command::{Command, CommandState, Registry};
use driver::{self, Phase};
use util::IntoSymbol;


pub trait Transform {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate;

    fn min_phase(&self) -> Phase {
        // Most transforms should run on expanded code.
        Phase::Phase2
    }
}


struct TransformCommand<T: Transform>(T);

impl<T: Transform> Command for TransformCommand<T> {
    fn run(&mut self, st: &CommandState, cx: &driver::Ctxt) {
        st.map_krate(|krate| {
            self.0.transform(krate, st, cx)
        });
    }

    fn min_phase(&self) -> Phase {
        self.0.min_phase()
    }
}



mod control_flow;
mod funcs;
mod retype;
mod rewrite;
mod statics;
mod structs;
mod test;
mod vars;
mod wrapping_arith;


pub fn register_transform_commands(reg: &mut Registry) {
    fn mk<T: Transform + 'static>(t: T) -> Box<Command> {
        Box::new(TransformCommand(t))
    }

    reg.register("reconstruct_while", |_args| mk(control_flow::ReconstructWhile));
    reg.register("reconstruct_for_range", |_args| mk(control_flow::ReconstructForRange));
    reg.register("remove_unused_labels", |_args| mk(control_flow::RemoveUnusedLabels));

    reg.register("func_to_method", |_args| mk(funcs::ToMethod));
    reg.register("fix_unused_unsafe", |_args| mk(funcs::FixUnusedUnsafe));
    reg.register("sink_unsafe", |_args| mk(funcs::SinkUnsafe));
    reg.register("wrap_extern", |_args| mk(funcs::WrapExtern));

    reg.register("retype_argument", |args| mk(retype::RetypeArgument {
        new_ty: args[0].clone(),
        wrap: args[1].clone(),
        unwrap: args[2].clone(),
    }));

    reg.register("rewrite_expr", |args| mk(rewrite::RewriteExpr {
        pat: args[0].clone(),
        repl: args[1].clone(),
        filter: if args.len() >= 3 { Some((&args[2]).into_symbol()) } else { None },
    }));
    reg.register("rewrite_ty", |args| mk(rewrite::RewriteTy {
        pat: args[0].clone(),
        repl: args[1].clone(),
        filter: if args.len() >= 3 { Some((&args[2]).into_symbol()) } else { None },
    }));

    reg.register("static_collect_to_struct", |args| mk(statics::CollectToStruct {
        struct_name: args[0].clone(),
        instance_name: args[1].clone(),
    }));
    reg.register("static_to_local_ref", |_args| mk(statics::Localize));

    reg.register("struct_assign_to_update", |_args| mk(structs::AssignToUpdate));
    reg.register("struct_merge_updates", |_args| mk(structs::MergeUpdates));
    reg.register("rename_struct", |args| mk(structs::Rename(args[0].clone())));

    reg.register("test_one_plus_one", |_args| mk(test::OnePlusOne));
    reg.register("test_f_plus_one", |_args| mk(test::FPlusOne));
    reg.register("test_replace_stmts", |args| mk(
            test::ReplaceStmts(args[0].clone(), args[1].clone())));

    reg.register("let_x_uninitialized", |_args| mk(vars::LetXUninitialized));
    reg.register("sink_lets", |_args| mk(vars::SinkLets));
    reg.register("fold_let_assign", |_args| mk(vars::FoldLetAssign));
    reg.register("uninit_to_default", |_args| mk(vars::UninitToDefault));

    reg.register("wrapping_arith_to_normal", |_args| mk(wrapping_arith::WrappingToNormal));
}
