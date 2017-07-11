use syntax::ast::Crate;

use driver::{self, Phase};

pub trait Transform {
    fn transform(&self, krate: Crate, cx: &driver::Ctxt) -> Crate;

    fn min_phase(&self) -> Phase {
        // Most transforms should run on expanded code.
        Phase::Phase2
    }
}

pub mod control_flow;
pub mod funcs;
pub mod statics;
pub mod structs;
pub mod test;
pub mod vars;
pub mod wrapping_arith;

fn mk_box<T: Transform + 'static>(x: T) -> Box<Transform> {
    Box::new(x)
}

pub fn get_transform(name: &str, args: &[String]) -> Option<Box<Transform>> {
    let tform =
        match name {
            "reconstruct_while" => mk_box(control_flow::ReconstructWhile),
            "reconstruct_for_range" => mk_box(control_flow::ReconstructForRange),
            "remove_unused_labels" => mk_box(control_flow::RemoveUnusedLabels),

            "func_to_method" => mk_box(funcs::ToMethod),
            "fix_unused_unsafe" => mk_box(funcs::FixUnusedUnsafe),
            "sink_unsafe" => mk_box(funcs::SinkUnsafe),

            "static_collect_to_struct" => mk_box(statics::CollectToStruct {
                struct_name: args[0].clone(),
                instance_name: args[1].clone(),
            }),
            "static_to_local_ref" => mk_box(statics::Localize),

            "struct_assign_to_update" => mk_box(structs::AssignToUpdate),
            "struct_merge_updates" => mk_box(structs::MergeUpdates),
            "rename_struct" => mk_box(structs::Rename(args[0].clone())),

            "test_one_plus_one" => mk_box(test::OnePlusOne),
            "test_f_plus_one" => mk_box(test::FPlusOne),
            "test_replace_stmts" => mk_box(test::ReplaceStmts(args[0].clone(), args[1].clone())),

            "let_x_uninitialized" => mk_box(vars::LetXUninitialized),

            "wrapping_arith_to_normal" => mk_box(wrapping_arith::WrappingToNormal),

            _ => return None,
        };
    Some(tform)
}
