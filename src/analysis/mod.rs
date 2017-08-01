pub mod type_eq;


use command::{Registry, FuncCommand};
use driver::Phase;


pub fn register_commands(reg: &mut Registry) {
    reg.register("test_analysis_type_eq", |args| {
        Box::new(FuncCommand::new(Phase::Phase3, move |st, cx| {
            let result = type_eq::analyze(cx.hir_map(), &cx.ty_ctxt());
            info!("{:?}", result);
        }))
    });
}
