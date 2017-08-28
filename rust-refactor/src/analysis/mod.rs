use std::collections::HashSet;

use command::{Registry, DriverCommand};
use driver::Phase;
use util::IntoSymbol;


pub mod labeled_ty;
pub mod ownership;
pub mod type_eq;


pub fn register_commands(reg: &mut Registry) {
    reg.register("test_analysis_type_eq", |args| {
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            let result = type_eq::analyze(cx.hir_map(), cx.ty_ctxt(), cx.ty_arena(), &st.krate());
            info!("{:?}", result);
        }))
    });

    reg.register("test_analysis_ownership", |args| {
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            let results = ownership::analyze(&st, &cx);
            ownership::dump_results(&cx, &results);
        }))
    });

    /*
    reg.register("mark_related_types", |args| {
        let label = args.get(0).map_or("target", |x| x).into_symbol();
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            let ty_class = type_eq::analyze(cx.hir_map(), cx.ty_ctxt(), cx.ty_arena(), &st.krate());

            let mut related_classes = HashSet::new();
            for &(id, l) in st.marks().iter() {
                if l == label {
                    if let Some(&cls) = ty_class.get(&id) {
                        related_classes.insert(cls);
                    }
                }
            }

            for (&id, &cls) in &ty_class {
                if related_classes.contains(&cls) {
                    st.add_mark(id, label);
                }
            }
        }))
    });
    */
}
