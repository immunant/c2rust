#![feature(
    i128_type,
    rustc_private,
    trace_macros,
)]
#[macro_use] extern crate idiomize;

use idiomize::command::{Registry, FuncCommand, RefactorState};

fn mark_fields(state: &mut RefactorState) {
    state.run("select", &["dummy",
        "
            crate;
            desc(struct && name(\"lh_table\"));
            mark(target0); mark(target1); mark(target2); mark(target3); mark(target4);
            reset;
        "]);

    state.run("print_marks", &[""]);
    let fields = ["collisions", "resizes", "lookups", "inserts", "deletes"];
    for (i, &field) in fields.iter().enumerate() {
        state.run("mark_field_uses", &[field, &format!("target{}", i)]);
        state.run("rename_marks", &[&format!("target{}", i) as &str, "target"]);
        eprintln!(" -- after {}", field);
        state.run("print_marks", &[""]);
    }
}

#[no_mangle]
pub fn register_commands(reg: &mut Registry) {
    reg.register("lh_table_counter_cell_1", |args| {
        let args = args.clone();
        Box::new(FuncCommand(move |state: &mut RefactorState| {

            mark_fields(state);
            state.run("rewrite_expr", &["marked!(__e)", "*__e.as_ptr()"]);

        }))
    });

    reg.register("lh_table_counter_cell_2", |args| {
        let args = args.clone();
        Box::new(FuncCommand(move |state: &mut RefactorState| {

            mark_fields(state);
            state.run("rewrite_expr", &["*marked!(__e).as_ptr() = __f", "__e.set(__f)"]);

            mark_fields(state);
            state.run("rewrite_expr", &["*marked!(__e).as_ptr()", "__e.get()"]);

        }))
    });
}
