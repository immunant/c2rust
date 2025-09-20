//! Helper for checking if a node or one of its descendants has a particular mark.
use rustc_ast::visit::{self, Visitor};
use rustc_ast::*;
use rustc_span::symbol::Symbol;

use crate::ast_builder::IntoSymbol;
use crate::ast_manip::Visit;
use crate::command::CommandState;

struct ContainsMarkVisitor<'a> {
    st: &'a CommandState,
    label: Symbol,
    found: bool,
}

macro_rules! gen_method {
    ($name:ident (& $lt:tt $ArgTy:ty) -> $walk:ident) => {
        fn $name(&mut self, x: & $lt $ArgTy) {
            if self.found {
                return;
            }

            if self.st.marked(x.id, self.label) {
                self.found = true;
                return;
            }

            visit::$walk(self, x);
        }
    };
}

impl<'a, 'ast> Visitor<'ast> for ContainsMarkVisitor<'a> {
    gen_method!(visit_expr(&'ast Expr) -> walk_expr);
    gen_method!(visit_pat(&'ast Pat) -> walk_pat);
    gen_method!(visit_ty(&'ast Ty) -> walk_ty);
    gen_method!(visit_stmt(&'ast Stmt) -> walk_stmt);
    gen_method!(visit_item(&'ast Item) -> walk_item);
}

/// Check if any descendant of a node has a particular mark.  It only looks at certain types of
/// nodes, though, so it's not completely reliable and probably shouldn't be used.
pub fn contains_mark<T, S>(target: &T, label: S, st: &CommandState) -> bool
where
    T: Visit,
    S: IntoSymbol,
{
    let mut v = ContainsMarkVisitor {
        st,
        label: label.into_symbol(),
        found: false,
    };
    target.visit(&mut v);
    v.found
}
