use indexmap::IndexSet;

use crate::c_ast::CDeclId;

/// Options that impact an expression and all of its subexpressions.
#[derive(Debug, Clone, Copy, Default)]
pub struct ExprContext {
    /// We will be referring to the expression by address. In this context we
    /// can't index arrays because they may legally go out of bounds. We also
    /// need to explicitly cast function references to fn() so we get their
    /// address in function pointer literals.
    is_address_needed: bool,

    is_bitfield_write: bool,

    /// In a Rust const context, for example in a static initializer or constant-like macro
    /// translation.
    is_const: bool,

    converting_macro: Option<CDeclId>,

    pub(crate) decay_ref: DecayRef,

    /// In a context where a pattern is expected, such as for `match` arms.
    /// This restricts what kinds of expressions can be emitted.
    is_pattern: bool,

    /// Evaluating a C global/static variable.
    /// This is usually in a const context, but doesn't have to be, for example with initializers
    /// that are executed by the `c2rust_run_static_initializers` function.
    is_static: bool,

    /// Whether the result value of the expression is used in a larger expression.
    ///
    /// When the result value is not used in a particular context, only the side effects of the
    /// expression matter. The `stmts` field of `WithStmts` should hold any statements with side
    /// effects, and the `val` field is expected to be discarded. It should not appear in the final
    /// transpiler output, and may be an expression that panics when evaluated.
    ///
    /// - `.unused()` should be called for the top-level expression of an `ExprStmt`, the increment
    /// expression of a `for` loop, the `lhs` of a comma operator expression, and other such cases.
    ///
    /// - `.used()` should be called if an expression is needed to evaluate the side effects of a
    /// parent expression, such as the arguments of a function call (unless the function is known to
    /// be pure), the operands of an assignment expression, the expression of a `return` statement,
    /// etc. An expression that sets `.used()` for one of its subexpressions should handle the case
    /// that its own context has `!is_used`, by moving its side effects into the `stmts` field;
    /// the `convert_side_effects_expr` helper can be used for this purpose.
    ///
    /// - If an expression is pure (has no side effects), then it should inherit its `is_used` value
    /// from its parent expression: if the parent expression is going to be discarded, then so are
    /// all of its pure child expressions.
    is_used: bool,
}

impl ExprContext {
    pub(crate) fn is_address_needed(&self) -> bool {
        self.is_address_needed
    }

    pub(crate) fn address_needed(self) -> Self {
        Self {
            is_address_needed: true,
            ..self
        }
    }

    pub(crate) fn not_address_needed(self) -> Self {
        Self {
            is_address_needed: false,
            ..self
        }
    }

    pub(crate) fn is_bitfield_write(&self) -> bool {
        self.is_bitfield_write
    }

    pub(crate) fn bitfield_write(self) -> Self {
        Self {
            is_bitfield_write: true,
            ..self
        }
    }

    pub(crate) fn is_const(&self) -> bool {
        self.is_const
    }

    pub(crate) fn const_(self) -> Self {
        Self {
            is_const: true,
            ..self
        }
    }

    pub(crate) fn not_const(self) -> Self {
        Self {
            is_const: false,
            ..self
        }
    }

    pub(crate) fn is_converting_macro(&self) -> bool {
        self.converting_macro.is_some()
    }

    /// Are we expanding the given macro in the current context?
    pub(crate) fn is_converting_macro_id(&self, mac: CDeclId) -> bool {
        self.converting_macro == Some(mac)
    }

    pub(crate) fn converting_macro(self, mac: CDeclId) -> Self {
        Self {
            converting_macro: Some(mac),
            ..self
        }
    }

    pub(crate) fn decay_ref(self) -> Self {
        Self {
            decay_ref: DecayRef::Yes,
            ..self
        }
    }

    pub(crate) fn is_pattern(&self) -> bool {
        self.is_pattern
    }

    pub(crate) fn pattern(self) -> Self {
        Self {
            is_pattern: true,
            ..self
        }
    }

    pub(crate) fn not_pattern(self) -> Self {
        Self {
            is_pattern: false,
            ..self
        }
    }

    pub(crate) fn is_static(&self) -> bool {
        self.is_static
    }

    pub(crate) fn static_(self) -> Self {
        Self {
            is_static: true,
            ..self
        }
    }

    pub(crate) fn is_used(&self) -> bool {
        self.is_used
    }

    pub(crate) fn used(self) -> Self {
        Self {
            is_used: true,
            ..self
        }
    }

    pub(crate) fn unused(self) -> Self {
        Self {
            is_used: false,
            ..self
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DecayRef {
    Yes,
    #[default]
    Default,
    No,
}

impl DecayRef {
    // Here we give intrinsic meaning to default to equate to yes/true
    // when actually evaluated
    pub(crate) fn is_yes(&self) -> bool {
        match self {
            DecayRef::Yes => true,
            DecayRef::Default => true,
            DecayRef::No => false,
        }
    }

    pub(crate) fn set_default_to_no(&mut self) {
        if *self == DecayRef::Default {
            *self = DecayRef::No;
        }
    }
}

impl From<bool> for DecayRef {
    fn from(b: bool) -> Self {
        match b {
            true => DecayRef::Yes,
            false => DecayRef::No,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct FuncContext {
    /// The name of the function we're currently translating
    pub(crate) name: Option<String>,
    /// The name we give to the Rust function argument corresponding
    /// to the ellipsis in variadic C functions.
    pub(crate) va_list_arg_name: Option<String>,
    /// The va_list decls that are either `va_start`ed or `va_copy`ed.
    pub(crate) va_list_decl_ids: Option<IndexSet<CDeclId>>,
    /// The name we give to the Rust variable holding all allocations made with `alloca`.
    pub(crate) alloca_allocations_name: Option<String>,
}

impl FuncContext {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn enter_new(&mut self, fn_name: &str) {
        *self = Self {
            name: Some(fn_name.to_string()),
            ..Default::default()
        };
    }

    pub(crate) fn get_va_list_arg_name(&self) -> &str {
        self.va_list_arg_name.as_ref().unwrap()
    }
}
