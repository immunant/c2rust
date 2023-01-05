use crate::c_ast::*;
use std::io::{Result, Write};

pub struct Printer<W: Write> {
    indent: u64,
    writer: W,
}

impl<W: Write> Printer<W> {
    pub fn new(writer: W) -> Printer<W> {
        Printer { indent: 0, writer }
    }

    /// Assuming the cursor is at the beginning of a line, print out whitespace to reach the indent
    /// stored in the `Printer`.
    pub fn pad(&mut self) -> Result<()> {
        for _i in 0..self.indent {
            self.writer.write_all(b" ")?;
        }
        Ok(())
    }

    /// Increase the indent stored in the `Printer`
    fn indent(&mut self) {
        self.indent += 2;
    }

    /// Decrease the indent stored in the `Printer`
    fn dedent(&mut self) {
        self.indent -= 2;
    }

    /// Print out a new line and pad to the right indent stored in the `Printer`.
    pub fn newline(&mut self) -> Result<()> {
        self.writer.write_all(b"\n")?;
        self.pad()
    }

    /// Pass in an action that will be optionally wrapped in parentheses
    pub fn parenthesize<F: FnMut(&mut Self) -> Result<()>>(
        &mut self,
        print_parens: bool,
        mut action: F,
    ) -> Result<()> {
        if print_parens {
            self.writer.write_all(b"(")?;
        }

        action(self)?;

        if print_parens {
            self.writer.write_all(b")")?;
        }

        Ok(())
    }

    pub fn print(&mut self, context: &TypedAstContext) -> Result<()> {
        for top_decl in context.c_decls_top.iter() {
            self.print_decl(*top_decl, true, true, context)?;
            self.writer.write_all(b"\n")?;
        }
        self.writer.flush()
    }

    pub fn print_expr(&mut self, expr_id: CExprId, context: &TypedAstContext) -> Result<()> {
        self.print_expr_prec(15, expr_id, context)
    }

    pub fn print_expr_prec(
        &mut self,
        _precedence: i32,
        expr_id: CExprId,
        context: &TypedAstContext,
    ) -> Result<()> {
        let expr = context
            .c_exprs
            .get(&expr_id)
            .map(|l| &l.kind)
            .unwrap_or_else(|| panic!("Could not find expression with ID {expr_id:?}"));
        use CExprKind::*;
        match expr {
            BadExpr => {
                self.writer.write_all(b"BAD")?;
            }
            DesignatedInitExpr(..) => {
                self.writer.write_all(b"DESIGNATED INIT EXPR")?;
            }
            ShuffleVector(..) => {
                self.writer.write_all(b"SHUFFLE")?;
            }
            ConvertVector(..) => {
                self.writer.write_all(b"CONVERT")?;
            }

            &Statements(_, compound_stmt_id) => {
                self.writer.write_all(b"(")?;
                self.print_stmt(compound_stmt_id, false, false, context)?;
                self.writer.write_all(b")")?;
            }

            &UnaryType(_, kind, opt_expr, arg_ty) => {
                self.writer.write_all(kind.as_str().as_bytes())?;
                self.writer.write_all(b"(")?;
                match opt_expr {
                    None => self.print_qtype(arg_ty, None, context),
                    Some(expr) => self.print_expr(expr, context),
                }?;

                self.writer.write_all(b" ")?;
            }

            OffsetOf(_, kind) => {
                use OffsetOfKind::*;
                match kind {
                    Constant(val) => {
                        self.writer.write_fmt(format_args!("{val}"))?;
                    }
                    Variable(qty, decl_id, expr_id) => {
                        self.writer.write_all(b"offset_of!(")?;
                        self.print_qtype(*qty, None, context)?;
                        self.writer.write_all(b", ")?;
                        self.print_decl_name(*decl_id, context)?;
                        self.writer.write_all(b"[")?;
                        self.print_expr(*expr_id, context)?;
                        self.writer.write_all(b"])")?;
                    }
                }
            }
            Literal(_, lit) => {
                self.print_lit(lit, context)?;
            }
            &Unary(_, op, rhs, _) => {
                if op.is_prefix() {
                    self.print_unop(&op, context)?;
                    self.print_expr(rhs, context)?;
                } else {
                    self.print_expr(rhs, context)?;
                    self.print_unop(&op, context)?;
                }
            }
            &Binary(_, op, lhs, rhs, _, _) => {
                self.print_expr(lhs, context)?;
                self.writer.write_all(b" ")?;
                self.print_binop(&op, context)?;
                self.writer.write_all(b" ")?;
                self.print_expr(rhs, context)?;
            }
            &ImplicitCast(_, expr, _, _, _) => {
                self.print_expr(expr, context)?;
            }
            &ExplicitCast(ty, expr, _, _, _) => {
                self.writer.write_all(b"(")?;
                self.print_qtype(ty, None, context)?;
                self.writer.write_all(b") ")?;
                self.print_expr(expr, context)?;
            }
            &ConstantExpr(_, expr, _) => {
                self.print_expr(expr, context)?;
            }
            &DeclRef(_, decl, _) => {
                self.print_decl_name(decl, context)?;
            }
            &Call(_, func, ref args) => {
                self.print_expr(func, context)?;
                self.writer.write_all(b"(")?;

                let mut first: bool = true;
                for arg in args {
                    if !first {
                        self.writer.write_all(b", ")?;
                    }
                    first = false;
                    self.print_expr(*arg, context)?;
                }

                self.writer.write_all(b")")?;
            }
            &Member(_, base, member, kind, _) => {
                use MemberKind::*;
                let operator = match kind {
                    Arrow => b"->".as_ref(),
                    Dot => b".".as_ref(),
                };
                self.print_expr(base, context)?;
                self.writer.write_all(operator)?;
                self.print_decl_name(member, context)?;
            }
            &ArraySubscript(_, lhs, rhs, _) => {
                self.print_expr(lhs, context)?;
                self.writer.write_all(b"[")?;
                self.print_expr(rhs, context)?;
                self.writer.write_all(b"]")?;
            }
            &Conditional(_, cond, lhs, rhs) => {
                self.print_expr(cond, context)?;
                self.writer.write_all(b" ? ")?;
                self.print_expr(lhs, context)?;
                self.writer.write_all(b" : ")?;
                self.print_expr(rhs, context)?;
            }
            &BinaryConditional(_, lhs, rhs) => {
                self.print_expr(lhs, context)?;
                self.writer.write_all(b" ?: ")?;
                self.print_expr(rhs, context)?;
            }
            InitList(_, ref xs, _, _) => {
                self.writer.write_all(b"{")?;
                let mut started = false;
                for x in xs {
                    if started {
                        self.writer.write_all(b" : ")?;
                    } else {
                        started = true;
                    }
                    self.print_expr(*x, context)?;
                }
                self.writer.write_all(b"?")?;
            }
            ImplicitValueInit { .. } => {
                self.writer.write_all(b"{}")?;
            }
            &Paren(_, val) => {
                self.writer.write_all(b"(")?;
                self.print_expr(val, context)?;
                self.writer.write_all(b")")?;
            }
            &CompoundLiteral(ty, val) => {
                self.writer.write_all(b"(")?;
                self.print_qtype(ty, None, context)?;
                self.writer.write_all(b")")?;
                self.print_expr(val, context)?;
            }
            &Predefined(_, val) => {
                self.print_expr(val, context)?;
            }

            &VAArg(_, val) => {
                self.print_expr(val, context)?;
            }

            &Choose(_, cond, lhs, rhs, _) => {
                self.writer.write_all(b"__builtin_choose_expr(")?;
                self.print_expr(cond, context)?;
                self.writer.write_all(b", ")?;
                self.print_expr(lhs, context)?;
                self.writer.write_all(b", ")?;
                self.print_expr(rhs, context)?;
                self.writer.write_all(b")")?;
            }

            &Atomic {
                ref name,
                ptr,
                order,
                val1,
                order_fail,
                val2,
                weak,
                ..
            } => {
                self.writer.write_fmt(format_args!("{name}("))?;

                self.print_expr(ptr, context)?;
                if let Some(val1) = val1 {
                    self.writer.write_all(b", ")?;
                    self.print_expr(val1, context)?;
                }
                if let Some(val2) = val2 {
                    self.writer.write_all(b", ")?;
                    self.print_expr(val2, context)?;
                }
                if let Some(weak) = weak {
                    self.writer.write_all(b", ")?;
                    self.print_expr(weak, context)?;
                }
                self.writer.write_all(b", ")?;
                self.print_expr(order, context)?;
                if let Some(order_fail) = order_fail {
                    self.writer.write_all(b", ")?;
                    self.print_expr(order_fail, context)?;
                }

                self.writer.write_all(b")")?;
            } // _ => unimplemented!("Printer::print_expr"),
        };
        Ok(())
    }

    pub fn print_unop(&mut self, op: &UnOp, _context: &TypedAstContext) -> Result<()> {
        self.writer.write_all(op.as_str().as_bytes())
    }

    pub fn print_binop(&mut self, op: &BinOp, _context: &TypedAstContext) -> Result<()> {
        self.writer.write_all(op.as_str().as_bytes())
    }

    pub fn print_lit(&mut self, lit: &CLiteral, _context: &TypedAstContext) -> Result<()> {
        use CLiteral::*;
        match *lit {
            Integer(i, _) => self.writer.write_fmt(format_args!("{i}")),
            Floating(f, ref str) if str.is_empty() => self.writer.write_fmt(format_args!("{f}")),
            Floating(_, ref str) if str.is_empty() => self.writer.write_fmt(format_args!("{str}")),
            _ => unimplemented!("Printer::print_lit"),
        }
    }

    pub fn print_stmt(
        &mut self,
        stmt_id: CStmtId,
        newline: bool,
        pad: bool,
        context: &TypedAstContext,
    ) -> Result<()> {
        if pad {
            self.pad()?;
        }

        let stmt = context
            .c_stmts
            .get(&stmt_id)
            .map(|l| &l.kind)
            .unwrap_or_else(|| panic!("Could not find statement with ID {stmt_id:?}"));

        use CStmtKind::*;
        match stmt {
            Compound(stmts) => {
                self.writer.write_all(b"{\n")?;
                self.indent();

                let mut first: bool = true;
                for stmt in stmts {
                    if !first {
                        self.writer.write_all(b"\n")?;
                    }
                    first = false;
                    self.print_stmt(*stmt, false, true, context)?;
                }

                self.writer.write_all(b"\n")?;
                self.dedent();
                self.pad()?;
                self.writer.write_all(b"}")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Expr(expr) => {
                self.print_expr(*expr, context)?;
                self.writer.write_all(b";")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Empty => {
                self.writer.write_all(b";")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            If {
                scrutinee,
                true_variant,
                false_variant,
            } => {
                self.writer.write_all(b"if (")?;
                self.print_expr(*scrutinee, context)?;
                self.writer.write_all(b") ")?;
                self.print_stmt(
                    *true_variant,
                    false_variant.is_none() && newline,
                    false,
                    context,
                )?;
                match *false_variant {
                    Some(f) => {
                        self.pad()?;
                        self.writer.write_all(b"else ")?;
                        self.print_stmt(f, newline, false, context)?;
                    }
                    None if newline => {
                        self.writer.write_all(b"\n")?;
                    }
                    _ => {}
                }
            }

            ForLoop {
                init,
                condition,
                increment,
                body,
            } => {
                self.writer.write_all(b"for (")?;
                match init {
                    None => self.writer.write_all(b";")?,
                    Some(init) => self.print_stmt(*init, false, false, context)?,
                }
                match condition {
                    None => {}
                    Some(condition) => {
                        self.writer.write_all(b" ")?;
                        self.print_expr(*condition, context)?;
                    }
                }
                self.writer.write_all(b";")?;
                match increment {
                    None => {}
                    Some(increment) => {
                        self.writer.write_all(b" ")?;
                        self.print_expr(*increment, context)?;
                    }
                }
                self.writer.write_all(b") ")?;
                self.print_stmt(*body, newline, false, context)?;
            }

            While { condition, body } => {
                self.writer.write_all(b"while (")?;
                self.print_expr(*condition, context)?;
                self.writer.write_all(b") ")?;
                self.print_stmt(*body, newline, false, context)?;
            }

            DoWhile { body, condition } => {
                self.writer.write_all(b"do ")?;
                self.print_stmt(*body, false, false, context)?;
                self.writer.write_all(b" while (")?;
                self.print_expr(*condition, context)?;
                self.writer.write_all(b")")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Return(returned) => {
                match returned {
                    Some(ret) => {
                        self.writer.write_all(b"return ")?;
                        self.print_expr(*ret, context)?;
                        self.writer.write_all(b";")?;
                    }
                    None => self.writer.write_all(b"return;")?,
                }
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }
            Break => {
                self.writer.write_all(b"break;")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }
            Continue => {
                self.writer.write_all(b"continue;")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Decls(decls) => {
                let mut first: bool = true;
                for decl in decls {
                    if !first {
                        self.writer.write_all(b"\n")?;
                    }
                    first = false;
                    self.print_decl(*decl, false, false, context)?;
                }

                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            _ => unimplemented!("Printer::print_stmt"),
        };

        Ok(())
    }

    pub fn print_decl(
        &mut self,
        decl_id: CDeclId,
        newline: bool,
        pad: bool,
        context: &TypedAstContext,
    ) -> Result<()> {
        if pad {
            self.pad()?;
        }

        let decl = context
            .c_decls
            .get(&decl_id)
            .map(|l| &l.kind)
            .unwrap_or_else(|| panic!("Could not find declaration with ID {decl_id:?}"));

        use CDeclKind::*;
        match decl {
            &Function {
                is_global,
                ref name,
                ref parameters,
                body,
                ..
            } => {
                if !is_global {
                    self.writer.write_all(b"static ")?;
                }
                // TODO typ
                self.writer.write_fmt(format_args!("{name}"))?;
                self.writer.write_all(b"(\n")?;
                self.indent();
                for parameter in parameters {
                    match context.c_decls.get(parameter).map(|l| &l.kind) {
                        Some(Variable { ident, typ, .. }) => {
                            self.pad()?;
                            self.print_qtype(*typ, Some(ident.as_str()), context)?;
                            self.writer.write_all(b",\n")?;
                        }
                        _ => panic!("Function argument is not VarDecl"),
                    }
                }
                self.dedent();
                self.pad()?;
                self.writer.write_all(b") ")?;
                match body {
                    Some(b) => self.print_stmt(b, newline, false, context),
                    None => self.writer.write_all(b"; "),
                }?;
            }

            &Variable {
                has_static_duration,
                has_thread_duration,
                is_externally_visible,
                is_defn,
                ref ident,
                initializer,
                typ,
                ..
            } => {
                if is_externally_visible && !is_defn {
                    self.writer.write_all(b"extern ")?;
                } else if !is_externally_visible && (has_static_duration || has_thread_duration) {
                    self.writer.write_all(b"static ")?;
                }
                if has_thread_duration {
                    self.writer.write_all(b"__thread ")?;
                }
                self.print_qtype(typ, Some(ident.as_str()), context)?;
                if let Some(init) = initializer {
                    self.writer.write_all(b" = ")?;
                    self.print_expr(init, context)?;
                }
                self.writer.write_all(b";")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Typedef { name, typ, .. } => {
                self.writer.write_fmt(format_args!("typedef {name} = "))?;
                self.print_qtype(*typ, None, context)?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Enum { name, variants, .. } => {
                self.writer.write_all(b"enum ")?;
                match name {
                    Some(n) => self.writer.write_fmt(format_args!("{n} {{\n"))?,
                    None => self.writer.write_all(b"{\n")?,
                }

                self.indent();
                for variant in variants {
                    self.print_decl(*variant, true, true, context)?;
                }
                self.dedent();

                self.pad()?;
                self.writer.write_all(b"}")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            EnumConstant { name, value, .. } => {
                self.writer.write_fmt(format_args!("{name} = {value:?},"))?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Struct { name, fields, .. } => {
                self.writer.write_all(b"struct ")?;
                match name {
                    Some(n) => self.writer.write_fmt(format_args!("{n} {{"))?,
                    None => self.writer.write_all(b"{\n")?,
                }
                self.indent();
                for &field in fields.as_ref().unwrap_or(&vec![]) {
                    self.pad()?;
                    self.print_decl(field, true, true, context)?;
                }
                self.indent();
                self.pad()?;
                self.writer.write_all(b"};")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Union { name, fields, .. } => {
                self.writer.write_all(b"union ")?;
                match name {
                    Some(n) => self.writer.write_fmt(format_args!("{n} {{"))?,
                    None => self.writer.write_all(b"{\n")?,
                }
                self.indent();
                for &field in fields.as_ref().unwrap_or(&vec![]) {
                    self.pad()?;
                    self.print_decl(field, true, true, context)?;
                }
                self.indent();
                self.pad()?;
                self.writer.write_all(b"};")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            Field { name, typ, .. } => {
                self.writer.write_fmt(format_args!("{}: ", &name))?;
                self.print_qtype(*typ, None, context)?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }
            }

            MacroObject { name } => {
                self.writer.write_fmt(format_args!("#define {name} "))?;
            }

            MacroFunction { name, .. } => {
                self.writer.write_fmt(format_args!("#define {name}() "))?;
            }

            &NonCanonicalDecl { canonical_decl } => {
                let name = context[canonical_decl]
                    .kind
                    .get_name()
                    .map(|s| s.as_str())
                    .unwrap_or("<unknown>");
                self.writer
                    .write_fmt(format_args!("// non-canonical decl for {name}"))?;
            }

            StaticAssert { .. } => {
                self.writer.write_fmt(format_args!("static_assert(...)"))?;
            } // _ => unimplemented!("Printer::print_decl"),
        };

        Ok(())
    }

    pub fn print_decl_name(&mut self, decl_id: CDeclId, context: &TypedAstContext) -> Result<()> {
        let name = context
            .c_decls
            .get(&decl_id)
            .and_then(|l| l.kind.get_name());

        match name {
            None => self
                .writer
                .write_fmt(format_args!("<some_decl {decl_id:?}>")),
            Some(s) => self.writer.write_fmt(format_args!("{s}")),
        }
    }

    pub fn print_type(
        &mut self,
        type_id: CTypeId,
        ident: Option<&str>,
        context: &TypedAstContext,
    ) -> Result<()> {
        let ty = context
            .c_types
            .get(&type_id)
            .map(|l| &l.kind)
            .unwrap_or_else(|| panic!("Could not find type with ID {type_id:?}"));
        use CTypeKind::*;
        match ty {
            Pointer(ref qual_ty) => {
                self.print_qtype(*qual_ty, None, context)?;
                self.writer.write_all(b"*")?;
                if let Some(i) = ident {
                    self.writer.write_fmt(format_args!("{i}"))?;
                }
            }
            &ConstantArray(typ, len) => {
                self.print_type(typ, ident, context)?;
                self.writer.write_fmt(format_args!("[{}]", &len))?;
            }
            &IncompleteArray(typ) => {
                self.print_type(typ, ident, context)?;
                self.writer.write_all(b"[]")?;
            }

            &Elaborated(ctype) => {
                self.print_type(ctype, ident, context)?;
            }
            &Decayed(ctype) => {
                self.print_type(ctype, ident, context)?;
            }
            &Paren(ctype) => {
                self.parenthesize(true, |slf| slf.print_type(ctype, ident, context))?;
            }

            Enum(enum_id) => {
                let decl = context
                    .c_decls
                    .get(enum_id)
                    .map(|l| &l.kind)
                    .unwrap_or_else(|| panic!("Could not find enum decl"));
                match decl {
                    CDeclKind::Enum {
                        name: Some(ref n), ..
                    } => self.writer.write_fmt(format_args!(" {n}"))?,
                    CDeclKind::Enum { name: None, .. } => unimplemented!(),
                    _ => panic!("An enum type is supposed to point to an enum decl"),
                }
                if let Some(i) = ident {
                    self.writer.write_fmt(format_args!(" {i}"))?;
                }
            }

            ty => {
                self.writer.write_all(ty.as_str().as_bytes())?;

                if let Some(i) = ident {
                    self.writer.write_fmt(format_args!(" {i}"))?;
                }
            }
        };
        Ok(())
    }

    pub fn print_qtype(
        &mut self,
        type_id: CQualTypeId,
        ident: Option<&str>,
        context: &TypedAstContext,
    ) -> Result<()> {
        let Qualifiers {
            is_const,
            is_restrict,
            is_volatile,
        } = type_id.qualifiers;

        self.print_type(type_id.ctype, ident, context)?;

        if is_const {
            self.writer.write_all(b" const")?
        }
        if is_restrict {
            self.writer.write_all(b" restrict")?
        }
        if is_volatile {
            self.writer.write_all(b" volatile")?
        }

        Ok(())
    }
}
