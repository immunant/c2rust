use c_ast::*;
use std::io::{Write, Result};

pub struct Printer<W: Write> {
    indent: u64,
    writer: W,
}

impl<W: Write> Printer<W> {

    pub fn new(writer: W) -> Printer<W> {
        Printer {
            indent: 0,
            writer,
        }
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
    pub fn parenthesize<F: FnMut(&mut Self) -> Result<()>>(&mut self, print_parens: bool, mut action: F) -> Result<()> {
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
            self.print_decl(*top_decl, true, true,context)?;
            self.writer.write_all(b"\n")?;
        }
        self.writer.flush()
    }

    pub fn print_expr(&mut self, expr_id: CExprId, context: &TypedAstContext) -> Result<()> {
        self.print_expr_prec(15, expr_id, context)
    }

    pub fn print_expr_prec(&mut self, precedence: i32, expr_id: CExprId, context: &TypedAstContext) -> Result<()> {

        match context.c_exprs.get(&expr_id).map(|l| &l.kind) {
            Some(&CExprKind::UnaryType(_, kind, arg_ty)) => {
                let kind_str = match kind {
                    UnTypeOp::SizeOf => b"sizeof(".as_ref(),
                    UnTypeOp::AlignOf => b"alignof(".as_ref(),
                };
                self.writer.write_all(kind_str)?;
                self.print_qtype(arg_ty, None,context)?;
                self.writer.write_all(b" ")?;
                Ok(())
            }
            Some(&CExprKind::Literal(_, ref lit)) => self.print_lit(&lit, context),
            Some(&CExprKind::Unary(_, op, rhs)) => {
                if op.is_prefix() {
                    self.print_unop(&op, context)?;
                    self.print_expr(rhs, context)
                } else {
                    self.print_expr(rhs, context)?;
                    self.print_unop(&op, context)
                }
            },
            Some(&CExprKind::Binary(_, op, lhs, rhs)) => {
                self.print_expr(lhs, context)?;
                self.writer.write_all(b" ")?;
                self.print_binop(&op, context)?;
                self.writer.write_all(b" ")?;
                self.print_expr(rhs, context)
            },
            Some(&CExprKind::ImplicitCast(_, expr, _)) => self.print_expr(expr, context),
            Some(&CExprKind::ExplicitCast(ty, expr, _)) => {
                self.writer.write_all(b"(")?;
                self.print_qtype(ty, None, context)?;
                self.writer.write_all(b") ")?;
                self.print_expr(expr, context)?;

                Ok(())
            },
            Some(&CExprKind::DeclRef(_, decl)) => self.print_decl_name(decl, context),
            Some(&CExprKind::Call(_, func, ref args)) => {
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

                self.writer.write_all(b")")
            },
            Some(&CExprKind::Member(_, base, member, kind)) => {
                let operator: &[u8] = match kind {
                    MemberKind::Arrow => b"->".as_ref(),
                    MemberKind::Dot => b".".as_ref(),
                };
                self.print_expr(base, context)?;
                self.writer.write_all(operator)?;
                self.print_decl_name(member, context)
            }
            Some(&CExprKind::ArraySubscript(_, lhs, rhs)) => {
                self.print_expr(lhs, context)?;
                self.writer.write_all(b"[")?;
                self.print_expr(rhs, context)?;
                self.writer.write_all(b"]")
            }
            Some(&CExprKind::Conditional(_, cond, lhs, rhs)) => {
                self.print_expr(cond, context)?;
                self.writer.write_all(b" ? ")?;
                self.print_expr(lhs, context)?;
                self.writer.write_all(b" : ")?;
                self.print_expr(rhs, context)
            }
            Some(&CExprKind::BinaryConditional(_, lhs, rhs)) => {
                self.print_expr(lhs, context)?;
                self.writer.write_all(b" ?: ")?;
                self.print_expr(rhs, context)
            }
            Some(&CExprKind::InitList(_, ref xs, _)) => {
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
                self.writer.write_all(b"?")
            }
            Some(&CExprKind::ImplicitValueInit{..}) =>
                self.writer.write_all(b"{}"),
            Some(&CExprKind::CompoundLiteral(ty, val)) => {
                self.writer.write_all(b"(")?;
                self.print_qtype(ty, None, context)?;
                self.writer.write_all(b")")?;
                self.print_expr(val, context)
            }
            None => panic!("Could not find expression with ID {:?}", expr_id),
           // _ => unimplemented!("Printer::print_expr"),
        }
    }

    pub fn print_unop(&mut self, op: &UnOp, _context: &TypedAstContext) -> Result<()> {
        match *op {
            UnOp::AddressOf => self.writer.write_all(b"&"),
            UnOp::Deref => self.writer.write_all(b"*"),
            UnOp::Plus => self.writer.write_all(b"+"),
            UnOp::PreIncrement => self.writer.write_all(b"++"),
            UnOp::PostIncrement => self.writer.write_all(b"++"),
            UnOp::Negate => self.writer.write_all(b"-"),
            UnOp::PreDecrement => self.writer.write_all(b"--"),
            UnOp::PostDecrement => self.writer.write_all(b"--"),
            UnOp::Complement => self.writer.write_all(b"~"),
            UnOp::Not => self.writer.write_all(b"!"),
        }
    }

    pub fn print_binop(&mut self, op: &BinOp, _context: &TypedAstContext) -> Result<()> {
        match *op {
            BinOp::Multiply => self.writer.write_all(b"*"),
            BinOp::Divide => self.writer.write_all(b"/"),
            BinOp::Modulus => self.writer.write_all(b"%"),
            BinOp::Add => self.writer.write_all(b"+"),
            BinOp::Subtract => self.writer.write_all(b"-"),
            BinOp::ShiftLeft => self.writer.write_all(b"<<"),
            BinOp::ShiftRight => self.writer.write_all(b">>"),
            BinOp::Less => self.writer.write_all(b"<"),
            BinOp::Greater => self.writer.write_all(b">"),
            BinOp::LessEqual => self.writer.write_all(b"<="),
            BinOp::GreaterEqual => self.writer.write_all(b">="),
            BinOp::EqualEqual => self.writer.write_all(b"=="),
            BinOp::NotEqual => self.writer.write_all(b"!="),
            BinOp::BitAnd => self.writer.write_all(b"&"),
            BinOp::BitXor => self.writer.write_all(b"^"),
            BinOp::BitOr => self.writer.write_all(b"|"),
            BinOp::And => self.writer.write_all(b"&&"),
            BinOp::Or => self.writer.write_all(b"||"),

            BinOp::AssignAdd => self.writer.write_all(b"+="),
            BinOp::AssignSubtract => self.writer.write_all(b"-="),
            BinOp::AssignMultiply => self.writer.write_all(b"*="),
            BinOp::AssignDivide => self.writer.write_all(b"/="),
            BinOp::AssignModulus => self.writer.write_all(b"%="),
            BinOp::AssignBitXor => self.writer.write_all(b"^="),
            BinOp::AssignShiftLeft => self.writer.write_all(b"<<="),
            BinOp::AssignShiftRight => self.writer.write_all(b">>="),
            BinOp::AssignBitOr => self.writer.write_all(b"|="),
            BinOp::AssignBitAnd => self.writer.write_all(b"&="),

            BinOp::Assign => self.writer.write_all(b"="),
            BinOp::Comma => self.writer.write_all(b", "),
        }
    }

    pub fn print_lit(&mut self, lit: &CLiteral, _context: &TypedAstContext) -> Result<()> {
        match *lit {
            CLiteral::Integer(i) => self.writer.write_fmt(format_args!("{}", i)),
            CLiteral::Floating(f) => self.writer.write_fmt(format_args!("{}", f)),
            _ => unimplemented!("Printer::print_lit"),
        }
    }
    
    pub fn print_stmt(&mut self, stmt_id: CStmtId, newline: bool, pad: bool, context: &TypedAstContext) -> Result<()> {
        if pad {
            self.pad()?;
        }

        match context.c_stmts.get(&stmt_id).map(|l| &l.kind) {
            Some(&CStmtKind::Compound(ref stmts)) => {
                self.writer.write_all(b"{\n")?;
                self.indent();

                let mut first: bool = true;
                for stmt in stmts {
                    if !first {
                        self.writer.write_all(b"\n")?;
                    }
                    first = false;
                    self.print_stmt(*stmt, false,true, context)?;
                }

                self.writer.write_all(b"\n")?;
                self.dedent();
                self.pad()?;
                self.writer.write_all(b"}")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CStmtKind::Expr(ref expr)) => {
                self.print_expr(*expr, context)?;
                self.writer.write_all(b";")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CStmtKind::Empty) => {
                self.writer.write_all(b";")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CStmtKind::If { ref scrutinee, ref true_variant, ref false_variant }) => {
                self.writer.write_all(b"if (")?;
                self.print_expr(*scrutinee, context)?;
                self.writer.write_all(b") ")?;
                self.print_stmt(*true_variant, false_variant.is_none() && newline,false, context)?;
                match false_variant {
                    &Some(ref f) => {
                        self.pad()?;
                        self.writer.write_all(b"else ")?;
                        self.print_stmt(*f, newline, false, context)?;
                    },
                    &None if newline => {
                        self.writer.write_all(b"\n")?;
                    },
                    _ => { },
                }

                Ok(())
            },

            Some(&CStmtKind::ForLoop { ref init, ref condition, ref increment, ref body }) => {
                self.writer.write_all(b"for (")?;
                match init {
                    &None => self.writer.write_all(b";")?,
                    &Some(ref init) => self.print_stmt(*init, false, false, context)?,
                }
                match condition {
                    &None => { },
                    &Some(ref condition) => {
                        self.writer.write_all(b" ")?;
                        self.print_expr(*condition, context)?;
                    },
                }
                self.writer.write_all(b";")?;
                match increment {
                    &None => { },
                    &Some(ref increment) => {
                        self.writer.write_all(b" ")?;
                        self.print_expr(*increment, context)?;
                    },
                }
                self.writer.write_all(b") ")?;
                self.print_stmt(*body, newline,false, context)
            },

            Some(&CStmtKind::While { ref condition, ref body }) => {
                self.writer.write_all(b"while (")?;
                self.print_expr(*condition, context)?;
                self.writer.write_all(b") ")?;
                self.print_stmt(*body, newline, false, context)
            },

            Some(&CStmtKind::DoWhile { ref body, ref condition }) => {
                self.writer.write_all(b"do ")?;
                self.print_stmt(*body, false, false,context)?;
                self.writer.write_all(b" while (")?;
                self.print_expr(*condition, context)?;
                self.writer.write_all(b")")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CStmtKind::Return(ref returned)) => {
                match returned {
                    &Some(ref ret) => {
                        self.writer.write_all(b"return ")?;
                        self.print_expr(*ret, context)?;
                        self.writer.write_all(b";")?;
                    },
                    &None => self.writer.write_all(b"return;")?,
                }
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },
            Some(&CStmtKind::Break) => {
                self.writer.write_all(b"break;")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },
            Some(&CStmtKind::Continue) => {
                self.writer.write_all(b"continue;")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CStmtKind::Decls(ref decls)) => {
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

                Ok(())
            },

            None => panic!("Could not find statement with ID {:?}", stmt_id),

            _ => unimplemented!("Printer::print_stmt"),
        }
    }

    pub fn print_decl(&mut self, decl_id: CDeclId, newline: bool, pad: bool, context: &TypedAstContext) -> Result<()> {
        if pad {
            self.pad()?;
        }

        match context.c_decls.get(&decl_id).map(|l| &l.kind) {
            Some(&CDeclKind::Function { is_extern, ref name, ref parameters, ref body, .. }) => {
                if is_extern {
                    self.writer.write_all(b"extern ")?;
                }
                // TODO typ
                self.writer.write_fmt(format_args!("{}", name))?;
                self.writer.write_all(b"(\n")?;
                self.indent();
                for parameter in parameters {
                    match context.c_decls.get(&parameter).map(|l| &l.kind) {
                        Some(&CDeclKind::Variable { ref ident, ref typ, .. }) => {
                            self.pad()?;
                            self.print_qtype(*typ, Some(ident.as_str()), context)?;
                            self.writer.write(b",\n")?;
                        }
                        _ => panic!("Function argument is not VarDecl"),
                    }
                }
                self.dedent();
                self.pad()?;
                self.writer.write_all(b") ")?;
                match body {
                    &Some(b) => self.print_stmt(b, newline, false, context),
                    &None => { self.writer.write_all(b"; ")?; Ok(()) }
                }
            },

            Some(&CDeclKind::Variable { is_static, is_extern, ref ident, ref initializer, ref typ }) => {
                if is_extern {
                    self.writer.write_all(b"extern ")?;
                } else if is_static {
                    self.writer.write_all(b"static ")?;
                }
                self.print_qtype(*typ, Some(ident.as_str()), context)?;
                match initializer {
                    &Some(ref init) => {
                        self.writer.write_all(b" = ")?;
                        self.print_expr(*init, context)?;
                    },
                    &None => { },
                }

                if newline {
                    self.writer.write_all(b";\n")
                } else {
                    self.writer.write_all(b";")
                }
            },

            Some(&CDeclKind::Typedef { ref name, ref typ }) => {
                self.writer.write_fmt(format_args!("typedef {} = ", name))?;
                self.print_qtype(*typ, None, context)?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CDeclKind::Enum { ref name, ref variants }) => {
                self.writer.write_all(b"enum ")?;
                match name {
                    &Some(ref n) => self.writer.write_fmt(format_args!("{} {{\n", n))?,
                    &None => self.writer.write_all(b"{\n")?,
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

                Ok(())
            },

            Some(&CDeclKind::EnumConstant { ref name, value }) => {
                self.writer.write_fmt(format_args!("{} = {},", name, value))?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            }

            Some(&CDeclKind::Struct { ref name, ref fields }) => {
                self.writer.write_all(b"struct ")?;
                match name {
                    &Some(ref n) => self.writer.write_fmt(format_args!("{} {{", n))?,
                    &None => self.writer.write_all(b"{\n")?,
                }
                self.indent();
                for field in fields {
                    self.pad()?;
                    self.print_decl(*field, true, true,context)?;
                }
                self.indent();
                self.pad()?;
                self.writer.write_all(b"};")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CDeclKind::Union { ref name, ref fields }) => {
                self.writer.write_all(b"union ")?;
                match name {
                    &Some(ref n) => self.writer.write_fmt(format_args!("{} {{", n))?,
                    &None => self.writer.write_all(b"{\n")?,
                }
                self.indent();
                for field in fields {
                    self.pad()?;
                    self.print_decl(*field, true, true,context)?;
                }
                self.indent();
                self.pad()?;
                self.writer.write_all(b"};")?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            Some(&CDeclKind::Field { ref name, typ }) => {
                self.writer.write_fmt(format_args!("{}: ", &name))?;
                self.print_qtype(typ, None, context)?;
                if newline {
                    self.writer.write_all(b"\n")?;
                }

                Ok(())
            },

            None => panic!("Could not find declaration with ID {:?}", decl_id),

           // _ => unimplemented!("Printer::print_decl"),
        }
    }

    pub fn print_decl_name(&mut self, decl_id: CDeclId, context: &TypedAstContext) -> Result<()> {
        let name = context.c_decls
            .get(&decl_id)
            .and_then(|l| l.kind.get_name());

        match name {
            None => self.writer.write_fmt(format_args!("<some_decl {:?}>", decl_id)),
            Some(s) => self.writer.write_fmt(format_args!("{}", s)),
        }
    }

    pub fn print_type(&mut self, type_id: CTypeId, ident: Option<&str>, context: &TypedAstContext) -> Result<()> {
        match context.c_types.get(&type_id).map(|l| &l.kind) {
            Some(&CTypeKind::Pointer(ref qual_ty)) => {
                self.print_qtype( *qual_ty, None, context)?;
                self.writer.write_all(b"*")?;
                if let Some(i) = ident {
                    self.writer.write_fmt(format_args!("{}", i))?;
                }

                Ok(())
            },
            Some(&CTypeKind::ConstantArray(typ, len)) => {
                self.print_type(typ, ident, context)?;
                self.writer.write_fmt(format_args!("[{}]", &len))
            }
            Some(&CTypeKind::IncompleteArray(typ)) => {
                self.print_type(typ, ident, context)?;
                self.writer.write_all(b"[]")
            },

            Some(&CTypeKind::Elaborated(ref ctype)) => self.print_type( *ctype, ident, context),
            Some(&CTypeKind::Decayed(ref ctype)) => self.print_type(*ctype, ident, context),
            Some(&CTypeKind::Paren(ref ctype)) => self.parenthesize(true, |slf| slf.print_type(*ctype, ident, context)),

            Some(&CTypeKind::Enum(ref enum_id)) => {
                match context.c_decls.get(&enum_id).map(|l| &l.kind) {
                    Some(&CDeclKind::Enum { name: Some(ref n), .. }) => self.writer.write_fmt(format_args!(" {}", n))?,
                    Some(&CDeclKind::Enum { name: None, .. }) => unimplemented!(),
                    Some(_) => panic!("An enum type  is supposed to point to an enum decl"),
                    None => panic!("Could not find enum decl"),
                }
                if let Some(i) = ident {
                    self.writer.write_fmt(format_args!(" {}", i))?;
                }

                Ok(())
            }

            Some(ty) => {
                match ty {
                    &CTypeKind::Void => self.writer.write_all(b"void"),
                    &CTypeKind::Bool => self.writer.write_all(b"_Bool"),
                    &CTypeKind::Size => self.writer.write_all(b"size_t"),
                    &CTypeKind::Char => self.writer.write_all(b"char"),
                    &CTypeKind::SChar => self.writer.write_all(b"signed char"),
                    &CTypeKind::Short => self.writer.write_all(b"signed short"),
                    &CTypeKind::Int => self.writer.write_all(b"int"),
                    &CTypeKind::Long => self.writer.write_all(b"long"),
                    &CTypeKind::LongLong => self.writer.write_all(b"long long"),
                    &CTypeKind::UChar => self.writer.write_all(b"unsigned char"),
                    &CTypeKind::UShort => self.writer.write_all(b"unsigned short"),
                    &CTypeKind::UInt => self.writer.write_all(b"unsigned int"),
                    &CTypeKind::ULong => self.writer.write_all(b"unsigned long"),
                    &CTypeKind::ULongLong => self.writer.write_all(b"unsigned long long"),
                    &CTypeKind::Float => self.writer.write_all(b"float"),
                    &CTypeKind::Double => self.writer.write_all(b"double"),
                    &CTypeKind::LongDouble => self.writer.write_all(b"long double"),
                    _ => unimplemented!("Printer::print_type")
                }?;

                if let Some(i) = ident {
                    self.writer.write_fmt(format_args!(" {}", i))?;
                }

                Ok(())
            },

            None => panic!("Could not find type with ID {:?}", type_id),
        }
    }

    pub fn print_qtype(&mut self, type_id: CQualTypeId, ident: Option<&str>, context: &TypedAstContext) -> Result<()> {

        let Qualifiers { is_const, is_restrict, is_volatile } = type_id.qualifiers;

        self.print_type(type_id.ctype, ident, context)?;

        if is_const { self.writer.write_all(b" const")? }
        if is_restrict { self.writer.write_all(b" restrict")? }
        if is_volatile { self.writer.write_all(b" volatile")? }

        Ok(())
    }

}


