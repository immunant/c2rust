use c_ast::*;

pub struct Printer {
    indent: u64,
}

impl Printer {

    pub fn new() -> Printer {
        Printer { indent: 0 }
    }

    pub fn pad(&self) {
        for _i in 0..self.indent { print!(" "); }
    }

    pub fn newline(&self) {
        println!();
        self.pad();
    }

    pub fn print(&mut self, context: &TypedAstContext) {
        for top_decl in context.c_decls_top.iter() {
            self.print_decl(*top_decl, context);
            println!();
        }
    }

    pub fn print_expr(&mut self, expr_id: CExprId, context: &TypedAstContext) {
        match context.c_exprs.get(&expr_id).map(|l| &l.kind) {
            Some(&CExprKind::Literal(_, lit)) => self.print_lit(&lit, context),
            Some(&CExprKind::Unary(_, op, true, rhs)) => {
                self.print_unop(&op, context);
                self.print_expr(rhs, context);
            },
            Some(&CExprKind::Unary(_, op, false, rhs)) => {
                self.print_expr(rhs, context);
                self.print_unop(&op, context);
            },
            Some(&CExprKind::Binary(_, op, lhs, rhs)) => {
                self.print_expr(lhs, context);
                print!(" ");
                self.print_binop(&op, context);
                print!(" ");
                self.print_expr(rhs, context);
            },
            Some(&CExprKind::ImplicitCast(_, expr)) => self.print_expr(expr, context),
            Some(&CExprKind::DeclRef(_, decl)) => self.print_decl_name(decl, context),
            Some(&CExprKind::Call(_, func, ref args)) => {
                self.print_expr(func, context);
                print!("(");
                for arg in args {
                    self.print_expr(*arg, context);
                }
                print!(")");
            },
            Some(&CExprKind::Member(_, base, member)) => {
                self.print_expr(base, context);
                print!(".");
                self.print_decl_name(member, context);
            }
            Some(&CExprKind::ArraySubscript(_, lhs, rhs)) => {
                self.print_expr(lhs, context);
                print!("[");
                self.print_expr(rhs, context);
                print!("]");
            }
            None => panic!("Could not find expression with ID {:?}", expr_id),
           // _ => unimplemented!("Printer::print_expr"),
        }
    }

    pub fn print_unop(&mut self, op: &UnOp, _context: &TypedAstContext) {
        match *op {
            UnOp::AddressOf => print!("&"),
            UnOp::Deref => print!("*"),
            UnOp::Plus => print!("+"),
            UnOp::Increment => print!("++"),
            UnOp::Negate => print!("-"),
            UnOp::Decrement => print!("--"),
            UnOp::Complement => print!("~"),
            UnOp::Not => print!("!"),
        }
    }

    pub fn print_binop(&mut self, op: &BinOp, _context: &TypedAstContext) {
        match *op {
            BinOp::Multiply => print!("*"),
            BinOp::Divide => print!("/"),
            BinOp::Modulus => print!("%"),
            BinOp::Add => print!("+"),
            BinOp::Subtract => print!("-"),
            BinOp::ShiftLeft => print!("<<"),
            BinOp::ShiftRight => print!(">>"),
            BinOp::Less => print!("<"),
            BinOp::Greater => print!(">"),
            BinOp::LessEqual => print!("<="),
            BinOp::GreaterEqual => print!(">="),
            BinOp::EqualEqual => print!("=="),
            BinOp::NotEqual => print!("!="),
            BinOp::BitAnd => print!("&"),
            BinOp::BitXor => print!("^"),
            BinOp::BitOr => print!("|"),
            BinOp::And => print!("&&"),
            BinOp::Or => print!("||"),

            BinOp::AssignAdd => print!("+="),
            BinOp::AssignSubtract => print!("-="),
            BinOp::AssignMultiply => print!("*="),
            BinOp::AssignDivide => print!("/="),
            BinOp::AssignModulus => print!("%="),
            BinOp::AssignBitXor => print!("^="),
            BinOp::AssignShiftLeft => print!("<<="),
            BinOp::AssignShiftRight => print!(">>="),
            BinOp::AssignBitOr => print!("|="),
            BinOp::AssignBitAnd => print!("&="),

            BinOp::Assign => print!("="),
            BinOp::Comma => print!(","),
        }
    }

    pub fn print_lit(&mut self, lit: &CLiteral, _context: &TypedAstContext) {
        match *lit {
            CLiteral::Integer(i) => print!("{}", i),
            CLiteral::Floating(f) => print!("{}", f),
            _ => unimplemented!("Printer::print_lit"),
        }
    }

    pub fn print_stmt(&mut self, stmt_id: CStmtId, context: &TypedAstContext) {
        match context.c_stmts.get(&stmt_id).map(|l| &l.kind) {
            Some(&CStmtKind::Compound(ref stmts)) => {
                println!("{{");
                self.indent += 2;
                for stmt in stmts {
                    self.print_stmt(*stmt, context);
                }
                self.indent -= 2;
                self.pad();
                println!("}}");
            },

            Some(&CStmtKind::Expr(ref expr)) => {
                self.pad();
                self.print_expr(*expr, context);
                println!();
            },

            Some(&CStmtKind::Empty) => {
                self.pad();
                println!(";");
            },

            Some(&CStmtKind::If { ref scrutinee, ref true_variant, ref false_variant }) => {
                self.pad();
                print!("if (");
                self.print_expr(*scrutinee, context);
                print!(") ");
                self.print_stmt(*true_variant, context);
                match false_variant {
                    &Some(ref f) => {
                        self.pad();
                        print!("else ");
                        self.print_stmt(*f, context);
                    },
                    &None => println!(),
                }
            },

            Some(&CStmtKind::ForLoop { ref init, ref condition, ref increment, ref body }) => {
                self.pad();
                println!("for (");
                self.indent += 2;
                self.pad();
                match init {
                    &None => println!(";"),
                    &Some(ref init) => self.print_stmt(*init, context),
                }
                self.pad();
                match condition {
                    &None => { },
                    &Some(ref condition) => self.print_expr(*condition, context),
                }
                println!(";");
                self.pad();
                match increment {
                    &None => { },
                    &Some(ref increment) => self.print_expr(*increment, context),
                }
                println!();
                self.indent -= 2;
                self.pad();
                print!(") ");
                self.print_stmt(*body, context);
            },

            Some(&CStmtKind::While { ref condition, ref body }) => {
                self.pad();
                print!("while (");
                self.print_expr(*condition, context);
                print!(") ");
                self.print_stmt(*body, context);
            },

            Some(&CStmtKind::DoWhile { ref body, ref condition }) => {
                self.pad();
                print!("do ");
                self.print_stmt(*body, context);
                print!("while (");
                self.print_expr(*condition, context);
                println!(")");
            },

            Some(&CStmtKind::Return(ref returned)) => {
                self.pad();
                match returned {
                    &Some(ref ret) => {
                        print!("return ");
                        self.print_expr(*ret, context);
                        println!(";");
                    },
                    &None => println!("return;"),
                }

            },
            Some(&CStmtKind::Break) => println!("break;"),
            Some(&CStmtKind::Continue) => println!("continue;"),

            Some(&CStmtKind::Decls(ref decls)) => {
                for decl in decls {
                    self.print_decl(*decl, context)
                }
            },

            None => panic!("Could not find statement with ID {:?}", stmt_id),

            _ => unimplemented!("Printer::print_stmt"),
        }
    }

    pub fn print_decl(&mut self, decl_id: CDeclId, context: &TypedAstContext) {
        match context.c_decls.get(&decl_id).map(|l| &l.kind) {
            Some(&CDeclKind::Function { ref typ, ref name, ref parameters, ref body }) => {
                // TODO typ
                self.pad();
                print!("{}", name);
                println!("(");
                self.indent += 2;
                for parameter in parameters {
                    self.print_decl(*parameter, context);
                }
                self.indent -= 2;
                self.pad();
                print!(") ");
                self.print_stmt(*body, context);
            },

            Some(&CDeclKind::Variable { ref ident, ref initializer, ref typ }) => {
                self.pad();
                self.print_qtype(*typ, context);
                print!(" {}", ident);
                match initializer {
                    &Some(ref init) => {
                        print!(" = ");
                        self.print_expr(*init, context)
                    },
                    &None => { },
                }
                println!(";");
            },

            Some(&CDeclKind::Typedef { ref name, ref typ }) => {
                self.pad();
                print!("typedef {} = ", name);
                self.print_type(*typ, context);
                println!();
            },

            Some(&CDeclKind::Record { ref name, ref fields }) => {
                self.pad();
                print!("struct ");
                match name {
                    &Some(ref n) => println!("{} {{", n),
                    &None => println!("{{"),
                }
                self.indent += 2;
                for field in fields {
                    self.pad();
                    self.print_decl(*field, context);
                }
                self.indent -= 2;
                self.pad();
                print!("}}");
            },

            Some(&CDeclKind::Field { ref name }) => {
                self.pad();
                println!("{},", &name);
            },

            None => panic!("Could not find declaration with ID {:?}", decl_id),

           // _ => unimplemented!("Printer::print_decl"),
        }
    }

    pub fn print_decl_name(&mut self, decl_id: CDeclId, context: &TypedAstContext) {
        let name = context.c_decls
            .get(&decl_id)
            .and_then(|l| l.kind.get_name());
        match name {
            None => print!("<some_decl {:?}>", decl_id),
            Some(s) => print!("{}", s),
        }

    }

    pub fn print_type(&mut self, type_id: CTypeId, context: &TypedAstContext) {
        match context.c_types.get(&type_id).map(|l| &l.kind) {
            Some(&CTypeKind::Void) => print!("void"),
            Some(&CTypeKind::Bool) => print!("_Bool"),
            Some(&CTypeKind::Size) => print!("size_t"),
            Some(&CTypeKind::Char) => print!("char"),
            Some(&CTypeKind::SChar) => print!("signed char"),
            Some(&CTypeKind::Short) => print!("signed short"),
            Some(&CTypeKind::Int) => print!("int"),
            Some(&CTypeKind::Long) => print!("long"),
            Some(&CTypeKind::LongLong) => print!("long long"),
            Some(&CTypeKind::UChar) => print!("unsigned char"),
            Some(&CTypeKind::UShort) => print!("unsigned short"),
            Some(&CTypeKind::UInt) => print!("unsigned int"),
            Some(&CTypeKind::ULong) => print!("unsigned long"),
            Some(&CTypeKind::ULongLong) => print!("unsigned long long"),
            Some(&CTypeKind::Float) => print!("float"),
            Some(&CTypeKind::Double) => print!("double"),
            Some(&CTypeKind::LongDouble) => print!("long double"),
            Some(&CTypeKind::Pointer(ref qual_ty)) => {
                print!("* ");
                self.print_qtype( *qual_ty, context);
            },

            None => panic!("Could not find type with ID {:?}", type_id),

            _ => unimplemented!("Printer::print_type"),
        }
    }

    pub fn print_qtype(&mut self, type_id: CQualTypeId, context: &TypedAstContext) {

        let Qualifiers { is_const, is_restrict, is_volatile } = type_id.qualifiers;

        if is_const { print!("const ") }
        if is_restrict { print!("restrict ") }
        if is_volatile { print!("volatile ") }

        self.print_type(type_id.ctype, context);
    }

}


