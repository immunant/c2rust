use c_ast::*;

pub struct Printer {
    indent: u64,
    context: TypedAstContext,
}

impl Printer {

    pub fn new(context: TypedAstContext) -> Printer {
        Printer {
            indent: 0,
            context
        }
    }

    pub fn print(&self) {
        for top_decl in self.context.c_decls_top.iter() {
            self.print_decl(*top_decl, 0);
            println!("");
        }
    }

    pub fn print_expr(&self, exprId: CExprId, indent: u64) {
        match self.context.c_exprs.get(&exprId).map(|l| &l.kind) {
            Some(&CExprKind::Literal(lit)) => self.print_lit(&lit, indent),
            Some(&CExprKind::Unary(op, rhs)) => {
                self.print_unop(&op, indent);
                self.print_expr(rhs, indent);
            },
            Some(&CExprKind::Binary(op, lhs, rhs)) => {
                self.print_expr(lhs, indent);
                print!(" ");
                self.print_binop(&op, indent);
                print!(" ");
                self.print_expr(rhs, indent);
            },
            Some(&CExprKind::ImplicitCast(_, expr)) => self.print_expr(expr, indent),
            Some(&CExprKind::DeclRef(decl)) => self.print_decl_name(decl, indent),
            None => panic!("Could not find expressions with ID {}", exprId),
            _ => unimplemented!("Printer::print_expr"),
        }
    }

    pub fn print_unop(&self, op: &UnOp, indent: u64) {
        match *op {
            UnOp::AddressOf => print!("&"),
            UnOp::Deref => print!("*"),
            UnOp::Plus => print!("+"),
            UnOp::Negate => print!("-"),
            UnOp::Complement => print!("~"),
            UnOp::Not => print!("!"),
        }
    }

    pub fn print_binop(&self, op: &BinOp, indent: u64) {
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
        }
    }

    pub fn print_lit(&self, lit: &CLiteral, indent: u64) {
        match *lit {
            CLiteral::Integer(i) => print!("{}", i),
            CLiteral::Floating(f) => print!("{}", f),
            _ => unimplemented!("Printer::print_lit"),
        }
    }

    pub fn print_stmt(&self, stmtId: CStmtId, indent: u64) {
        match self.context.c_stmts.get(&stmtId).map(|l| &l.kind) {
            Some(&CStmtKind::Compound(ref stmts)) => {
                println!("{{");
                for stmt in stmts {
                    for i in 0..(indent + 2) { print!(" "); }
                    self.print_stmt(*stmt, indent + 2);
                }
                println!("}}");
            },

            Some(&CStmtKind::Expr(ref expr)) => {
                self.print_expr(*expr, indent);
                println!("");
                for i in 0..indent { print!(" "); }
            },

            Some(&CStmtKind::Empty) => {
                println!(";");
            },

            Some(&CStmtKind::If { ref scrutinee, ref true_variant, ref false_variant }) => {
                print!("if (");
                self.print_expr(*scrutinee, indent);
                print!(")");
                self.print_stmt(*true_variant, indent);
                match false_variant {
                    &Some(ref f) => {
                        for i in 0..indent { print!(" "); }
                        print!("else ");
                        self.print_stmt(*f, indent);
                    },
                    &None => { },
                }
            },

            Some(&CStmtKind::While { ref condition, ref body }) => {
                print!("while (");
                self.print_expr(*condition, indent);
                print!(")");
                self.print_stmt(*body, indent);
            },

            Some(&CStmtKind::DoWhile { ref body, ref condition }) => {
                self.print_stmt(*body, indent);
                print!("while (");
                self.print_expr(*condition, indent);
                println!(")");
            },

            Some(&CStmtKind::Return(ref returned)) => {
                match returned {
                    &Some(ref ret) => {
                        print!("return ");
                        self.print_expr(*ret, indent);
                        println!(";");
                    },
                    &None => println!("return;"),
                }

            },
            Some(&CStmtKind::Break) => println!("break;"),
            Some(&CStmtKind::Continue) => println!("continue;"),

            Some(&CStmtKind::Decl(decl)) => self.print_decl(decl, indent),
        /*    Some(&CStmtKind::Label(stmt)) => {
                print!("{}: ", stmt);
                self.print_stmt(stmt, indent);
            }

            Some(&CStmtKind::Case(stmt)) => {
                print!("case {}: ", stmt);
                self.print_stmt(stmt, indent);
            }
            Case(CExpr, CStmtId), // The second argument is only the immediately following statement
            Default(CStmtId),

            Switch {
                scrutinee: CExprId,
                body: CStmtId,
            },



            ForLoop {
                init: CStmtId,      // This can be an 'Expr'
                condition: CExprId,
                increment: CExprId,
                body: CStmtId,
            },

            // Jump statements (6.8.6)
            Goto(CLabelId),
            */
            None => panic!("Could not find statement with ID {}", stmtId),

            _ => unimplemented!("Printer::print_stmt"),
        }
    }

    pub fn print_decl(&self, declId: CDeclId, indent: u64) {
        match self.context.c_decls.get(&declId).map(|l| &l.kind) {
            Some(&CDeclKind::Function { ref typ, ref name, ref body }) => {
                // TODO typ
                print!("{}", name);
                print!("() ");
                self.print_stmt(*body, indent);
            },

            Some(&CDeclKind::Variable { ref ident, ref initializer, ref typ }) => {
                self.print_type(*typ, indent);
                print!(" {} = ", ident);
                match initializer {
                    &Some(ref init) => self.print_expr(*init, indent),
                    &None => { },
                }
                println!(";");
            },

            Some(&CDeclKind::Record { ref name, ref fields }) => {
                print!("struct ");
                match name {
                    &Some(ref n) => println!("{} {{", n),
                    &None => println!("{{"),
                }
                for field in fields {
                    for i in 0..(indent + 2) { print!(" ") }
                    self.print_decl(*field, indent + 2);
                }
                for i in 0..indent { print!(" ") }
                print!("}}");
            },

            Some(&CDeclKind::Field { ref name }) => {
                println!("{},", &name);
            },

            None => panic!("Could not find declaration with ID {}", declId),

            _ => unimplemented!("Printer::print_decl"),
        }
    }

    pub fn print_decl_name(&self, declId: CDeclId, indent: u64) {
        let name = self.context.c_decls
            .get(&declId)
            .and_then(|l| l.kind.get_name());
        match name {
            None => print!("<some_decl>"),
            Some(s) => print!("{}", s),
        }

    }

    pub fn print_type(&self, typeId: CTypeId, indent: u64) {
        match self.context.c_types.get(&typeId).map(|l| &l.kind) {
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
                self.print_qtype( *qual_ty, indent);
            },

            None => panic!("Could not find type with ID {}", typeId),

            _ => unimplemented!("Printer::print_type"),
        }
    }

    pub fn print_qtype(&self, typeId: CQualTypeId, indent: u64) {

        let Qualifiers { is_const, is_restrict, is_volatile } = typeId.qualifiers;

        if is_const { print!("const ") }
        if is_restrict { print!("restrict ") }
        if is_volatile { print!("volatile ") }

        self.print_type(typeId.ctype, indent);
    }

}


