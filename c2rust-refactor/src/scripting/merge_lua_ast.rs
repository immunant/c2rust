use rlua::prelude::{LuaError, LuaResult, LuaString, LuaTable, LuaValue};
use rustc_target::spec::abi::Abi;
use syntax::ast::{
    Arg, BindingMode, Block, Crate, Expr, ExprKind, FloatTy, FnDecl, ImplItem, ImplItemKind,
    Item, ItemKind, Lit, LitKind, Local, Mod, Mutability::*, NodeId, Pat, PatKind, Path, PathSegment,
    Stmt, StmtKind, UintTy, IntTy, LitIntType, Ident, DUMMY_NODE_ID, BinOpKind, UnOp, BlockCheckMode,
    Label, StrStyle, TyKind, Ty, MutTy, Unsafety, FunctionRetTy, BareFnTy, UnsafeSource::*, Field,
    AnonConst, Lifetime, AngleBracketedArgs, GenericArgs, GenericArg, VisibilityKind, InlineAsm,
    AsmDialect, InlineAsmOutput, Constness, FnHeader, Generics, IsAsync, ImplPolarity, Defaultness,
    UseTree, UseTreeKind, Arm, Guard,
};
use syntax::source_map::symbol::Symbol;
use syntax::source_map::{DUMMY_SP, Span, SpanData, Spanned};
use syntax::ptr::P;
use syntax::ThinVec;

use std::rc::Rc;

use crate::ast_manip::fn_edit::{FnKind, FnLike};
use crate::scripting::into_lua_ast::{LuaSpan, LuaSyntaxContext};

fn dummy_spanned<T>(node: T) -> Spanned<T> {
    Spanned {
        node,
        span: DUMMY_SP,
    }
}

fn dummy_expr() -> P<Expr> {
    P(Expr {
        attrs: ThinVec::new(),
        id: DUMMY_NODE_ID,
        node: ExprKind::Err,
        span: DUMMY_SP,
    })
}

fn dummy_block() -> P<Block> {
    P(Block {
        id: DUMMY_NODE_ID,
        rules: BlockCheckMode::Default,
        span: DUMMY_SP,
        stmts: Vec::new(),
    })
}

fn dummy_ty() -> P<Ty> {
    P(Ty {
        id: DUMMY_NODE_ID,
        node: TyKind::Err,
        span: DUMMY_SP,
    })
}

fn dummy_fn_decl() -> P<FnDecl> {
    P(FnDecl {
        inputs: Vec::new(),
        output: FunctionRetTy::Default(DUMMY_SP),
        c_variadic: false,
    })
}

fn dummy_pat() -> P<Pat> {
    P(Pat {
        id: DUMMY_NODE_ID,
        node: PatKind::Wild,
        span: DUMMY_SP,
    })
}

fn dummy_path() -> Path {
    Path {
        span: DUMMY_SP,
        segments: Vec::new(),
    }
}

fn dummy_generic_args() -> GenericArgs {
    GenericArgs::AngleBracketed(AngleBracketedArgs {
        span: DUMMY_SP,
        args: Vec::new(),
        constraints: Vec::new(),
    })
}

fn dummy_generic_arg() -> GenericArg {
    GenericArg::Lifetime(Lifetime {
        id: DUMMY_NODE_ID,
        ident: Ident::from_str("placeholder"),
    })
}

fn dummy_stmt() -> Stmt {
    Stmt {
        id: DUMMY_NODE_ID,
        node: StmtKind::Expr(dummy_expr()),
        span: DUMMY_SP,
    }
}

fn dummy_local() -> P<Local> {
    P(Local {
        attrs: ThinVec::new(),
        id: DUMMY_NODE_ID,
        init: None,
        pat: dummy_pat(),
        span: DUMMY_SP,
        ty: None,
    })
}

fn dummy_item() -> P<Item> {
    P(Item {
        attrs: Vec::new(),
        id: DUMMY_NODE_ID,
        ident: Ident::from_str("placeholder"),
        node: ItemKind::ExternCrate(None),
        span: DUMMY_SP,
        tokens: None,
        vis: dummy_spanned(VisibilityKind::Public),
    })
}

fn dummy_impl_item() -> ImplItem {
    ImplItem {
        attrs: Vec::new(),
        defaultness: Defaultness::Default,
        generics: Generics::default(),
        id: DUMMY_NODE_ID,
        ident: Ident::from_str("placeholder"),
        node: ImplItemKind::Existential(Vec::new()),
        span: DUMMY_SP,
        tokens: None,
        vis: dummy_spanned(VisibilityKind::Public),
    }
}

fn dummy_use_tree() -> UseTree {
    UseTree {
        prefix: Path::from_ident(Ident::from_str("placeholder")),
        kind: UseTreeKind::Glob,
        span: DUMMY_SP,
    }
}

fn dummy_arm() -> Arm {
    Arm {
        attrs: Vec::new(),
        pats: Vec::new(),
        guard: None,
        body: dummy_expr(),
        span: DUMMY_SP,
    }
}

fn get_node_id_or_default(table: &LuaTable<'_>, field_name: &str) -> LuaResult<NodeId> {
    let opt_id: Option<u32> = table.get(field_name)?;

    Ok(opt_id.map(|id| NodeId::from_u32(id)).unwrap_or(DUMMY_NODE_ID))
}

fn get_span_or_default(table: &LuaTable<'_>, field_name: &str) -> LuaResult<Span> {
    let opt_span_data: Option<LuaSpan> = table.get(field_name)?;
    let opt_span: Option<Span> = opt_span_data.map(|data| {
        let SpanData {lo, hi, ctxt} = data.0;

        Ok(Span::new(lo, hi, ctxt))
    }).transpose()?;

    Ok(opt_span.unwrap_or(DUMMY_SP))
}

pub(crate) trait MergeLuaAst {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()>;
}

impl MergeLuaAst for FnLike {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.kind = match table.get::<_, LuaString>("kind")?.to_str()? {
            "Normal" => FnKind::Normal,
            "ImplMethod" => FnKind::ImplMethod,
            "TraitMethod" => FnKind::TraitMethod,
            "Foreign" => FnKind::Foreign,
            _ => self.kind,
        };
        self.id = get_node_id_or_default(&table, "id")?;
        self.ident.name = Symbol::intern(table.get::<_, LuaString>("ident")?.to_str()?);
        self.decl.merge_lua_ast(table.get("decl")?)?;
        self.span = get_span_or_default(&table, "span")?;

        if let Some(ref mut block) = self.block {
            block.merge_lua_ast(table.get("block")?)?;
        }

        // TODO: Attrs

        Ok(())
    }
}

impl MergeLuaAst for P<Block> {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_stmts: LuaTable = table.get("stmts")?;

        self.id = get_node_id_or_default(&table, "id")?;
        self.span = get_span_or_default(&table, "span")?;
        self.rules = match table.get::<_, LuaString>("rules")?.to_str()? {
            "Default" => BlockCheckMode::Default,
            "CompilerGeneratedUnsafe" => BlockCheckMode::Unsafe(CompilerGenerated),
            "UserProvidedUnsafe" => BlockCheckMode::Unsafe(UserProvided),
            e => unimplemented!("Found unknown block rule: {}", e),
        };

        let mut stmts = Vec::new();

        for lua_stmt in lua_stmts.sequence_values::<LuaTable>() {
            let mut stmt = dummy_stmt();

            stmt.merge_lua_ast(lua_stmt?)?;
            stmts.push(stmt);
        }

        self.stmts = stmts;

        Ok(())
    }
}

impl MergeLuaAst for Stmt {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind: LuaString = table.get("kind")?;

        self.id = get_node_id_or_default(&table, "id")?;
        self.span = get_span_or_default(&table, "span")?;
        self.node = match kind.to_str()? {
            "Local" => {
                let mut local = dummy_local();

                local.merge_lua_ast(table)?;

                StmtKind::Local(local)
            },
            "Item" => {
                let mut item = dummy_item();
                let lua_item = table.get("item")?;

                item.merge_lua_ast(lua_item)?;

                StmtKind::Item(item)
            },
            "Expr" => {
                let lua_expr = table.get("expr")?;
                let mut expr = dummy_expr();

                expr.merge_lua_ast(lua_expr)?;

                StmtKind::Expr(expr)
            },
            "Semi" => {
                let lua_expr = table.get("expr")?;
                let mut expr = dummy_expr();

                expr.merge_lua_ast(lua_expr)?;

                StmtKind::Semi(expr)
            },
            e => unimplemented!("MergeLuaAst unimplemented for StmtKind::{}", e),
        };

        Ok(())
    }
}

impl MergeLuaAst for P<FnDecl> {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_args: LuaTable = table.get("args")?;
        let lua_return_type: Option<LuaTable> = table.get("return_type")?;
        let return_type = lua_return_type.map(|lua_ty| {
            let mut ty = dummy_ty();

            ty.merge_lua_ast(lua_ty).map(|_| ty)
        }).transpose()?;

        self.c_variadic = table.get("c_variadic")?;
        self.output = match return_type {
            Some(ty) => FunctionRetTy::Ty(ty),
            None => FunctionRetTy::Default(DUMMY_SP),
        };

        let mut args = Vec::new();

        for lua_arg in lua_args.sequence_values::<LuaTable>() {
            let lua_arg = lua_arg?;
            let mut arg = Arg {
                attrs: ThinVec::new(),
                ty: dummy_ty(),
                pat: dummy_pat(),
                id: get_node_id_or_default(&lua_arg, "id")?,
            };

            arg.merge_lua_ast(lua_arg)?;
            args.push(arg);
        }

        self.inputs = args;

        Ok(())
    }
}

impl MergeLuaAst for Arg {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.id = get_node_id_or_default(&table, "id")?;
        self.pat.merge_lua_ast(table.get("pat")?)?;
        self.ty.merge_lua_ast(table.get("ty")?)?;
        // TODO: Attrs

        Ok(())
    }
}

impl MergeLuaAst for P<Pat> {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind: LuaString = table.get("kind")?;

        self.span = get_span_or_default(&table, "span")?;
        self.id = get_node_id_or_default(&table, "id")?;
        self.node = match kind.to_str()? {
            "Wild" => PatKind::Wild,
            "Ident" => {
                let binding: LuaString = table.get("binding")?;
                let binding = match binding.to_str()? {
                    "ByRefImmutable" => BindingMode::ByRef(Immutable),
                    "ByRefMutable" => BindingMode::ByRef(Mutable),
                    "ByValueImmutable" => BindingMode::ByValue(Immutable),
                    "ByValueMutable" => BindingMode::ByValue(Mutable),
                    e => unimplemented!("Unknown ident binding: {}", e),
                };
                let ident: LuaString = table.get("ident")?;
                let ident = Ident::from_str(ident.to_str()?);

                // TODO: Sub-pattern
                PatKind::Ident(binding, ident, None)
            },
            "Tuple" => {
                let fragment_pos = table.get("fragment_pos")?;
                let lua_patterns: LuaTable = table.get("pats")?;
                let mut patterns = Vec::new();

                for lua_pat in lua_patterns.sequence_values::<LuaTable>() {
                    let mut pat = dummy_pat();

                    pat.merge_lua_ast(lua_pat?)?;
                    patterns.push(pat);
                }

                PatKind::Tuple(patterns, fragment_pos)
            },
            "Lit" => {
                let lua_expr = table.get("expr")?;
                let mut expr = dummy_expr();

                expr.merge_lua_ast(lua_expr)?;

                PatKind::Lit(expr)
            },
            e => unimplemented!("MergeLuaAst unimplemented pat: {:?}", e),
        };

        Ok(())
    }
}

impl MergeLuaAst for Local {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let opt_lua_ty: Option<LuaTable> = table.get("ty")?;
        let pat: LuaTable = table.get("pat")?;
        let opt_init: Option<LuaTable> = table.get("init")?;

        self.pat.merge_lua_ast(pat)?;

        match &mut self.init {
            Some(existing_init) => {
                match opt_init {
                    Some(init) => existing_init.merge_lua_ast(init)?,
                    None => self.init = None,
                }
            },
            None => {
                if let Some(init) = opt_init {
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(init)?;

                    self.init = Some(expr);
                }
            },
        }

        match &mut self.ty {
            Some(existing_ty) => {
                match opt_lua_ty {
                    Some(ty) => existing_ty.merge_lua_ast(ty)?,
                    None => self.ty = None,
                }
            },
            None => {
                if let Some(ty) = opt_lua_ty {
                    let mut expr = dummy_ty();

                    expr.merge_lua_ast(ty)?;

                    self.ty = Some(expr);
                }
            },
        }

        Ok(())
    }
}

impl MergeLuaAst for Crate {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.module.merge_lua_ast(table.get("module")?)?;
        self.span = get_span_or_default(&table, "span")?;

        Ok(())
    }
}

impl MergeLuaAst for Mod {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_items: LuaTable = table.get("items")?;

        self.inline = table.get("inline")?;
        self.inner = get_span_or_default(&table, "inner")?;

        // TODO: This may need to be improved if we want to delete or add
        // items as it currently expects items to be 1-1
        self.items.iter_mut().enumerate().map(|(i, item)| {
            let item_table: LuaTable = lua_items.get(i + 1)?;

            item.merge_lua_ast(item_table)
        }).collect()
    }
}

impl MergeLuaAst for P<Item> {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind = table.get::<_, LuaString>("kind")?;
        let kind = kind.to_str()?;

        self.span = get_span_or_default(&table, "span")?;
        self.id = get_node_id_or_default(&table, "id")?;
        self.ident.name = Symbol::intern(&table.get::<_, LuaString>("ident")?.to_str()?);
        self.node = match kind {
            "Fn" => {
                let lua_fn_decl = table.get("decl")?;
                let lua_block = table.get("block")?;
                let mut block = dummy_block();
                let mut decl = dummy_fn_decl();
                let unsafety = match table.get::<_, LuaString>("unsafety")?.to_str()? {
                    "Unsafe" => Unsafety::Unsafe,
                    "Normal" => Unsafety::Normal,
                    e => unimplemented!("Unknown unsafety: {}", e),
                };
                let constness = match table.get::<_, bool>("is_const")? {
                    true => dummy_spanned(Constness::Const),
                    false => dummy_spanned(Constness::NotConst),
                };
                let abi = match table.get::<_, LuaString>("abi")?.to_str()? {
                    "Cdecl" => Abi::Cdecl,
                    "C" => Abi::C,
                    "Rust" => Abi::Rust,
                    e => unimplemented!("Abi: {}", e),
                };
                let fn_header = FnHeader {
                    abi,
                    asyncness: dummy_spanned(IsAsync::NotAsync), // TODO
                    constness,
                    unsafety,
                };
                let generics = Generics::default(); // TODO

                block.merge_lua_ast(lua_block)?;
                decl.merge_lua_ast(lua_fn_decl)?;

                ItemKind::Fn(decl, fn_header, generics, block)
            },
            "Impl" => {
                let lua_items: LuaTable = table.get("items")?;
                let lua_ty = table.get("ty")?;
                let mut items = Vec::new();
                let unsafety = Unsafety::Normal; // TODO
                let polarity = ImplPolarity::Positive; // TODO
                let defaultness = Defaultness::Default; // TODO
                let generics = Generics::default(); // TODO
                let trait_ref = None; // TODO
                let mut ty = dummy_ty();

                ty.merge_lua_ast(lua_ty)?;

                for lua_item in lua_items.sequence_values::<LuaTable>() {
                    let lua_item = lua_item?;
                    let mut item = dummy_impl_item();

                    item.merge_lua_ast(lua_item)?;
                    items.push(item);
                }

                ItemKind::Impl(unsafety, polarity, defaultness, generics, trait_ref, ty, items)
            },
            "Use" => {
                let lua_use_tree = table.get("tree")?;
                let mut use_tree = dummy_use_tree();

                use_tree.merge_lua_ast(lua_use_tree)?;

                ItemKind::Use(P(use_tree))
            },
            "Ty" => {
                let lua_ty = table.get("ty")?;
                let mut ty = dummy_ty();

                ty.merge_lua_ast(lua_ty)?;

                // TODO: Generics
                ItemKind::Ty(ty, Generics::default())
            },
            "Static" => {
                let lua_ty = table.get("ty")?;
                let lua_expr = table.get("expr")?;
                let mutability = match table.get::<_, LuaString>("mutability")?.to_str()? {
                    "Immutable" => Immutable,
                    "Mutable" => Mutable,
                    e => panic!("Found unknown addrof mutability: {}", e),
                };
                let mut ty = dummy_ty();
                let mut expr = dummy_expr();

                ty.merge_lua_ast(lua_ty)?;
                expr.merge_lua_ast(lua_expr)?;

                ItemKind::Static(ty, mutability, expr)
            }
            ref e => unimplemented!("MergeLuaAst unimplemented item kind: {:?}", e),
        };

        Ok(())
    }
}

fn lit_from_int(int: u128, suffix: Option<&str>) -> LitKind {
    let suffix = match suffix {
        None => LitIntType::Unsuffixed,
        Some("u8") => LitIntType::Unsigned(UintTy::U8),
        Some("u16") => LitIntType::Unsigned(UintTy::U16),
        Some("u32") => LitIntType::Unsigned(UintTy::U32),
        Some("u64") => LitIntType::Unsigned(UintTy::U64),
        Some("u128") => LitIntType::Unsigned(UintTy::U128),
        Some("usize") => LitIntType::Unsigned(UintTy::Usize),
        Some("i8") => LitIntType::Signed(IntTy::I8),
        Some("i16") => LitIntType::Signed(IntTy::I16),
        Some("i32") => LitIntType::Signed(IntTy::I32),
        Some("i64") => LitIntType::Signed(IntTy::I64),
        Some("i128") => LitIntType::Signed(IntTy::I128),
        Some("isize") => LitIntType::Signed(IntTy::Isize),
        _ => unreachable!("Unknown int suffix"),
    };

    LitKind::Int(int, suffix)
}

impl MergeLuaAst for P<Expr> {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind = table.get::<_, LuaString>("kind")?;
        let kind = kind.to_str()?;

        self.span = get_span_or_default(&table, "span")?;
        self.id = get_node_id_or_default(&table, "id")?;
        self.node = match kind {
            "Path" => {
                let lua_segments: LuaTable = table.get("segments")?;
                let mut path = dummy_path();

                path.merge_lua_ast(lua_segments)?;

                // TODO: QSelf support
                ExprKind::Path(None, path)
            },
            "Lit" => {
                let val: LuaValue = table.get("value")?;
                let suffix: Option<LuaString> = table.get("suffix")?;
                let suffix = suffix.as_ref().map(|s| s.to_str()).transpose()?;
                let lit_kind = match val {
                    LuaValue::Boolean(val) => LitKind::Bool(val),
                    LuaValue::Integer(i) => lit_from_int(i as u128, suffix),
                    LuaValue::Number(num) => {
                        let num_kind: LuaString = table.get("num_kind")?;

                        // (R)Lua will convert any value > i64::max_value()
                        // to a float so we must stringly type that it's
                        // expected to be an integer
                        if num_kind.to_str()? == "Int" {
                            lit_from_int(num as u128, suffix)
                        } else {
                            let mut string = num.to_string();

                            // to_string won't add a trailing period if unsuffixed
                            if !string.contains('.') {
                                string.push('.');
                            }

                            let sym = Symbol::intern(&string);

                            match suffix {
                                None => LitKind::FloatUnsuffixed(sym),
                                Some("f32") => LitKind::Float(sym, FloatTy::F32),
                                Some("f64") => LitKind::Float(sym, FloatTy::F64),
                                Some(e) => unreachable!("Unknown float suffix: {}{}", num, e),
                            }
                        }
                    },
                    LuaValue::String(lua_string) => {
                        let str_kind = table.get::<_, LuaString>("str_kind")?;
                        let str_kind = str_kind.to_str()?;
                        let is_bytes = str_kind == "ByteStr";
                        let is_char = str_kind == "Char";

                        if is_bytes {
                            let bytes: Vec<u8> = lua_string
                                .as_bytes()
                                .iter()
                                .map(|b| *b)
                                .collect();

                            LitKind::ByteStr(Rc::new(bytes))
                        } else {
                            let string = lua_string.to_str()?;

                            if is_char {
                                let ch = string
                                    .chars()
                                    .next()
                                    .ok_or(LuaError::external("Found empty string where char was expected."));

                                LitKind::Char(ch?)
                            } else {
                                // TODO: Raw strings?
                                let symbol = Symbol::intern(string);
                                let style = StrStyle::Cooked;

                                LitKind::Str(symbol, style)
                            }
                        }
                    },
                    LuaValue::Nil => {
                        let symbol = Symbol::intern("NIL");

                        LitKind::Err(symbol)
                    },
                    _ => unimplemented!("MergeLuaAst unimplemented lit: {:?}", val),
                };

                ExprKind::Lit(Lit {
                    token: lit_kind.to_lit_token(),
                    node: lit_kind,
                    span: DUMMY_SP,
                })
            },
            "Binary" | "AssignOp" | "Assign" => {
                let lua_lhs = table.get("lhs")?;
                let lua_rhs = table.get("rhs")?;
                let op: Option<LuaString> = table.get("op")?;
                let op = op.map(|s| Ok(match s.to_str()? {
                    "+" => BinOpKind::Add,
                    "-" => BinOpKind::Sub,
                    "*" => BinOpKind::Mul,
                    "/" => BinOpKind::Div,
                    "%" => BinOpKind::Rem,
                    "&&" => BinOpKind::And,
                    "||" => BinOpKind::Or,
                    "^" => BinOpKind::BitXor,
                    "&" => BinOpKind::BitAnd,
                    "|" => BinOpKind::BitOr,
                    "<<" => BinOpKind::Shl,
                    ">>" => BinOpKind::Shr,
                    "==" => BinOpKind::Eq,
                    "<" => BinOpKind::Lt,
                    "<=" => BinOpKind::Le,
                    "!=" => BinOpKind::Ne,
                    ">=" => BinOpKind::Ge,
                    ">" => BinOpKind::Gt,
                    e => unreachable!("Unknown BinOpKind: {}", e),
                })).transpose()?;

                let mut lhs = dummy_expr();
                let mut rhs = dummy_expr();

                lhs.merge_lua_ast(lua_lhs)?;
                rhs.merge_lua_ast(lua_rhs)?;

                match kind {
                    "Binary" => ExprKind::Binary(dummy_spanned(op.unwrap()), lhs, rhs),
                    "AssignOp" => ExprKind::AssignOp(dummy_spanned(op.unwrap()), lhs, rhs),
                    "Assign" => ExprKind::Assign(lhs, rhs),
                    _ => unreachable!(),
                }
            },
            "Array" => {
                let lua_exprs: LuaTable = table.get("values")?;
                let mut exprs = Vec::new();

                for lua_expr in lua_exprs.sequence_values::<LuaTable>() {
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(lua_expr?)?;

                    exprs.push(expr);
                }

                ExprKind::Array(exprs)
            },
            "Index" => {
                let lua_indexed: LuaTable = table.get("indexed")?;
                let lua_index: LuaTable = table.get("index")?;
                let mut indexed = dummy_expr();
                let mut index = dummy_expr();

                indexed.merge_lua_ast(lua_indexed)?;
                index.merge_lua_ast(lua_index)?;

                ExprKind::Index(indexed, index)
            },
            "Unary" => {
                let op: LuaString = table.get("op")?;
                let lua_expr: LuaTable = table.get("expr")?;
                let op = match op.to_str()? {
                    "*" => UnOp::Deref,
                    "!" => UnOp::Not,
                    "-" => UnOp::Neg,
                    e => unreachable!("Unknown UnOp: {}", e),
                };
                let mut expr = dummy_expr();

                expr.merge_lua_ast(lua_expr)?;

                ExprKind::Unary(op, expr)
            },
            "Call" => {
                let mut path = dummy_expr();
                let lua_path = table.get("path")?;
                let lua_args: LuaTable = table.get("args")?;
                let mut args = Vec::new();

                path.merge_lua_ast(lua_path)?;

                for lua_arg in lua_args.sequence_values::<LuaTable>() {
                    let mut arg = dummy_expr();

                    arg.merge_lua_ast(lua_arg?)?;
                    args.push(arg);
                }

                ExprKind::Call(path, args)
            },
            "MethodCall" => {
                let lua_args: LuaTable = table.get("args")?;
                let mut args = Vec::new();

                for lua_arg in lua_args.sequence_values::<LuaTable>() {
                    let mut arg = dummy_expr();

                    arg.merge_lua_ast(lua_arg?)?;
                    args.push(arg);
                }

                let lua_segment: LuaTable = table.get("segment")?;
                let name: LuaString = lua_segment.get("ident")?;
                let mut segment = PathSegment::from_ident(Ident::from_str(name.to_str()?));
                let lua_generics: Option<LuaTable> = lua_segment.get("generics")?;
                let opt_generics = lua_generics.map(|lua_generics| {
                    let mut generics = dummy_generic_args();

                    generics.merge_lua_ast(lua_generics).map(|_| P(generics))
                }).transpose()?;

                segment.id = get_node_id_or_default(&lua_segment, "id")?;
                segment.args = opt_generics;

                ExprKind::MethodCall(segment, args)
            },
            "Field" => {
                let lua_expr: LuaTable = table.get("expr")?;
                let mut expr = dummy_expr();
                let name: LuaString = table.get("name")?;

                expr.merge_lua_ast(lua_expr)?;

                ExprKind::Field(expr, Ident::from_str(name.to_str()?))
            },
            "Ret" => {
                let opt_lua_expr: Option<LuaTable> = table.get("value")?;

                match opt_lua_expr {
                    Some(lua_expr) => {
                        let mut expr = dummy_expr();

                        expr.merge_lua_ast(lua_expr)?;

                        ExprKind::Ret(Some(expr))
                    },
                    None => ExprKind::Ret(None),
                }
            },
            "Tup" => {
                let lua_exprs: LuaTable = table.get("exprs")?;
                let mut exprs = Vec::new();

                for lua_expr in lua_exprs.sequence_values::<LuaTable>() {
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(lua_expr?)?;
                    exprs.push(expr);
                }

                ExprKind::Tup(exprs)
            },
            "AddrOf" => {
                let lua_expr = table.get("expr")?;
                let mut expr = dummy_expr();

                expr.merge_lua_ast(lua_expr)?;

                let mutability = match table.get::<_, LuaString>("mutability")?.to_str()? {
                    "Immutable" => Immutable,
                    "Mutable" => Mutable,
                    e => panic!("Found unknown addrof mutability: {}", e),
                };

                ExprKind::AddrOf(mutability, expr)
            },
            "Block" => {
                let lua_block = table.get("block")?;
                let mut block = dummy_block();
                let opt_label_str: Option<LuaString> = table.get("label")?;
                let opt_label = opt_label_str.map(|s| Ok(Label {
                    ident: Ident::from_str(s.to_str()?)
                })).transpose()?;

                block.merge_lua_ast(lua_block)?;

                ExprKind::Block(block, opt_label)
            },
            "While" => {
                let lua_cond = table.get("cond")?;
                let lua_block = table.get("block")?;
                let mut cond = dummy_expr();
                let mut block = dummy_block();
                let opt_label_str: Option<LuaString> = table.get("label")?;
                let opt_label = opt_label_str.map(|s| Ok(Label {
                    ident: Ident::from_str(s.to_str()?)
                })).transpose()?;

                block.merge_lua_ast(lua_block)?;
                cond.merge_lua_ast(lua_cond)?;

                ExprKind::While(cond, block, opt_label)
            },
            "If" => {
                let lua_cond = table.get("cond")?;
                let lua_then = table.get("then")?;
                let opt_lua_else: Option<_> = table.get("else")?;
                let mut cond = dummy_expr();
                let mut then = dummy_block();

                cond.merge_lua_ast(lua_cond)?;
                then.merge_lua_ast(lua_then)?;

                let opt_else = opt_lua_else.map(|lua_else| {
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(lua_else).map(|_| expr)
                }).transpose()?;

                ExprKind::If(cond, then, opt_else)
            },
            "Cast" => {
                let lua_expr = table.get("expr")?;
                let mut expr = dummy_expr();
                let lua_ty = table.get("ty")?;
                let mut ty = dummy_ty();

                expr.merge_lua_ast(lua_expr)?;
                ty.merge_lua_ast(lua_ty)?;

                ExprKind::Cast(expr, ty)
            },
            "Struct" => {
                let lua_path = table.get("path")?;
                let lua_fields: LuaTable = table.get("fields")?;
                let opt_lua_expr: Option<_> = table.get("expr")?;
                let opt_expr = opt_lua_expr.map(|lua_expr| {
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(lua_expr).map(|_| expr)
                }).transpose()?;
                let mut path = dummy_path();
                let mut fields = Vec::new();

                path.merge_lua_ast(lua_path)?;

                for field in lua_fields.sequence_values::<LuaTable>() {
                    let field = field?;
                    let string: LuaString = field.get("ident")?;
                    let ident = Ident::from_str(string.to_str()?);
                    let lua_expr = field.get("expr")?;
                    let mut expr = dummy_expr();
                    let is_shorthand = field.get("is_shorthand")?;
                    let span = get_span_or_default(&field, "span")?;

                    expr.merge_lua_ast(lua_expr)?;

                    fields.push(Field {
                        ident,
                        expr,
                        span,
                        is_shorthand,
                        attrs: ThinVec::new(), // TODO
                    })
                }

                ExprKind::Struct(path, fields, opt_expr)
            },
            "Repeat" => {
                let lua_expr = table.get("expr")?;
                let lua_ac_expr = table.get("anon_const")?;
                let mut expr = dummy_expr();
                let mut ac_expr = dummy_expr();
                let id = get_node_id_or_default(&table, "id")?;

                expr.merge_lua_ast(lua_expr)?;
                ac_expr.merge_lua_ast(lua_ac_expr)?;

                let anon_const = AnonConst {
                    id,
                    value: ac_expr,
                };

                ExprKind::Repeat(expr, anon_const)
            },
            "InlineAsm" => {
                let asm: LuaString = table.get("asm")?;
                let asm = Symbol::intern(asm.to_str()?);
                let dialect: LuaString = table.get("dialect")?;
                let dialect = match dialect.to_str()? {
                    "Att" => AsmDialect::Att,
                    "Intel" => AsmDialect::Intel,
                    e => unimplemented!("Unknown ASM dialect: {}", e),
                };
                let lua_inputs: LuaTable = table.get("inputs")?;
                let lua_outputs: LuaTable = table.get("outputs")?;
                let lua_clobbers: LuaTable = table.get("clobbers")?;
                let lua_syn_ctxt: LuaSyntaxContext = table.get("ctxt")?;
                let mut outputs = Vec::new();
                let mut inputs = Vec::new();
                let mut clobbers = Vec::new();

                for lua_output in lua_outputs.sequence_values::<LuaTable>() {
                    let lua_output = lua_output?;
                    let lua_expr = lua_output.get("expr")?;
                    let lua_constraint: LuaString = lua_output.get("constraint")?;
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(lua_expr)?;

                    let output = InlineAsmOutput {
                        constraint: Symbol::intern(lua_constraint.to_str()?),
                        expr,
                        is_indirect: lua_output.get("is_indirect")?,
                        is_rw: lua_output.get("is_rw")?,
                    };
                    outputs.push(output);
                }

                for lua_input in lua_inputs.sequence_values::<LuaTable>() {
                    let lua_input = lua_input?;
                    let lua_symbol: LuaString = lua_input.get("symbol")?;
                    let symbol = Symbol::intern(lua_symbol.to_str()?);
                    let lua_expr = lua_input.get("expr")?;
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(lua_expr)?;
                    inputs.push((symbol, expr))
                }

                for lua_clobber in lua_clobbers.sequence_values::<LuaString>() {
                    let lua_clobber = lua_clobber?;
                    let symbol = Symbol::intern(lua_clobber.to_str()?);

                    clobbers.push(symbol);
                }

                ExprKind::InlineAsm(P(InlineAsm {
                    asm,
                    asm_str_style: StrStyle::Cooked, // TODO: Raw strings
                    outputs,
                    inputs,
                    clobbers,
                    volatile: table.get("volatile")?,
                    alignstack: table.get("alignstack")?,
                    dialect,
                    ctxt: lua_syn_ctxt.0,
                }))
            },
            "Loop" => {
                let lua_block = table.get("block")?;
                let lua_label: Option<LuaString> = table.get("label")?;
                let opt_label = lua_label.map(|string| Ok(Label {
                    ident: Ident::from_str(string.to_str()?)
                })).transpose()?;
                let mut block = dummy_block();

                block.merge_lua_ast(lua_block)?;

                ExprKind::Loop(block, opt_label)
            },
            "Break" => {
                let lua_expr: Option<LuaTable> = table.get("expr")?;
                let lua_label: Option<LuaString> = table.get("label")?;
                let opt_label = lua_label.map(|string| Ok(Label {
                    ident: Ident::from_str(string.to_str()?)
                })).transpose()?;
                let opt_expr = lua_expr.map(|lua_expr| {
                    let mut expr = dummy_expr();

                    expr.merge_lua_ast(lua_expr).map(|_| expr)
                }).transpose()?;

                ExprKind::Break(opt_label, opt_expr)
            },
            "Continue" => {
                let lua_label: Option<LuaString> = table.get("label")?;
                let opt_label = lua_label.map(|string| Ok(Label {
                    ident: Ident::from_str(string.to_str()?)
                })).transpose()?;

                ExprKind::Continue(opt_label)
            },
            "Match" => {
                let lua_expr = table.get("expr")?;
                let lua_arms: LuaTable = table.get("arms")?;
                let mut expr = dummy_expr();
                let mut arms = Vec::new();

                for lua_arm in lua_arms.sequence_values::<LuaTable>() {
                    let mut arm = dummy_arm();

                    arm.merge_lua_ast(lua_arm?)?;
                    arms.push(arm);
                }

                expr.merge_lua_ast(lua_expr)?;

                ExprKind::Match(expr, arms)
            },
            "ForLoop" => {
                let lua_expr = table.get("expr")?;
                let lua_pat = table.get("pat")?;
                let lua_block = table.get("block")?;
                let lua_label: Option<LuaString> = table.get("label")?;
                let opt_label = lua_label.map(|string| Ok(Label {
                    ident: Ident::from_str(string.to_str()?)
                })).transpose()?;
                let mut expr = dummy_expr();
                let mut pat = dummy_pat();
                let mut block = dummy_block();

                block.merge_lua_ast(lua_block)?;
                expr.merge_lua_ast(lua_expr)?;
                pat.merge_lua_ast(lua_pat)?;

                ExprKind::ForLoop(pat, expr, block, opt_label)
            },
            "Paren" => {
                let lua_expr = table.get("expr")?;
                let mut expr = dummy_expr();

                expr.merge_lua_ast(lua_expr)?;

                ExprKind::Paren(expr)
            }
            ref e => {
                warn!("MergeLuaAst unimplemented expr: {:?}", e);
                return Ok(());
            },
        };

        Ok(())
    }
}

impl MergeLuaAst for ImplItem {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.ident.name = Symbol::intern(&table.get::<_, LuaString>("ident")?.to_str()?);
        self.span = get_span_or_default(&table, "span")?;
        self.id = get_node_id_or_default(&table, "id")?;

        // TODO: Allow for inplace kind mutations
        match &mut self.node {
            ImplItemKind::Method(sig, block) => {
                let lua_decl: LuaTable = table.get("decl")?;
                let lua_block: LuaTable = table.get("block")?;

                sig.decl.merge_lua_ast(lua_decl)?;
                block.merge_lua_ast(lua_block)?;

                // TODO: generics, attrs, ..
            },
            ref e => unimplemented!("MergeLuaAst for {:?}", e),
        }

        Ok(())
    }
}

impl MergeLuaAst for P<Ty> {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind: LuaString = table.get("kind")?;
        let kind = kind.to_str()?;

        self.span = get_span_or_default(&table, "span")?;
        self.id = get_node_id_or_default(&table, "id")?;
        self.node = match kind {
            "Path" => {
                let lua_segments: LuaTable = table.get("path")?;
                let mut path = dummy_path();

                path.merge_lua_ast(lua_segments)?;

                // TODO: QSelf support
                TyKind::Path(None, path)
            },
            "Ptr" | "Rptr" => {
                let mutbl = match table.get::<_, LuaString>("mutbl")?.to_str()? {
                    "Immutable" => Immutable,
                    "Mutable" => Mutable,
                    e => panic!("Found unknown ptr mutability: {}", e),
                };
                let lua_ty = table.get("ty")?;
                let mut ty = dummy_ty();

                ty.merge_lua_ast(lua_ty)?;

                let mut_ty = MutTy {ty, mutbl};

                if kind == "Ptr" {
                    TyKind::Ptr(mut_ty)
                } else {
                    let lua_lifetime: Option<LuaString> = table.get("lifetime")?;
                    let opt_lifetime = lua_lifetime.map(|s| {
                        s.to_str()
                            .map(|s| Ident::from_str(s))
                            .map(|i| Lifetime {id: DUMMY_NODE_ID, ident: i})
                    }).transpose()?;

                    TyKind::Rptr(opt_lifetime, mut_ty)
                }
            },
            "BareFn" => {
                let lua_decl = table.get("decl")?;
                let mut decl = dummy_fn_decl();
                let unsafety = match table.get::<_, LuaString>("unsafety")?.to_str()? {
                    "Unsafe" => Unsafety::Unsafe,
                    "Normal" => Unsafety::Normal,
                    e => panic!("Found unknown unsafety: {}", e),
                };
                let abi = match table.get::<_, LuaString>("abi")?.to_str()? {
                    "Cdecl" => Abi::Cdecl,
                    "C" => Abi::C,
                    "Rust" => Abi::Rust,
                    e => unimplemented!("Abi: {}", e),
                };

                decl.merge_lua_ast(lua_decl)?;

                let bare_fn = BareFnTy {
                    unsafety,
                    abi,
                    generic_params: Vec::new(), // TODO
                    decl,
                };

                TyKind::BareFn(P(bare_fn))
            },
            "Array" => {
                let lua_ty = table.get("ty")?;
                let lua_ac_expr = table.get("anon_const")?;
                let mut ac_expr = dummy_expr();
                let mut ty = dummy_ty();

                ac_expr.merge_lua_ast(lua_ac_expr)?;
                ty.merge_lua_ast(lua_ty)?;

                let anon_const = AnonConst {
                    id: DUMMY_NODE_ID,
                    value: ac_expr,
                };

                TyKind::Array(ty, anon_const)
            },
            "Typeof" => {
                let lua_ac_expr = table.get("anon_const")?;
                let mut ac_expr = dummy_expr();

                ac_expr.merge_lua_ast(lua_ac_expr)?;

                let anon_const = AnonConst {
                    id: DUMMY_NODE_ID,
                    value: ac_expr,
                };

                TyKind::Typeof(anon_const)
            },
            "Paren" | "Slice" => {
                let lua_ty = table.get("ty")?;
                let mut ty = dummy_ty();

                ty.merge_lua_ast(lua_ty)?;

                if kind == "Paren" {
                    TyKind::Paren(ty)
                } else {
                    TyKind::Slice(ty)
                }
            },
            "Tup" => {
                let lua_tys: LuaTable = table.get("tys")?;
                let mut tys = Vec::new();

                for lua_ty in lua_tys.sequence_values::<LuaTable>() {
                    let mut ty = dummy_ty();

                    ty.merge_lua_ast(lua_ty?)?;
                    tys.push(ty);
                }

                TyKind::Tup(tys)
            },
            "Never" => TyKind::Never,
            "ImplicitSelf" => TyKind::ImplicitSelf,
            "CVarArgs" => TyKind::CVarArgs,
            "Infer" => TyKind::Infer,
            ref e => unimplemented!("MergeLuaAst unimplemented ty kind: {:?}", e),
        };

        Ok(())
    }
}

impl MergeLuaAst for Path {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let mut segments = Vec::new();

        for lua_segment in table.sequence_values::<LuaTable>() {
            let lua_segment = lua_segment?;
            let ident: LuaString = lua_segment.get("ident")?;
            let lua_generics: Option<LuaTable> = lua_segment.get("generics")?;
            let mut path_segment = PathSegment::from_ident(Ident::from_str(ident.to_str()?));
            let opt_generics = lua_generics.map(|lua_generics| {
                let mut generics = dummy_generic_args();

                generics.merge_lua_ast(lua_generics).map(|_| P(generics))
            }).transpose()?;

            path_segment.id = get_node_id_or_default(&lua_segment, "id")?;
            path_segment.args = opt_generics;

            segments.push(path_segment);
        }

        self.segments = segments;

        Ok(())
    }
}

impl MergeLuaAst for GenericArgs {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind: LuaString = table.get("kind")?;
        let kind = kind.to_str()?;
        let span = get_span_or_default(&table, "span")?;

        *self = match kind {
            "AngleBracketed" => {
                let lua_args: LuaTable = table.get("args")?;
                let mut args = Vec::new();

                for lua_arg in lua_args.sequence_values::<LuaTable>() {
                    let mut arg = dummy_generic_arg();

                    arg.merge_lua_ast(lua_arg?)?;
                    args.push(arg);
                }

                GenericArgs::AngleBracketed(AngleBracketedArgs {
                    args,
                    span,
                    constraints: Vec::new(), // TODO
                })
            },
            "Parenthesized" => unimplemented!("MergeLuaAst unimplemented for Parenthesized"),
            e => unimplemented!("Unknown GenericArgs kind: {}", e),
        };

        Ok(())
    }
}

impl MergeLuaAst for GenericArg {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind: LuaString = table.get("kind")?;
        let kind = kind.to_str()?;

        *self = match kind {
            "Type" => {
                let lua_ty = table.get("ty")?;
                let mut ty = dummy_ty();

                ty.merge_lua_ast(lua_ty)?;

                GenericArg::Type(ty)
            },
            e => unimplemented!("Unknown GenericArg kind: {}", e),
        };

        Ok(())
    }
}

impl MergeLuaAst for UseTree {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let kind: LuaString = table.get("kind")?;
        let kind = kind.to_str()?;
        let lua_prefix_segments = table.get::<_, LuaTable>("prefix")?;

        self.span = get_span_or_default(&table, "span")?;
        self.prefix.merge_lua_ast(lua_prefix_segments)?;
        self.kind = match kind {
            "Simple" => {
                let lua_ident: Option<LuaString> = table.get("ident")?;
                let opt_ident = lua_ident.map(|lua_string|
                    Ok(Ident::from_str(lua_string.to_str()?))
                ).transpose()?;

                UseTreeKind::Simple(opt_ident, DUMMY_NODE_ID, DUMMY_NODE_ID)
            },
            "Nested" => {
                let lua_trees: LuaTable = table.get("trees")?;
                let mut trees = Vec::new();

                for lua_tree in lua_trees.sequence_values::<LuaTable>() {
                    let lua_tree = lua_tree?;
                    let mut tree = dummy_use_tree();

                    tree.merge_lua_ast(lua_tree)?;
                    trees.push((tree, DUMMY_NODE_ID));
                }

                UseTreeKind::Nested(trees)
            },
            "Glob" => UseTreeKind::Glob,
            e => unimplemented!("Unknown UseTree kind: {}", e),
        };

        Ok(())
    }
}

impl MergeLuaAst for Arm {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_body = table.get("body")?;
        let lua_guard: Option<LuaTable> = table.get("guard")?;
        let lua_pats: LuaTable = table.get("pats")?;
        let mut pats = Vec::new();

        self.body.merge_lua_ast(lua_body)?;

        match (&mut self.guard, lua_guard) {
            (Some(Guard::If(expr)), Some(ref lua_guard)) => expr.merge_lua_ast(lua_guard.clone())?,
            (Some(_), None) => self.guard = None,
            (None, Some(lua_guard)) => {
                let mut expr = dummy_expr();

                expr.merge_lua_ast(lua_guard)?;

                self.guard = Some(Guard::If(expr));
            },
            _ => (),
        }

        for lua_pat in lua_pats.sequence_values::<LuaTable>() {
            let mut pat = dummy_pat();

            pat.merge_lua_ast(lua_pat?)?;
            pats.push(pat);
        }

        self.pats = pats;

        // TODO: Attrs

        Ok(())
    }
}
