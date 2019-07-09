#![deny(missing_docs)]
//! This module provides translation for bitfield structs and operations on them. Generated code
//! requires the use of the c2rust-bitfields crate.

use std::collections::HashSet;
use std::ops::Index;

use super::TranslationError;
use crate::c_ast::{
    BinOp, CDeclId, CDeclKind, CExprId, CExprKind, CQualTypeId, CTypeId, MemberKind, UnOp,
};
use crate::translator::{simple_metaitem, ConvertedDecl, ExprContext, Translation};
use crate::with_stmts::WithStmts;
use c2rust_ast_builder::mk;
use syntax::ast::{
    self, AttrStyle, BinOpKind, Expr, ExprKind, Lit, LitIntType, LitKind, MetaItemKind,
    NestedMetaItem, StmtKind, StrStyle, StructField, Ty, TyKind,
};
use syntax::ptr::P;
use syntax::source_map::symbol::Symbol;
use syntax_pos::{Span, DUMMY_SP};

use itertools::EitherOrBoth::{Both, Right};
use itertools::Itertools;

/// (name, type, bitfield_width, platform_bit_offset, platform_type_bitwidth)
type FieldInfo = (String, CQualTypeId, Option<u64>, u64, u64);

#[derive(Debug)]
enum FieldType {
    BitfieldGroup {
        start_bit: u64,
        field_name: String,
        bytes: u64,
        attrs: Vec<(String, P<Ty>, String)>,
    },
    Padding {
        bytes: u64,
    },
    Regular {
        name: String,
        ctype: CTypeId,
        field: StructField,
    },
}

fn contains_block(expr_kind: &ExprKind) -> bool {
    match expr_kind {
        ExprKind::Block(..) => true,
        ExprKind::Assign(lhs, rhs) => contains_block(&lhs.node) || contains_block(&rhs.node),
        ExprKind::AssignOp(_, lhs, rhs) => contains_block(&lhs.node) || contains_block(&rhs.node),
        ExprKind::Binary(_, lhs, rhs) => contains_block(&lhs.node) || contains_block(&rhs.node),
        ExprKind::Unary(_, e) => contains_block(&e.node),
        ExprKind::MethodCall(_, exprs) => exprs.iter().map(|e| contains_block(&e.node)).any(|b| b),
        ExprKind::Cast(e, _) => contains_block(&e.node),
        _ => false,
    }
}

fn assigment_metaitem(lhs: &str, rhs: &str) -> NestedMetaItem {
    let node = LitKind::Str(Symbol::intern(rhs), StrStyle::Cooked);
    let token = node.to_lit_token();
    let meta_item = mk().meta_item(
        vec![lhs],
        MetaItemKind::NameValue(Lit {
            token,
            node,
            span: DUMMY_SP
        }),
    );

    mk().nested_meta_item(NestedMetaItem::MetaItem(meta_item))
}

impl<'a> Translation<'a> {
    /// This method aggregates bitfield struct field information by way of:
    /// 1. Collecting consecutive bytes of bitfields into a single FieldType::BitfieldGroup
    /// 2. Summing up the number of padding bytes between fields (or at the end of a struct)
    ///    into a FieldType::Padding
    /// 3. A standard field into a FieldType::Regular
    fn get_field_types(
        &self,
        field_info: Vec<FieldInfo>,
        platform_byte_size: u64,
    ) -> Result<Vec<FieldType>, TranslationError> {
        let mut reorganized_fields = Vec::new();
        let mut last_bitfield_group: Option<FieldType> = None;
        let mut next_byte_pos = 0;
        let mut encountered_bytes = HashSet::new();

        for (field_name, ty, bitfield_width, bit_index, platform_ty_bitwidth) in field_info {
            let ctype = ty.ctype;
            let ty = self.convert_type(ctype)?;
            let bitfield_width = match bitfield_width {
                // Bitfield widths of 0 should just be markers for clang,
                // we shouldn't need to explicitly handle it ourselves
                Some(0) => {
                    // Hit non bitfield group so existing one is all set
                    if let Some(field_group) = last_bitfield_group.take() {
                        reorganized_fields.push(field_group);
                    }

                    continue;
                }
                None => {
                    // Hit non bitfield group so existing one is all set
                    if let Some(field_group) = last_bitfield_group.take() {
                        reorganized_fields.push(field_group);
                    }

                    let bytes = bit_index / 8 - next_byte_pos;

                    // Need to add padding first
                    if bytes > 1 {
                        reorganized_fields.push(FieldType::Padding { bytes });
                    }

                    let field = mk().pub_().struct_field(field_name.clone(), ty);

                    reorganized_fields.push(FieldType::Regular {
                        name: field_name,
                        ctype,
                        field,
                    });

                    next_byte_pos = (bit_index + platform_ty_bitwidth) / 8;

                    continue;
                }
                Some(bw) => bw,
            };

            // Ensure we aren't looking at overlapping bits in the same byte
            if bit_index / 8 > next_byte_pos {
                let bytes = (bit_index / 8) - next_byte_pos;

                reorganized_fields.push(FieldType::Padding { bytes });
            }

            match last_bitfield_group {
                Some(FieldType::BitfieldGroup {
                    start_bit,
                    field_name: ref mut name,
                    ref mut bytes,
                    ref mut attrs,
                }) => {
                    name.push('_');
                    name.push_str(&field_name);

                    let end_bit = bit_index + bitfield_width;

                    // Add to the total byte size of the bitfield group only if
                    // we have not already enountered this byte
                    for bit in bit_index..end_bit {
                        let byte = bit / 8;

                        if !encountered_bytes.contains(&byte) {
                            *bytes += 1;
                            encountered_bytes.insert(byte);
                        }
                    }

                    let bit_start = bit_index - start_bit;
                    let bit_end = bit_start + bitfield_width - 1;
                    let bit_range = format!("{}..={}", bit_start, bit_end);

                    attrs.push((field_name.clone(), ty, bit_range));
                }
                Some(_) => unreachable!("Found last bitfield group which is not a group"),
                None => {
                    let mut bytes = 0;
                    let end_bit = bit_index + bitfield_width;

                    // Add to the total byte size of the bitfield group only if
                    // we have not already enountered this byte
                    for bit in bit_index..end_bit {
                        let byte = bit / 8;

                        if !encountered_bytes.contains(&byte) {
                            bytes += 1;
                            encountered_bytes.insert(byte);
                        }
                    }

                    let bit_range = format!("0..={}", bitfield_width - 1);
                    let attrs = vec![(field_name.clone(), ty, bit_range)];

                    last_bitfield_group = Some(FieldType::BitfieldGroup {
                        start_bit: bit_index,
                        field_name,
                        bytes,
                        attrs,
                    });
                }
            }

            next_byte_pos = (bit_index + bitfield_width - 1) / 8 + 1;
        }

        // Find leftover bitfield group at end: it's all set
        if let Some(field_group) = last_bitfield_group.take() {
            reorganized_fields.push(field_group);
        }

        // Packed structs can cause platform_byte_size < next_byte_pos
        if platform_byte_size > next_byte_pos {
            let bytes = platform_byte_size - next_byte_pos;

            // Need to add padding to end if we haven't hit the expected total byte size
            reorganized_fields.push(FieldType::Padding { bytes });
        }

        Ok(reorganized_fields)
    }

    /// Here we output a struct derive to generate bitfield data that looks like this:
    ///
    /// ```no_run
    /// #[derive(BitfieldStruct, Clone, Copy)]
    /// #[repr(C, align(2))]
    /// struct Foo {
    ///     #[bitfield(name = "bf1", ty = "libc::c_char", bits = "0..=9")]
    ///     #[bitfield(name = "bf2", ty = "libc::c_uchar",bits = "10..=15")]
    ///     bf1_bf2: [u8; 2],
    ///     non_bf: u64,
    ///     _pad: [u8; 2],
    /// }
    /// ```
    pub fn convert_bitfield_struct_decl(
        &self,
        name: String,
        manual_alignment: Option<u64>,
        platform_byte_size: u64,
        span: Span,
        field_info: Vec<FieldInfo>,
    ) -> Result<ConvertedDecl, TranslationError> {
        self.extern_crates.borrow_mut().insert("c2rust_bitfields");

        let item_store = &mut self.items.borrow_mut()[&self.main_file];

        item_store.add_use(vec!["c2rust_bitfields".into()], "BitfieldStruct");

        let mut field_entries = Vec::with_capacity(field_info.len());
        // We need to clobber bitfields in consecutive bytes together (leaving
        // regular fields alone) and add in padding as necessary
        let reorganized_fields = self.get_field_types(field_info, platform_byte_size)?;

        let mut padding_count = 0;

        for field_type in reorganized_fields {
            match field_type {
                FieldType::BitfieldGroup {
                    start_bit: _,
                    field_name,
                    bytes,
                    attrs,
                } => {
                    let ty = mk().array_ty(
                        mk().ident_ty("u8"),
                        mk().lit_expr(mk().int_lit(bytes.into(), LitIntType::Unsuffixed)),
                    );
                    let mut field = mk();
                    let field_attrs = attrs.iter().map(|attr| {
                        let ty_str = match &attr.1.node {
                            TyKind::Path(_, path) => format!("{}", path),
                            _ => unreachable!("Found type other than path"),
                        };
                        let field_attr_items = vec![
                            assigment_metaitem("name", &attr.0),
                            assigment_metaitem("ty", &ty_str),
                            assigment_metaitem("bits", &attr.2),
                        ];

                        mk().meta_item("bitfield", MetaItemKind::List(field_attr_items))
                    });

                    for field_attr in field_attrs {
                        field = field.meta_item_attr(AttrStyle::Outer, field_attr);
                    }

                    field_entries.push(field.pub_().struct_field(field_name, ty));
                }
                FieldType::Padding { bytes } => {
                    let field_name = if padding_count == 0 {
                        "_pad".into()
                    } else {
                        format!("_pad{}", padding_count + 1)
                    };
                    let ty = mk().array_ty(
                        mk().ident_ty("u8"),
                        mk().lit_expr(mk().int_lit(bytes.into(), LitIntType::Unsuffixed)),
                    );

                    // Mark it with `#[bitfield(padding)]`
                    let field_padding_inner = mk().meta_item("padding", MetaItemKind::Word);
                    let field_padding_inner =
                        vec![mk().nested_meta_item(NestedMetaItem::MetaItem(field_padding_inner))];
                    let field_padding_outer =
                        mk().meta_item("bitfield", MetaItemKind::List(field_padding_inner));
                    let field = mk()
                        .meta_item_attr(AttrStyle::Outer, field_padding_outer)
                        .pub_()
                        .struct_field(field_name, ty);

                    padding_count += 1;

                    field_entries.push(field);
                }
                FieldType::Regular { field, .. } => field_entries.push(field),
            }
        }

        let mut repr_items = vec![simple_metaitem("C")];

        if let Some(align) = manual_alignment {
            repr_items.push(simple_metaitem(&format!("align({})", align)));
        }

        let repr_attr = mk().meta_item("repr", MetaItemKind::List(repr_items));

        let item = mk()
            .span(span)
            .pub_()
            .call_attr("derive", vec!["BitfieldStruct", "Clone", "Copy"])
            .meta_item_attr(AttrStyle::Outer, repr_attr)
            .struct_item(name, field_entries);

        Ok(ConvertedDecl::Item(item))
    }

    /// Here we output a block to generate a struct literal initializer in.
    /// It looks like this in locals and (sectioned) statics:
    ///
    /// ```no_run
    /// {
    ///     let mut init = Foo {
    ///         bf1_bf2: [0; 2],
    ///         non_bf: 32,
    ///         _pad: [0; 2],
    ///     };
    ///     init.set_bf1(-12);
    ///     init.set_bf2(34);
    ///     init
    /// }
    /// ```
    pub fn convert_bitfield_struct_literal(
        &self,
        name: String,
        platform_byte_size: u64,
        field_ids: &[CExprId],
        field_info: Vec<FieldInfo>,
        ctx: ExprContext,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let mut fields = Vec::with_capacity(field_ids.len());
        let reorganized_fields = self.get_field_types(field_info.clone(), platform_byte_size)?;
        let local_pat = mk().mutbl().ident_pat("init");
        let mut padding_count = 0;

        // Add in zero inits for both padding as well as bitfield groups
        for field_type in reorganized_fields {
            match field_type {
                FieldType::BitfieldGroup {
                    field_name, bytes, ..
                } => {
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed)),
                        mk().lit_expr(mk().int_lit(bytes.into(), LitIntType::Unsuffixed)),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::Padding { bytes } => {
                    let field_name = if padding_count == 0 {
                        "_pad".into()
                    } else {
                        format!("_pad{}", padding_count + 1)
                    };
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed)),
                        mk().lit_expr(mk().int_lit(bytes.into(), LitIntType::Unsuffixed)),
                    );
                    let field = mk().field(field_name, array_expr);

                    padding_count += 1;

                    fields.push(WithStmts::new_val(field));
                }
                _ => {}
            }
        }

        // Bitfield widths of 0 should just be markers for clang,
        // we shouldn't need to explicitly handle it ourselves
        let field_info_iter = field_info.iter().filter(|info| info.2 != Some(0));
        let zipped_iter = field_ids.iter().zip_longest(field_info_iter);
        let mut bitfield_inits = Vec::new();

        // Specified record fields which are not bitfields need to be added
        for item in zipped_iter {
            match item {
                Right((field_name, ty, bitfield_width, _, _)) => {
                    if bitfield_width.is_some() {
                        continue;
                    }

                    let init = self.implicit_default_expr(ty.ctype, ctx.is_static)?;
                    if !init.is_pure() {
                        return Err(TranslationError::generic(
                            "Expected no statements in field expression"
                        ));
                    }
                    let field = init.map(|init| mk().field(field_name, init));
                    fields.push(field);
                }
                Both(field_id, (field_name, _, bitfield_width, _, _)) => {
                    let expr = self.convert_expr(ctx.used(), *field_id)?;

                    if !expr.is_pure() {
                        return Err(TranslationError::generic(
                            "Expected no statements in field expression"
                        ));
                    }

                    if bitfield_width.is_some() {
                        bitfield_inits.push((field_name, expr));

                        continue;
                    }

                    fields.push(expr.map(|expr| mk().field(field_name, expr)));
                }
                _ => unreachable!(),
            }
        }

        fields.into_iter()
            .collect::<WithStmts<Vec<ast::Field>>>()
            .and_then(|fields| {
                let struct_expr = mk().struct_expr(name.as_str(), fields);
                let local_variable = P(mk().local(
                    local_pat,
                    None as Option<P<Ty>>,
                    Some(struct_expr))
                );

                let mut is_unsafe = false;
                let mut stmts = vec![mk().local_stmt(local_variable)];

                // Now we must use the bitfield methods to initialize bitfields
                for (field_name, val) in bitfield_inits {
                    let field_name_setter = format!("set_{}", field_name);
                    let struct_ident = mk().ident_expr("init");
                    is_unsafe |= val.is_unsafe();
                    let val = val.to_pure_expr()
                        .expect("Expected no statements in bitfield initializer");
                    let expr = mk().method_call_expr(struct_ident, field_name_setter, vec![val]);

                    stmts.push(mk().expr_stmt(expr));
                }

                let struct_ident = mk().ident_expr("init");

                stmts.push(mk().expr_stmt(struct_ident));

                let val = mk().block_expr(mk().block(stmts)); 

                if is_unsafe {
                    Ok(WithStmts::new_unsafe_val(val))
                } else {
                    Ok(WithStmts::new_val(val))
                }
           })
    }

    /// This method handles zero-initializing bitfield structs including bitfields
    /// & padding fields
    pub fn bitfield_zero_initializer(
        &self,
        name: String,
        field_ids: &[CDeclId],
        platform_byte_size: u64,
        is_static: bool,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let field_info: Vec<FieldInfo> = field_ids
            .iter()
            .map(|field_id| match self.ast_context.index(*field_id).kind {
                CDeclKind::Field {
                    typ,
                    bitfield_width,
                    platform_bit_offset,
                    platform_type_bitwidth,
                    ..
                } => {
                    let name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(None, *field_id)
                        .unwrap();
                    (
                        name,
                        typ,
                        bitfield_width,
                        platform_bit_offset,
                        platform_type_bitwidth,
                    )
                }
                _ => unreachable!("Found non-field in record field list"),
            })
            .collect();
        let reorganized_fields = self.get_field_types(field_info, platform_byte_size)?;
        let mut fields = Vec::with_capacity(reorganized_fields.len());
        let mut padding_count = 0;

        for field_type in reorganized_fields {
            match field_type {
                FieldType::BitfieldGroup {
                    field_name, bytes, ..
                } => {
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed)),
                        mk().lit_expr(mk().int_lit(bytes.into(), LitIntType::Unsuffixed)),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::Padding { bytes } => {
                    let field_name = if padding_count == 0 {
                        "_pad".into()
                    } else {
                        format!("_pad{}", padding_count + 1)
                    };
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed)),
                        mk().lit_expr(mk().int_lit(bytes.into(), LitIntType::Unsuffixed)),
                    );
                    let field = mk().field(field_name, array_expr);

                    padding_count += 1;

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::Regular { ctype, name, .. } => {
                    let field_init = self.implicit_default_expr(ctype, is_static)?;
                    if !field_init.is_pure() {
                        return Err(TranslationError::generic(
                            "Expected no statements in field expression"
                        ));
                    }
                    fields.push(field_init.map(|init| mk().field(name, init)))
                }
            }
        }

        Ok(fields.into_iter()
            .collect::<WithStmts<Vec<ast::Field>>>()
            .map(|fields| mk().struct_expr(name.as_str(), fields)))
    }

    /// This method handles conversion of assignment operators on bitfields.
    /// Regular fields would look like this:
    /// A) bf.a = 1;
    /// B) bf.a += 1;
    ///
    /// However, since we need to call methods for read and write, we generate this:
    /// A) bf.set_a(1);
    /// B) bf.set_a(bf.a() + 1);
    ///
    /// Note that B) requires NLL to be valid rust
    pub fn convert_bitfield_assignment_op_with_rhs(
        &self,
        ctx: ExprContext,
        op: BinOp,
        lhs: CExprId,
        rhs_expr: P<Expr>,
        field_id: CDeclId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let ctx = ctx.set_bitfield_write(true);
        let named_reference = self.name_reference_write_read(ctx, lhs)?;
        named_reference.and_then(|named_reference| {
            let lhs_expr = named_reference.0;
            let field_name = self
                .type_converter
                .borrow()
                .resolve_field_name(None, field_id)
                .ok_or("Could not find bitfield name")?;
            let setter_name = format!("set_{}", field_name);
            let lhs_expr_read =
                mk().method_call_expr(lhs_expr.clone(), field_name, Vec::<P<Expr>>::new());
            // Allow the value of this assignment to be used as the RHS of other assignments
            let val = lhs_expr_read.clone();
            let param_expr = match op {
                BinOp::AssignAdd => mk().binary_expr(BinOpKind::Add, lhs_expr_read, rhs_expr),
                BinOp::AssignSubtract => mk().binary_expr(BinOpKind::Sub, lhs_expr_read, rhs_expr),
                BinOp::AssignMultiply => mk().binary_expr(BinOpKind::Mul, lhs_expr_read, rhs_expr),
                BinOp::AssignDivide => mk().binary_expr(BinOpKind::Div, lhs_expr_read, rhs_expr),
                BinOp::AssignModulus => mk().binary_expr(BinOpKind::Rem, lhs_expr_read, rhs_expr),
                BinOp::AssignBitXor => mk().binary_expr(BinOpKind::BitXor, lhs_expr_read, rhs_expr),
                BinOp::AssignShiftLeft => mk().binary_expr(BinOpKind::Shl, lhs_expr_read, rhs_expr),
                BinOp::AssignShiftRight => mk().binary_expr(BinOpKind::Shr, lhs_expr_read, rhs_expr),
                BinOp::AssignBitOr => mk().binary_expr(BinOpKind::BitOr, lhs_expr_read, rhs_expr),
                BinOp::AssignBitAnd => mk().binary_expr(BinOpKind::BitAnd, lhs_expr_read, rhs_expr),
                BinOp::Assign => rhs_expr,
                _ => panic!("Cannot convert non-assignment operator"),
            };

            let mut stmts = vec![];

            // If there's just one statement we should be able to be able to fit it into one line without issue
            // If there's a block we can flatten it into the current scope, and if the expr contains a block it's
            // likely complex enough to warrant putting it into a temporary variable to avoid borrowing issues
            match param_expr.node {
                ExprKind::Block(ref block, _) => {
                    let last = block.stmts.len() - 1;

                    for (i, stmt) in block.stmts.iter().enumerate() {
                        if i == last {
                            break;
                        }

                        stmts.push(stmt.clone());
                    }

                    let last_expr = match block.stmts[last].node {
                        StmtKind::Expr(ref expr) => expr.clone(),
                        _ => return Err(TranslationError::generic("Expected Expr StmtKind")),
                    };
                    let method_call = mk().method_call_expr(lhs_expr, setter_name, vec![last_expr]);

                    stmts.push(mk().expr_stmt(method_call));
                }
                _ if contains_block(&param_expr.node) => {
                    let name = self.renamer.borrow_mut().pick_name("rhs");
                    let name_ident = mk().mutbl().ident_pat(name.clone());
                    let temporary_stmt =
                        mk().local(name_ident, None as Option<P<Ty>>, Some(param_expr.clone()));
                    let assignment_expr =
                        mk().method_call_expr(lhs_expr, setter_name, vec![mk().ident_expr(name)]);

                    stmts.push(mk().local_stmt(P(temporary_stmt)));
                    stmts.push(mk().expr_stmt(assignment_expr));
                }
                _ => {
                    let assignment_expr =
                        mk().method_call_expr(lhs_expr, setter_name, vec![param_expr.clone()]);

                    stmts.push(mk().expr_stmt(assignment_expr));
                }
            };

            return Ok(WithStmts::new(stmts, val));
        })
    }

    /// This method will convert a bitfield member one of four ways:
    /// A) bf.a()
    /// B) (*bf).a()
    /// C) bf
    /// D) (*bf)
    ///
    /// The first two are when we know this bitfield member is going to be read
    /// from (default), possibly requiring a dereference first. The latter two
    /// are generated when we are expecting to require a write, which will need
    /// to make a method call with some input which we do not yet have access
    /// to and will have to be handled elsewhere, IE `bf.set_a(1)`
    pub fn convert_bitfield_member_expr(
        &self,
        ctx: ExprContext,
        field_name: String,
        expr_id: CExprId,
        kind: MemberKind,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let mut val = match kind {
            MemberKind::Dot => self.convert_expr(ctx, expr_id)?,
            MemberKind::Arrow => {
                if let CExprKind::Unary(_, UnOp::AddressOf, subexpr_id, _) =
                    self.ast_context[expr_id].kind
                {
                    self.convert_expr(ctx, subexpr_id)?
                } else {
                    let val = self.convert_expr(ctx, expr_id)?;

                    val.map(|v| mk().unary_expr(ast::UnOp::Deref, v))
                }
            }
        };

        if !ctx.is_bitfield_write() {
            val = val.map(|v| mk().method_call_expr(v, field_name, vec![] as Vec<P<Expr>>));
        }

        Ok(val)
    }
}
