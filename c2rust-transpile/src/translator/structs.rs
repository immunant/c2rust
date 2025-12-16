//! This module provides translation for bitfield structs and operations on them. Generated code
//! requires the use of the c2rust-bitfields crate.

use std::collections::HashSet;
use std::ops::Index;

use super::named_references::NamedReference;
use super::TranslationError;
use crate::c_ast::{BinOp, CDeclId, CDeclKind, CExprId, CRecordId, CTypeId};
use crate::diagnostics::TranslationResult;
use crate::translator::{ConvertedDecl, ExprContext, Translation, PADDING_SUFFIX};
use crate::with_stmts::WithStmts;
use crate::ExternCrate;
use c2rust_ast_builder::mk;
use c2rust_ast_printer::pprust;
use proc_macro2::Span;
use syn::{
    self, BinOp as RBinOp, Expr, ExprAssign, ExprBinary, ExprBlock, ExprCast, ExprMethodCall,
    ExprUnary, Field, Stmt, Type,
};

use itertools::EitherOrBoth::{Both, Right};
use itertools::Itertools;

impl<'a> Translation<'a> {
    pub fn convert_struct(
        &self,
        decl_id: CDeclId,
        span: Span,
        fields: &[CDeclId],
        is_packed: bool,
        platform_byte_size: u64,
        manual_alignment: Option<u64>,
        max_field_alignment: Option<u64>,
    ) -> TranslationResult<ConvertedDecl> {
        let name = self
            .type_converter
            .borrow()
            .resolve_decl_name(decl_id)
            .unwrap();

        // Check if the last field might be a flexible array member
        if let Some(last_id) = fields.last() {
            let field_decl = &self.ast_context[*last_id];
            if let CDeclKind::Field { typ, .. } = field_decl.kind {
                if self.ast_context.maybe_flexible_array(typ.ctype) {
                    self.potential_flexible_array_members
                        .borrow_mut()
                        .insert(*last_id);
                }
            }
        }

        // Pre-declare all the field names, checking for duplicates
        for &x in fields {
            if let CDeclKind::Field { ref name, .. } = self.ast_context.index(x).kind {
                self.type_converter
                    .borrow_mut()
                    .declare_field_name(decl_id, x, name);
            }
        }

        // Gather up all the field names and field types
        let (field_entries, contains_va_list) =
            self.convert_struct_fields(decl_id, fields, platform_byte_size)?;

        let mut derives = vec![];
        if !contains_va_list {
            derives.push("Copy");
            derives.push("Clone");
        };
        let has_bitfields =
            fields
                .iter()
                .any(|field_id| match self.ast_context.index(*field_id).kind {
                    CDeclKind::Field { bitfield_width, .. } => bitfield_width.is_some(),
                    _ => unreachable!("Found non-field in record field list"),
                });
        if has_bitfields {
            derives.push("BitfieldStruct");
            self.use_crate(ExternCrate::C2RustBitfields);
        }

        let mut reprs = vec![mk().meta_path("C")];
        let max_field_alignment = if is_packed {
            // `__attribute__((packed))` forces a max alignment of 1,
            // overriding `#pragma pack`; this is also what clang does
            Some(1)
        } else {
            max_field_alignment
        };
        match max_field_alignment {
            Some(1) => reprs.push(mk().meta_path("packed")),
            Some(mf) if mf > 1 => reprs.push(mk().meta_list("packed", vec![mf])),
            _ => {}
        }

        if let Some(alignment) = manual_alignment {
            // This is the most complicated case: we have `align(N)` which
            // might be mixed with or included into a `packed` structure,
            // which Rust doesn't currently support; instead, we split
            // the structure into 2 structures like this:
            //   #[align(N)]
            //   pub struct Foo(pub Foo_Inner);
            //   #[packed(M)]
            //   pub struct Foo_Inner {
            //     ...fields...
            //   }
            //
            // TODO: right now, we always emit the pair of structures
            // instead, we should only split when needed, but that
            // would significantly complicate the implementation
            assert!(self.ast_context.has_inner_struct_decl(decl_id));
            let inner_name = self.resolve_decl_inner_name(decl_id);
            let inner_ty = mk().path_ty(vec![inner_name.clone()]);
            let inner_struct = mk()
                .span(span)
                .pub_()
                .call_attr("derive", derives)
                .call_attr("repr", reprs)
                .struct_item(inner_name.clone(), field_entries, false);

            let outer_ty = mk().path_ty(vec![name.to_owned()]);
            let outer_field = mk().pub_().enum_field(mk().ident_ty(inner_name));
            let outer_struct = mk()
                .span(span)
                .pub_()
                .call_attr("derive", vec!["Copy", "Clone"])
                .call_attr(
                    "repr",
                    vec![
                        mk().meta_path("C"),
                        mk().meta_list("align", vec![alignment]),
                        // TODO: copy others from `reprs` above
                    ],
                )
                .struct_item(name, vec![outer_field], true);

            // Emit `const X_PADDING: usize = size_of(Outer) - size_of(Inner);`
            let padding_name = self
                .type_converter
                .borrow_mut()
                .resolve_decl_suffix_name(decl_id, PADDING_SUFFIX)
                .to_owned();
            let padding_ty = mk().path_ty(vec!["usize"]);
            let outer_size = self.mk_size_of_ty_expr(outer_ty)?.to_expr();
            let inner_size = self.mk_size_of_ty_expr(inner_ty)?.to_expr();
            let padding_value =
                mk().binary_expr(RBinOp::Sub(Default::default()), outer_size, inner_size);
            let padding_const = mk()
                .span(span)
                .call_attr("allow", vec!["dead_code", "non_upper_case_globals"])
                .const_item(padding_name, padding_ty, padding_value);

            let structs = vec![outer_struct, inner_struct, padding_const];
            Ok(ConvertedDecl::Items(structs))
        } else {
            assert!(!self.ast_context.has_inner_struct_decl(decl_id));
            let mut mk_ = mk()
                .span(span)
                .pub_()
                .call_attr("derive", derives)
                .call_attr("repr", reprs);

            if contains_va_list {
                mk_ = mk_.generic_over(mk().lt_param(mk().ident("a")))
            }

            Ok(ConvertedDecl::Item(mk_.struct_item(
                name,
                field_entries,
                false,
            )))
        }
    }

    /// Here we output a struct derive to generate bitfield data that looks like this:
    ///
    /// ```no_run
    /// # use c2rust_bitfields::BitfieldStruct;
    /// #
    /// #[derive(BitfieldStruct, Clone, Copy)]
    /// #[repr(C, align(2))]
    /// struct Foo {
    ///     #[bitfield(name = "bf1", ty = "std::ffi::c_char", bits = "0..=9")]
    ///     #[bitfield(name = "bf2", ty = "std::ffi::c_uchar",bits = "10..=15")]
    ///     bf1_bf2: [u8; 2],
    ///     non_bf: u64,
    ///     _pad: [u8; 2],
    /// }
    /// ```
    fn convert_struct_fields(
        &self,
        struct_id: CRecordId,
        field_ids: &[CDeclId],
        platform_byte_size: u64,
    ) -> TranslationResult<(Vec<Field>, bool)> {
        let mut field_entries = Vec::with_capacity(field_ids.len());
        // We need to clobber bitfields in consecutive bytes together (leaving
        // regular fields alone) and add in padding as necessary
        let reorganized_fields = self.get_field_types(struct_id, field_ids, platform_byte_size)?;

        let contains_va_list = reorganized_fields.iter().any(|field| match field {
            FieldType::Regular { is_va_list, .. } => *is_va_list,
            _ => false,
        });

        let mut padding_count = 0;
        let mut next_padding_field = || {
            let field_name = self
                .type_converter
                .borrow_mut()
                .declare_padding(struct_id, padding_count);
            padding_count += 1;
            field_name
        };

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
                        mk().lit_expr(mk().int_unsuffixed_lit(bytes)),
                    );
                    let mut field = mk();
                    for attr in attrs {
                        let ty_str = match &*attr.1 {
                            Type::Path(syn::TypePath { path, .. }) => pprust::path_to_string(path),
                            _ => unreachable!("Found type other than path"),
                        };
                        let field_attr_items = vec![
                            mk().meta_namevalue("name", &attr.0),
                            mk().meta_namevalue("ty", &ty_str),
                            mk().meta_namevalue("bits", &attr.2),
                        ];

                        field = field.call_attr("bitfield", field_attr_items)
                    }

                    field_entries.push(field.pub_().struct_field(field_name, ty));
                }
                FieldType::Padding { bytes } => {
                    let field_name = next_padding_field();
                    let ty = mk().array_ty(
                        mk().ident_ty("u8"),
                        mk().lit_expr(mk().int_unsuffixed_lit(bytes)),
                    );

                    // Mark it with `#[bitfield(padding)]`
                    let field = mk()
                        .call_attr("bitfield", vec!["padding"])
                        .pub_()
                        .struct_field(field_name, ty);

                    field_entries.push(field);
                }
                FieldType::ComputedPadding { ident } => {
                    let field_name = next_padding_field();
                    let ty = mk().array_ty(mk().ident_ty("u8"), mk().ident_expr(ident));

                    // TODO: disable cross-checks on this field
                    let field = mk().pub_().struct_field(field_name, ty);

                    field_entries.push(field);
                }
                FieldType::Regular { field, .. } => field_entries.push(*field),
            }
        }
        Ok((field_entries, contains_va_list))
    }

    /// Here we output a block to generate a struct literal initializer in.
    /// It looks like this in locals and (sectioned) statics:
    ///
    /// ```no_run
    /// # use c2rust_bitfields::BitfieldStruct;
    /// #
    /// # #[derive(BitfieldStruct, Clone, Copy)]
    /// # #[repr(C, align(2))]
    /// # struct Foo {
    /// #     #[bitfield(name = "bf1", ty = "std::ffi::c_char", bits = "0..=9")]
    /// #     #[bitfield(name = "bf2", ty = "std::ffi::c_uchar",bits = "10..=15")]
    /// #     bf1_bf2: [u8; 2],
    /// #     non_bf: u64,
    /// #     _pad: [u8; 2],
    /// # }
    /// #
    /// # let _ =
    /// {
    ///     let mut init = Foo {
    ///         bf1_bf2: [0; 2],
    ///         non_bf: 32,
    ///         _pad: [0; 2],
    ///     };
    ///     init.set_bf1(-12i8 as _);
    ///     init.set_bf2(34);
    ///     init
    /// }
    /// # ;
    /// ```
    pub fn convert_struct_literal(
        &self,
        ctx: ExprContext,
        struct_id: CRecordId,
        field_expr_ids: &[CExprId],
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let name = self.resolve_decl_inner_name(struct_id);
        log::debug!("importing struct {name}, id {struct_id:?}");
        self.add_import(struct_id, &name);

        let (field_decl_ids, platform_byte_size) = match self.ast_context.index(struct_id).kind {
            CDeclKind::Struct {
                fields: Some(ref fields),
                platform_byte_size,
                ..
            } => (fields, platform_byte_size),

            CDeclKind::Struct { fields: None, .. } => {
                return Err(TranslationError::generic(
                    "Attempted to zero-initialize forward-declared struct",
                ))
            }

            _ => panic!("Struct literal declaration mismatch"),
        };

        let mut fields = Vec::with_capacity(field_decl_ids.len());
        let reorganized_fields =
            self.get_field_types(struct_id, field_decl_ids, platform_byte_size)?;
        let mut padding_count = 0;
        let mut next_padding_field = || {
            let field_name = self
                .type_converter
                .borrow_mut()
                .declare_padding(struct_id, padding_count);
            padding_count += 1;
            field_name
        };

        // Add in zero inits for both padding as well as bitfield groups
        for field_type in reorganized_fields {
            match field_type {
                FieldType::BitfieldGroup {
                    field_name, bytes, ..
                } => {
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_unsuffixed_lit(0)),
                        mk().lit_expr(mk().int_unsuffixed_lit(bytes)),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::Padding { bytes } => {
                    let field_name = next_padding_field();
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_unsuffixed_lit(0)),
                        mk().lit_expr(mk().int_unsuffixed_lit(bytes)),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::ComputedPadding { ident } => {
                    let field_name = next_padding_field();
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_unsuffixed_lit(0)),
                        mk().ident_expr(ident),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                _ => {}
            }
        }

        // Bitfield widths of 0 should just be markers for clang,
        // we shouldn't need to explicitly handle it ourselves
        let is_packed = self.ast_context.is_packed_struct_decl(struct_id);
        let field_info_iter = field_decl_ids.iter().filter_map(|field_id| {
            match self.ast_context.index(*field_id).kind {
                CDeclKind::Field {
                    bitfield_width: Some(0),
                    ..
                } => None,
                CDeclKind::Field {
                    typ,
                    bitfield_width,
                    ..
                } => {
                    let field_name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(None, *field_id)
                        .unwrap();

                    let use_inner_type =
                        is_packed && self.ast_context.is_aligned_struct_type(typ.ctype);
                    Some((field_name, typ, bitfield_width, use_inner_type))
                }
                _ => None,
            }
        });
        let zipped_iter = field_expr_ids.iter().zip_longest(field_info_iter);
        let mut bitfield_inits = Vec::new();

        // Specified record fields which are not bitfields need to be added
        for item in zipped_iter {
            match item {
                Right((field_name, ty, bitfield_width, use_inner_type)) => {
                    if bitfield_width.is_some() {
                        continue;
                    }

                    let mut init = self.implicit_default_expr(ty.ctype, ctx.is_static)?;
                    if !init.is_pure() {
                        return Err(TranslationError::generic(
                            "Expected no statements in field expression",
                        ));
                    }
                    if use_inner_type {
                        // Small hack: we need a value of the inner type,
                        // but `implicit_default_expr` produced a value
                        // of the outer type, so unwrap it manually
                        init = init.map(|fi| mk().anon_field_expr(fi, 0));
                    }
                    let field = init.map(|init| mk().field(field_name, init));
                    fields.push(field);
                }
                Both(field_id, (field_name, ty, bitfield_width, use_inner_type)) => {
                    let mut expr = self.convert_expr(ctx.used(), *field_id, Some(ty))?;

                    if use_inner_type {
                        // See comment above
                        expr = expr.map(|fi| mk().anon_field_expr(fi, 0));
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

        let fields = fields
            .into_iter()
            .collect::<WithStmts<Vec<syn::FieldValue>>>();
        let mut val = fields.map(|fields| mk().struct_expr(name.as_str(), fields));

        if !bitfield_inits.is_empty() {
            val = val.and_then(|val| -> TranslationResult<_> {
                let local_pat = mk().mutbl().ident_pat("init");
                let local_variable = Box::new(mk().local(local_pat, None, Some(val)));

                let mut is_unsafe = false;
                let mut stmts = vec![mk().local_stmt(local_variable)];

                // Now we must use the bitfield methods to initialize bitfields
                for (field_name, val) in bitfield_inits {
                    let field_name_setter = format!("set_{}", field_name);
                    let struct_ident = mk().ident_expr("init");
                    is_unsafe |= val.is_unsafe();
                    let val = val
                        .to_pure_expr()
                        .expect("Expected no statements in bitfield initializer");
                    let expr = mk().method_call_expr(struct_ident, field_name_setter, vec![val]);

                    stmts.push(mk().semi_stmt(expr));
                }

                stmts.push(mk().expr_stmt(mk().ident_expr("init")));

                let val = mk().block_expr(mk().block(stmts));

                if is_unsafe {
                    Ok(WithStmts::new_unsafe_val(val))
                } else {
                    Ok(WithStmts::new_val(val))
                }
            })?;
        }

        // If the structure is split into an outer/inner,
        // wrap the inner initializer using the outer structure
        if self.ast_context.has_inner_struct_decl(struct_id) {
            let outer_name = self
                .type_converter
                .borrow()
                .resolve_decl_name(struct_id)
                .unwrap();

            let outer_path = mk().path_expr(vec![outer_name]);
            val = val.map(|val| mk().call_expr(outer_path, vec![val]));
        }

        Ok(val)
    }

    /// This method handles zero-initializing bitfield structs including bitfields
    /// & padding fields
    pub fn convert_struct_zero_initializer(
        &self,
        decl_id: CRecordId,
        name_decl_id: CDeclId,
        field_ids: &[CDeclId],
        platform_byte_size: u64,
        is_static: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let name = self.resolve_decl_inner_name(name_decl_id);
        let reorganized_fields = self.get_field_types(decl_id, field_ids, platform_byte_size)?;
        let mut fields = Vec::with_capacity(reorganized_fields.len());

        let mut padding_count = 0;
        let mut next_padding_field = || {
            let field_name = self
                .type_converter
                .borrow_mut()
                .declare_padding(decl_id, padding_count);
            padding_count += 1;
            field_name
        };

        for field_type in reorganized_fields {
            match field_type {
                FieldType::BitfieldGroup {
                    field_name, bytes, ..
                } => {
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_unsuffixed_lit(0)),
                        mk().lit_expr(mk().int_unsuffixed_lit(bytes)),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::Padding { bytes } => {
                    let field_name = next_padding_field();
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_unsuffixed_lit(0)),
                        mk().lit_expr(mk().int_unsuffixed_lit(bytes)),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::ComputedPadding { ident } => {
                    let field_name = next_padding_field();
                    let array_expr = mk().repeat_expr(
                        mk().lit_expr(mk().int_unsuffixed_lit(0)),
                        mk().ident_expr(ident),
                    );
                    let field = mk().field(field_name, array_expr);

                    fields.push(WithStmts::new_val(field));
                }
                FieldType::Regular {
                    ctype,
                    name,
                    use_inner_type,
                    ..
                } => {
                    let mut field_init = self.implicit_default_expr(ctype, is_static)?;
                    if !field_init.is_pure() {
                        return Err(TranslationError::generic(
                            "Expected no statements in field expression",
                        ));
                    }
                    if use_inner_type {
                        // See comment above
                        field_init = field_init.map(|fi| mk().anon_field_expr(fi, 0));
                    }
                    fields.push(field_init.map(|init| mk().field(name, init)))
                }
            }
        }

        Ok(fields
            .into_iter()
            .collect::<WithStmts<Vec<syn::FieldValue>>>()
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
        rhs_expr: Box<Expr>,
        field_id: CDeclId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let ctx = ctx.set_bitfield_write(true);
        let named_reference = self.name_reference_write_read(ctx, lhs)?;
        named_reference.and_then(
            |NamedReference {
                 lvalue: lhs_expr, ..
             }| {
                let field_name = self
                    .type_converter
                    .borrow()
                    .resolve_field_name(None, field_id)
                    .ok_or("Could not find bitfield name")?;
                let setter_name = format!("set_{}", field_name);
                let lhs_expr_read = mk().method_call_expr(lhs_expr.clone(), field_name, Vec::new());
                // Allow the value of this assignment to be used as the RHS of other assignments
                let val = lhs_expr_read.clone();
                let param_expr = match op {
                    BinOp::AssignAdd => {
                        mk().binary_expr(RBinOp::Add(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignSubtract => {
                        mk().binary_expr(RBinOp::Sub(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignMultiply => {
                        mk().binary_expr(RBinOp::Mul(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignDivide => {
                        mk().binary_expr(RBinOp::Div(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignModulus => {
                        mk().binary_expr(RBinOp::Rem(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignBitXor => mk().binary_expr(
                        RBinOp::BitXor(Default::default()),
                        lhs_expr_read,
                        rhs_expr,
                    ),
                    BinOp::AssignShiftLeft => {
                        mk().binary_expr(RBinOp::Shl(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignShiftRight => {
                        mk().binary_expr(RBinOp::Shr(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignBitOr => {
                        mk().binary_expr(RBinOp::BitOr(Default::default()), lhs_expr_read, rhs_expr)
                    }
                    BinOp::AssignBitAnd => mk().binary_expr(
                        RBinOp::BitAnd(Default::default()),
                        lhs_expr_read,
                        rhs_expr,
                    ),
                    BinOp::Assign => rhs_expr,
                    _ => panic!("Cannot convert non-assignment operator"),
                };

                let mut stmts = vec![];

                // If there's just one statement we should be able to be able to fit it into one line without issue
                // If there's a block we can flatten it into the current scope, and if the expr contains a block it's
                // likely complex enough to warrant putting it into a temporary variable to avoid borrowing issues
                match *param_expr {
                    Expr::Block(ExprBlock { block, .. }) => {
                        let last = block.stmts.len() - 1;

                        for (i, stmt) in block.stmts.iter().enumerate() {
                            if i == last {
                                break;
                            }

                            stmts.push(stmt.clone());
                        }

                        let last_expr = match block.stmts[last] {
                            Stmt::Expr(ref expr, None) => expr.clone(),
                            _ => return Err(TranslationError::generic("Expected Expr Stmt")),
                        };
                        let method_call =
                            mk().method_call_expr(lhs_expr, setter_name, vec![Box::new(last_expr)]);

                        stmts.push(mk().semi_stmt(method_call));
                    }
                    _ if contains_block(&param_expr) => {
                        let name = self.renamer.borrow_mut().pick_name("rhs");
                        let name_ident = mk().mutbl().ident_pat(name.clone());
                        let temporary_stmt = mk().local(name_ident, None, Some(param_expr.clone()));
                        let assignment_expr = mk().method_call_expr(
                            lhs_expr,
                            setter_name,
                            vec![mk().ident_expr(name)],
                        );

                        stmts.push(mk().local_stmt(Box::new(temporary_stmt)));
                        stmts.push(mk().semi_stmt(assignment_expr));
                    }
                    _ => {
                        let assignment_expr =
                            mk().method_call_expr(lhs_expr, setter_name, vec![param_expr.clone()]);

                        stmts.push(mk().semi_stmt(assignment_expr));
                    }
                };

                Ok(WithStmts::new(stmts, val))
            },
        )
    }

    /// This method aggregates bitfield struct field information by way of:
    /// 1. Collecting consecutive bytes of bitfields into a single FieldType::BitfieldGroup
    /// 2. Summing up the number of padding bytes between fields (or at the end of a struct)
    ///    into a FieldType::Padding
    /// 3. A standard field into a FieldType::Regular
    fn get_field_types(
        &self,
        record_id: CRecordId,
        field_ids: &[CDeclId],
        platform_byte_size: u64,
    ) -> TranslationResult<Vec<FieldType>> {
        let mut reorganized_fields = Vec::new();
        let mut last_bitfield_group: Option<FieldType> = None;
        let mut next_byte_pos = 0;
        let mut encountered_bytes = HashSet::new();

        for field_id in field_ids {
            if let CDeclKind::Field {
                typ,
                bitfield_width,
                platform_bit_offset,
                platform_type_bitwidth,
                ..
            } = self.ast_context.index(*field_id).kind
            {
                let field_name = self
                    .type_converter
                    .borrow()
                    .resolve_field_name(Some(record_id), *field_id)
                    .unwrap();

                let ctype = typ.ctype;
                // TODO: clean up code and avoid code duplication
                // TODO: handle or panic on structs with more than one va_list?
                let is_va_list = self.ast_context.is_va_list(ctype);
                let mut ty = if is_va_list {
                    let path = vec![
                        mk().path_segment("core"),
                        mk().path_segment("ffi"),
                        mk().path_segment_with_args(
                            "VaListImpl",
                            mk().angle_bracketed_args(vec![mk().lifetime("a")]),
                        ),
                    ];
                    mk().abs_path_ty(path)
                } else {
                    self.convert_type(ctype)?
                };
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

                            // Need to add padding first
                            if (platform_bit_offset / 8) > next_byte_pos {
                                let bytes = (platform_bit_offset / 8) - next_byte_pos;
                                reorganized_fields.push(FieldType::Padding { bytes });
                            }
                        }

                        let mut use_inner_type = false;
                        let mut extra_fields = vec![];
                        if self.ast_context.is_packed_struct_decl(record_id)
                            && self.ast_context.is_aligned_struct_type(ctype)
                        {
                            // If we're embedding an aligned structure inside a packed one,
                            // we need to use the `_Inner` version and add padding
                            let decl_id = self
                                .ast_context
                                .resolve_type(ctype)
                                .kind
                                .as_underlying_decl()
                                .unwrap();

                            let inner_name = self.resolve_decl_inner_name(decl_id);
                            ty = mk().path_ty(vec![inner_name]);

                            use_inner_type = true;

                            // Add the padding field
                            let padding_name = self
                                .type_converter
                                .borrow_mut()
                                .resolve_decl_suffix_name(decl_id, PADDING_SUFFIX)
                                .to_owned();
                            extra_fields.push(FieldType::ComputedPadding {
                                ident: padding_name,
                            })
                        }

                        let field = mk().pub_().struct_field(field_name.clone(), ty);

                        reorganized_fields.push(FieldType::Regular {
                            name: field_name,
                            ctype,
                            field: Box::new(field),
                            use_inner_type,
                            is_va_list,
                        });
                        reorganized_fields.extend(extra_fields.into_iter());

                        next_byte_pos = (platform_bit_offset + platform_type_bitwidth) / 8;

                        continue;
                    }
                    Some(bw) => bw,
                };

                // Ensure we aren't looking at overlapping bits in the same byte
                if (platform_bit_offset / 8) > next_byte_pos {
                    let bytes = (platform_bit_offset / 8) - next_byte_pos;
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

                        let end_bit = platform_bit_offset + bitfield_width;

                        // Add to the total byte size of the bitfield group only if
                        // we have not already encountered this byte
                        for bit in platform_bit_offset..end_bit {
                            let byte = bit / 8;

                            if !encountered_bytes.contains(&byte) {
                                *bytes += 1;
                                encountered_bytes.insert(byte);
                            }
                        }

                        let bit_start = platform_bit_offset - start_bit;
                        let bit_end = bit_start + bitfield_width - 1;
                        let bit_range = format!("{}..={}", bit_start, bit_end);

                        attrs.push((field_name.clone(), ty, bit_range));
                    }
                    Some(_) => unreachable!("Found last bitfield group which is not a group"),
                    None => {
                        let mut bytes = 0;
                        let end_bit = platform_bit_offset + bitfield_width;

                        // Add to the total byte size of the bitfield group only if
                        // we have not already encountered this byte
                        for bit in platform_bit_offset..end_bit {
                            let byte = bit / 8;

                            if !encountered_bytes.contains(&byte) {
                                bytes += 1;
                                encountered_bytes.insert(byte);
                            }
                        }

                        let bit_range = format!("0..={}", bitfield_width - 1);
                        let attrs = vec![(field_name.clone(), ty, bit_range)];

                        last_bitfield_group = Some(FieldType::BitfieldGroup {
                            start_bit: platform_bit_offset,
                            field_name,
                            bytes,
                            attrs,
                        });
                    }
                }

                next_byte_pos = (platform_bit_offset + bitfield_width - 1) / 8 + 1;
            }
        }

        // Find leftover bitfield group at end: it's all set
        if let Some(field_group) = last_bitfield_group.take() {
            reorganized_fields.push(field_group);

            // Packed structs can cause platform_byte_size < next_byte_pos
            if platform_byte_size > next_byte_pos {
                let bytes = platform_byte_size - next_byte_pos;

                // Need to add padding to end if we haven't hit the expected total byte size
                reorganized_fields.push(FieldType::Padding { bytes });
            }
        }

        Ok(reorganized_fields)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
enum FieldType {
    BitfieldGroup {
        start_bit: u64,
        field_name: String,
        bytes: u64,
        attrs: Vec<(String, Box<Type>, String)>,
    }, // 64 bytes
    Padding {
        bytes: u64,
    },
    ComputedPadding {
        ident: String,
    },
    /// [`Field`] is is 528 bytes, so [`Box`] it.
    Regular {
        name: String,
        ctype: CTypeId,
        field: Box<Field>, // would be 528 bytes
        use_inner_type: bool,
        is_va_list: bool,
    }, // would be 562 bytes
}

fn contains_block(expr_kind: &Expr) -> bool {
    use Expr::*;
    match expr_kind {
        Block(..) => true,
        Assign(ExprAssign { left, right, .. }) | Binary(ExprBinary { left, right, .. }) => {
            [left, right].iter().any(|expr| contains_block(expr))
        }
        Unary(ExprUnary { expr, .. }) | Cast(ExprCast { expr, .. }) => contains_block(expr),
        MethodCall(ExprMethodCall { args, .. }) => args.iter().any(contains_block),
        _ => false,
    }
}
