#![deny(missing_docs)]
//! This module provides translation for bitfield structs and operations on them. Generated code
//! requires the use of the bitfield crate.

// TODO: See if union bitfields are supportable or not
// FIXME: size_of won't call the bitfield struct with required generic params, same
// for variable bindings: `let foo: Foo` should be `let foo: Foo<[u8; X]>`
// TODO: We can generate static bitfields in a slightly less nice, but safe, way:
// static mut bitfield: BitField<[u8; X]> = BitField([A, B, C, D]); Either we figure
// out what bits to assign here, or else we reuse the local var version but ensure that
// static bitfields get sectioned off

use c_ast::{CExprId, CQualTypeId};
use c2rust_ast_builder::mk;
use syntax::ast::{AttrStyle, Expr, MacDelimiter, MetaItemKind, NestedMetaItem, NestedMetaItemKind, Lit, LitKind, StrStyle, Ty, LitIntType};
use syntax::ext::quote::rt::Span;
use syntax::parse::token::{Nonterminal, Token};
use syntax::ptr::P;
use syntax::source_map::symbol::Symbol;
use syntax::tokenstream::TokenTree;
use syntax_pos::DUMMY_SP;
use translator::{DecayRef, ExprUse, Translation, ConvertedDecl, simple_metaitem};
use with_stmts::WithStmts;

/// (name, bitfield_width, platform_bit_offset, platform_type_bitwidth)
type NameWidthOffset = (String, Option<u64>, u64, u64);
/// (name, type, bitfield_width, platform_type_bitwidth)
type NameTypeWidth = (String, CQualTypeId, Option<u64>, u64);

const PACKED_STRUCT_CRATE: bool = true;

fn assigment_metaitem(lhs: &str, rhs: &str) -> NestedMetaItem {
    let meta_item = mk().meta_item(
        vec![lhs],
        MetaItemKind::NameValue(Lit {
            span: DUMMY_SP,
            node: LitKind::Str(Symbol::intern(rhs), StrStyle::Cooked)
        }),
    );

    mk().nested_meta_item(NestedMetaItemKind::MetaItem(meta_item))
}

/// Figure out what's the smallest byte int that can hold this bitfield width
/// This is required by packed_struct as you cannot currently use arbitrary sized
/// ints for fields like C lets you do..
fn get_ty_from_bit_width(bit_width: u64) -> Result<P<Ty>, &'static str> {
    // REVIEW: What about signed int fields?
    match bit_width {
        0..=8 => Ok(mk().ident_ty("u8")),
        9..=16 => Ok(mk().ident_ty("u16")),
        17..=32 => Ok(mk().ident_ty("u32")),
        33..=64 => Ok(mk().ident_ty("u64")),
        65..=128 => Ok(mk().ident_ty("u128")),
        _ => Err("Unsupported bitfield width found greater than 128 bits"),
    }
}

impl Translation {
    /// Here we output a macro invocation to generate bitfield data that looks like this:
    ///
    /// ```no_run
    /// bitfield! {
    ///     #[repr(C)]
    ///     #[derive(Copy, Clone)]
    ///     pub struct Foo([u8]);
    ///     pub int_size, field, set_field: end_idx, start_idx;
    ///     pub int_size2, field2, set_field2: end_idx2, start_idx2;
    ///     // ...
    /// }
    /// ```
    /// FIXME
    pub fn convert_bitfield_struct_decl(
        &self,
        name: String,
        platform_byte_size: u64,
        span: Span,
        field_info: Vec<NameWidthOffset>,
    ) -> Result<ConvertedDecl, String> {
        if PACKED_STRUCT_CRATE {
            let mut item_store = self.item_store.borrow_mut();

            item_store.uses
                .get_mut(vec!["packed_struct".into(), "prelude".into()])
                .insert("Integer");
            item_store.uses
                .get_mut(vec!["packed_struct_codegen".into()])
                .insert("PackedStruct");
            // TODO: Trait needed for pack() calls:
            // item_store.uses
            //     .get_mut(vec!["packed_struct".into()])
            //     .insert("PackedStruct");

            let mut field_entries = Vec::with_capacity(field_info.len());

            for (field_name, bitfield_width, bit_index, platform_ty_bitwidth) in field_info {
                // Bitfield widths of 0 should just be markers for clang,
                // we shouldn't need to explicitly handle it ourselves
                if let Some(0) = bitfield_width {
                    continue
                }

                let bit_width = bitfield_width.unwrap_or(platform_ty_bitwidth);
                let base_ty = get_ty_from_bit_width(bit_width)?;
                let bit_struct_name = format!("Bits{}", bit_width);
                // let field_generic_tys = mk().angle_bracketed_args(vec![base_ty, mk().ident_ty(bit_struct_name.clone())]);
                // let field_ty = mk().path_segment_with_args("Integer", field_generic_tys);
                let bit_range = format!("{}..={}", bit_index, bit_index + bit_width - 1);
                let field_attr_items = vec![assigment_metaitem("bits", &bit_range)];
                let field_attr = mk().meta_item("packed_field", MetaItemKind::List(field_attr_items));
                let field = mk()
                    .meta_item_attr(AttrStyle::Outer, field_attr)
                    // .struct_field(field_name, mk().path_ty(vec![field_ty]));
                    .struct_field(field_name, base_ty);

                item_store.uses
                    .get_mut(vec!["packed_struct".into(), "prelude".into(), "packed_bits".into()])
                    .insert(bit_struct_name);

                field_entries.push(field);
            }

            #[cfg(target_endian = "little")]
            let (bit_endian, endian) = ("lsb0", "msb");

            // Big endian untested
            #[cfg(target_endian = "big")]
            let (bit_endian, endian) = ("msb0", "lsb");

            let packed_struct_attr_items = vec![
                assigment_metaitem("bit_numbering", bit_endian),
                assigment_metaitem("endian", endian),
                assigment_metaitem("size_bytes", &platform_byte_size.to_string()),
            ];
            let packed_struct_attr = mk().meta_item("packed_struct", MetaItemKind::List(packed_struct_attr_items));

            let item = mk()
                .span(span)
                .pub_()
                .call_attr("derive", vec!["Copy", "Clone", "PackedStruct"])
                .meta_item_attr(AttrStyle::Outer, packed_struct_attr)
                .struct_item(name, field_entries);

            return Ok(ConvertedDecl::Item(item));
        }

        let u8_slice = mk().span(DUMMY_SP).struct_field("", mk().slice_ty(mk().ident_ty("u8")));
        let reprs = vec![simple_metaitem("C")];
        let repr_meta_item = mk().meta_item("repr", MetaItemKind::List(reprs));
        let repr_attr = mk().meta_item_attr(AttrStyle::Outer, repr_meta_item);
        let derives = vec![simple_metaitem("Copy"), simple_metaitem("Clone")];
        let derive_meta_item = mk().meta_item("derive", MetaItemKind::List(derives));
        let attrs = repr_attr.meta_item_attr(AttrStyle::Outer, derive_meta_item);
        let struct_item = Nonterminal::NtItem(attrs.pub_().tuple_struct_item(name, vec![u8_slice]));

        let mut macro_body = Vec::with_capacity(1 + field_info.len() * 10);

        macro_body.push(TokenTree::Token(DUMMY_SP, Token::interpolated(struct_item)));

        for (name, bitfield_width, bit_index, platform_ty_bitwidth) in field_info {
            let start = bit_index as u128;
            let end = start + bitfield_width.unwrap_or(platform_ty_bitwidth) as u128 - 1;
            let ty_item = Nonterminal::NtTy(mk().never_ty());
            let setter_ident = Nonterminal::NtIdent(mk().ident(format!("set_{}", name)), false);
            let getter_ident = Nonterminal::NtIdent(mk().ident(name), false);
            let start_idx = Nonterminal::NtLiteral(mk().lit_expr(mk().int_lit(start, LitIntType::Unsuffixed)));
            let end_idx = Nonterminal::NtLiteral(mk().lit_expr(mk().int_lit(end, LitIntType::Unsuffixed)));

            macro_body.extend_from_slice(&[
                TokenTree::Token(DUMMY_SP, Token::interpolated(ty_item)),
                TokenTree::Token(DUMMY_SP, Token::Comma),
                TokenTree::Token(DUMMY_SP, Token::interpolated(getter_ident)),
                TokenTree::Token(DUMMY_SP, Token::Comma),
                TokenTree::Token(DUMMY_SP, Token::interpolated(setter_ident)),
                TokenTree::Token(DUMMY_SP, Token::Colon),
                TokenTree::Token(DUMMY_SP, Token::interpolated(end_idx)),
                TokenTree::Token(DUMMY_SP, Token::Comma),
                TokenTree::Token(DUMMY_SP, Token::interpolated(start_idx)),
                TokenTree::Token(DUMMY_SP, Token::Semi),
            ]);
        }

        let mac = mk().mac(mk().path("bitfield"), macro_body, MacDelimiter::Brace);

        Ok(ConvertedDecl::Item(mk().span(span).mac_item(mac)))
    }

    /// Here we output a block to generate a struct literal initializer in.
    /// It looks like this in locals:
    ///
    /// FIXME
    ///
    /// ```no_run
    /// {
    ///     let mut foo = Foo([0; PLATFORM_BYTE_SIZE]);
    ///     foo.set_field(123);
    ///     foo.set_field2(456);
    ///     // ...
    ///     foo
    /// }
    /// ```
    ///
    /// and in statics:
    /// TODO
    pub fn convert_bitfield_struct_literal(&self, name: String, field_ids: &[CExprId], field_info: Vec<NameTypeWidth>, platform_byte_size: u64, is_static: bool) -> Result<WithStmts<P<Expr>>, String> {
        // REVIEW: statics?
        if PACKED_STRUCT_CRATE {
            let mut stmts = Vec::with_capacity(field_ids.len());
            let mut fields = Vec::with_capacity(field_ids.len());

            // Specified record fields
            for (&field_id, (field_name, _, bitfield_width, platform_type_bitwidth)) in field_ids.iter().zip(field_info.iter()) {
                // Bitfield widths of 0 should just be markers for clang,
                // we shouldn't need to explicitly handle it ourselves
                if let Some(0) = bitfield_width {
                    continue;
                }

                let bitwidth = bitfield_width.unwrap_or(*platform_type_bitwidth);
                // With some type analysis, we could skip this cast sometimes
                let cast_ty = get_ty_from_bit_width(bitwidth)?;

                let mut expr = self.convert_expr(ExprUse::Used, field_id, is_static, DecayRef::Default)?;

                stmts.append(&mut expr.stmts);

                let cast = mk().cast_expr(expr.val, cast_ty);
                // let field = mk().method_call_expr(cast, "into", Vec::new() as Vec<P<Expr>>);

                fields.push(mk().field(field_name, cast));
            }

            // Pad out remaining omitted record fields
            for (field_name, ty, _, _) in field_info[field_ids.len()..].iter() {
                fields.push(mk().field(field_name, self.implicit_default_expr(ty.ctype, is_static)?));
            }

            return Ok(WithStmts {
                stmts,
                val: mk().struct_expr(vec![mk().path_segment(name)], fields),
            })
        }

        // REVIEW: statics? Likely need to const_transmute byte array?
        let variable_name = format!("{}_bf", name.clone());
        let struct_ident = mk().ident_expr(variable_name.clone());
        let lit_zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
        let lit_bitwidth = mk().lit_expr(mk().int_lit(platform_byte_size as u128, LitIntType::Unsuffixed));
        let zero_init = vec![mk().repeat_expr(lit_zero, lit_bitwidth)];
        let tuple_struct_init = mk().call_expr(mk().ident_expr(name), zero_init);
        let local_pat = mk().mutbl().ident_pat(variable_name);
        let local_variable = P(mk().local(local_pat, None as Option<P<Ty>>, Some(tuple_struct_init)));

        let mut stmts = Vec::with_capacity(field_info.len() + 2);

        stmts.push(mk().local_stmt(local_variable));

        // Specified record fields
        for (&field_id, (field_name, _, _, _)) in field_ids.iter().zip(field_info) {
            let expr = self.convert_expr(ExprUse::Used, field_id, is_static, DecayRef::Default)?;
            let setter_call_name = format!("set_{}", field_name);
            let call_params = vec![expr.val];
            let method_call = mk().method_call_expr(struct_ident.clone(), setter_call_name, call_params);

            stmts.push(mk().expr_stmt(method_call));
        }

        stmts.push(mk().expr_stmt(struct_ident));

        let val = mk().block_expr(mk().block(stmts));

        Ok(WithStmts {
            stmts: Vec::new(),
            val,
        })
    }
}
