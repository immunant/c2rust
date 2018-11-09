#![deny(missing_docs)]
//! This module provides translation for bitfield structs and operations on them. Generated code
//! requires the use of the bitfield crate.

// TODO: See if union bitfields are supportable or not
// FIXME: size_of won't call the bitfield struct with required generic params, same
// for variable bindings: `let foo: Foo` should be `let foo: Foo<[u8; X]>`

use c_ast::{CExprId, CQualTypeId};
use c2rust_ast_builder::mk;
use syntax::ast::{AttrStyle, Expr, MacDelimiter, MetaItemKind, Ty, LitIntType};
use syntax::ext::quote::rt::Span;
use syntax::parse::token::{Nonterminal, Token};
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;
use translator::{DecayRef, ExprUse, Translation, ConvertedDecl, simple_metaitem};
use with_stmts::WithStmts;

use super::DUMMY_SP; // TODO: Remove

/// (name, type, bitfield_width)
type FieldNameTypeWidth = (String, P<Ty>, Option<u64>);
type FieldNameType = (String, CQualTypeId);

impl Translation {
    /// Here we output a macro invocation to generate bitfield data that looks like this:
    /// bitfield! {
    ///     pub struct Foo([u8]);
    ///     int_size, field, set_field: end_idx, start_idx;
    ///     int_size2, field2, set_field2: end_idx2, start_idx2;
    ///     ...
    /// }
    pub fn convert_bitfield_struct_decl(&self, name: String, span: Span, field_info: Vec<FieldNameTypeWidth>, is_packed: bool, manual_alignment: Option<u64>) -> ConvertedDecl {
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

        for (name, ty, bitfield_width) in field_info {
            // bitfield_width.unwrap(); // FIXME: Need to handle non bitfield case
            let ty_item = Nonterminal::NtTy(ty);
            let setter_ident = Nonterminal::NtIdent(mk().ident(format!("set_{}", name)), false);
            let getter_ident = Nonterminal::NtIdent(mk().ident(name), false);
            let start_idx = Nonterminal::NtLiteral(mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed))); // FIXME
            let end_idx = Nonterminal::NtLiteral(mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed))); // FIXME

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

        ConvertedDecl::Item(mk().span(span).mac_item(mac))
    }

    // REVIEW: Can bitfields be packed? Might need to take that into account
    /// Determine the (platform specific!) byte size of the bitfield struct
    fn determine_struct_total_byte_size(&self, field_info: &[FieldNameTypeWidth]) -> u64 {
        unimplemented!()
    }

    /// TODO
    pub fn convert_bitfield_struct_literal(&self, name: String, field_ids: &[CExprId], field_info: Vec<FieldNameType>, platform_byte_size: u64, is_static: bool) -> Result<WithStmts<P<Expr>>, String> {
        // REVIEW: statics? Likely need to const_transmute byte array?
        // TODO: Derive Copy, Clone?
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
        for (i, &field_id) in field_ids.iter().enumerate() {
            let &(ref field_name, _) = &field_info[i];
            let mut x = self.convert_expr(ExprUse::Used, field_id, is_static, DecayRef::Default)?;

            let setter_call_name = format!("set_{}", field_name);
            let call_params = vec![
                x.val
            ];
            let method_call = mk().method_call_expr(struct_ident.clone(), setter_call_name, call_params);

            stmts.push(mk().expr_stmt(method_call));
        }

        // REVIEW: ommitted record fields? Might be fine to leave them as they will be zero'd?
        // Pad out remaining omitted record fields
        // for i in ids.len()..fields.len() {
        //     let &(ref field_name, ty) = &field_decls[i];
        //     fields.push(mk().field(field_name, self.implicit_default_expr(ty.ctype, is_static)?));
        // }

        stmts.push(mk().expr_stmt(struct_ident));

        let val = mk().block_expr(mk().block(stmts));
        let stmts = vec![];

        Ok(WithStmts {
            stmts,
            val,
        })
    }
}
