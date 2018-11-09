#![deny(missing_docs)]
//! This module provides translation for bitfield structs and operations on them. Generated code
//! requires the use of the bitfield crate.

// TODO: See if union bitfields are supportable or not

use c_ast::{CExprId, CQualTypeId};
use c2rust_ast_builder::mk;
use syntax::ast::{Expr, MacDelimiter, Ty, LitIntType};
use syntax::ext::quote::rt::Span;
use syntax::parse::token::{Nonterminal, Token};
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;
use translator::{Translation, ConvertedDecl};
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
         // FIXME: spans

        let u8_slice = mk().span(DUMMY_SP).struct_field("", mk().slice_ty(mk().ident_ty("u8")));
        let struct_item = Nonterminal::NtItem(mk().pub_().tuple_struct_item(name, vec![u8_slice]));

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
    pub fn convert_bitfield_struct_literal(&self, name: String, field_ids: &[CExprId], field_info: Vec<FieldNameType>) -> Result<WithStmts<P<Expr>>, String> {
        // REVIEW: drop explicit ty (since it's not necessary) instead of infer_ty?
        // REVIEW: statics? Likely need to const_transmute byte array?
        let tmp_name = format!("{}_bitfield", name);
        let lit_zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
        let lit_bitwidth = mk().lit_expr(mk().int_lit(3, LitIntType::Unsuffixed));
        let zero_init = vec![mk().repeat_expr(lit_zero, lit_bitwidth)];
        let mut stmts = Vec::with_capacity(field_info.len() + 2);
        stmts.push(
            mk().item_stmt(
                mk().pub_().const_item(tmp_name.clone(), mk().infer_ty(),
                    mk().call_expr(mk().ident_expr(name), zero_init)))
        );
        for (name, ty) in field_info {
            let setter_call_name = format!("set_{}", name);
            let tmp_name_ident = mk().ident_expr(tmp_name.clone());
            let call_params = Vec::new() as Vec<P<Expr>>;
            stmts.push(mk().expr_stmt(mk().method_call_expr(tmp_name_ident, setter_call_name, call_params)))
        }
        stmts.push(mk().expr_stmt(mk().ident_expr(tmp_name)));
        let val = mk().block_expr(mk().block(stmts));
        let stmts = vec![];
        Ok(WithStmts {
            stmts,
            val,
        })
    }
}
