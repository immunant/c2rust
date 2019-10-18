#![deny(missing_docs)]
//! This module provides basic support for converting inline assembly statements.

use super::*;

impl<'c> Translation<'c> {
    /// Convert an inline-assembly statement into one or more Rust statements.
    /// If inline assembly translation is not enabled this will result in an
    /// error message instead of a conversion. Because the inline assembly syntax
    /// used in C is different than the one used in Rust (Rust uses the LLVM syntax
    /// directly) the resulting translated assembly statements will be unlikely to work
    /// without further manual translation. The translator will properly translate
    /// the arguments to the assembly statement, however.
    pub fn convert_asm(
        &self,
        ctx: ExprContext,
        span: Span,
        is_volatile: bool,
        asm: &str,
        inputs: &[AsmOperand],
        outputs: &[AsmOperand],
        clobbers: &[String],
    ) -> Result<Vec<Stmt>, TranslationError> {
        if !self.tcfg.translate_asm {
            return Err(TranslationError::generic(
                "Inline assembly tranlationg not enabled.",
            ));
        }

        self.use_feature("asm");

        fn push_expr(tokens: &mut Vec<TokenTree>, expr: P<Expr>) {
            tokens.push(TokenTree::token(token::Interpolated(Rc::new(Nonterminal::NtExpr(expr))), DUMMY_SP));
        }

        let mut stmts: Vec<Stmt> = vec![];
        let mut post_stmts: Vec<Stmt> = vec![];
        let mut tokens: Vec<TokenTree> = vec![];
        let mut first;

        // Assembly template
        push_expr(&mut tokens, mk().lit_expr(mk().str_lit(asm)));

        let mut tied_operands = HashMap::new();
        for (input_idx, &AsmOperand {
            ref constraints,
            ..
        }) in inputs.iter().enumerate()
        {
            let constraints_digits = constraints.trim_matches(|c: char| !c.is_ascii_digit());
            if let Ok(output_idx) = constraints_digits.parse::<usize>() {
                let output_key = (output_idx, true);
                let input_key = (input_idx, false);
                tied_operands.insert(output_key, input_idx);
                tied_operands.insert(input_key, output_idx);
            }
        }

        // Outputs and Inputs
        let mut operand_renames = HashMap::new();
        for &(list, is_output) in &[(outputs, true), (inputs, false)] {
            first = true;
            tokens.push(TokenTree::token(token::Colon, DUMMY_SP)); // Always emitted, even if list is empty

            for (operand_idx, &AsmOperand {
                ref constraints,
                expression,
            }) in list.iter().enumerate()
            {
                if first {
                    first = false
                } else {
                    tokens.push(TokenTree::token(token::Comma, DUMMY_SP))
                }

                let mut result = self.convert_expr(ctx.used(), expression)?;
                stmts.append(result.stmts_mut());

                let mut result = result.into_value();
                if constraints.contains('*') {
                    // If the constraint string contains `*`, then
                    // c2rust-ast-exporter added it (there's no gcc equivalent);
                    // in this case, we need to do what clang does and pass in
                    // the operand by-address instead of by-value
                    if is_output {
                        result = mk().mutbl().addr_of_expr(result);
                    } else {
                        result = mk().addr_of_expr(result);
                    }
                }

                if let Some(tied_operand) = tied_operands.get(&(operand_idx, is_output)) {
                    // If we have an input operand tied to an output operand,
                    // we need to replicate clang's behavior: the inline assembly
                    // uses the larger type internally, and the smaller value gets
                    // extended to the larger one before the call, and truncated
                    // back after (if needed). For portability, we moved the
                    // type conversions into the `c2rust-asm-casts` crate,
                    // so we call into that one from here.
                    if is_output {
                        // Convert `x` into `let freshN = &mut x; *x`
                        let output_name = self.renamer.borrow_mut().fresh();
                        let output_local = mk().local(
                            mk().ident_pat(&output_name),
                            None as Option<P<Ty>>,
                            Some(mk().mutbl().addr_of_expr(result)),
                        );
                        stmts.push(mk().local_stmt(P(output_local)));

                        // `let mut freshN;`
                        let inner_name = self.renamer.borrow_mut().fresh();
                        let inner_local = mk().local(
                            mk().ident_pat(&inner_name),
                            None as Option<P<Ty>>,
                            None as Option<P<Expr>>,
                        );
                        stmts.push(mk().local_stmt(P(inner_local)));

                        result = mk().ident_expr(&inner_name);
                        operand_renames.insert(operand_idx, (output_name, inner_name));
                    } else {
                        self.extern_crates.borrow_mut().insert("c2rust_asm_casts");

                        // Import the trait at the top level
                        let item_store = &mut self.items.borrow_mut()[&self.main_file];
                        item_store.add_use(vec!["c2rust_asm_casts".into()], "AsmCastTrait");

                        let (output_name, inner_name) = operand_renames
                            .get(&tied_operand)
                            .unwrap();

                        let input_name = self.renamer.borrow_mut().fresh();
                        let input_local = mk().local(
                            mk().ident_pat(&input_name),
                            None as Option<P<Ty>>,
                            Some(result),
                        );
                        stmts.push(mk().local_stmt(P(input_local)));

                        // Replace `result` with
                        // `c2rust_asm_casts::AsmCast::cast_in(output, input)`
                        let path_expr = mk().path_expr(
                            vec!["c2rust_asm_casts", "AsmCast", "cast_in"]
                        );
                        let output = mk().ident_expr(output_name);
                        let input = mk().ident_expr(input_name);
                        result = mk().call_expr(path_expr, vec![output.clone(), input.clone()]);

                        // Append the cast-out call after the assembly macro:
                        // `c2rust_asm_casts::AsmCast::cast_out(output, input, inner);`
                        let path_expr = mk().path_expr(
                            vec!["c2rust_asm_casts", "AsmCast", "cast_out"]);
                        let inner = mk().ident_expr(inner_name);
                        let cast_out = mk().call_expr(path_expr, vec![output, input, inner]);
                        post_stmts.push(mk().semi_stmt(cast_out));
                    }
                }

                push_expr(&mut tokens, mk().lit_expr(mk().str_lit(constraints)));
                push_expr(&mut tokens, mk().paren_expr(result));
            }
        }

        // Clobbers
        first = true;
        tokens.push(TokenTree::token(token::Colon, DUMMY_SP));
        for clobber in clobbers {
            if first {
                first = false
            } else {
                tokens.push(TokenTree::token(token::Comma, DUMMY_SP))
            }
            push_expr(&mut tokens, mk().lit_expr(mk().str_lit(clobber)));
        }

        // Options
        if is_volatile {
            tokens.push(TokenTree::token(token::Colon, DUMMY_SP));
            push_expr(&mut tokens, mk().lit_expr(mk().str_lit("volatile")));
        }

        let mac = mk().mac(
            vec!["asm"],
            tokens.into_iter().collect::<TokenStream>(),
            MacDelimiter::Parenthesis,
        );
        let mac = mk().mac_expr(mac);
        let mac = mk().span(span).expr_stmt(mac);
        stmts.push(mac);

        // Push the post-macro statements
        stmts.extend(post_stmts.into_iter());

        Ok(stmts)
    }
}
