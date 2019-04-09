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
    pub fn convert_asm
        (&self,
         ctx: ExprContext,
         span: Span,
         is_volatile: bool,
         asm: &str,
         inputs: &[AsmOperand],
         outputs: &[AsmOperand],
         clobbers: &[String])
        -> Result<Vec<Stmt>, TranslationError> {

        if !self.tcfg.translate_asm {
            return Err(TranslationError::generic("Inline assembly not enabled, to enable use --translate-asm"))
        }

        self.use_feature("asm");

        fn push_expr(tokens: &mut Vec<Token>, expr: P<Expr>) {
            tokens.push(Token::Interpolated(Lrc::new(Nonterminal::NtExpr(expr))));
        }

        let mut stmts: Vec<Stmt> = vec![];
        let mut tokens: Vec<Token> = vec![];
        let mut first;

        // Assembly template
        push_expr(&mut tokens, mk().lit_expr(mk().str_lit(asm)));

        // Outputs and Inputs
        for list in vec![outputs, inputs] {

            first = true;
            tokens.push(Token::Colon); // Always emitted, even if list is empty

            for &AsmOperand { ref constraints, expression } in list {
                if first { first = false } else { tokens.push(Token::Comma) }

                let mut result = self.convert_expr(ctx.used(), expression)?;
                stmts.append(&mut result.stmts);

                push_expr(&mut tokens, mk().lit_expr(mk().str_lit(constraints)));
                push_expr(&mut tokens, mk().paren_expr(result.val));
            }
        }

        // Clobbers
        first = true;
        tokens.push(Token::Colon);
        for clobber in clobbers {
            if first { first = false } else { tokens.push(Token::Comma) }
            push_expr(&mut tokens, mk().lit_expr(mk().str_lit(clobber)));
        }

        // Options
        if is_volatile {
            tokens.push(Token::Colon);
            push_expr(&mut tokens, mk().lit_expr(mk().str_lit("volatile")));
        }

        let mac = mk().mac(vec!["asm"], tokens.into_iter().collect::<TokenStream>(), MacDelimiter::Parenthesis);
        let mac = mk().mac_expr(mac);
        let mac = mk().span(span).expr_stmt(mac);
        stmts.push(mac);

        Ok(stmts)
    }
}
