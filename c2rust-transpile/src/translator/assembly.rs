#![deny(missing_docs)]
//! This module provides basic support for converting inline assembly statements.

use super::*;
use proc_macro2::{TokenStream, TokenTree};
use syn::__private::ToTokens;

enum RegHandling {
    In,
    Out,
    InOut,
    LateOut,
    InLateOut,
}

impl ToString for RegHandling {
    fn to_string(&self) -> String {
        use RegHandling::*;
        match self {
            In => "in",
            Out => "out",
            InOut => "inout",
            LateOut => "lateout",
            InLateOut => "inlateout",
        }.to_owned()
    }
}

fn parse_constraints(mut constraints: &str) -> Option<(RegHandling, bool, String)> {
    use RegHandling::*;
    let mut is_output = match constraints.chars().next() {
        Some('+') => true,
        Some('=') => false,
        _ => {
            let mem_only = if constraints.starts_with('*') {
                constraints = &constraints[1..];
                true
            } else {
                false
            };
            return Some((In, mem_only, constraints.replace('{', "\"").replace('}', "\"")))
        },
    };
    // Skip +/=
    constraints = &constraints[1..];

    let early_clobber = if constraints.starts_with('&') {
        constraints = &constraints[1..];
        true
    } else {
        false
    };

    let mem_only = if constraints.starts_with('*') {
        constraints = &constraints[1..];
        true
    } else {
        false
    };

    let mut split = constraints.splitn(2, ',');
    constraints = match split.next() {
        Some(c) => c,
        // Parse error
        _ => return None,
    };
    // If a comma is present, this is an output of form =[&]foo,N
    if split.next().is_some() {
        if !is_output {
            is_output = true;
        } else {
            // '+' followed by ',' is a parse error
            return None;
        }
    }

    let mode = if is_output {
        if early_clobber {InOut} else {InLateOut}
    } else {
        if early_clobber {Out} else {LateOut}
    };

    Some((mode, mem_only, constraints.replace('{', "\"").replace('}', "\"")))
}

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
                "Inline assembly translation not enabled.",
            ));
        }

        self.use_feature("asm");

        fn push_expr(tokens: &mut Vec<TokenTree>, expr: Box<Expr>) {
            tokens.extend(expr.to_token_stream());
        }

        let mut stmts: Vec<Stmt> = vec![];
        let mut post_stmts: Vec<Stmt> = vec![];
        let mut tokens: Vec<TokenTree> = vec![];

        // Assembly template
        push_expr(&mut tokens, mk().lit_expr(asm));

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
            for (operand_idx, &AsmOperand {
                ref constraints,
                expression,
            }) in list.iter().enumerate()
            {
                tokens.push(TokenTree::Punct(Punct::new(',', Alone)));

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
                            None as Option<Box<Type>>,
                            Some(mk().mutbl().addr_of_expr(result)),
                        );
                        stmts.push(mk().local_stmt(Box::new(output_local)));

                        // `let mut freshN;`
                        let inner_name = self.renamer.borrow_mut().fresh();
                        let inner_local = mk().local(
                            mk().ident_pat(&inner_name),
                            None as Option<Box<Type>>,
                            None as Option<Box<Expr>>,
                        );
                        stmts.push(mk().local_stmt(Box::new(inner_local)));

                        result = mk().ident_expr(&inner_name);
                        operand_renames.insert(operand_idx, (output_name, inner_name));
                    } else {
                        self.use_crate(ExternCrate::C2RustAsmCasts);

                        // Import the trait into scope
                        self.with_cur_file_item_store(|item_store| {
                            item_store.add_use(vec!["c2rust_asm_casts".into()], "AsmCastTrait");
                        });

                        let (output_name, inner_name) = operand_renames
                            .get(&tied_operand)
                            .unwrap();

                        let input_name = self.renamer.borrow_mut().fresh();
                        let input_local = mk().local(
                            mk().ident_pat(&input_name),
                            None as Option<Box<Type>>,
                            Some(result),
                        );
                        stmts.push(mk().local_stmt(Box::new(input_local)));

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

                let (reg_handling, mem_only, parsed_constraints) = match parse_constraints(constraints) {
                    Some(x) => x,
                    None => return Err(TranslationError::new(None, failure::err_msg(
                        "Inline assembly constraints could not be parsed: ".to_owned() + constraints,
                    ).context(TranslationErrorKind::Generic)))
                };
                push_expr(&mut tokens, mk().ident_expr(reg_handling.to_string()));
                push_expr(&mut tokens, mk().paren_expr(if parsed_constraints.contains('"') {
                    mk().lit_expr(parsed_constraints.trim_matches('"'))
                } else {
                    mk().ident_expr(parsed_constraints)
                }));
                tokens.push(TokenTree::Punct(Punct::new(' ', Alone)));
                push_expr(&mut tokens, result);
            }
        }

        // Clobbers
        for clobber in clobbers {
            tokens.push(TokenTree::Punct(Punct::new(',', Alone)));
            let result = mk().call_expr(mk().ident_expr("out"), vec![mk().lit_expr(clobber)]);
            push_expr(&mut tokens, result);
            tokens.push(TokenTree::Punct(Punct::new(' ', Alone)));
            push_expr(&mut tokens, mk().ident_expr("_"));
        }

        // Options
        /*if !is_volatile {
            tokens.push(TokenTree::Punct(Punct::new(',', Alone)));
            let result = mk().call_expr(mk().ident_expr("options"), vec![mk().ident_expr("pure")]);
            push_expr(&mut tokens, result);
        }*/

        self.with_cur_file_item_store(|item_store| {
            let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" }.to_string();
            item_store.add_use(vec![std_or_core, "arch".into()], "asm");
        });

        let mac = mk().mac(
            vec!["asm"],
            tokens.into_iter().collect::<TokenStream>(),
            MacroDelimiter::Paren(Default::default()),
        );
        let mac = mk().mac_expr(mac);
        let mac = mk().span(span).semi_stmt(mac);
        stmts.push(mac);

        // Push the post-macro statements
        stmts.extend(post_stmts.into_iter());

        Ok(stmts)
    }
}
