#![deny(missing_docs)]
//! This module provides basic support for converting inline assembly statements.

use super::*;
use proc_macro2::{TokenStream, TokenTree};
use syn::__private::ToTokens;

/// An argument direction specifier for a Rust asm! expression
enum ArgDirSpec {
    In,
    Out,
    InOut,
    LateOut,
    InLateOut,
}

impl ToString for ArgDirSpec {
    fn to_string(&self) -> String {
        use ArgDirSpec::*;
        match self {
            In => "in",
            Out => "out",
            InOut => "inout",
            LateOut => "lateout",
            InLateOut => "inlateout",
        }.to_owned()
    }
}

impl ArgDirSpec {
    fn with_in(&self) -> Self {
        use ArgDirSpec::*;
        match self {
            In => In,
            Out => InOut,
            InOut => InOut,
            LateOut => InLateOut,
            InLateOut => InLateOut,
        }
    }
}

fn parse_constraints(mut constraints: &str) ->
    Result<(ArgDirSpec, bool, String), TranslationError> {
    let parse_error = |constraints| {
        Err(TranslationError::new(None, failure::err_msg(
            "Inline assembly constraints could not be parsed: ".to_owned() + constraints,
        ).context(TranslationErrorKind::Generic)))
    };
    use ArgDirSpec::*;
    let mut is_input = match constraints.chars().next() {
        Some('+') => {
            constraints = &constraints[1..];
            true
        },
        Some('=') => {
            constraints = &constraints[1..];
            false
        },
        _ => true,
    };

    let early_clobber = if constraints.starts_with('&') {
        constraints = &constraints[1..];
        true
    } else {
        false
    };

    let mut mem_only = if constraints.starts_with('*') {
        constraints = &constraints[1..];
        true
    } else {
        false
    };

    let mut split = constraints.splitn(2, ',');
    constraints = match split.next() {
        Some(c) => c,
        // Parse error
        _ => return parse_error(constraints),
    };
    // If a comma is present, this is an output of form =[&]foo,N
    if split.next().is_some() {
        if !is_input {
            is_input = true;
        } else {
            // '+' followed by ',' is a parse error
            return parse_error(constraints);
        }
    }

    // Handle register names
    let mut constraints = constraints.replace('{', "\"").replace('}', "\"");

    match &*constraints {
        "m" => {
            mem_only = true;
            constraints = "reg".into();
        },
        "r" => {
            constraints = "reg".into();
        }
        _ => {},
    };

    let mode = if mem_only {
        In
    } else {
        if is_input {
            if early_clobber {InOut} else {InLateOut}
        } else {
            if early_clobber {Out} else {LateOut}
        }
    };

    Ok((mode, mem_only, constraints))
}

fn translate_modifier(modifier: char, arch: &str) -> Option<char> {
    Some(match arch {
        "x86" => match modifier {
            'k' => 'e',
            'q' => 'r',
            'b' => 'l',
            'h' => 'h',
            'w' => 'x',
            _ => return None,
        },
        "aarch64" => modifier,
        "arm" => match modifier {
            'p'|'q' => return None,
            _ => modifier,
        },
        "riscv" => modifier,
        _ => return None,
    })
}

/// Rust-native asm! operands, which may be inputs, outputs, or both.
struct BidirAsmOperand {
    dir_spec: ArgDirSpec,
    mem_only: bool,
    constraints: String,
    name: Option<String>,
    // At least one of these is non-None
    in_expr: Option<(usize, CExprId)>,
    out_expr: Option<(usize, CExprId)>,
}

impl BidirAsmOperand {
    /// Return whether an operand is positional (as opposed to named or using an explicit register)
    fn is_positional(&self) -> bool {
        !self.constraints.contains('"') && self.name.is_none()
    }
}

/// Return the register and corresponding template modifiers if the constraint
/// uses a reserved register. Assumes x86_64.
fn reg_is_reserved(constraint: &str) -> Option<(&str, &str)> {
    Some(match constraint {
        "\"bl\"" | "\"bh\"" | "\"bx\"" | "\"ebx\"" | "\"rbx\"" => {
            let reg = constraint.trim_matches('"');
            let mods = if reg.len() == 2 {
                &reg[1..] // l/h/x
            } else {
                &reg[..1] // e/r
            };
            (reg, mods)
        },
        _ => return None,
    })
}

/// Emit mov instructions and modify inline assembly operands to copy in and/or
/// out when an operand uses a reserved register. Instead of constraining the
/// operand to the reserved register, constrain it to any register and then copy
/// between the reserved register and the (suitably modified, e.g. `{0:x}`)
/// operand.
/// This also requires reordering the operands because we convert them to
/// named operands, which must precede explicit register operands.
///
/// Modifies operands and returns a pair of prefix and suffix strings that
/// should be appended to the assembly template.
fn rewrite_reserved_reg_operands(att_syntax: bool, operands: &mut Vec<BidirAsmOperand>) -> (String, String) {
    let (mut prolog, mut epilog) = (String::new(), String::new());

    let mut rewrite_idxs = vec![];
    let mut total_positional = 0;

    // Determine which operands must be rewritten and how many
    // positional operands there are. Positional operands must precede named
    // operands, so this tells us where to reinsert operands we rewrite.
    for (i, operand) in operands.iter().enumerate() {
        if operand.is_positional() {
            total_positional += 1;
        } else if let Some((reg, mods)) = reg_is_reserved(&*operand.constraints) {
            rewrite_idxs.push((i, reg.to_owned(), mods.to_owned()));
        }
    }

    let mut n_moved = 0;
    for (idx, reg, mods) in rewrite_idxs {
        let operand = &mut operands[idx];
        let name = format!("restmp{}", n_moved);
        if let Some((_idx, in_expr)) = operand.in_expr {
            let move_input = if att_syntax {
                format!("mov %{}, {{{}:{}}}\n", reg, name, mods)
            } else {
                format!("mov {{{}:{}}}\n, {}", name, mods, reg)
            };
            prolog.push_str(&move_input);
        }
        if let Some((_idx, out_expr)) = operand.out_expr {
            let move_output = if att_syntax {
                format!("\nmov {{{}:{}}}, %{}", name, mods, reg)
            } else {
                format!("\nmov {}, {{{}:{}}}", reg, name, mods)
            };
            epilog.push_str(&move_output);
        }
        operand.constraints = "reg".into();
        operand.name = Some(name);

        // Move operand to after all positional arguments. This does not
        // interfere with moving subsequent operands that use reserved registers
        // because explicit register operands must all come after positional
        // and named operands.
        //let (positional, named, explicit) = split_operands(operands);
        let nth_non_positional = total_positional + n_moved;
        operands.swap(idx, nth_non_positional);
        n_moved += 1;
    }

    (prolog, epilog)
}

fn asm_is_att_syntax(asm: &str) -> bool {
    // For GCC, AT&T syntax is default... unless -masm=intel is passed. This
    // means we can hope but not guarantee that x86 asm with no syntax directive
    // uses AT&T syntax.
    // To handle other cases, try to heuristically detect the variant we get
    // (assuming it's actually x86 asm in the first place...).
    // As the rust x86 default is intel syntax, we need to emit the "att_syntax"
    // option if we get a hint that this asm uses AT&T syntax.

    let intel_directive = asm.find(".intel_syntax");
    let att_directive = asm.find(".att_syntax");
    if let (Some(intel_pos), Some(att_pos)) = (intel_directive, att_directive) {
        // Both directives are present; presumably this asm switches to one at
        // its start and restores the default at the end. Whichever comes first
        // should be what the asm uses.
        att_pos < intel_pos
    } else if intel_directive.is_some() {
        false
    } else if att_directive.is_some() {
        true
    } else if asm.contains("word ptr") {
        false
    } else {
        asm.contains('$')
    }
}

/// References of the form $0 need to be converted to {0}, and references
/// that are mem-only need to be converted to [{0}].
fn rewrite_asm<F: Fn(&str) -> bool>(asm: &str, is_mem_only: F) -> String {
    let mut out = String::with_capacity(asm.len());

    let mut first = true;
    let mut last_empty = false;

    // Iterate over $-prefixed chunks
    for chunk in asm.split('$') {
        // No modification needed for first chunk
        if first {
            first = false;
            out.push_str(chunk);
            continue
        }

        // Pass-through $$ as one $
        if last_empty {
            last_empty = false;
            out.push_str("$");
            out.push_str(chunk);
            continue;
        }

        // Do not re-wrap ${...}, but do translate modifiers
        if chunk.starts_with('{') {
            // Translate operand modifiers ("template modifiers" per Rust)
            if let Some(end_idx) = chunk.find('}') {
                let ref_str = &chunk[..end_idx];
                if let Some(colon_idx) = ref_str.find(':') {
                    let (before_mods, modifiers) = ref_str.split_at(colon_idx + 1);
                    out.push_str(before_mods);

                    let modifiers = ref_str[colon_idx + 1..].chars();
                    for modifier in modifiers {
                        if let Some(new) = translate_modifier(modifier, "x86") {
                            out.push(new);
                        }
                    }
                    out.push_str(&chunk[end_idx..]);
                }
            } else {
                out.push_str(chunk);
            }
            continue
        }

        if chunk == "" {
            last_empty = true;
            continue;
        }

        let ref_str = chunk.trim_matches(|c: char| !c.is_ascii_alphanumeric());
        let mem_only = is_mem_only(ref_str);
        // Push the reference wrapped in {}, or in [{}] if mem-only
        out.push_str(if mem_only { "[{" } else {"{"});
        out.push_str(ref_str);
        out.push_str(if mem_only { "}]" } else {"}"});
        // Push the rest of the chunk
        out.push_str(&chunk[ref_str.len()..]);
    }
    out
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

        fn operand_is_mem_only(operand: &AsmOperand) -> bool {
            if let Ok((_dir_spec, mem_only, _parsed)) = parse_constraints(&operand.constraints) {
                mem_only
            } else {
                println!("could not parse asm constraints: {}", operand.constraints);
                false
            }
        }

        // Rewrite arg references in assembly template
        let rewritten_asm = rewrite_asm(asm, |ref_str: &str| {
            if let Ok(idx) = ref_str.parse::<usize>() {
                inputs.iter()
                    .chain(outputs.iter())
                    .nth(idx)
                    .map(operand_is_mem_only)
                    .unwrap_or(false)
            } else {
                false
            }
        });

        // Detect and pair inputs/outputs that constrain themselves to the same register
        let mut inputs_by_register = HashMap::new();
        for (i, input) in inputs.iter().enumerate() {
            let (_dir_spec, mem_only, parsed) = parse_constraints(&input.constraints)?;
            inputs_by_register.insert(parsed, (i, input.clone()));
        }

        // Convert gcc asm arguments (input and output lists) into a single list
        // of operands with explicit arg dir specs (asm!-style)

        // The unified arg list
        let mut args = Vec::<BidirAsmOperand>::new();

        // Add outputs as inout if a matching input is found, else as outputs
        for (i, output) in outputs.into_iter().enumerate() {
            match parse_constraints(&output.constraints) {
                Ok((mut dir_spec, mem_only, parsed)) => {
                    // Add to args list; if a matching in_expr is found, this is
                    // an inout and we remove the output from the outputs list
                    let mut in_expr = inputs_by_register.remove(&parsed);
                    if in_expr.is_none() {
                        // Also check for by-index references to this output
                        in_expr = inputs_by_register.remove(&i.to_string());
                    }
                    // Extract expression
                    let in_expr = in_expr.map(|(i, operand)| (i, operand.expression));

                    // For inouts, change the dirspec to include 'in'
                    if in_expr.is_some() {
                        dir_spec = dir_spec.with_in();
                    }
                    args.push(BidirAsmOperand {
                        dir_spec,
                        mem_only,
                        name: None,
                        constraints: parsed,
                        in_expr,
                        out_expr: Some((i, output.expression))
                    });
                },
                // Constraint could not be parsed, drop it
                Err(e) => eprintln!("{}", e),
            }
        }
        // Add unmatched inputs
        for (_, (i, input)) in inputs_by_register {
            let (dir_spec, mem_only, parsed) = match parse_constraints(&input.constraints) {
                Ok(x) => x,
                Err(e) => {eprintln!("{}", e); continue;}
            };
            args.push(BidirAsmOperand {
                dir_spec,
                mem_only,
                name: None,
                constraints: parsed,
                in_expr: Some((i, input.expression)),
                out_expr: None,
            });
        }

        // Determine whether the assembly is in AT&T syntax
        let att_syntax = asm_is_att_syntax(&*rewritten_asm);

        // Add workaround for reserved registers (e.g. rbx on x86_64)
        let (prolog, epilog) = rewrite_reserved_reg_operands(att_syntax, &mut args);
        let rewritten_asm = prolog + &rewritten_asm + &epilog;

        // Emit assembly template
        push_expr(&mut tokens, mk().lit_expr(rewritten_asm));

        // Outputs and Inputs
        let mut operand_renames = HashMap::new();
        for operand in args {
            tokens.push(TokenTree::Punct(Punct::new(',', Alone)));

            // First, convert output expr if present
            let out_expr = if let Some((output_idx, out_expr)) = operand.out_expr {
                let mut out_expr = self.convert_expr(ctx.used(), out_expr)?;
                stmts.append(out_expr.stmts_mut());
                let mut out_expr = out_expr.into_value();

                if operand.mem_only {
                    // If the constraint string contains `*`, then
                    // c2rust-ast-exporter added it (there's no gcc equivalent);
                    // in this case, we need to do what clang does and pass in
                    // the operand by-address instead of by-value
                    out_expr = mk().mutbl().addr_of_expr(out_expr);
                }

                if let Some(tied_operand) = tied_operands.get(&(output_idx, true)) {
                    // If we have an input operand tied to an output operand,
                    // we need to replicate clang's behavior: the inline assembly
                    // uses the larger type internally, and the smaller value gets
                    // extended to the larger one before the call, and truncated
                    // back after (if needed). For portability, we moved the
                    // type conversions into the `c2rust-asm-casts` crate,
                    // so we call into that one from here.

                    // Convert `x` into `let freshN = &mut x; *x`
                    let output_name = self.renamer.borrow_mut().fresh();
                    let output_local = mk().local(
                        mk().ident_pat(&output_name),
                        None as Option<Box<Type>>,
                        Some(mk().mutbl().addr_of_expr(out_expr)),
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

                    out_expr = mk().ident_expr(&inner_name);
                    operand_renames.insert(output_idx, (output_name, inner_name));
                }
                Some(out_expr)
            } else {
                None
            };

            // Then, handle input expr if present
            let in_expr = if let Some((input_idx, in_expr)) = operand.in_expr {
                let mut in_expr = self.convert_expr(ctx.used(), in_expr)?;
                stmts.append(in_expr.stmts_mut());
                let mut in_expr = in_expr.into_value();

                if operand.mem_only {
                    in_expr = mk().addr_of_expr(in_expr);
                }
                if let Some(tied_operand) = tied_operands.get(&(input_idx, false)) {
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
                        Some(in_expr),
                    );
                    stmts.push(mk().local_stmt(Box::new(input_local)));

                    // Replace `in_expr` with
                    // `c2rust_asm_casts::AsmCast::cast_in(output, input)`
                    let path_expr = mk().path_expr(
                        vec!["c2rust_asm_casts", "AsmCast", "cast_in"]
                    );
                    let output = mk().ident_expr(output_name);
                    let input = mk().ident_expr(input_name);
                    in_expr = mk().call_expr(path_expr, vec![output.clone(), input.clone()]);

                    // Append the cast-out call after the assembly macro:
                    // `c2rust_asm_casts::AsmCast::cast_out(output, input, inner);`
                    let path_expr = mk().path_expr(
                        vec!["c2rust_asm_casts", "AsmCast", "cast_out"]);
                    let inner = mk().ident_expr(inner_name);
                    let cast_out = mk().call_expr(path_expr, vec![output, input, inner]);
                    post_stmts.push(mk().semi_stmt(cast_out));
                }
                Some(in_expr)
            } else {
                None
            };

            // Emit "name =" if a name is given
            if let Some(name) = operand.name {
                push_expr(&mut tokens, mk().ident_expr(name));
                tokens.push(TokenTree::Punct(Punct::new('=', Alone)));
            }

            // Emit dir_spec(constraint), quoting constraint if needed
            push_expr(&mut tokens, mk().ident_expr(operand.dir_spec.to_string()));
            let is_regname_or_int = operand.constraints.contains('"') ||
                operand.constraints.starts_with(|c: char| c.is_ascii_digit());
            let constraints_ident = if is_regname_or_int {
                mk().lit_expr(operand.constraints.trim_matches('"'))
            } else {
                mk().ident_expr(operand.constraints)
            };

            // Emit input and/or output expressions, separated by "=>" if both
            push_expr(&mut tokens, mk().paren_expr(constraints_ident));
            if let Some(in_expr) = in_expr {
                push_expr(&mut tokens, in_expr);
                if out_expr.is_some() {
                    tokens.push(TokenTree::Punct(Punct::new('=', Joint)));
                    tokens.push(TokenTree::Punct(Punct::new('>', Alone)));
                } else {
                    // If inout but no out expr was given, mark clobbered ('_')
                    if let ArgDirSpec::InOut | ArgDirSpec::InLateOut = operand.dir_spec {
                        tokens.push(TokenTree::Punct(Punct::new('=', Joint)));
                        tokens.push(TokenTree::Punct(Punct::new('>', Alone)));

                        tokens.push(TokenTree::Punct(Punct::new('_', Alone)));
                    }
                }
            }
            if let Some(out_expr) = out_expr {
                push_expr(&mut tokens, out_expr);
            }
        }

        let mut preserves_flags = true;
        let mut read_only = true;

        // Clobbers
        for clobber in clobbers {
            // Process and drop non-register clobbers
            if clobber == "cc" {
                preserves_flags = false;
                continue
            };
            if clobber == "memory" {
                read_only = false;
                continue;
            };

            // We must drop clobbers of reserved registers, even though this
            // really means we're misinforming the compiler of what's been
            // overwritten. Warn verbosely.
            let quoted = format!("\"{}\"", clobber);
            if reg_is_reserved(&quoted).is_some() {
                warn!("Attempting to clobber reserved register ({}), dropping clobber! \
                This likely means the potential for miscompilation has been introduced. \
                Please rewrite this assembly to save/restore the value of this register \
                if at all possible.", clobber);
                continue;
            }

            tokens.push(TokenTree::Punct(Punct::new(',', Alone)));
            let result = mk().call_expr(mk().ident_expr("out"), vec![mk().lit_expr(clobber)]);
            push_expr(&mut tokens, result);
            tokens.push(TokenTree::Punct(Punct::new(' ', Alone)));
            push_expr(&mut tokens, mk().ident_expr("_"));
        }

        // Options
        {
            let mut options = vec![];
            if preserves_flags {
                options.push(mk().ident_expr("preserves_flags"));
            }
            if !is_volatile {
                // Pure cannot be applied if we have no outputs
                if read_only && (outputs.len() + clobbers.len()) > 0 {
                    options.push(mk().ident_expr("pure"));
                    options.push(mk().ident_expr("readonly"));
                }
                // We never emit [pure, nomem] right now, but it would be nice
            }

            if att_syntax {
                options.push(mk().ident_expr("att_syntax"));
            }

            if !options.is_empty() {
                tokens.push(TokenTree::Punct(Punct::new(',', Alone)));
                let result = mk().call_expr(mk().ident_expr("options"), options);
                push_expr(&mut tokens, result);
            }
        }

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
