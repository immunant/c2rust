#![deny(missing_docs)]
//! This module provides basic support for converting inline assembly statements.

use crate::diagnostics::TranslationResult;

use super::*;
use log::warn;
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
        }
        .to_owned()
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

/// A machine architecture that rustc inline assembly knows about
#[derive(Copy, Clone, PartialEq)]
enum Arch {
    X86OrX86_64,
    Arm,
    Aarch64,
    Riscv,
}

/// Parse a machine architecture from a target tuple. This is a best-effort attempt.
fn parse_arch(target_tuple: &str) -> Option<Arch> {
    if target_tuple.starts_with("i386")
        || target_tuple.starts_with("i486")
        || target_tuple.starts_with("i586")
        || target_tuple.starts_with("i686")
        || target_tuple.starts_with("x86_64")
        || target_tuple.starts_with("x86")
    {
        Some(Arch::X86OrX86_64)
    } else if target_tuple.starts_with("aarch64")
        || target_tuple.starts_with("armv8")
        || target_tuple.starts_with("arm64")
    {
        Some(Arch::Aarch64)
    } else if target_tuple.starts_with("arm") || target_tuple.starts_with("thumbv") {
        Some(Arch::Arm)
    } else if target_tuple.starts_with("riscv") {
        Some(Arch::Riscv)
    } else {
        None
    }
}

fn parse_constraints(
    mut constraints: &str,
    arch: Arch,
) -> TranslationResult<(ArgDirSpec, bool, String)> {
    let parse_error = |constraints| {
        Err(TranslationError::new(
            None,
            failure::err_msg(
                "Inline assembly constraints could not be parsed: ".to_owned() + constraints,
            )
            .context(TranslationErrorKind::Generic),
        ))
    };
    use ArgDirSpec::*;
    let mut is_input = match constraints.chars().next() {
        Some('+') => {
            constraints = &constraints[1..];
            true
        }
        Some('=') => {
            constraints = &constraints[1..];
            false
        }
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

    // Convert (simple) constraints to ones rustc understands
    match &*constraints {
        "m" => {
            mem_only = true;
            constraints = "reg".into();
        }
        "r" => {
            constraints = "reg".into();
        }
        "i" => {
            // Rust inline assembly has no constraint for that, but uses the argument as an
            // immediate value anyway
            constraints = "reg".into();
        }
        _ => {
            let is_explicit_reg = constraints.starts_with('"');
            let is_tied = !constraints.contains(|c: char| !c.is_ascii_digit());

            if !(is_explicit_reg || is_tied) {
                // Attempt to parse machine-specific constraints
                if let Some((machine_constraints, is_mem)) =
                    translate_machine_constraint(&constraints, arch)
                {
                    constraints = machine_constraints.into();
                    mem_only = is_mem;
                } else {
                    warn!(
                        "Did not recognize inline asm constraint: {}\n\
                    It is likely that this will cause compilation errors or \
                    incorrect semantics in the translated program; please \
                    manually correct.",
                        constraints
                    );
                }
            }
        }
    };

    let mode = if mem_only {
        In
    } else {
        match (is_input, early_clobber) {
            (false, false) => LateOut,
            (false, true) => Out,
            (true, false) => InLateOut,
            (true, true) => InOut,
        }
    };

    Ok((mode, mem_only, constraints))
}

fn is_regname_or_int(parsed_constraint: &str) -> bool {
    parsed_constraint.contains('"') || parsed_constraint.starts_with(|c: char| c.is_ascii_digit())
}

/// Translate an architecture-specific assembly constraint from llvm/gcc
/// to those accepted by the Rust asm! macro. "Simple" (arch-independent)
/// constraints are handled in `parse_constraints`, not here.
/// See <https://gcc.gnu.org/onlinedocs/gcc/Machine-Constraints.html>,
/// <https://llvm.org/docs/LangRef.html#constraint-codes>, and
/// <https://doc.rust-lang.org/nightly/reference/inline-assembly.html#register-operands>
fn translate_machine_constraint(constraint: &str, arch: Arch) -> Option<(&str, bool)> {
    let mem = &mut false;
    // Many constraints are not handled here, because rustc does. The best we can
    let constraint = match arch {
        Arch::X86OrX86_64 => match constraint {
            // "R" => "reg_word", // rust does not support this
            "Q" => "reg_abcd",
            "q" => "reg_byte",
            "a" => "\"a\"",
            "b" => "\"b\"",
            "c" => "\"c\"",
            "d" => "\"d\"",
            "S" => "\"si\"",
            "D" => "\"di\"",
            // "A" => "a_and_d", // rust does not support this
            "U" => {
                warn!(
                    "the x86 'U' inline assembly operand constraint cannot \
                be translated correctly. It corresponds to the `clobber_abi` \
                option for `asm!`, but c2rust does not know the ABI being \
                used, so it cannot be translated automatically. Please correct \
                manually after translation."
                );
                return None;
            }
            "f" => "x87_reg",
            "t" => "\"st(0)\"",
            "u" => "\"st(1)\"",
            "x" => "xmm_reg", // this could also translate as ymm_reg
            "y" => "mmx_reg",
            "v" => "zmm_reg",
            "Yz" => "\"xmm0\"",
            "Yk" => "kreg",

            _ => return None,
        },
        Arch::Aarch64 => match constraint {
            "k" => "\"SP\"",
            "w" => "vreg",
            "x" => "vreg_low16",
            // "y" => "vreg_low8", // rust does not support this
            // "Upl" => "preg_low8", // rust does not support this
            "Upa" => "preg",
            "Q" => {
                *mem = true;
                "reg"
            }
            "Ump" => {
                *mem = true;
                "reg"
            }
            _ => return None,
        },
        Arch::Arm => match constraint {
            // "h" => "reg_8_15", // rust does not support this
            "k" => "\"SP\"",
            "l" => "reg",
            "t" => "sreg",
            "x" => "sreg_low16",
            "w" => "dreg",
            // "y" => "sreg",
            // "z" => "sreg",
            "Q" => {
                *mem = true;
                "reg"
            }
            "Uv" | "Uy" | "Uq" => {
                *mem = true;
                "reg"
            }
            _ => return None,
        },
        Arch::Riscv => match constraint {
            "f" => "freg",
            _ => return None,
        },
    };
    Some((constraint, *mem))
}

/// Translate a template modifier from llvm/gcc asm template argument modifiers
/// to those accepted by the Rust asm! macro. This is arch-dependent, so we need
/// to know which architecture the asm targets.
/// See <https://doc.rust-lang.org/nightly/reference/inline-assembly.html#template-modifiers>
fn translate_modifier(modifier: char, arch: Arch) -> Option<char> {
    Some(match arch {
        Arch::X86OrX86_64 => match modifier {
            'k' => 'e',
            'q' => 'r',
            'b' => 'l',
            'h' => 'h',
            'w' => 'x',
            _ => return None,
        },
        Arch::Aarch64 => modifier,
        Arch::Arm => match modifier {
            'p' | 'q' => return None,
            _ => modifier,
        },
        Arch::Riscv => modifier,
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
/// uses a reserved register.
fn reg_is_reserved(constraint: &str, arch: Arch) -> Option<(&str, &str)> {
    Some(match arch {
        Arch::X86OrX86_64 => match constraint {
            // rbx is reserved on x86_64 but not x86, and esi is reserved on x86
            // but not x86_64. It would be nice to distinguish these
            // architectures here.
            "\"bl\"" | "\"bh\"" | "\"bx\"" | "\"ebx\"" | "\"rbx\"" => {
                let reg = constraint.trim_matches('"');
                let mods = if reg.len() == 2 {
                    &reg[1..] // l/h/x
                } else {
                    &reg[..1] // e/r
                };
                (reg, mods)
            }
            _ => return None,
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
fn rewrite_reserved_reg_operands(
    att_syntax: bool,
    arch: Arch,
    operands: &mut [BidirAsmOperand],
) -> (String, String) {
    let (mut prolog, mut epilog) = (String::new(), String::new());

    let mut rewrite_idxs = vec![];
    let mut total_positional = 0;

    // Determine which operands must be rewritten and how many
    // positional operands there are. Positional operands must precede named
    // operands, so this tells us where to reinsert operands we rewrite.
    for (i, operand) in operands.iter().enumerate() {
        if operand.is_positional() {
            total_positional += 1;
        } else if let Some((reg, mods)) = reg_is_reserved(&operand.constraints, arch) {
            rewrite_idxs.push((i, reg.to_owned(), mods.to_owned()));
        }
    }

    for (n_moved, (idx, reg, mods)) in rewrite_idxs.into_iter().enumerate() {
        let operand = &mut operands[idx];
        let name = format!("restmp{}", n_moved);
        if let Some((_idx, _in_expr)) = operand.in_expr {
            let move_input = if att_syntax {
                format!("mov %{}, {{{}:{}}}\n", reg, name, mods)
            } else {
                format!("mov {{{}:{}}}\n, {}", name, mods, reg)
            };
            prolog.push_str(&move_input);
        }
        if let Some((_idx, _out_expr)) = operand.out_expr {
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
    }

    (prolog, epilog)
}

/// Remove comments from an x86 assembly template. Used only to provide a less-
/// confounding input to our Intel-vs-AT&T detection in `asm_is_att_syntax`.
fn remove_comments(mut asm: &str) -> String {
    // Remove C-style comments
    let mut without_c_comments = String::with_capacity(asm.len());
    while let Some(comment_begin) = asm.find("/*") {
        let comment_len = asm[comment_begin..]
            .find("*/")
            // Comments with no terminator extend to the end of the string
            .unwrap_or_else(|| asm[comment_begin..].len());
        let before_comment = &asm[..comment_begin];
        without_c_comments.push_str(before_comment);
        asm = &asm[comment_begin + comment_len..];
    }
    // Push whatever is left after the final comment
    without_c_comments.push_str(asm);

    // Remove EOL comments from each line
    let mut without_comments = String::with_capacity(without_c_comments.len());
    for line in without_c_comments.lines() {
        if let Some(line_comment_idx) = line.find('#') {
            without_comments.push_str(&line[..line_comment_idx]);
        } else {
            without_comments.push_str(line);
        }
        without_comments.push('\n');
    }
    without_comments
}

/// Detect whether an x86(_64) gcc inline asm string uses Intel or AT&T syntax.
/// For gcc, AT&T syntax is default... unless `-masm=intel` is passed. This
/// means we can hope but not guarantee that x86 asm with no syntax directive
/// uses AT&T syntax.
/// To handle other cases, try to heuristically detect the variant we get
/// (assuming it's actually x86 asm in the first place...).
/// As the rust x86 default is intel syntax, we need to emit the "att_syntax"
/// option if we get a hint that this asm uses AT&T syntax.
fn asm_is_att_syntax(asm: &str) -> bool {
    // First, remove comments, so we can look at only the semantically
    // significant parts of the asm template.
    let asm = &*remove_comments(asm);

    // Look for syntax directives.
    let intel_directive = asm.find(".intel_syntax");
    let att_directive = asm.find(".att_syntax");
    match (intel_directive, att_directive) {
        (Some(intel_pos), Some(att_pos)) => {
            // Both directives are present; presumably this asm switches to one at
            // its start and restores the default at the end. Whichever comes first
            // should be what the asm uses.
            att_pos < intel_pos
        }
        (Some(_intel), None) => false,
        (None, Some(_att)) => true,
        (None, None) => {
            #[allow(clippy::needless_bool)]
            if asm.contains("word ptr") {
                false
            } else if asm.contains('$') || asm.contains('%') || asm.contains('(') {
                // Guess based on sigils used in AT&T assembly:
                // $ for constants, % for registers, and ( for address calculations
                true
            } else if asm.contains('[') {
                // default to true, because AT&T is the default for gcc inline asm
                false
            } else {
                true
            }
        }
    }
}

/// If applicable, maps the index of an input operand to the
/// output operand it is tied to.
///
/// Utilizes the fact that GNU inline assembly specifies all
/// operands in a particular order of [output1, ... outputN]
/// followed by [input1, ..., inputN] where the indices for all
/// input operands are indexed relative to the size of the
/// output operand sequence.
fn map_input_op_idx(
    idx: usize,
    num_output_operands: usize,
    tied_operands: &HashMap<(usize, bool), usize>,
) -> usize {
    if let Some(adj_idx) = idx.checked_sub(num_output_operands) {
        // will only be Some(idx) if it was an input operand
        match tied_operands.get(&(adj_idx, false)) {
            Some(&out_idx) => {
                return out_idx;
            }
            None => {
                // get number of tied inputs before this index
                // TODO: can calculate this once before the call, not once for each input
                let num_tied_before = tied_operands
                    .keys()
                    .filter(|&&(iidx, is_out)| !is_out && iidx < adj_idx)
                    .count();
                // shift the original index by the number of tied input operands prior to the current one
                return idx - num_tied_before;
            }
        };
    }

    idx
}

/// Rewrite a LLVM inline assembly template string into an asm!-compatible one
/// by translating its references to operands (of the form $0 or $x0) to {0} or
/// {0:y} (and wrapping mem-only references in square brackets).
fn rewrite_asm<F: Fn(&str) -> bool, M: Fn(usize) -> usize>(
    asm: &str,
    input_op_mapper: M,
    is_mem_only: F,
    arch: Arch,
) -> TranslationResult<String> {
    let mut out = String::with_capacity(asm.len());

    let mut first = true;
    let mut last_empty = false;

    // Iterate over $-prefixed chunks
    for chunk in asm.split('$') {
        // No modification needed for first chunk
        if first {
            first = false;
            out.push_str(chunk);
            continue;
        }

        // Pass-through $$ as one $
        if last_empty {
            last_empty = false;
            out.push('$');
            out.push_str(chunk);
            continue;
        }

        // Note empty chunks
        if chunk.is_empty() {
            last_empty = true;
            continue;
        }

        // Do not re-wrap ${...}, but do translate modifiers
        if chunk.starts_with('{') {
            // Translate operand modifiers ("template modifiers" per Rust)
            if let Some(end_idx) = chunk.find('}') {
                let ref_str = &chunk[..end_idx];
                if let Some(colon_idx) = ref_str.find(':') {
                    let (before_mods, _modifiers) = ref_str.split_at(colon_idx + 1);
                    out.push('{');
                    let idx: usize = before_mods
                        .trim_matches(|c: char| !c.is_ascii_digit())
                        .parse()
                        .map_err(|_| TranslationError::generic("could not parse operand idx"))?;
                    out.push_str(input_op_mapper(idx).to_string().as_str());
                    out.push(':');
                    let modifiers = ref_str[colon_idx + 1..].chars();
                    for modifier in modifiers {
                        if let Some(new) = translate_modifier(modifier, arch) {
                            out.push(new);
                        }
                    }
                    out.push_str(&chunk[end_idx..]);
                }
            } else {
                out.push_str(chunk);
            }
            continue;
        }

        // Translate references of the form %k0 or %3, which look like $k0
        // or $3 in LLVM asm.
        if chunk.starts_with(|c: char| c.is_ascii_alphanumeric()) {
            // Find the end of the reference itself (after 'k0' or '3').
            let end_idx = chunk
                .find(|c: char| c == ',' || !c.is_ascii_alphanumeric())
                .unwrap_or(chunk.len());
            let ref_str = &chunk[..end_idx];

            let index_str;
            let mut new_modifiers = String::new();
            // If the ref string starts with a letter, it's a modifier to translate.
            if let Some(true) = ref_str.chars().next().map(|c| c.is_ascii_alphabetic()) {
                let (modifiers, index) = ref_str.split_at(1);

                index_str = index;

                for modifier in modifiers.chars() {
                    if let Some(new) = translate_modifier(modifier, arch) {
                        new_modifiers.push(new);
                    }
                }
            } else {
                // Just digits
                index_str = ref_str;
            }
            let mem_only = is_mem_only(index_str);
            // Push the reference wrapped in {}, or in [{}] if mem-only
            out.push_str(if mem_only { "[{" } else { "{" });
            let idx: usize = index_str
                .parse()
                .map_err(|_| TranslationError::generic("could not parse operand idx"))?;
            out.push_str(input_op_mapper(idx).to_string().as_str());
            if !new_modifiers.is_empty() {
                out.push(':');
                out.push_str(&new_modifiers);
            }
            out.push_str(if mem_only { "}]" } else { "}" });
            // Push the rest of the chunk
            out.push_str(&chunk[end_idx..]);
            continue;
        }

        // We failed to parse this operand reference
        out.push_str(chunk);
    }
    Ok(out)
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
    ) -> TranslationResult<Vec<Stmt>> {
        if !self.tcfg.translate_asm {
            return Err(TranslationError::generic(
                "Inline assembly translation not enabled.",
            ));
        }

        let arch = match parse_arch(&self.ast_context.target) {
            Some(arch) => arch,
            None => {
                return Err(TranslationError::generic(
                    "Cannot translate inline assembly for unfamiliar architecture",
                ))
            }
        };

        self.use_feature("asm");

        fn push_expr(tokens: &mut Vec<TokenTree>, expr: Box<Expr>) {
            tokens.extend(expr.to_token_stream());
        }

        let mut stmts: Vec<Stmt> = vec![];
        let mut post_stmts: Vec<Stmt> = vec![];
        let mut tokens: Vec<TokenTree> = vec![];

        let mut tied_operands = HashMap::new();
        for (
            input_idx,
            &AsmOperand {
                ref constraints, ..
            },
        ) in inputs.iter().enumerate()
        {
            let constraints_digits = constraints.trim_matches(|c: char| !c.is_ascii_digit());
            if let Ok(output_idx) = constraints_digits.parse::<usize>() {
                let output_key = (output_idx, true);
                let input_key = (input_idx, false);
                tied_operands.insert(output_key, input_idx);
                tied_operands.insert(input_key, output_idx);
            }
        }

        let operand_is_mem_only = |operand: &AsmOperand| -> bool {
            if let Ok((_dir_spec, mem_only, _parsed)) =
                parse_constraints(&operand.constraints, arch)
            {
                mem_only
            } else {
                println!("could not parse asm constraints: {}", operand.constraints);
                false
            }
        };

        // Rewrite arg references in assembly template
        let rewritten_asm = rewrite_asm(
            asm,
            |idx: usize| map_input_op_idx(idx, outputs.len(), &tied_operands),
            |ref_str: &str| {
                if let Ok(idx) = ref_str.parse::<usize>() {
                    outputs
                        .iter()
                        .chain(inputs.iter())
                        .nth(idx)
                        .map(operand_is_mem_only)
                        .unwrap_or(false)
                } else {
                    false
                }
            },
            arch,
        )?;

        // Detect and pair inputs/outputs that constrain themselves to the same register
        let mut inputs_by_register = HashMap::new();
        let mut other_inputs = Vec::new();
        for (i, input) in inputs.iter().enumerate() {
            let (_dir_spec, _mem_only, parsed) = parse_constraints(&input.constraints, arch)?;
            // Only pair operands with an explicit register or index
            if is_regname_or_int(&parsed) {
                inputs_by_register.insert(parsed, (i, input.clone()));
            } else {
                other_inputs.push((parsed, (i, input.clone())));
            }
        }

        // Convert gcc asm arguments (input and output lists) into a single list
        // of operands with explicit arg dir specs (asm!-style)

        // The unified arg list
        let mut args = Vec::new();

        // Add outputs as inout if a matching input is found, else as outputs
        for (i, output) in outputs.iter().enumerate() {
            match parse_constraints(&output.constraints, arch) {
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
                        out_expr: Some((i, output.expression)),
                    });
                }
                // Constraint could not be parsed, drop it
                Err(e) => eprintln!("{}", e),
            }
        }
        // Add unmatched inputs
        for (_, (i, input)) in inputs_by_register
            .into_iter()
            .chain(other_inputs.into_iter())
        {
            let (dir_spec, mem_only, parsed) = match parse_constraints(&input.constraints, arch) {
                Ok(x) => x,
                Err(e) => {
                    eprintln!("{}", e);
                    continue;
                }
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
        let att_syntax = match arch {
            Arch::X86OrX86_64 => asm_is_att_syntax(&rewritten_asm),
            _ => false,
        };

        // Add workaround for reserved registers (e.g. rbx on x86_64)
        let (prolog, epilog) = rewrite_reserved_reg_operands(att_syntax, arch, &mut args);
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

                if let Some(_tied_operand) = tied_operands.get(&(output_idx, true)) {
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
                        None,
                        Some(mk().mutbl().addr_of_expr(out_expr)),
                    );
                    stmts.push(mk().local_stmt(Box::new(output_local)));

                    // `let mut freshN;`
                    let inner_name = self.renamer.borrow_mut().fresh();
                    let inner_local = mk().local(mk().ident_pat(&inner_name), None, None);
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

                    let (output_name, inner_name) = operand_renames.get(tied_operand).unwrap();

                    let input_name = self.renamer.borrow_mut().fresh();
                    let input_local = mk().local(mk().ident_pat(&input_name), None, Some(in_expr));
                    stmts.push(mk().local_stmt(Box::new(input_local)));

                    // Replace `in_expr` with
                    // `c2rust_asm_casts::AsmCast::cast_in(output, input)`
                    let path_expr = mk().path_expr(vec!["c2rust_asm_casts", "AsmCast", "cast_in"]);
                    let output = mk().ident_expr(output_name);
                    let input = mk().ident_expr(input_name);
                    in_expr = mk().call_expr(path_expr, vec![output.clone(), input.clone()]);

                    // Append the cast-out call after the assembly macro:
                    // `c2rust_asm_casts::AsmCast::cast_out(output, input, inner);`
                    let path_expr = mk().path_expr(vec!["c2rust_asm_casts", "AsmCast", "cast_out"]);
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
            let constraints_ident = if is_regname_or_int(&operand.constraints) {
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
                continue;
            };
            if clobber == "memory" {
                read_only = false;
                continue;
            };

            // We must drop clobbers of reserved registers, even though this
            // really means we're misinforming the compiler of what's been
            // overwritten. Warn verbosely.
            let quoted = format!("\"{}\"", clobber);
            if reg_is_reserved(&quoted, arch).is_some() {
                warn!(
                    "Attempting to clobber reserved register ({}), dropping clobber! \
                This likely means the potential for miscompilation has been introduced. \
                Please rewrite this assembly to save/restore the value of this register \
                if at all possible.",
                    clobber
                );
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
            item_store.add_use(vec!["core".into(), "arch".into()], "asm");
        });

        let mac = mk().mac(
            mk().path(vec!["asm"]),
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
