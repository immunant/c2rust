//! The overall implementation strategy for rewriting is:
//!
//! 1. Using the pointer permissions and flags inferred by the analysis, annotate MIR statements
//!    with the desired rewrites. These MIR-level rewrites are abstract changes to MIR statements,
//!    such as adding a cast to a particular assignment statement. This is defined in the
//!    `rewrite::expr::mir_op` module.
//!
//! 2. For each HIR expression, look at the MIR statements generated from this HIR expression and
//!    lift any MIR rewrites into HIR rewrites. HIR rewrites are expressed as concrete operations
//!    on source code, such as replacing an expression with one of its subexpressions (both
//!    identified by their `Span`s) or wrapping an expression in a ref or deref operation. The
//!    HIR-level rewrite type is `rewrite::Rewrite`; the `rewrite::expr::distribute` and
//!    `rewrite::expr::convert` modules implement the lifting.
//!
//! 3. Apply the rewrites to the source code of the input program. This reads the source of each
//!    file and emits a new string consisting of the file source with certain `Span`s rewritten as
//!    specified by the HIR rewrites. The code for this is in `rewrite::apply`.
//!
//! This covers rewriting of expressions; rewriting of types is similar but mostly skips step 1,
//! since an abstract description of the changes to be made can be obtained by inspecting the
//! pointer permissions and flags directly. This code is in `rewrite::ty`. All type and expr
//! rewrites are collected and applied in one pass in step 3 (as rewriting in two passes would
//! require us to update the `Span`s mentioned in the later rewrites to account for the changes in
//! the source code produced by the earlier ones).

use log::{debug, warn};
use rustc_hir::Mutability;
use rustc_middle::ty::TyCtxt;
use rustc_span::{FileName, Span};
use std::collections::HashMap;
use std::fmt;
use std::fs;

mod apply;
mod expr;
mod shim;
mod span_index;
mod statics;
mod ty;

pub use self::expr::gen_expr_rewrites;
pub use self::shim::{gen_shim_call_rewrites, gen_shim_definition_rewrite, ManualShimCasts};
pub use self::statics::gen_static_rewrites;
pub use self::ty::dump_rewritten_local_tys;
pub use self::ty::{gen_adt_ty_rewrites, gen_ty_rewrites};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LifetimeName {
    Explicit(String),
    Elided,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub enum Rewrite<S = Span> {
    /// Take the original expression unchanged.
    #[default]
    Identity,
    /// Extract the subexpression at the given index.
    Sub(usize, S),

    /// Emit some fixed text.
    Text(String),
    /// Extract some text from the input source code before any rewrites were applied.
    Extract(S),

    // Expression builders
    /// `&e`, `&mut e`
    Ref(Box<Rewrite>, Mutability),
    /// `core::ptr::addr_of!(e)`, `core::ptr::addr_of_mut!(e)`
    AddrOf(Box<Rewrite>, Mutability),
    /// `*e`
    Deref(Box<Rewrite>),
    /// `arr[idx]`
    Index(Box<Rewrite>, Box<Rewrite>),
    /// `arr[idx1..idx2]`.  Both `idx1` and `idx2` are optional.
    SliceRange(Box<Rewrite>, Option<Box<Rewrite>>, Option<Box<Rewrite>>),
    /// `e as T`
    Cast(Box<Rewrite>, Box<Rewrite>),
    /// Placeholder for a redundant cast that has already been removed.  This allows
    /// `MirRewrite::RemoveCast` to still apply even though the cast is already gone.
    RemovedCast(Box<Rewrite>),
    /// The integer literal `0`.
    LitZero,
    /// Function calls
    Call(String, Vec<Rewrite>),
    /// Method calls
    MethodCall(String, Box<Rewrite>, Vec<Rewrite>),
    /// A block of statements, followed by an optional result expression.  This rewrite inserts a
    /// semicolon after each statement.
    Block(Vec<Rewrite>, Option<Box<Rewrite>>),
    /// A multi-variable `let` binding, like `let (x, y) = (rw0, rw1)`.  Note that this rewrite
    /// does not include a trailing semicolon.
    ///
    /// Since these variable bindings are not hygienic, a `Let` can invalidate the expression
    /// produced by `Identity` or `Sub` rewrites used later in the same scope.  In general, `Let`
    /// should only be used inside a `Block`, and `Identity` and `Sub` rewrites should not be used
    /// later in that block.
    Let(Vec<(String, Rewrite)>),
    /// Single-variable `let` binding.  This has the same scoping issues as multi-variable `Let`;
    /// because of this, `Let` should generally be used instead of multiple `Let1`s.
    Let1(String, Box<Rewrite>),
    /// Single-argument closure.  As with `Let` and `Let1`, the body must be carefully constructed
    /// to avoid potential shadowing.
    Closure1(String, Box<Rewrite>),
    /// Match expression.  The `Vec` is a list of cases, each consisting of a pattern and an
    /// expression.
    Match(Box<Rewrite>, Vec<(String, Rewrite)>),

    // Type builders
    /// Emit a complete pretty-printed type, discarding the original annotation.
    Print(String),
    /// `*const T`, `*mut T`
    TyPtr(Box<Rewrite>, Mutability),
    /// `&T`, `&mut T`
    TyRef(LifetimeName, Box<Rewrite>, Mutability),
    /// `[T]`
    TySlice(Box<Rewrite>),
    /// `Foo<T1, T2>`
    TyCtor(String, Vec<Rewrite>),
    /// `<'a, 'b, ...>`
    /// needed for cases when the span of the ADT name
    /// is different from ADT generic params
    _TyGenericParams(Vec<Rewrite>),
    // `static` builders
    /// `static` mutability (`static` <-> `static mut`)
    StaticMut(Mutability, S),

    // `fn` builders
    /// Define a function.
    DefineFn {
        name: String,
        arg_tys: Vec<Rewrite>,
        return_ty: Option<Box<Rewrite>>,
        body: Box<Rewrite>,
    },
    /// Emit the name of a function argument.  Only useful inside the body of `DefineFn`.
    FnArg(usize),
}

impl fmt::Display for Rewrite {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        apply::emit_rewrite(&mut FormatterSink(f), self)
    }
}

impl Rewrite {
    /// Try to substitute `subst` for `Rewrite::Identity` anywhere it occurs within `self`.
    /// Returns `None` if substitution fails; this happens when `self` contains `Rewrite::Sub`.
    fn try_subst(&self, subst: &Rewrite) -> Option<Rewrite> {
        use self::Rewrite::*;

        let try_subst =
            |rw: &Rewrite| -> Option<Box<Rewrite>> { Some(Box::new(rw.try_subst(subst)?)) };
        let try_subst_option = |rw: &Option<Box<Rewrite>>| -> Option<Option<Box<Rewrite>>> {
            let new_rw = if let Some(rw) = rw.as_ref() {
                Some(try_subst(rw)?)
            } else {
                None
            };
            // Outer option indicates success/failure of the rewrite.  Inner option indicates
            // presence/absence of `rw`.
            Some(new_rw)
        };
        let try_subst_vec = |rws: &[Rewrite]| -> Option<Vec<Rewrite>> {
            let mut new_rws = Vec::with_capacity(rws.len());
            for rw in rws {
                new_rws.push(rw.try_subst(subst)?);
            }
            Some(new_rws)
        };

        Some(match *self {
            Identity => subst.clone(),
            Sub(..) => return None,

            Text(ref s) => Text(String::clone(s)),
            Extract(span) => Extract(span),

            Ref(ref rw, mutbl) => Ref(try_subst(rw)?, mutbl),
            AddrOf(ref rw, mutbl) => AddrOf(try_subst(rw)?, mutbl),
            Deref(ref rw) => Deref(try_subst(rw)?),
            Index(ref arr, ref idx) => Index(try_subst(arr)?, try_subst(idx)?),
            SliceRange(ref arr, ref lo, ref hi) => SliceRange(
                try_subst(arr)?,
                try_subst_option(lo)?,
                try_subst_option(hi)?,
            ),
            Cast(ref expr, ref ty) => Cast(try_subst(expr)?, try_subst(ty)?),
            RemovedCast(ref rw) => RemovedCast(try_subst(rw)?),
            LitZero => LitZero,
            Call(ref func, ref args) => Call(String::clone(func), try_subst_vec(args)?),
            MethodCall(ref func, ref receiver, ref args) => MethodCall(
                String::clone(func),
                try_subst(receiver)?,
                try_subst_vec(args)?,
            ),
            Block(ref stmts, ref expr) => Block(try_subst_vec(stmts)?, try_subst_option(expr)?),
            Let(ref vars) => {
                let mut new_vars = Vec::with_capacity(vars.len());
                for (ref name, ref rw) in vars {
                    new_vars.push((String::clone(name), rw.try_subst(subst)?));
                }
                Let(new_vars)
            }
            Let1(ref name, ref rw) => Let1(String::clone(name), try_subst(rw)?),
            Closure1(ref name, ref rw) => Closure1(String::clone(name), try_subst(rw)?),
            Match(ref expr, ref cases) => {
                let mut new_cases = Vec::with_capacity(cases.len());
                for &(ref pat, ref body) in cases {
                    new_cases.push((String::clone(pat), body.try_subst(subst)?));
                }
                Match(try_subst(expr)?, new_cases)
            }

            Print(ref s) => Print(String::clone(s)),
            TyPtr(ref rw, mutbl) => TyPtr(try_subst(rw)?, mutbl),
            TyRef(ref lt, ref rw, mutbl) => TyRef(LifetimeName::clone(lt), try_subst(rw)?, mutbl),
            TySlice(ref rw) => TySlice(try_subst(rw)?),
            TyCtor(ref name, ref tys) => TyCtor(String::clone(name), try_subst_vec(tys)?),
            _TyGenericParams(ref tys) => _TyGenericParams(try_subst_vec(tys)?),
            StaticMut(mutbl, span) => StaticMut(mutbl, span),

            DefineFn {
                ref name,
                ref arg_tys,
                ref return_ty,
                ref body,
            } => DefineFn {
                name: String::clone(name),
                arg_tys: try_subst_vec(arg_tys)?,
                return_ty: try_subst_option(return_ty)?,
                body: try_subst(body)?,
            },
            FnArg(idx) => FnArg(idx),
        })
    }
}

struct FormatterSink<'a, 'b>(&'a mut fmt::Formatter<'b>);

impl apply::Sink for FormatterSink<'_, '_> {
    type Error = fmt::Error;
    const PARENTHESIZE_EXPRS: bool = false;

    fn emit_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_str(s)
    }
    fn emit_orig_str(&mut self, s: &str, _line: usize) -> fmt::Result {
        self.0.write_str(s)
    }
    fn emit_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        self.0.write_fmt(args)
    }
    fn emit_expr(&mut self) -> fmt::Result {
        self.0.write_str("$e")
    }
    fn emit_sub(&mut self, idx: usize, _span: Span) -> fmt::Result {
        self.0.write_fmt(format_args!("${}", idx))
    }
    fn emit_span(&mut self, span: Span) -> fmt::Result {
        self.0.write_fmt(format_args!("<span {:?}>", span))
    }
}

/// Return a copy of `src` with `annotations` added as comments.  If `line_map` is provided, the
/// line numbers in `annotations` are remapped using `line_map` before the annotations are inserted
/// in `src`.
fn add_annotations(
    src: String,
    line_map: Option<&[usize]>,
    mut annotations: Vec<(usize, String)>,
) -> String {
    if annotations.is_empty() {
        return src;
    }

    let map_line = |i: usize| -> usize {
        if let Some(line_map) = line_map {
            line_map.get(i).copied().unwrap_or(usize::MAX)
        } else {
            i
        }
    };

    // Stable sort by input line, preserving the order in which annotations were added.
    annotations.sort_by_key(|&(line, _)| line);
    // The `usize` in each `annotations` entry is an input line number.  Map it to an output line
    // number using `line_map`.  Input lines with no matching output line are placed at the end.
    for &mut (ref mut line, _) in &mut annotations {
        *line = map_line(*line);
    }
    // Now stable sort by output line, preserving the order of input line if several input lines
    // map to one output line, and also preserving the order in which annotations were added.
    annotations.sort_by_key(|&(line, _)| line);

    let mut out = String::with_capacity(src.len());
    let mut idx = 0;
    for (j, line) in src.lines().enumerate() {
        out.push_str(line);
        out.push('\n');
        // Now emit all annotations associated with line `j`.
        while let Some((_, ann)) = annotations.get(idx).filter(|&&(line, _)| line == j) {
            idx += 1;
            let indent_len = line.len() - line.trim_start().len();
            out.push_str(&line[..indent_len]);
            out.push_str("// ");
            out.push_str(ann);
            out.push('\n');
        }
    }

    // Emit any leftover annotations at the end of the file.
    while let Some((_, ann)) = annotations.get(idx) {
        idx += 1;
        out.push_str("// ");
        out.push_str(ann);
        out.push('\n');
    }

    out
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum UpdateFiles {
    No,
    InPlace,
    Alongside,
    /// Update mode used for pointwise testing.  If the file being rewritten is `foo.rs`, and the
    /// rewriting mode is `AlongsidePointwise("bar")`, then the rewritten code is written to
    /// `foo.bar.rs`.
    AlongsidePointwise(rustc_span::symbol::Symbol),
}

pub fn apply_rewrites(
    tcx: TyCtxt,
    rewrites: Vec<(Span, Rewrite)>,
    mut annotations: HashMap<FileName, Vec<(usize, String)>>,
    update_files: UpdateFiles,
) {
    let emit = |filename, src: String| {
        println!("\n\n ===== BEGIN {:?} =====", filename);
        for line in src.lines() {
            // Omit filecheck directives from the debug output, as filecheck can get confused due
            // to directives matching themselves (e.g. `// CHECK: foo` will match the `foo` in the
            // line `// CHECK: foo`).
            if let Some((pre, _post)) = line.split_once("// CHECK") {
                println!("{}// (FileCheck directive omitted)", pre);
            } else {
                println!("{}", line);
            }
        }
        println!(" ===== END {:?} =====", filename);

        if !matches!(update_files, UpdateFiles::No) {
            let mut path_ok = false;
            if let FileName::Real(ref rfn) = filename {
                if let Some(path) = rfn.local_path() {
                    let path = match update_files {
                        UpdateFiles::InPlace => path.to_owned(),
                        UpdateFiles::Alongside => path.with_extension("new.rs"),
                        UpdateFiles::AlongsidePointwise(ref s) => {
                            let ext = format!("{}.rs", s);
                            let p = path.with_extension(&ext);
                            debug!("writing to {:?}", p);
                            p
                        }
                        UpdateFiles::No => unreachable!(),
                    };
                    fs::write(path, src).unwrap();
                    path_ok = true;
                }
            }
            if !path_ok {
                warn!("couldn't write to non-real file {filename:?}");
            }
        }
    };

    let new_src = apply::apply_rewrites(tcx.sess.source_map(), rewrites);
    for (filename, file_rw) in new_src {
        let annotations = annotations.remove(&filename).unwrap_or_default();
        let new_src = add_annotations(file_rw.new_src, Some(&file_rw.line_map), annotations);
        emit(filename, new_src);
    }

    // Also emit files that have annotations but no rewrites.
    if !annotations.is_empty() {
        let mut leftover_annotations = annotations.into_iter().collect::<Vec<_>>();
        leftover_annotations.sort();
        let sm = tcx.sess.source_map();
        for (filename, annotations) in leftover_annotations {
            let sf = sm.get_source_file(&filename).unwrap();
            let src = match sf.src {
                Some(ref x) => String::clone(x),
                None => continue,
            };
            let src = add_annotations(src, None, annotations);
            emit(filename, src);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn identity() -> Box<Rewrite> {
        Box::new(Rewrite::Identity)
    }

    fn ref_(rw: Box<Rewrite>) -> Box<Rewrite> {
        Box::new(Rewrite::Ref(rw, Mutability::Not))
    }

    fn index(arr: Box<Rewrite>, idx: Box<Rewrite>) -> Box<Rewrite> {
        Box::new(Rewrite::Index(arr, idx))
    }

    fn cast_usize(rw: Box<Rewrite>) -> Box<Rewrite> {
        Box::new(Rewrite::Cast(
            rw,
            Box::new(Rewrite::Print("usize".to_owned())),
        ))
    }

    /// Test precedence handling in `Rewrite::pretty`
    #[test]
    fn rewrite_pretty_precedence() {
        // Ref vs Index
        assert_eq!(ref_(index(identity(), identity())).to_string(), "&$e[$e]",);

        assert_eq!(
            index(ref_(identity()), ref_(identity())).to_string(),
            "(&$e)[&$e]",
        );

        // Ref vs Cast
        assert_eq!(cast_usize(ref_(identity())).to_string(), "&$e as usize",);

        assert_eq!(ref_(cast_usize(identity())).to_string(), "&($e as usize)",);

        // Cast vs Index
        assert_eq!(
            cast_usize(index(identity(), identity())).to_string(),
            "$e[$e] as usize",
        );

        assert_eq!(
            index(cast_usize(identity()), cast_usize(identity())).to_string(),
            "($e as usize)[$e as usize]",
        );

        // Index vs Index
        assert_eq!(
            index(index(identity(), identity()), identity()).to_string(),
            "$e[$e][$e]",
        );
    }
}
