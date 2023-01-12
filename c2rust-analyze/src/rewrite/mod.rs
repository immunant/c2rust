use rustc_hir::Mutability;
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;
use std::fmt;

mod apply;
mod expr;
mod span_index;

pub use self::expr::gen_expr_rewrites;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Rewrite<S = Span> {
    /// Take the original expression unchanged.
    Identity,
    /// Extract the subexpression at the given index.
    Sub(usize, S),
    /// `&e`, `&mut e`
    Ref(Box<Rewrite>, Mutability),
    /// `core::ptr::addr_of!(e)`, `core::ptr::addr_of_mut!(e)`
    AddrOf(Box<Rewrite>, Mutability),
    /// `*e`
    Deref(Box<Rewrite>),
    /// `arr[idx]`
    Index(Box<Rewrite>, Box<Rewrite>),
    /// `arr[idx..]`
    SliceTail(Box<Rewrite>, Box<Rewrite>),
    /// `e as usize`
    CastUsize(Box<Rewrite>),
    /// The integer literal `0`.
    LitZero,
}

impl Rewrite {
    fn pretty(&self, f: &mut fmt::Formatter, prec: usize) -> fmt::Result {
        fn parenthesize_if(
            cond: bool,
            f: &mut fmt::Formatter,
            inner: impl FnOnce(&mut fmt::Formatter) -> fmt::Result,
        ) -> fmt::Result {
            if cond {
                f.write_str("(")?;
            }
            inner(f)?;
            if cond {
                f.write_str(")")?;
            }
            Ok(())
        }

        // Precedence:
        // - Index, SliceTail: 3
        // - Ref, Deref: 2
        // - CastUsize: 1

        match *self {
            Rewrite::Identity => write!(f, "$e"),
            Rewrite::Sub(i, _) => write!(f, "${}", i),
            Rewrite::Ref(ref rw, mutbl) => parenthesize_if(prec > 2, f, |f| {
                match mutbl {
                    Mutability::Not => write!(f, "&")?,
                    Mutability::Mut => write!(f, "&mut ")?,
                }
                rw.pretty(f, 2)
            }),
            Rewrite::AddrOf(ref rw, mutbl) => {
                match mutbl {
                    Mutability::Not => write!(f, "core::ptr::addr_of!")?,
                    Mutability::Mut => write!(f, "core::ptr::addr_of_mut!")?,
                }
                f.write_str("(")?;
                rw.pretty(f, 0)?;
                f.write_str(")")
            }
            Rewrite::Deref(ref rw) => parenthesize_if(prec > 2, f, |f| {
                write!(f, "*")?;
                rw.pretty(f, 2)
            }),
            Rewrite::Index(ref arr, ref idx) => parenthesize_if(prec > 3, f, |f| {
                arr.pretty(f, 3)?;
                write!(f, "[")?;
                idx.pretty(f, 0)?;
                write!(f, "]")
            }),
            Rewrite::SliceTail(ref arr, ref idx) => parenthesize_if(prec > 3, f, |f| {
                arr.pretty(f, 3)?;
                write!(f, "[")?;
                // Rather than figure out the right precedence for `..`, just force
                // parenthesization in this position.
                idx.pretty(f, 999)?;
                write!(f, " ..]")
            }),
            Rewrite::CastUsize(ref rw) => parenthesize_if(prec > 1, f, |f| {
                rw.pretty(f, 1)?;
                write!(f, " as usize")
            }),
            Rewrite::LitZero => write!(f, "0"),
        }
    }
}

impl fmt::Display for Rewrite {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.pretty(f, 0)
    }
}

pub fn apply_rewrites(tcx: TyCtxt, rewrites: Vec<(Span, Rewrite)>) {
    // TODO: emit new source code properly instead of just printing
    let new_src = apply::apply_rewrites(tcx.sess.source_map(), rewrites);

    for (filename, src) in new_src {
        eprintln!("\n\n ===== BEGIN {:?} =====", filename);
        for line in src.lines() {
            // Omit filecheck directives from the debug output, as filecheck can get confused due
            // to directives matching themselves (e.g. `// CHECK: foo` will match the `foo` in the
            // line `// CHECK: foo`).
            if let Some((pre, post)) = line.split_once("// CHECK") {
                eprintln!("{}// (FileCheck directive omitted)", pre);
            } else {
                eprintln!("{}", line);
            }
        }
        eprintln!(" ===== END {:?} =====", filename);
    }
}
