//! `AstEquiv` trait for checking equivalence of two ASTs.
use rustc_ast::format::{
    FormatArgPosition, FormatArgsPiece, FormatArgumentKind, FormatCount, FormatOptions,
    FormatPlaceholder,
};
use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::{DelimSpan, LazyAttrTokenStream, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_span::hygiene::SyntaxContext;
use rustc_span::source_map::{Span, Spanned};
use rustc_span::symbol::{Ident, Symbol};
use rustc_target::spec::abi::Abi;
use std::rc::Rc;
use thin_vec::ThinVec;

/// Trait for checking equivalence of AST nodes.  This is similar to `PartialEq`, but less strict,
/// as it ignores some fields that have no bearing on the semantics of the AST (particularly
/// `Span`s and `NodeId`s).
pub trait AstEquiv {
    fn ast_equiv(&self, other: &Self) -> bool;

    /// Checks for structural and name equivalence of AST nodes, while ignoring
    /// any difference in the names of C2Rust_Unnamed* types
    fn unnamed_equiv(&self, other: &Self) -> bool;
}

fn format_argument_kind_equiv(left: &FormatArgumentKind, right: &FormatArgumentKind) -> bool {
    match (left, right) {
        (FormatArgumentKind::Normal, FormatArgumentKind::Normal) => true,
        (FormatArgumentKind::Named(left), FormatArgumentKind::Named(right))
        | (FormatArgumentKind::Captured(left), FormatArgumentKind::Captured(right)) => {
            left.ast_equiv(right)
        }
        _ => false,
    }
}

fn format_arg_position_equiv(left: &FormatArgPosition, right: &FormatArgPosition) -> bool {
    left.index == right.index && left.kind == right.kind
}

fn format_count_equiv(left: &FormatCount, right: &FormatCount) -> bool {
    match (left, right) {
        (FormatCount::Literal(left), FormatCount::Literal(right)) => left == right,
        (FormatCount::Argument(left), FormatCount::Argument(right)) => {
            format_arg_position_equiv(left, right)
        }
        _ => false,
    }
}

fn optional_format_count_equiv(left: Option<&FormatCount>, right: Option<&FormatCount>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => format_count_equiv(left, right),
        (None, None) => true,
        _ => false,
    }
}

fn format_options_equiv(left: &FormatOptions, right: &FormatOptions) -> bool {
    optional_format_count_equiv(left.width.as_ref(), right.width.as_ref())
        && optional_format_count_equiv(left.precision.as_ref(), right.precision.as_ref())
        && left.alignment == right.alignment
        && left.fill == right.fill
        && left.sign == right.sign
        && left.alternate == right.alternate
        && left.zero_pad == right.zero_pad
        && left.debug_hex == right.debug_hex
}

fn format_placeholder_equiv(left: &FormatPlaceholder, right: &FormatPlaceholder) -> bool {
    format_arg_position_equiv(&left.argument, &right.argument)
        && left.format_trait == right.format_trait
        && format_options_equiv(&left.format_options, &right.format_options)
}

fn format_args_piece_equiv(left: &FormatArgsPiece, right: &FormatArgsPiece) -> bool {
    match (left, right) {
        (FormatArgsPiece::Literal(left), FormatArgsPiece::Literal(right)) => left == right,
        (FormatArgsPiece::Placeholder(left), FormatArgsPiece::Placeholder(right)) => {
            format_placeholder_equiv(left, right)
        }
        _ => false,
    }
}

/// Compare the non-expression structure of two parsed `format_args!` invocations, ignoring spans.
///
/// This includes all formatting semantics: literal pieces, placeholder positions, formatting
/// traits and options, and the kind and name of each argument.  Expression equivalence is left to
/// the caller so this can also guard rewrites that intentionally change argument expressions.
pub(crate) fn format_args_structure_equiv(left: &FormatArgs, right: &FormatArgs) -> bool {
    left.template.len() == right.template.len()
        && left
            .template
            .iter()
            .zip(&right.template)
            .all(|(left, right)| format_args_piece_equiv(left, right))
        && {
            let left = left.arguments.all_args();
            let right = right.arguments.all_args();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| format_argument_kind_equiv(&left.kind, &right.kind))
        }
}

impl AstEquiv for FormatArgs {
    fn ast_equiv(&self, other: &Self) -> bool {
        let left = self.arguments.all_args();
        let right = other.arguments.all_args();
        format_args_structure_equiv(self, other)
            && left
                .iter()
                .zip(right)
                .all(|(left, right)| left.expr.ast_equiv(&right.expr))
    }

    fn unnamed_equiv(&self, other: &Self) -> bool {
        let left = self.arguments.all_args();
        let right = other.arguments.all_args();
        format_args_structure_equiv(self, other)
            && left
                .iter()
                .zip(right)
                .all(|(left, right)| left.expr.unnamed_equiv(&right.expr))
    }
}

impl<'a, T: AstEquiv> AstEquiv for &'a T {
    fn ast_equiv(&self, other: &&'a T) -> bool {
        <T as AstEquiv>::ast_equiv(*self, *other)
    }
    fn unnamed_equiv(&self, other: &&'a T) -> bool {
        <T as AstEquiv>::unnamed_equiv(*self, *other)
    }
}

impl<T: AstEquiv + ?Sized> AstEquiv for P<T> {
    fn ast_equiv(&self, other: &P<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &P<T>) -> bool {
        <T as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv + ?Sized> AstEquiv for Box<T> {
    fn ast_equiv(&self, other: &Box<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &Box<T>) -> bool {
        <T as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv + ?Sized> AstEquiv for Rc<T> {
    fn ast_equiv(&self, other: &Rc<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &Rc<T>) -> bool {
        <T as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Spanned<T> {
    fn ast_equiv(&self, other: &Spanned<T>) -> bool {
        self.node.ast_equiv(&other.node)
    }
    fn unnamed_equiv(&self, other: &Spanned<T>) -> bool {
        self.node.unnamed_equiv(&other.node)
    }
}

impl<T: AstEquiv> AstEquiv for [T] {
    fn ast_equiv(&self, other: &[T]) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (l, r) in self.iter().zip(other.iter()) {
            if !l.ast_equiv(r) {
                return false;
            }
        }
        true
    }
    fn unnamed_equiv(&self, other: &[T]) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (l, r) in self.iter().zip(other.iter()) {
            if !l.unnamed_equiv(r) {
                return false;
            }
        }
        true
    }
}

impl<T: AstEquiv> AstEquiv for Vec<T> {
    fn ast_equiv(&self, other: &Vec<T>) -> bool {
        <[T] as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &Vec<T>) -> bool {
        <[T] as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for ThinVec<T> {
    fn ast_equiv(&self, other: &ThinVec<T>) -> bool {
        <[T] as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &ThinVec<T>) -> bool {
        <[T] as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Option<T> {
    fn ast_equiv(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref x), &Some(ref y)) => x.ast_equiv(y),
            (&None, &None) => true,
            (_, _) => false,
        }
    }
    fn unnamed_equiv(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref x), &Some(ref y)) => x.unnamed_equiv(y),
            (&None, &None) => true,
            (_, _) => false,
        }
    }
}

impl<A: AstEquiv, B: AstEquiv> AstEquiv for (A, B) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) && self.1.ast_equiv(&other.1)
    }
    fn unnamed_equiv(&self, other: &Self) -> bool {
        self.0.unnamed_equiv(&other.0) && self.1.unnamed_equiv(&other.1)
    }
}

impl<A: AstEquiv, B: AstEquiv, C: AstEquiv> AstEquiv for (A, B, C) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) && self.1.ast_equiv(&other.1) && self.2.ast_equiv(&other.2)
    }
    fn unnamed_equiv(&self, other: &Self) -> bool {
        self.0.unnamed_equiv(&other.0)
            && self.1.unnamed_equiv(&other.1)
            && self.2.unnamed_equiv(&other.2)
    }
}

// Implementations for specific AST types are auto-generated.
include!(concat!(env!("OUT_DIR"), "/ast_equiv_gen.inc.rs"));

impl AstEquiv for Ident {
    fn ast_equiv(&self, other: &Self) -> bool {
        // Exhaustiveness check
        match self {
            &Ident {
                name: ref _name,
                span: ref _span,
            } => {}
        }

        // Comparison
        match (self, other) {
            (
                &Ident {
                    name: ref name1,
                    span: ref span1,
                },
                &Ident {
                    name: ref name2,
                    span: ref span2,
                },
            ) => AstEquiv::ast_equiv(name1, name2) && AstEquiv::ast_equiv(span1, span2) && true,
        }
    }

    fn unnamed_equiv(&self, other: &Self) -> bool {
        (self.as_str().contains("C2Rust_Unnamed") && other.as_str().contains("C2Rust_Unnamed"))
            || self.ast_equiv(other)
    }
}
