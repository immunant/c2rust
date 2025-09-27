//! Miscellaneous utility functions.
use rustc_ast::*;
use rustc_span::Symbol;
use smallvec::SmallVec;

pub mod cursor;
pub mod dataflow;

/// Move the lone item out of a 1-element container.
pub trait Lone<T> {
    fn lone(self) -> T;
}

impl<T> Lone<T> for T {
    fn lone(self) -> T {
        self
    }
}

impl<T> Lone<T> for Vec<T> {
    fn lone(mut self) -> T {
        assert!(self.len() == 1);
        self.pop().unwrap()
    }
}

impl<T> Lone<T> for SmallVec<[T; 1]> {
    fn lone(mut self) -> T {
        assert!(self.len() == 1);
        self.pop().unwrap()
    }
}

// These were moved into Session as methods for some unknown reason
// upstream, so duplicate them as functions so we can call them directly.
pub fn contains_name(attrs: &[Attribute], name: Symbol) -> bool {
    attrs.iter().any(|item| item.has_name(name))
}

pub fn find_by_name(attrs: &[Attribute], name: Symbol) -> Option<&Attribute> {
    attrs.iter().find(|attr| attr.has_name(name))
}

pub fn first_attr_value_str_by_name(attrs: &[Attribute], name: Symbol) -> Option<Symbol> {
    attrs
        .iter()
        .find(|at| at.has_name(name))
        .and_then(|at| at.value_str())
}
