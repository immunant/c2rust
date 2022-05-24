extern crate proc_macro2;
extern crate syn;
//extern crate quote;

mod builder;
pub use crate::builder::{mk, properties, Builder, Make};
