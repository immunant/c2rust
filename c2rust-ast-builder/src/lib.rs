extern crate proc_macro2;
extern crate syn;
//extern crate quote;

mod builder;
pub use builder::{mk, properties, Builder, Make};
