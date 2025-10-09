pub mod backend;
pub mod global_runtime;
pub mod scoped_runtime;
pub mod skip;

use std::{
    error::Error,
    sync::{Condvar, Mutex},
};

use once_cell::sync::Lazy;

type AnyError = Box<dyn Error + Send + Sync + 'static>;

pub trait Detect: Sized {
    fn detect() -> Result<Self, AnyError>;
}

static FINISHED: Lazy<(Mutex<bool>, Condvar)> = Lazy::new(|| (Mutex::new(false), Condvar::new()));
