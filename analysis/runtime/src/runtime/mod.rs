pub mod backend;
pub mod global_runtime;
pub mod scoped_runtime;
pub mod skip;

use std::{error::Error, sync::{Mutex, Condvar}};

use once_cell::sync::Lazy;

type AnyError = Box<dyn Error + Send + Sync + 'static>;

static FINISHED: Lazy<(Mutex<bool>, Condvar)> = Lazy::new(|| (Mutex::new(false), Condvar::new()));
