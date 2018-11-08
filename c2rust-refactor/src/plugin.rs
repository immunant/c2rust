//! Simple plugin loading infrastructure.
//!
//! See PLUGINS.txt for more details on plugins.
use std::ffi::CString;
use std::mem;
use std::path::Path;
use libc::{dlopen, dlsym, RTLD_LAZY};

use command::Registry;

/// Find the named plugins in the search path, and pass `reg` to each of their `register_commands`
/// entry points.
pub fn load_plugins(search_path: &[String], plugins: &[String], reg: &mut Registry) {
    let sym_name = CString::new("register_commands").unwrap();

    for name in plugins {
        eprintln!("loading {}...", name);
        let mut found = false;
        for dir in search_path {
            let path_str = format!("{}/lib{}.so", dir, name);
            let path = Path::new(&path_str);
            if path.exists() {
                let c_path = CString::new(path_str.clone()).unwrap();
                unsafe {
                    let so = dlopen(c_path.as_ptr(), RTLD_LAZY);
                    if so.is_null() {
                        panic!("failed to open plugin `{}`", path_str);
                    }
                    let sym = dlsym(so, sym_name.as_ptr());
                    if sym.is_null() {
                        panic!("failed to locate symbol `register_commands` in `{}`", path_str);
                    }
                    let f: fn(&mut Registry) = mem::transmute(sym);
                    f(reg);
                }

                found = true;
                break;
            }
        }

        if !found {
            panic!("plugin `{}` was not found in search path ({:?})", name, search_path);
        }
    }
}
