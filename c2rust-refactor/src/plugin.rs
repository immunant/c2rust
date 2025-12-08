//! Simple plugin loading infrastructure.
//!
//! See PLUGINS.txt for more details on plugins.
use std::ffi::CString;
use std::path::Path;

use crate::command::Registry;

/// Find the named plugins in the search path, and pass `reg` to each of their `register_commands`
/// entry points.
pub fn load_plugins(search_path: &[String], plugins: &[String], reg: &mut Registry) {
    for name in plugins {
        eprintln!("loading {}...", name);
        let mut found = false;
        for dir in search_path {
            let path_str = format!("{}/lib{}.so", dir, name);
            let path = Path::new(&path_str);
            if path.exists() {
                unsafe {
                    let lib = libloading::Library::new(path_str.clone());
                    if lib.is_err() {
                        panic!("failed to open plugin `{}`", path_str);
                    }
                    let lib = lib.unwrap();
                    let f: Result<libloading::Symbol<fn(&mut Registry)>, libloading::Error> = lib.get(b"register_commands");
                    if f.is_err() {
                        panic!(
                            "failed to locate symbol `register_commands` in `{}`",
                            path_str
                        );
                    }
                    let f = f.unwrap();
                    f(reg);
                }

                found = true;
                break;
            }
        }

        if !found {
            panic!(
                "plugin `{}` was not found in search path ({:?})",
                name, search_path
            );
        }
    }
}
