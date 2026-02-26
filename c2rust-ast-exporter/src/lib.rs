use serde_cbor::{from_slice, Value};
use std::collections::HashMap;
use std::ffi::{c_char, c_int, CStr, CString};
use std::io::{Error, ErrorKind};
use std::path::Path;
use std::slice;
use std::sync::Mutex;

use crate::clang_ast::BuiltinVaListKind;

pub mod clang_ast;

impl Default for BuiltinVaListKind {
    fn default() -> Self {
        Self::CharPtrBuiltinVaList
    }
}

pub fn get_clang_major_version() -> Option<u32> {
    let s = unsafe { CStr::from_ptr(clang_version()) };
    s.to_str()
        .unwrap()
        .split('.')
        .next()
        .unwrap()
        .parse::<u32>()
        .ok()
}

pub fn get_untyped_ast(
    file_path: &Path,
    cc_db: &Path,
    extra_args: &[&str],
    debug: bool,
) -> Result<clang_ast::AstContext, Error> {
    let cbors = get_ast_cbors(file_path, cc_db, extra_args, debug);
    let buffer = cbors
        .values()
        .next()
        .ok_or_else(|| Error::new(ErrorKind::InvalidData, "Could not parse input file"))?;

    // let cbor_path = file_path.with_extension("cbor");
    // let mut cbor_file = File::create(&cbor_path)?;
    // cbor_file.write_all(&buffer[..])?;
    // eprintln!("Dumped CBOR to {}", cbor_path.to_string_lossy());

    let items: Value = from_slice(&buffer[..]).unwrap();

    clang_ast::process(items).map_err(|e| Error::new(ErrorKind::InvalidData, format!("{}", e)))
}

/// libClangTooling is not thread-safe, so we must not allow concurrent calls to `ast_exporter`.
static CLANG_MUTEX: Mutex<()> = Mutex::new(());

fn get_ast_cbors(
    file_path: &Path,
    cc_db: &Path,
    extra_args: &[&str],
    debug: bool,
) -> HashMap<String, Vec<u8>> {
    let mut res = 0;

    let mut args_owned = vec![CString::new("ast_exporter").unwrap()];
    args_owned.push(CString::new(file_path.to_str().unwrap()).unwrap());
    args_owned.push(CString::new("-p").unwrap());
    args_owned.push(CString::new(cc_db.to_str().unwrap()).unwrap());

    for &arg in extra_args {
        args_owned.push(CString::new(["-extra-arg=", arg].join("")).unwrap())
    }

    let args_ptrs: Vec<*const c_char> = args_owned.iter().map(|x| x.as_ptr()).collect();

    let hashmap;
    unsafe {
        let lock = CLANG_MUTEX.lock().unwrap();
        let ptr = ast_exporter(
            args_ptrs.len() as c_int,
            args_ptrs.as_ptr(),
            debug.into(),
            &mut res,
        );
        drop(lock);
        hashmap = marshal_result(ptr);
        drop_export_result(ptr);
    }
    hashmap
}

#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
#[allow(dead_code)]
mod ffi {
    include!(concat!(env!("OUT_DIR"), "/cppbindings.rs"));
}

extern "C" {
    // Safety: Not thread-safe; must not be called multiple times concurrently.
    // ExportResult *ast_exporter(int argc, char *argv[]);
    fn ast_exporter(
        argc: c_int,
        argv: *const *const c_char,
        debug: c_int,
        res: *mut c_int,
    ) -> *mut ffi::ExportResult;

    // void drop_export_result(ExportResult *result);
    fn drop_export_result(ptr: *mut ffi::ExportResult);

    fn clang_version() -> *const c_char;
}

unsafe fn marshal_result(result: *const ffi::ExportResult) -> HashMap<String, Vec<u8>> {
    let mut output = HashMap::new();

    let n = (*result).entries as isize;
    for i in 0..n {
        let res = &*result;

        // Convert name field
        let cname = CStr::from_ptr(*res.names.offset(i));
        let name = cname.to_str().unwrap().to_owned();

        // Convert CBOR bytes
        let csize = *res.sizes.offset(i);
        let cbytes = *res.bytes.offset(i);
        #[allow(clippy::unnecessary_cast /*, reason = "needed on x86_64-unknown-linux-gnu" */)]
        let bytes = slice::from_raw_parts(cbytes, csize as usize);
        let mut v = Vec::new();
        v.extend_from_slice(bytes);

        output.insert(name, v);
    }
    output
}
