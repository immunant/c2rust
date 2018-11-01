#![allow(non_camel_case_types)]
extern crate libc;
extern crate serde_cbor;

use serde_cbor::{Value, from_slice};
use std::collections::HashMap;
use std::ffi::{CString,CStr};
use std::io::Error;
use std::path::Path;
use std::slice;

pub mod clang_ast;

pub fn get_untyped_ast(file_path: &Path) -> Result<clang_ast::AstContext, Error> {
    let cbors = get_ast_cbors(&[file_path.to_str().unwrap()]);
    let buffer = cbors.values().next().unwrap();
    let items: Value = from_slice(&buffer[..]).unwrap();

    match clang_ast::process(items) {
        Ok(cxt) => Ok(cxt),
        Err(e) => panic!("{:#?}", e),
    }
}

fn get_ast_cbors(c_files: &[&str]) -> HashMap<String, Vec<u8>> {
    let mut res = 0;

    let mut args_owned = vec![CString::new("ast_extractor").unwrap()];

    for &c_file in c_files {
        args_owned.push(CString::new(c_file).unwrap())
    }

    let args_ptrs: Vec<*const libc::c_char> = args_owned.iter().map(|x| x.as_ptr()).collect();

    let hashmap;
    unsafe {
        let ptr = ast_extractor(args_ptrs.len() as libc::c_int, args_ptrs.as_ptr(), &mut res);
        hashmap = marshal_result(ptr);
        drop_export_result(ptr);
    }
    hashmap
}


include!(concat!(env!("OUT_DIR"), "/cppbindings.rs"));

extern "C" {
    // ExportResult *ast_extractor(int argc, char *argv[]);
    #[no_mangle]
    fn ast_extractor(argc: libc::c_int, argv: *const *const libc::c_char, res: *mut libc::c_int) -> *mut ExportResult;

    // void drop_export_result(ExportResult *result);
    #[no_mangle]
    fn drop_export_result(ptr: *mut ExportResult);
}

unsafe fn marshal_result(result: *const ExportResult) -> HashMap<String, Vec<u8>> {
    let mut output = HashMap::new();

    let n = (*result).entries as isize;
    for i in 0 .. n {
        let ref res = *result;

        // Convert name field
        let cname = CStr::from_ptr(*res.names.offset(i));
        let name = cname.to_str().unwrap().to_owned();

        // Convert CBOR bytes
        let csize = *res.sizes.offset(i);
        let cbytes = *res.bytes.offset(i);
        let bytes = slice::from_raw_parts(cbytes, csize);
        let mut v = Vec::new();
        v.extend_from_slice(bytes);

        output.insert(name, v);
    }
    output
}
