#![feature(libc, int_to_from_bytes)]

#[macro_use] extern crate lazy_static;
extern crate libc;
extern crate zstd;

use std::env;
use std::fs::File;
use std::io::Write;
use std::sync::Mutex;

type XCheckWriter = zstd::stream::Encoder<File>;

lazy_static! {
    static ref RB_XCHECK_MUTEX: Mutex<Option<XCheckWriter>> = {
        extern fn cleanup() {
            // Flush and close the file on program exit
            let mut guard = RB_XCHECK_MUTEX.lock().unwrap();
            let out = guard.take().unwrap();
            out.finish().expect("Failed to finish encoding");
        }
        unsafe { libc::atexit(cleanup) };

        let xchecks_file = env::var("FAKECHECKS_OUTPUT_FILE")
            .expect("Expected file path in FAKECHECKS_OUTPUT_FILE variable");
        let file = File::create(xchecks_file.clone())
            .expect(&format!("Failed to create fakechecks file: {}", xchecks_file));
        let encoder = zstd::stream::Encoder::new(file, 0)
            .expect("Failed to create zstd encoder");
        Mutex::new(Some(encoder))
    };
}

#[no_mangle]
pub extern fn rb_xcheck(tag: u8, val: u64) {
    let mut guard = RB_XCHECK_MUTEX.lock().unwrap();
    let out = guard.as_mut().unwrap();
    out.write(&[tag]).expect("Failed to write tag");
    out.write(&val.to_le().to_bytes()).expect("Failed to write value");
}

