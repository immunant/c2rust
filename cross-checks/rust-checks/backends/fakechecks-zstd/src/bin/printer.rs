#![feature(int_to_from_bytes)]

extern crate zstd;

use std::fs::File;
use std::io::Read;
use std::env;

pub fn main() -> Result<(), std::io::Error> {
    for arg in env::args() {
        let file = File::open(arg)?;
        let mut reader = zstd::stream::Decoder::new(file)?;
        loop {
            let mut tag_buf = [0u8; 1];
            if reader.read_exact(&mut tag_buf).is_err() {
                break;
            }
            let mut val_buf = [0u8; 8];
            if reader.read_exact(&mut val_buf).is_err() {
                break;
            }
            let val = u64::from_le(u64::from_bytes(val_buf));
            println!("XCHECK({0}):{1:}/0x{1:08x}", tag_buf[0], val);
        }
    }
    Ok(())
}
