#![feature(int_to_from_bytes)]

extern crate zstd;

use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::env;

pub fn main() -> Result<(), std::io::Error> {
    let mut out = io::BufWriter::new(io::stdout());
    for arg in env::args() {
        let file = File::open(arg)?;
        let mut reader = zstd::stream::Decoder::new(file)?;
        loop {
            let mut buf = [0u8; 9];
            if reader.read_exact(&mut buf).is_err() {
                break;
            }
            let mut val_buf = [0u8; 8];
            val_buf.copy_from_slice(&buf[1..]);
            let val = u64::from_le(u64::from_bytes(val_buf));
            writeln!(out, "XCHECK({0}):{1:}/0x{1:08x}", buf[0], val)?;
        }
    }
    Ok(())
}
