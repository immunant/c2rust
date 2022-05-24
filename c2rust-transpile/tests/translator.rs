use std::{
    io,
    process::{self, Command},
};

#[test]
fn test_translator() -> io::Result<()> {
    let status = Command::new("../scripts/test_translator.py")
        .args(&["../tests/"])
        .status()?;
    process::exit(status.code().unwrap_or_else(|| match status.success() {
        true => 0,
        false => 1,
    }));
}
