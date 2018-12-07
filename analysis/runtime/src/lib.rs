pub fn malloc(size: usize, address: usize) {
    eprintln!("Recording a malloc of size {} at address 0x{:x}", size, address);
}
pub fn realloc(orig: usize, size: usize, address: usize) {
    eprintln!("Recording an realloc of 0x{:x} to size {} at address 0x{:x}", orig, size, address);
}
