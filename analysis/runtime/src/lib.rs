pub fn malloc(size: u64, result: usize) {
    eprintln!("Recording a malloc of size {} at address 0x{:x}", size, result);
}
pub fn realloc(ptr: usize, size: u64, result: usize) {
    eprintln!("Recording a realloc of 0x{:x} to size {} at address 0x{:x}", ptr, size, result);
}
}
