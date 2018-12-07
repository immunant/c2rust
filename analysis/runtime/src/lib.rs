pub fn malloc(size: u64, result: usize) {
    eprintln!("Recording a malloc of size {} at address 0x{:x}", size, result);
}
pub fn free(ptr: usize, result: ()) {
    eprintln!("Recording a free of address 0x{:x}", ptr);
}
pub fn calloc(nmemb: u64, size: u64, result: usize) {
    eprintln!("Recording a calloc of {} members of size {} at address 0x{:x}", nmemb, size, result);
}
pub fn realloc(ptr: usize, size: u64, result: usize) {
    eprintln!("Recording a realloc of 0x{:x} to size {} at address 0x{:x}", ptr, size, result);
}
pub fn reallocarray(ptr: usize, nmemb: u64, size: u64, result: usize) {
    eprintln!("Recording a reallocarray of 0x{:x} to {} members of size {} at address 0x{:x}", ptr, nmemb, size, result);
}
