use crate::span::SOURCE_SPANS;

pub fn malloc(span: usize, size: u64, result: usize) {
    eprintln!("Recording a malloc ({:?}) of size {} at address 0x{:x}", SOURCE_SPANS[span], size, result);
}
pub fn free(span: usize, ptr: usize, _result: ()) {
    eprintln!("Recording a free ({:?}) of address 0x{:x}", SOURCE_SPANS[span], ptr);
}
pub fn calloc(span: usize, nmemb: u64, size: u64, result: usize) {
    eprintln!("Recording a calloc ({:?}) of {} members of size {} at address 0x{:x}", SOURCE_SPANS[span], nmemb, size, result);
}
pub fn realloc(span: usize, ptr: usize, size: u64, result: usize) {
    eprintln!("Recording a realloc ({:?}) of 0x{:x} to size {} at address 0x{:x}", SOURCE_SPANS[span], ptr, size, result);
}
pub fn reallocarray(span: usize, ptr: usize, nmemb: u64, size: u64, result: usize) {
    eprintln!("Recording a reallocarray ({:?}) of 0x{:x} to {} members of size {} at address 0x{:x}", SOURCE_SPANS[span], ptr, nmemb, size, result);
}


pub fn ptr_deref(span: usize, ptr: usize) {
    eprintln!("Recording a pointer dereference at {:?} of 0x{:x}", SOURCE_SPANS[span], ptr);
}

pub fn ptr_assign(span: usize, ptr: usize) {
    eprintln!("Recording a pointer assignment at {:?} of 0x{:x}", SOURCE_SPANS[span], ptr);
}

pub fn ptr_arg(span: usize, ptr: usize) {
    eprintln!("Recording a pointer argument at {:?} of 0x{:x}", SOURCE_SPANS[span], ptr);
}
