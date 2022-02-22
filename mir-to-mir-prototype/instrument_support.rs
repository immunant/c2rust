use std::fmt::Debug;
pub fn handle_call<T: Debug>(name: &'static str, args: T) {
    println!("[CALL] {}{:?}", name, args);
}
