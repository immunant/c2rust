struct S {
    x: u32,
}

impl S {
    fn by_val(self) -> u32 {
        self.x
    }
    fn by_ref(&self) -> u32 {
        self.x
    }
    fn by_mut_ref(&mut self, new_x: u32) {
        self.x = new_x;
    }
    fn by_ref_with_lt<'a>(&'a self) -> &'a u32 {
        &self.x
    }
    fn static_method(x: i32, y: i32) -> i32 {
        x + y
    }
}

fn main() {
    let mut s = S { x: 0 };
    println!("by_ref: {}", (&s).by_ref());
    (&mut s).by_mut_ref(1);
    println!("by_ref_with_lt: {}", (&s).by_ref_with_lt());
    println!("by_val: {}", s.by_val());
    println!("static_method: {}", crate::S::static_method(100, -100));
}
