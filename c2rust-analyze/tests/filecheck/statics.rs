static unused: usize = 2;
static mut unused_mut: usize = 6;
static read: usize = 9;
static mut read_mut: usize = 21;
static mut written_mut: usize = 3;

fn main() {
    let x = read;
    if x > unsafe { read_mut } {
         unsafe { written_mut = 6000; }
    }
}
