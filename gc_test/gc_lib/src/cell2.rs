use std::cell::UnsafeCell;

#[derive(Debug)]
pub struct Cell2<T>(UnsafeCell<T>);

impl<T> Cell2<T> {
    pub fn new(x: T) -> Cell2<T> {
        Cell2(UnsafeCell::new(x))
    }

    pub fn get(&self) -> T
    where T: SimpleClone {
        unsafe {
            (*self.0.get()).clone()
        }
    }

    pub fn set(&self, x: T) {
        unsafe {
            *self.0.get() = x;
        }
    }
}

impl<T: SimpleClone> Clone for Cell2<T> {
    fn clone(&self) -> Cell2<T> {
        Cell2::new(self.get())
    }
}

/// Trait to identify types whose `Clone` impl can't invoke `Cell2::set`.
pub unsafe trait SimpleClone: Clone {}

// TODO: strictly speaking, this is unsound, since `clone()` is not guaranteed to do the same thing
// as `Copy`.
unsafe impl<T: Copy> SimpleClone for T {}
