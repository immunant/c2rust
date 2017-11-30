
use std::cell::{Cell, RefCell, Ref, RefMut};

pub enum LoopType {
    For,
    While,
    DoWhile,
}

pub struct Loop {
    pub loop_type: LoopType,
    pub label: Option<String>,
    pub body_label: Option<String>,
    pub has_break: bool,
    pub has_continue: bool,
}

impl Loop {
    pub fn new(lt: LoopType) -> Loop {
        Loop {
            loop_type: lt,
            label: None,
            body_label: None,
            has_break: false,
            has_continue: false,
        }
    }

    pub fn get_or_create_label(&mut self, ctx: &LoopContext) -> String {
        self.label.get_or_insert_with(|| {
            format!("'loop{}", ctx.loop_index.get())
        }).clone()
    }

    pub fn get_or_create_body_label(&mut self, ctx: &LoopContext) -> String {
        self.body_label.get_or_insert_with(|| {
            format!("'body{}", ctx.body_index.get())
        }).clone()
    }
}

struct Counter(Cell<u64>);

impl Counter {
    fn new() -> Counter {
        Counter(Cell::new(0))
    }

    fn get(&self) -> u64 {
        let next = self.0.get();
        self.0.set(next + 1);
        next
    }
}

pub struct LoopContext {
    loops: RefCell<Vec<Loop>>,

    /// Loop index used for loop labels
    loop_index: Counter,

    /// Loop body index
    body_index: Counter,
}

impl LoopContext {
    pub fn new() -> LoopContext {
        LoopContext {
            loops: RefCell::new(vec![]),
            loop_index: Counter::new(),
            body_index: Counter::new(),
        }
    }

    /// Push a new loop
    pub fn push_loop(&self, lt: LoopType) {
        self.loops.borrow_mut().push(Loop::new(lt));
    }

    /// Pop the current loop off the stack and return it
    pub fn pop_loop(&self) -> Loop {
        self.loops.borrow_mut().pop().expect("Expected valid loop to pop()")
    }

    pub fn current_loop(&self) -> Ref<Loop> {
        Ref::map(self.loops.borrow(), |l| l.last().expect("Expected valid loop"))
    }

    pub fn current_loop_mut(&self) -> RefMut<Loop> {
        RefMut::map(self.loops.borrow_mut(), |l| l.last_mut().expect("Expected valid loop"))
    }
}
