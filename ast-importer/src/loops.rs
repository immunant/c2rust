
pub enum LoopType {
    For,
    While,
    DoWhile,
}

pub struct Loop {
    pub loop_type: LoopType,
    pub label: Option<String>,
    pub has_break: bool,
    pub has_continue: bool,
}

impl Loop {
    pub fn new(lt: LoopType) -> Loop {
        Loop {
            loop_type: lt,
            label: None,
            has_break: false,
            has_continue: false,
        }
    }
}

pub struct LoopContext {
    pub loops: Vec<Loop>,

    /// Loop index returned by get_index()
    next_index: u64,
}

impl LoopContext {
    pub fn new() -> LoopContext {
        LoopContext {
            loops: vec![],
            next_index: 0,
        }
    }

    /// Push a new loop
    pub fn push_loop(&mut self, lt: LoopType) {
        self.loops.push(Loop::new(lt));
    }

    /// Pop the current loop off the stack and return it
    pub fn pop_loop(&mut self) -> Loop {
        self.loops.pop().expect("Expected valid loop to pop()")
    }

    pub fn current_loop(&self) -> &Loop {
        self.loops.last().expect("Expected valid loop")
    }

    pub fn current_loop_mut(&mut self) -> &mut Loop {
        self.loops.last_mut().expect("Expected valid loop")
    }

    pub fn current_loop_label(&mut self) -> String {
        if let Some(ref s) = self.current_loop().label {
            return s.clone();
        }
        {
            let loop_label = format!("'loop{}", self.next_index);
            self.next_index += 1;
            self.current_loop_mut().label = Some(loop_label);
        }
        self.current_loop().label.as_ref().unwrap().clone()
    }
}
