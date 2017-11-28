
use renamer::Renamer;

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

    /// Lazily get a label for this loop
    pub fn get_label(&mut self, renamer: &mut Renamer<String>) -> &str {
        // FIXME: get the name from somewhere other than fresh()???
        self.label.get_or_insert_with(|| renamer.fresh())
    }
}

pub struct LoopContext {
    pub loops: Vec<Loop>,
}

impl LoopContext {
    pub fn new() -> LoopContext {
        LoopContext {
            loops: vec![],
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
}
