use crate::{code::Instructions, object::Closure};

#[derive(Clone, Debug)]
pub struct Frame {
    pub cl: Closure,
    pub ip: isize,
    pub bp: usize,
}

impl Frame {
    pub fn new(cl: Closure, bp: usize) -> Self {
        Self { cl, ip: -1, bp }
    }

    pub fn instructions(&self) -> Instructions {
        self.cl.func.instructions.clone()
    }
}
