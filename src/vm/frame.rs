use crate::{code::Instructions, object::ClosureObject};

#[derive(Clone, Debug)]
pub struct Frame {
    pub cl: ClosureObject,
    pub ip: isize,
    pub bp: usize,
}

impl Frame {
    pub fn new(cl: ClosureObject, bp: usize) -> Self {
        Self { cl, ip: -1, bp }
    }

    pub fn instructions(&self) -> Instructions {
        self.cl.func.instructions.clone()
    }
}
