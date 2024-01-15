use hashbrown::HashMap;

use crate::{
    code::{self, Instructions, Opcode},
    compiler::Bytecode,
    object::{
        builtins::BUILTINS, Array, Bool, Builtin, BuiltinFunction, Char, Closure, CompiledFunction,
        Dict, Error, Float, HashPair, Hashable, Int, Iter, Iterable, Object, Range, Str,
    },
};

#[cfg(test)]
mod tests;

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

const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

const TRUE: Object = Object::Bool(Bool { value: true });
const FALSE: Object = Object::Bool(Bool { value: false });
const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct VirtualMachine {
    constants: Vec<Object>,
    globals: Vec<Object>,

    stack: Vec<Object>,
    sp: usize,

    pub last_popped_stack_elem: Option<Object>,

    frames: Vec<Frame>,
    frames_index: usize,
}

impl VirtualMachine {
    pub fn new(bytecode: &Bytecode) -> Self {
        let main_fn = CompiledFunction {
            instructions: bytecode.instructions.clone(),
            num_locals: 0,
            num_parameters: 0,
        };

        let main_closure = Closure {
            func: main_fn,
            free: Vec::new(),
        };
        let main_frame = Frame::new(main_closure, 0);

        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        Self {
            constants: bytecode.constants.clone(),

            stack: std::vec::from_elem(Object::Null, STACK_SIZE),
            sp: 0,

            globals: Vec::with_capacity(GLOBAL_SIZE),
            last_popped_stack_elem: None,

            frames,
            frames_index: 1,
        }
    }

    pub fn new_with_global_store(bytecode: &Bytecode, s: &[Object]) -> Self {
        let main_fn = CompiledFunction {
            instructions: bytecode.instructions.clone(),
            num_locals: 0,
            num_parameters: 0,
        };

        let main_closure = Closure {
            func: main_fn,
            free: Vec::new(),
        };
        let main_frame = Frame::new(main_closure, 0);

        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        Self {
            constants: bytecode.constants.clone(),

            stack: std::vec::from_elem(Object::Null, STACK_SIZE),
            sp: 0,

            globals: s.to_vec(),
            last_popped_stack_elem: None,

            frames,
            frames_index: 1,
        }
    }

    pub fn stack_top(&self) -> Option<Object> {
        if self.sp == 0 {
            None
        } else {
            Some(self.stack[self.sp - 1].clone())
        }
    }

    pub fn get_globals(&self) -> Vec<Object> {
        self.globals.clone()
    }

    pub fn get_stack(&self) -> Vec<Object> {
        self.stack.clone()
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut ip;
        let mut ins;
        let mut op;

        while self.current_frame().ip < (self.current_frame().instructions().len() - 1) as isize {
            self.current_frame().ip += 1;

            ip = self.current_frame().ip as usize;
            ins = self.current_frame().instructions();

            op = TryInto::<Opcode>::try_into(ins[ip])?;

            match op {
                Opcode::Constant => {
                    let const_idx = code::read_uint16(&ins, ip + 1);
                    self.current_frame().ip += 2;

                    self.push(self.constants[const_idx].clone())?;
                }

                Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::Div
                | Opcode::BitXor
                | Opcode::BitAnd
                | Opcode::BitOr
                | Opcode::Shl
                | Opcode::Shr => {
                    self.execute_binary_operation(op)?;
                }

                Opcode::Pop => {
                    self.pop();
                }

                Opcode::PopNoRet => {
                    self.pop();
                    self.last_popped_stack_elem = None;
                }

                Opcode::True => self.push(TRUE)?,
                Opcode::False => self.push(FALSE)?,
                Opcode::Null => self.push(NULL)?,

                Opcode::Equal
                | Opcode::GreaterThan
                | Opcode::GreaterThanEqual
                | Opcode::NotEqual => {
                    self.execute_comparison(op)?;
                }

                Opcode::Bang => {
                    self.execute_bang_operator()?;
                }

                Opcode::Minus => {
                    self.execute_minus_operator()?;
                }

                Opcode::And | Opcode::Or => {
                    self.execute_boolean_operator(op)?;
                }

                Opcode::Jump => {
                    let pos = code::read_uint16(&ins, ip + 1);
                    self.current_frame().ip = (pos - 1) as isize;
                }

                Opcode::JumpNotTruthy => {
                    let pos = code::read_uint16(&ins, ip + 1);
                    self.current_frame().ip += 2;

                    let condition = self.pop();
                    if !is_truthy(&condition) {
                        self.current_frame().ip = (pos - 1) as isize;
                    }
                }

                Opcode::SetGlobal => {
                    let global_idx = code::read_uint16(&ins, ip + 1);
                    self.current_frame().ip += 2;

                    let obj = self.pop();

                    if global_idx >= self.globals.len() {
                        self.globals.push(obj);
                    } else {
                        self.globals[global_idx] = obj;
                    }
                }

                Opcode::GetGlobal => {
                    let global_idx = code::read_uint16(&ins, ip + 1);
                    self.current_frame().ip += 2;

                    let obj = self.globals[global_idx].clone();

                    self.push(obj)?;
                }

                Opcode::Array => {
                    let num_elements = code::read_uint16(&ins, ip + 1);
                    self.current_frame().ip += 2;

                    let mut elements = Vec::new();
                    for _ in 0..num_elements {
                        elements.push(self.pop());
                    }

                    elements.reverse();
                    self.push(Object::Array(Array { elements }))?;
                }

                Opcode::Hash => {
                    let num_pairs = code::read_uint16(&ins, ip + 1);
                    self.exec_hash_literal(num_pairs)?;
                }

                Opcode::Index => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(&left, &index)?;
                }

                Opcode::Range => {
                    let has_step = code::read_bool(&ins, ip + 1);
                    self.current_frame().ip += 1;

                    self.exec_range(has_step)?;
                }

                Opcode::Call => {
                    let num_args = code::read_uint8(&ins, ip + 1);
                    self.current_frame().ip += 1;

                    self.exec_call(num_args)?;
                }

                Opcode::ReturnValue => {
                    let return_value = self.pop();

                    let frame = self.pop_frame();
                    self.sp = frame.bp - 1;

                    self.push(return_value)?;
                }

                Opcode::Return => {
                    let frame = self.pop_frame();
                    self.sp = frame.bp - 1;

                    self.push(NULL)?;
                }

                Opcode::SetLocal => {
                    let local_index = code::read_uint8(&ins, ip + 1);
                    self.current_frame().ip += 1;

                    let base_pointer = self.current_frame().bp;

                    self.stack[base_pointer + local_index] = self.pop();
                }

                Opcode::GetLocal => {
                    let local_index = code::read_uint8(&ins, ip + 1);
                    self.current_frame().ip += 1;

                    let base_pointer = self.current_frame().bp;
                    let obj = self.stack[base_pointer + local_index].clone();

                    self.push(obj)?;
                }

                Opcode::GetBuiltin => {
                    let builtin_idx = code::read_uint8(&ins, ip + 1);
                    self.current_frame().ip += 1;

                    let (name, func) = BUILTINS[builtin_idx];

                    self.push(Object::Builtin(Builtin {
                        name: name.to_owned(),
                        func,
                        caller: None,
                    }))?;
                }

                Opcode::Closure => {
                    let const_idx = code::read_uint16(&ins, ip + 1);
                    let num_free = code::read_uint8(&ins, ip + 3);

                    self.current_frame().ip += 3;

                    self.push_closure(const_idx, num_free)?;
                }

                Opcode::GetFree => {
                    let free_idx = code::read_uint8(&ins, ip + 1);
                    self.current_frame().ip += 1;

                    let current_closure = self.current_frame().cl.clone();

                    self.push(current_closure.free[free_idx].clone())?;
                }

                Opcode::CurrentClosure => {
                    let current_closure = self.current_frame().cl.clone();
                    self.push(Object::Closure(current_closure))?;
                }

                Opcode::Dup => {
                    self.dup()?;
                }

                Opcode::Method => {
                    let method_idx = code::read_uint8(&ins, ip + 1);
                    let has_arguments = code::read_bool(&ins, ip + 2);
                    let num_args = code::read_uint8(&ins, ip + 3);

                    self.current_frame().ip += 3;

                    self.exec_method_expression(num_args, method_idx, has_arguments)?;
                }

                Opcode::Start => {
                    let iter_obj = self.pop();

                    let iter = Iterable::from_object(iter_obj.clone())
                        .ok_or_else(|| format!("{} is not iterable", iter_obj.kind()))?;

                    self.push(Object::Iter(Iter {
                        size: iter.count(),
                        expr: iter,
                        current: 0,
                    }))?;
                }

                Opcode::Next => {
                    let Object::Iter(iter) = self.pop() else {
                        return Err("Object is not an iterator".to_string())?;
                    };

                    self.push(Object::Iter(Iter {
                        current: iter.current + 1,
                        ..iter.clone()
                    }))?;

                    self.push(iter.expr.get(iter.current))?;
                }

                Opcode::JumpEnd => {
                    let jump_pos = code::read_uint16(&ins, ip + 1);
                    let symbol_idx = code::read_uint16(&ins, ip + 3);
                    self.current_frame().ip += 4;

                    let Object::Iter(iter) = self.stack_top().unwrap() else {
                        return Err("Object is not an iterator".to_string())?;
                    };

                    if iter.current >= iter.size {
                        self.pop();
                        self.current_frame().ip = (jump_pos - 1) as isize;
                        self.globals.remove(symbol_idx);
                    }
                }

                Opcode::Delete => {
                    let index = code::read_uint16(&ins, ip + 1);
                    self.current_frame().ip += 2;

                    self.last_popped_stack_elem = Some(self.globals.remove(index));
                }

                _ => todo!(),
            }
        }

        Ok(())
    }

    fn exec_hash_literal(&mut self, num_pairs: usize) -> Result<(), String> {
        self.current_frame().ip += 2;
        let mut pairs = HashMap::new();
        for _ in 0..num_pairs {
            let value = self.pop();
            let key = self.pop();

            let hashable = Hashable::from_object(&key)
                .ok_or_else(|| format!("unusable as hash key: {}", key.kind()))?;

            pairs.insert(
                hashable.hash_key(),
                HashPair {
                    key: hashable,
                    value,
                },
            );
        }
        self.push(Object::Dict(Dict { pairs }))?;
        Ok(())
    }

    fn exec_method_expression(
        &mut self,
        num_args: usize,
        method_idx: usize,
        has_arguments: bool,
    ) -> Result<(), String> {
        let mut args = Vec::new();
        for _ in 0..num_args {
            args.push(self.pop());
        }

        args.reverse();
        let caller = self.pop();
        let ret = caller.call_method(
            u8::try_from(method_idx).unwrap(),
            has_arguments.then_some(&args),
        );
        self.push(ret)?;
        Ok(())
    }

    fn exec_range(&mut self, has_step: bool) -> Result<(), String> {
        let end = self.pop();
        let start = self.pop();
        let Object::Int(Int { value: start }) = start else {
            return Err(format!(
                "cannot use {} as step in range. expected: INT",
                start.kind()
            ));
        };
        let Object::Int(Int { value: end }) = end else {
            return Err(format!(
                "cannot use {} as step in range. expected: INT",
                end.kind()
            ));
        };
        let step = if has_step {
            if start > end {
                -1
            } else {
                1
            }
        } else {
            let step = self.pop();

            let Object::Int(Int { value: step }) = step else {
                return Err(format!(
                    "cannot use {} as step in range. expected: INT",
                    step.kind()
                ));
            };

            step
        };
        self.push(Object::Range(Range { start, end, step }))?;

        Ok(())
    }

    fn push(&mut self, o: Object) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            return Err("stack overflow".to_string());
        }

        self.stack[self.sp] = o;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Object {
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        self.last_popped_stack_elem = Some(obj.clone());

        obj
    }

    fn dup(&mut self) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            return Err("stack overflow".to_string());
        }

        self.stack[self.sp] = self.stack[self.sp - 1].clone();
        self.sp += 1;

        Ok(())
    }

    fn current_frame(&mut self) -> &mut Frame {
        self.frames.get_mut(self.frames_index - 1).unwrap()
    }

    fn push_frame(&mut self, f: Frame) {
        self.frames.push(f);
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        self.frames.pop().unwrap()
    }

    fn push_closure(&mut self, const_idx: usize, num_free: usize) -> Result<(), String> {
        let constant = self.constants[const_idx].clone();

        if let Object::CompiledFunction(func) = constant {
            let mut free = Vec::with_capacity(num_free);
            for i in 0..num_free {
                free.push(self.stack[self.sp - num_free + i].clone());
            }

            self.sp -= num_free;

            self.push(Object::Closure(Closure { func, free }))
        } else {
            Err(format!("not a function: {constant:#?}"))
        }
    }
}

impl VirtualMachine {
    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();

        match (&left, &right) {
            (Object::Int(Int { value: left_value }), Object::Int(Int { value: right_value })) => {
                self.execute_binary_int_operation(op, *left_value, *right_value)
            }
            (
                Object::Float(Float { value: left_value }),
                Object::Float(Float { value: right_value }),
            ) => self.execute_binary_float_operation(op, *left_value, *right_value),
            (Object::Str(Str { value: left_value }), Object::Str(Str { value: right_value })) => {
                self.execute_binary_string_operation(op, left_value, right_value)
            }
            (Object::Str(Str { value: left_value }), Object::Char(Char { value: right_value })) => {
                self.execute_binary_char_operation(op, left_value, *right_value)
            }
            (
                Object::Char(Char { value: left_value }),
                Object::Char(Char { value: right_value }),
            ) => self.execute_binary_char_operation(op, &left_value.to_string(), *right_value),
            _ => {
                return Err(format!(
                    "unsupported types for binary operation: {} {op} {}",
                    left.kind(),
                    right.kind()
                ))
            }
        }?;

        Ok(())
    }

    fn execute_binary_int_operation(
        &mut self,
        op: Opcode,
        left: isize,
        right: isize,
    ) -> Result<(), String> {
        let value = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            Opcode::BitXor => left ^ right,
            Opcode::BitAnd => left & right,
            Opcode::BitOr => left | right,
            Opcode::Shr => left >> right,
            Opcode::Shl => left << right,
            _ => return Err(format!("unknown integer operation: {op}")),
        };

        self.push(Object::Int(Int { value }))
    }

    fn execute_binary_float_operation(
        &mut self,
        op: Opcode,
        left: f64,
        right: f64,
    ) -> Result<(), String> {
        let value = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => return Err(format!("unknown float operation: {op}")),
        };

        self.push(Object::Float(Float { value }))
    }

    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: &str,
        right: &str,
    ) -> Result<(), String> {
        if op != Opcode::Add {
            return Err(format!("unknown string operation: {op}"));
        }

        self.push(Object::Str(Str {
            value: [left, right].concat(),
        }))
    }

    fn execute_binary_char_operation(
        &mut self,
        op: Opcode,
        left: &str,
        right: char,
    ) -> Result<(), String> {
        if op != Opcode::Add {
            return Err(format!("unknown string operation: {op}"));
        }

        self.push(Object::Str(Str {
            value: [left, &right.to_string()].concat(),
        }))
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();

        match (&left, &right) {
            (Object::Int(Int { value: left_value }), Object::Int(Int { value: right_value })) => {
                self.execute_int_comparison(op, *left_value, *right_value)
            }
            (
                Object::Float(Float { value: left_value }),
                Object::Float(Float { value: right_value }),
            ) => self.execute_float_comparison(op, *left_value, *right_value),
            (
                Object::Char(Char { value: left_value }),
                Object::Char(Char { value: right_value }),
            ) => self.execute_char_comparison(op, *left_value, *right_value),
            _ => match op {
                Opcode::Equal => self.push(if left == right { TRUE } else { FALSE }),
                Opcode::NotEqual => self.push(if left == right { FALSE } else { TRUE }),
                _ => {
                    return Err(format!(
                        "unknown operator: {} ({} {})",
                        op,
                        left.kind(),
                        right.kind()
                    ))
                }
            },
        }?;

        Ok(())
    }

    fn execute_int_comparison(
        &mut self,
        op: Opcode,
        left: isize,
        right: isize,
    ) -> Result<(), String> {
        let value = match op {
            Opcode::Equal => left == right,
            Opcode::NotEqual => left != right,
            Opcode::GreaterThan => left > right,
            Opcode::GreaterThanEqual => left >= right,
            _ => return Err(format!("unknown operator: {op}")),
        };

        self.push(if value { TRUE } else { FALSE })
    }

    fn execute_float_comparison(
        &mut self,
        op: Opcode,
        left: f64,
        right: f64,
    ) -> Result<(), String> {
        let value = match op {
            Opcode::Equal => (left - right).abs() < f64::EPSILON,
            Opcode::NotEqual => (left - right).abs() > f64::EPSILON,
            Opcode::GreaterThan => left > right,
            Opcode::GreaterThanEqual => left >= right,
            _ => return Err(format!("unknown operator: {op}")),
        };

        self.push(if value { TRUE } else { FALSE })
    }

    fn execute_char_comparison(
        &mut self,
        op: Opcode,
        left: char,
        right: char,
    ) -> Result<(), String> {
        let value = match op {
            Opcode::Equal => left == right,
            Opcode::NotEqual => left != right,
            Opcode::GreaterThan => left > right,
            Opcode::GreaterThanEqual => left >= right,
            _ => return Err(format!("unknown operator: {op}")),
        };

        self.push(if value { TRUE } else { FALSE })
    }

    fn execute_bang_operator(&mut self) -> Result<(), String> {
        let operand = self.pop();

        if is_truthy(&operand) {
            self.push(FALSE)
        } else {
            self.push(TRUE)
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), String> {
        let operand = self.pop();

        if let Object::Int(Int { value }) = operand {
            self.push(Object::Int(Int { value: -value }))
        } else if let Object::Float(Float { value }) = operand {
            self.push(Object::Float(Float { value: -value }))
        } else {
            Err(format!("unsupported type for negation: {}", operand.kind()))
        }
    }

    fn execute_boolean_operator(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();

        let result = match op {
            Opcode::And => {
                if is_truthy(&left) {
                    right
                } else {
                    left
                }
            }
            Opcode::Or => {
                if is_truthy(&left) {
                    left
                } else {
                    right
                }
            }
            _ => unreachable!(),
        };

        self.push(result)
    }

    fn execute_index_expression(&mut self, left: &Object, index: &Object) -> Result<(), String> {
        match (left, index) {
            (Object::Array(Array { elements }), Object::Int(Int { value })) => {
                self.exec_array_index_expression(elements, *value)?;
            }
            (Object::Str(Str { value: left }), Object::Int(Int { value })) => {
                self.exec_string_index_expression(left, *value)?;
            }
            (Object::Array(Array { elements }), Object::Range(Range { start, end, step })) => {
                self.exec_array_slice_expression(elements, *start, *end, *step)?;
            }
            (Object::Str(Str { value }), Object::Range(Range { start, end, step })) => {
                self.exec_string_slice_expression(value, *start, *end, *step)?;
            }
            (Object::Dict(Dict { pairs }), _) => {
                self.exec_hash_index_expression(pairs, index)?;
            }
            _ => {
                return Err(format!(
                    "index operator not supported: {}[{}]",
                    left.kind(),
                    index.kind()
                ))
            }
        }

        Ok(())
    }

    fn exec_array_index_expression(&mut self, array: &[Object], idx: isize) -> Result<(), String> {
        let max: isize = TryInto::<isize>::try_into(array.len()).unwrap();

        if idx >= max || idx < -max {
            return Err(format!("index out of bounds. got: {idx}"));
        }

        self.push(array[normalize_index(idx, max)].clone())
    }

    fn exec_string_index_expression(&mut self, string: &str, idx: isize) -> Result<(), String> {
        let max = TryInto::<isize>::try_into(string.len()).unwrap();

        if idx >= max || idx < -max {
            return Err(format!("index out of bounds. got: {idx}"));
        }

        self.push(Object::Char(Char {
            value: string.chars().nth(normalize_index(idx, max)).unwrap(),
        }))
    }

    fn exec_array_slice_expression(
        &mut self,
        array: &[Object],
        start: isize,
        end: isize,
        step: isize,
    ) -> Result<(), String> {
        let max = (array.len() - 1).try_into().unwrap();

        if start > max || end > max || start < 0 || end < 0 || start > end {
            return Err("cannot slice ARRAY using this range".to_string());
        }

        let mut elements = Vec::new();

        let mut i = start;
        while i < end {
            elements.push(array[TryInto::<usize>::try_into(i).unwrap()].clone());
            i += step;
        }

        self.push(Object::Array(Array { elements }))
    }

    fn exec_string_slice_expression(
        &mut self,
        string: &str,
        start: isize,
        end: isize,
        step: isize,
    ) -> Result<(), String> {
        let max = (string.len() - 1).try_into().unwrap();

        if start > max || end > max || start < 0 || end < 0 || start > end {
            return Err(format!("cannot slice STR using {start}..{end}..{step}"));
        }

        let mut value = String::new();

        let mut i = start;
        while i < end {
            value.push(string.chars().nth(i.try_into().unwrap()).unwrap());
            i += step;
        }

        self.push(Object::Str(Str { value }))
    }

    fn exec_hash_index_expression(
        &mut self,
        pairs: &HashMap<u64, HashPair>,
        index: &Object,
    ) -> Result<(), String> {
        let Some(hashable) = Hashable::from_object(index) else {
            return Err(format!("unusable as hash key: {}", index.kind()));
        };

        pairs.get(&hashable.hash_key()).map_or_else(
            || Err(format!("key error: no entry found for key \"{index}\"")),
            |pair| self.push(pair.value.clone()),
        )
    }

    fn exec_call(&mut self, num_args: usize) -> Result<(), String> {
        let callee = self.stack[self.sp - 1 - num_args].clone();
        match callee {
            Object::Closure(callee) => self.call_closure(&callee, num_args),

            Object::Builtin(Builtin { func, caller, .. }) => {
                self.call_builtin(func, &(caller.unwrap_or_else(|| Box::new(NULL))), num_args)
            }

            _ => Err(format!(
                "calling non-function and non-builtin: {}",
                callee.kind()
            )),
        }
    }

    fn call_closure(&mut self, cl: &Closure, num_args: usize) -> Result<(), String> {
        if num_args != cl.func.num_parameters {
            return Err(format!(
                "wrong number of arguments. got: {num_args}, want: {}",
                cl.func.num_parameters
            ));
        }

        let frame = Frame::new(cl.clone(), self.sp - num_args);
        self.push_frame(frame.clone());

        self.sp = frame.bp + cl.func.num_locals;

        Ok(())
    }

    fn call_builtin(
        &mut self,
        func: BuiltinFunction,
        caller: &Object,
        num_args: usize,
    ) -> Result<(), String> {
        let args = &self.stack[self.sp - num_args..self.sp];

        self.sp = self.sp - num_args - 1;

        self.push((func)(caller, args))?;

        Ok(())
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Bool(Bool { value }) => *value,
        Object::Int(Int { value }) => *value != 0,
        Object::Str(Str { value }) => !value.is_empty(),
        Object::Char(Char { value }) => *value != '\0',
        Object::Array(Array { elements }) => !elements.is_empty(),
        Object::Dict(Dict { pairs }) => !pairs.is_empty(),
        Object::Float(Float { value }) => !(value.is_nan() || *value == 0f64),
        Object::Error(Error { message }) => !message.is_empty(),
        _ => true,
    }
}

fn normalize_index(idx: isize, max: isize) -> usize {
    usize::try_from(if idx.is_negative() { max - idx } else { idx }).unwrap()
}
