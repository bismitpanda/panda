#[cfg(test)]
mod symbol_table_tests;
#[cfg(test)]
mod tests;

pub mod symbol_table;

use std::{
    hash::{Hash, Hasher},
    path::PathBuf,
};

use ahash::AHasher;
pub use symbol_table::*;

use crate::{
    ast::{
        AssignAst, Assignable, BlockStatement, CallAst, ConstructorAst, DeclarationAst, DeleteAst,
        Expression, ExpressionStmtAst, ForAst, FunctionAst, IdentifierAst, IfAst, ImportAst,
        IndexAst, InfixAst, LambdaAst, Literal, LiteralAst, MethodAst, Node, Operator, PrefixAst,
        RangeAst, ReturnAst, ScopeAst, Statement, WhileAst,
    },
    code::{make, Instructions, Opcode},
    lexer::Lexer,
    object::{
        builtins::BUILTINS, CharObject, CompiledFunctionObject, CompiledModuleObject, FloatObject,
        IntObject, Object, StrObject, DIR_ENV_VAR_NAME,
    },
    parser::Parser,
};

use self::symbol_table::SymbolTable;

pub struct CompilationScope {
    instructions: Instructions,
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

#[derive(Clone, Copy)]
pub struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

pub fn new_with_state(s: SymbolTable, constants: &[Object]) -> Compiler {
    let mut comp = Compiler::new();
    comp.symbol_table = s;
    comp.constants = constants.to_vec();

    comp
}

#[derive(Default)]
pub struct LoopState {
    in_loop: bool,
    continues: Vec<usize>,
    breaks: Vec<usize>,
}

pub struct Compiler {
    constants: Vec<Object>,
    symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,

    loop_state: LoopState,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope {
            instructions: Vec::new(),
            last_instruction: EmittedInstruction {
                opcode: Opcode::Pop,
                position: 0,
            },
            previous_instruction: EmittedInstruction {
                opcode: Opcode::Pop,
                position: 0,
            },
        };

        let mut symbol_table = SymbolTable::new();

        for (i, (name, _)) in BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(name, i);
        }

        Self {
            constants: Vec::new(),
            symbol_table,

            scopes: Vec::from([main_scope]),
            scope_index: 0,

            loop_state: LoopState::default(),
        }
    }

    pub fn get_symbol_table(&self) -> SymbolTable {
        self.symbol_table.clone()
    }

    pub fn compile(&mut self, node: Node) -> Result<(), String> {
        match node {
            Node::Program { statements, .. } => {
                for s in statements {
                    self.compile(Node::Stmt(s))?;
                }
            }

            Node::Stmt(stmt) => match stmt {
                Statement::ExpressionStmt(ExpressionStmtAst {
                    expression,
                    returns,
                    ..
                }) => {
                    self.compile(Node::Expr(expression))?;
                    if returns {
                        self.emit(Opcode::Pop, &[]);
                    } else {
                        self.emit(Opcode::PopNoRet, &[]);
                    }
                }

                Statement::Declaration(DeclarationAst {
                    name,
                    mutable,
                    value,
                    ..
                }) => {
                    let symbol = self.symbol_table.define(&name, mutable);

                    if let Some(value) = value {
                        self.compile(Node::Expr(value))?;
                    } else {
                        self.emit(Opcode::Null, &[]);
                    }

                    if symbol.scope == SymbolScope::Global {
                        self.emit(Opcode::SetGlobal, &[symbol.index]);
                    } else {
                        self.emit(Opcode::SetLocal, &[symbol.index]);
                    }
                }

                Statement::Return(ReturnAst { return_value, .. }) => {
                    self.compile(Node::Expr(return_value))?;
                    self.emit(Opcode::ReturnValue, &[]);
                }

                Statement::Delete(DeleteAst { delete_ident, .. }) => {
                    let Some(symbol) = self.symbol_table.delete(&delete_ident) else {
                        return Err(format!("undefined variable `{delete_ident}`"));
                    };

                    self.emit(Opcode::Delete, &[symbol.index]);
                }

                Statement::Function(FunctionAst {
                    ident,
                    parameters,
                    body,
                    ..
                }) => {
                    let symbol = self.symbol_table.define(&ident, false);

                    self.enter_scope();

                    self.symbol_table.define_function_name(&ident);

                    let num_parameters = parameters.len();

                    for p in parameters {
                        self.symbol_table.define(&p, false);
                    }

                    self.compile_block_statements(body)?;

                    if self.last_instruction_is(Opcode::Pop) {
                        self.replace_last_pop_with(Opcode::ReturnValue);
                    }

                    if self.last_instruction_is(Opcode::PopNoRet) {
                        self.replace_last_pop_with(Opcode::Return);
                    }

                    if !self.last_instruction_is(Opcode::ReturnValue)
                        && !self.last_instruction_is(Opcode::Return)
                    {
                        self.emit(Opcode::Return, &[]);
                    }

                    let free_symbols = self.symbol_table.free_symbols.clone();
                    let num_locals = self.symbol_table.num_definitions;
                    let instructions = self.leave_scope();

                    for symbol in &free_symbols {
                        self.load_symbol(symbol.clone());
                    }

                    let compiled_fn = Object::CompiledFunction(CompiledFunctionObject {
                        instructions,
                        num_locals,
                        num_parameters,
                    });

                    let idx = self.add_constant(compiled_fn);
                    self.emit(Opcode::Closure, &[idx, free_symbols.len()]);

                    if symbol.scope == SymbolScope::Global {
                        self.emit(Opcode::SetGlobal, &[symbol.index]);
                    } else {
                        self.emit(Opcode::SetLocal, &[symbol.index]);
                    }
                }

                Statement::While(WhileAst {
                    condition, body, ..
                }) => {
                    self.loop_state.in_loop = true;

                    let start_pos = self.current_instructions().len();

                    self.compile(Node::Expr(condition))?;
                    let jump_not_truthy_pos = self.emit(Opcode::JumpNotTruthy, &[9999]);

                    self.compile_block_statements(body)?;

                    self.emit(Opcode::Jump, &[start_pos]);
                    self.emit(Opcode::Pop, &[]);

                    let after_loop_pos = self.current_instructions().len();
                    self.change_operand(jump_not_truthy_pos, after_loop_pos);

                    for continue_pos in self.loop_state.continues.clone() {
                        self.change_operand(continue_pos, start_pos);
                    }

                    for break_pos in self.loop_state.breaks.clone() {
                        self.change_operand(break_pos, after_loop_pos);
                    }

                    self.loop_state.in_loop = false;
                }

                Statement::Break(_) => {
                    if self.loop_state.in_loop {
                        let pos = self.emit(Opcode::Jump, &[9999]);
                        self.loop_state.breaks.push(pos);
                    } else {
                        return Err("cannot use `break` outside loops".to_string());
                    }
                }

                Statement::Continue(_) => {
                    if self.loop_state.in_loop {
                        let pos = self.emit(Opcode::Jump, &[9999]);
                        self.loop_state.continues.push(pos);
                    } else {
                        return Err("cannot use `continue` outside loops".to_string());
                    }
                }

                Statement::ClassDecl(decl) => {
                    self.symbol_table.define_type(decl.ident.clone(), decl);
                }

                Statement::Import(ImportAst { path, alias, .. }) => {
                    let path_buf = PathBuf::from(&path);
                    let start_dir =
                        std::env::var(DIR_ENV_VAR_NAME).map_err(|err| err.to_string())?;

                    if let Some(ext) = path_buf.extension() {
                        if ext != "pd" {
                            return Err("cannot import non panda files".to_string());
                        }
                        let file =
                            std::fs::read_to_string(PathBuf::from(start_dir).join(&path_buf))
                                .map_err(|err| err.to_string())?;

                        let mut lexer = Lexer::new(file);
                        let mut parser = Parser::new(&mut lexer);

                        let program = parser.parse_program();

                        if !parser.errors.is_empty() {
                            println!("parser errors:");
                            for msg in &parser.errors {
                                println!("\t{msg}");
                            }
                            return Err(format!("could not import \"{path}\" as it had errors."));
                        }

                        let mut comp = Compiler::new();
                        if let Err(err) = comp.compile(program.unwrap()) {
                            println!("compiler error:\n\t{err}");
                            return Ok(());
                        };

                        let module_symbol_table = comp.symbol_table;

                        let module_name = alias.unwrap_or_else(|| {
                            path_buf.file_stem().unwrap().to_str().unwrap().to_string()
                        });

                        self.symbol_table.define_import(
                            module_name.clone(),
                            CompiledModuleObject {
                                symbol_table: module_symbol_table,
                                name: module_name,
                                constants: comp.constants,
                            },
                        );
                    }
                }

                Statement::For(ForAst {
                    ident,
                    iterator,
                    body,
                    ..
                }) => {
                    self.loop_state.in_loop = true;

                    let symbol = self.symbol_table.define(&ident, false);
                    self.emit(Opcode::Null, &[]);

                    if symbol.scope == SymbolScope::Global {
                        self.emit(Opcode::SetGlobal, &[symbol.index]);
                    } else {
                        self.emit(Opcode::SetLocal, &[symbol.index]);
                    }

                    self.compile(Node::Expr(iterator))?;
                    self.emit(Opcode::Start, &[]);

                    let start_pos = self.current_instructions().len();

                    let jump_iter_end_pos = self.emit(Opcode::JumpEnd, &[9999, 9999]);

                    self.emit(Opcode::Next, &[]);

                    if symbol.scope == SymbolScope::Global {
                        self.emit(Opcode::SetGlobal, &[symbol.index]);
                    } else {
                        self.emit(Opcode::SetLocal, &[symbol.index]);
                    }

                    self.compile_block_statements(body)?;

                    self.emit(Opcode::Jump, &[start_pos]);

                    self.emit(Opcode::Pop, &[]);

                    let after_loop_pos = self.current_instructions().len();
                    self.change_operands(jump_iter_end_pos, &[after_loop_pos, symbol.index]);

                    self.symbol_table.delete(&ident);

                    for continue_pos in self.loop_state.continues.clone() {
                        self.change_operand(continue_pos, start_pos);
                    }

                    for break_pos in self.loop_state.breaks.clone() {
                        self.change_operand(break_pos, after_loop_pos);
                    }

                    self.loop_state.in_loop = false;
                }
            },

            Node::Expr(expr) => match expr {
                Expression::Infix(InfixAst {
                    left,
                    operator,
                    right,
                    ..
                }) => {
                    if operator == Operator::Lt {
                        self.compile(Node::Expr(*right))?;
                        self.compile(Node::Expr(*left))?;

                        self.emit(Opcode::GreaterThan, &[]);

                        return Ok(());
                    } else if operator == Operator::LtEq {
                        self.compile(Node::Expr(*right))?;
                        self.compile(Node::Expr(*left))?;

                        self.emit(Opcode::GreaterThanEqual, &[]);

                        return Ok(());
                    }

                    self.compile(Node::Expr(*left))?;
                    self.compile(Node::Expr(*right))?;

                    match operator {
                        Operator::Add => self.emit(Opcode::Add, &[]),
                        Operator::Sub => self.emit(Opcode::Sub, &[]),
                        Operator::Mul => self.emit(Opcode::Mul, &[]),
                        Operator::Div => self.emit(Opcode::Div, &[]),
                        Operator::BitXor => self.emit(Opcode::BitXor, &[]),
                        Operator::BitAnd => self.emit(Opcode::BitAnd, &[]),
                        Operator::BitOr => self.emit(Opcode::BitOr, &[]),
                        Operator::Shr => self.emit(Opcode::Shr, &[]),
                        Operator::Shl => self.emit(Opcode::Shl, &[]),
                        Operator::Gt => self.emit(Opcode::GreaterThan, &[]),
                        Operator::GtEq => self.emit(Opcode::GreaterThanEqual, &[]),
                        Operator::Eq => self.emit(Opcode::Equal, &[]),
                        Operator::NotEq => self.emit(Opcode::NotEqual, &[]),
                        Operator::And => self.emit(Opcode::And, &[]),
                        Operator::Or => self.emit(Opcode::Or, &[]),
                        _ => return Err(format!("unknown operator: '{operator}'")),
                    };
                }

                Expression::Prefix(PrefixAst {
                    operator, right, ..
                }) => {
                    self.compile(Node::Expr(*right))?;

                    match operator {
                        Operator::Bang => self.emit(Opcode::Bang, &[]),
                        Operator::Sub => self.emit(Opcode::Minus, &[]),
                        _ => return Err(format!("unknown operator: {operator}")),
                    };
                }

                Expression::Literal(LiteralAst { lit, .. }) => match lit {
                    Literal::Int { value } => {
                        let integer = Object::Int(IntObject { value });
                        let operand = self.add_constant(integer);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Literal::Float { value } => {
                        let float = Object::Float(FloatObject { value });
                        let operand = self.add_constant(float);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Literal::Char { value } => {
                        let ch = Object::Char(CharObject { value });
                        let operand = self.add_constant(ch);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Literal::Str { value } => {
                        let str = Object::Str(StrObject { value });
                        let operand = self.add_constant(str);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Literal::Bool { value } => {
                        if value {
                            self.emit(Opcode::True, &[])
                        } else {
                            self.emit(Opcode::False, &[])
                        };
                    }

                    Literal::Null => {
                        self.emit(Opcode::Null, &[]);
                    }

                    Literal::Array { elements } => {
                        let n = elements.len();
                        for el in elements {
                            self.compile(Node::Expr(el))?;
                        }

                        self.emit(Opcode::Array, &[n]);
                    }

                    Literal::Hash { pairs } => {
                        let n = pairs.len();

                        for (k, v) in pairs {
                            self.compile(Node::Expr(k))?;
                            self.compile(Node::Expr(v))?;
                        }

                        self.emit(Opcode::Hash, &[n]);
                    }
                },

                Expression::If(IfAst {
                    condition,
                    consequence,
                    alternative,
                    ..
                }) => {
                    self.compile(Node::Expr(*condition))?;

                    let jump_not_truthy_pos = self.emit(Opcode::JumpNotTruthy, &[9999]);
                    self.compile_block_statements(consequence)?;

                    if self.last_instruction_is(Opcode::Pop) {
                        self.remove_last_pop();
                    }

                    let jump_pos = self.emit(Opcode::Jump, &[9999]);

                    let after_conseqence_pos = self.current_instructions().len();
                    self.change_operand(jump_not_truthy_pos, after_conseqence_pos);

                    if let Some(alternative) = alternative {
                        self.compile_block_statements(alternative)?;

                        if self.last_instruction_is(Opcode::Pop) {
                            self.remove_last_pop();
                        }
                    } else {
                        self.emit(Opcode::Null, &[]);
                    }

                    let after_alternative_pos = self.current_instructions().len();
                    self.change_operand(jump_pos, after_alternative_pos);
                }

                Expression::Identifier(IdentifierAst { value, .. }) => {
                    let symbol = self
                        .symbol_table
                        .resolve(&value)
                        .ok_or_else(|| format!("undefined variable {value}"))?;

                    self.load_symbol(symbol);
                }

                Expression::Index(IndexAst { left, index, .. }) => {
                    self.compile(Node::Expr(*left))?;
                    self.compile(Node::Expr(*index))?;

                    self.emit(Opcode::Index, &[]);
                }

                Expression::Range(RangeAst {
                    start, stop, step, ..
                }) => {
                    if let Some(step) = step {
                        self.compile(Node::Expr(*step))?;
                        self.emit(Opcode::Range, &[3]);
                        return Ok(());
                    }

                    self.compile(Node::Expr(*start))?;
                    self.compile(Node::Expr(*stop))?;
                    self.emit(Opcode::Range, &[2]);
                }

                Expression::Lambda(LambdaAst {
                    parameters,
                    body,
                    name,
                    ..
                }) => {
                    self.enter_scope();

                    if !name.is_empty() {
                        self.symbol_table.define_function_name(&name);
                    }

                    let num_parameters = parameters.len();

                    for p in parameters {
                        self.symbol_table.define(&p, false);
                    }

                    self.compile_block_statements(body)?;

                    if self.last_instruction_is(Opcode::Pop) {
                        self.replace_last_pop_with(Opcode::ReturnValue);
                    }

                    if self.last_instruction_is(Opcode::PopNoRet) {
                        self.replace_last_pop_with(Opcode::Return);
                    }

                    if !self.last_instruction_is(Opcode::ReturnValue)
                        && !self.last_instruction_is(Opcode::Return)
                    {
                        self.emit(Opcode::Return, &[]);
                    }

                    let free_symbols = self.symbol_table.free_symbols.clone();
                    let num_locals = self.symbol_table.num_definitions;
                    let instructions = self.leave_scope();

                    for symbol in &free_symbols {
                        self.load_symbol(symbol.clone());
                    }

                    let compiled_fn = Object::CompiledFunction(CompiledFunctionObject {
                        instructions,
                        num_locals,
                        num_parameters,
                    });

                    let idx = self.add_constant(compiled_fn);
                    self.emit(Opcode::Closure, &[idx, free_symbols.len()]);
                }

                Expression::Call(CallAst {
                    function,
                    arguments,
                    ..
                }) => {
                    self.compile(Node::Expr(*function))?;

                    let n = arguments.len();

                    for arg in arguments {
                        self.compile(Node::Expr(arg))?;
                    }

                    self.emit(Opcode::Call, &[n]);
                }

                Expression::Assign(AssignAst { to, value, .. }) => match to {
                    Assignable::Identifier(IdentifierAst { value: name, .. }) => {
                        self.compile(Node::Expr(*value))?;

                        let symbol = self
                            .symbol_table
                            .resolve(&name)
                            .ok_or_else(|| format!("undefined variable {name}"))?;

                        self.emit(Opcode::Dup, &[]);

                        if symbol.scope == SymbolScope::Global {
                            self.emit(Opcode::SetGlobal, &[symbol.index]);
                        } else {
                            self.emit(Opcode::SetLocal, &[symbol.index]);
                        }
                    }

                    _ => todo!(),
                },

                Expression::Method(MethodAst {
                    left,
                    method,
                    arguments,
                    ..
                }) => {
                    self.compile(Node::Expr(*left))?;

                    let method_hash = hash_method_name(&method);
                    dbg!(method_hash);

                    let has_arguments = arguments.is_some();

                    let n = if let Some(arguments) = arguments {
                        let n = arguments.len();

                        for arg in arguments {
                            self.compile(Node::Expr(arg))?;
                        }

                        n
                    } else {
                        0
                    };

                    self.emit(
                        Opcode::Method,
                        &[method_hash as usize, usize::from(has_arguments), n],
                    );
                }

                Expression::Scope(ScopeAst { module, .. }) => {
                    let (_, pos) = self
                        .symbol_table
                        .resolve_import(&module)
                        .ok_or_else(|| format!("no module named {module} found."))?;

                    self.emit(Opcode::Scope, &[pos]);
                }

                Expression::Constructor(ConstructorAst { .. }) => {
                    let (_, pos) = self.symbol_table.resolve_type("").ok_or_else(String::new)?;

                    self.emit(Opcode::Constructor, &[pos]);
                    todo!();
                }
            },
        }

        Ok(())
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            SymbolScope::Global => self.emit(Opcode::GetGlobal, &[symbol.index]),
            SymbolScope::Local => self.emit(Opcode::GetLocal, &[symbol.index]),
            SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, &[symbol.index]),
            SymbolScope::Free => self.emit(Opcode::GetFree, &[symbol.index]),
            SymbolScope::Function => self.emit(Opcode::CurrentClosure, &[]),
        };
    }

    pub fn byte_code(&mut self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: Opcode, operands: &[usize]) -> usize {
        let ins = make(op, operands);
        let pos = self.add_instruction(&ins);

        self.set_last_instruction(op, pos);

        pos
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos_new_instruction = self.current_instructions().len();
        let mut updates_instructions = self.current_instructions().clone();
        updates_instructions.extend_from_slice(ins);

        self.scopes[self.scope_index].instructions = updates_instructions;

        pos_new_instruction
    }

    fn compile_block_statements(&mut self, statements: BlockStatement) -> Result<(), String> {
        for s in statements {
            self.compile(Node::Stmt(s))?;
        }

        Ok(())
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.scopes[self.scope_index].last_instruction;
        let last = EmittedInstruction {
            opcode: op,
            position: pos,
        };

        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = last;
    }

    fn last_instruction_is(&mut self, op: Opcode) -> bool {
        if self.current_instructions().is_empty() {
            return false;
        }
        self.scopes[self.scope_index].last_instruction.opcode == op
    }

    fn remove_last_pop(&mut self) {
        let last = self.scopes[self.scope_index].last_instruction;
        let previous = self.scopes[self.scope_index].previous_instruction;

        let old = self.current_instructions();
        let new = &old[..last.position];

        self.scopes[self.scope_index].instructions = new.to_vec();
        self.scopes[self.scope_index].last_instruction = previous;
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: &[u8]) {
        self.scopes[self.scope_index].instructions[pos..(new_instruction.len() + pos)]
            .copy_from_slice(new_instruction);
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        let op = Opcode::try_from(self.current_instructions()[op_pos]).unwrap();
        let new_instruction = make(op, &[operand]);

        self.replace_instruction(op_pos, &new_instruction);
    }

    fn change_operands(&mut self, op_pos: usize, operands: &[usize]) {
        let op = Opcode::try_from(self.current_instructions()[op_pos]).unwrap();
        let new_instruction = make(op, operands);

        self.replace_instruction(op_pos, &new_instruction);
    }

    fn current_instructions(&mut self) -> &mut Instructions {
        self.scopes[self.scope_index].instructions.as_mut()
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: Vec::new(),
            last_instruction: EmittedInstruction {
                opcode: Opcode::Constant,
                position: 0,
            },
            previous_instruction: EmittedInstruction {
                opcode: Opcode::Constant,
                position: 0,
            },
        };

        self.scopes.push(scope);
        self.scope_index += 1;

        self.symbol_table = SymbolTable::new_enclosed(self.symbol_table.clone());
    }

    fn leave_scope(&mut self) -> Instructions {
        let instructions = self.current_instructions().clone();

        self.scopes.pop();
        self.scope_index -= 1;

        self.symbol_table = *self.symbol_table.outer.clone().unwrap();

        instructions
    }

    fn replace_last_pop_with(&mut self, op: Opcode) {
        let last = self.scopes[self.scope_index].last_instruction.position;
        self.replace_instruction(last, &make(op, &[]));

        self.scopes[self.scope_index].last_instruction.opcode = op;
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

fn hash_method_name(name: &str) -> u8 {
    let mut hasher = AHasher::default();
    name.hash(&mut hasher);
    let hash = hasher.finish();

    let mut out = 0u8;
    for i in 0..8 {
        out ^= (hash >> (i * 8)) as u8
    }

    out
}
