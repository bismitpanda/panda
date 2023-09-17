#[cfg(test)]
mod symbol_table_tests;
#[cfg(test)]
mod tests;

pub mod symbol_table;

use std::path::PathBuf;
pub use symbol_table::*;

use crate::{
    ast::{
        Assign, Assignable, BlockStatement, Call, ClassStatement, Constructable, Constructor,
        Declaration, Delete, Expression, ExpressionStmt, For, Function, Identifier, If, Import,
        Index, Infix, Lambda, Lit, Literal, Method, Node, Operator, Prefix, Range, Return, Scope,
        Statement, While,
    },
    code::{make, Instructions, Opcode},
    lexer::Lexer,
    object::{
        builtins::BUILTINS, hash_method_name, Char, CompiledFunction, CompiledModule, Float, Int,
        Object, Str, DIR_ENV_VAR_NAME,
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
                Statement::ExpressionStmt(ExpressionStmt {
                    expression,
                    returns,
                    ..
                }) => {
                    self.compile(Node::Expr(expression))?;
                    if returns {
                        self.emit_op(Opcode::Pop);
                    } else {
                        self.emit_op(Opcode::PopNoRet);
                    }
                }

                Statement::Declaration(Declaration {
                    name,
                    mutable,
                    value,
                    ..
                }) => {
                    let symbol = self.symbol_table.define(&name, mutable);

                    if let Some(value) = value {
                        self.compile(Node::Expr(value))?;
                    } else {
                        self.emit_op(Opcode::Null);
                    }

                    if symbol.scope == SymbolScope::Global {
                        self.emit(Opcode::SetGlobal, &[symbol.index]);
                    } else {
                        self.emit(Opcode::SetLocal, &[symbol.index]);
                    }
                }

                Statement::Return(Return { return_value, .. }) => {
                    self.compile(Node::Expr(return_value))?;
                    self.emit_op(Opcode::ReturnValue);
                }

                Statement::Delete(Delete { delete_ident, .. }) => {
                    let Some(symbol) = self.symbol_table.delete(&delete_ident) else {
                        return Err(format!("undefined variable `{delete_ident}`"));
                    };

                    self.emit(Opcode::Delete, &[symbol.index]);
                }

                Statement::Function(Function {
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
                        self.emit_op(Opcode::Return);
                    }

                    let free_symbols = self.symbol_table.free_symbols.clone();
                    let num_locals = self.symbol_table.num_definitions;
                    let instructions = self.leave_scope();

                    for symbol in &free_symbols {
                        self.load_symbol(symbol);
                    }

                    let compiled_fn = Object::CompiledFunction(CompiledFunction {
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

                Statement::While(While {
                    condition, body, ..
                }) => {
                    self.loop_state.in_loop = true;

                    let start_pos = self.current_instructions().len();

                    self.compile(Node::Expr(condition))?;
                    let jump_not_truthy_pos = self.emit(Opcode::JumpNotTruthy, &[9999]);

                    self.compile_block_statements(body)?;

                    self.emit(Opcode::Jump, &[start_pos]);
                    self.emit_op(Opcode::Pop);

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

                Statement::Import(Import { path, alias, .. }) => {
                    let path_buf = PathBuf::from(&path);
                    let start_dir =
                        std::env::var(DIR_ENV_VAR_NAME).map_err(|err| err.to_string())?;

                    if let Some(ext) = path_buf.extension() {
                        if ext != "pd" {
                            return Err("cannot import non panda files".to_string());
                        }
                        let import_file =
                            std::fs::read_to_string(PathBuf::from(start_dir).join(&path_buf))
                                .map_err(|err| err.to_string())?;

                        let mut lexer = Lexer::new(&import_file);
                        let mut parser = Parser::new(&mut lexer);

                        let program = parser.parse_program();

                        if !parser.errors.is_empty() {
                            println!("parser errors:");
                            for msg in &parser.errors {
                                println!("\t{msg}");
                            }
                            return Err(format!("could not import \"{path}\" as it had errors."));
                        }

                        let mut comp = Self::new();
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
                            CompiledModule {
                                symbol_table: module_symbol_table,
                                name: module_name,
                                constants: comp.constants,
                            },
                        );
                    }
                }

                Statement::For(For {
                    ident,
                    iterator,
                    body,
                    ..
                }) => {
                    self.loop_state.in_loop = true;

                    let symbol = self.symbol_table.define(&ident, false);
                    self.emit_op(Opcode::Null);

                    if symbol.scope == SymbolScope::Global {
                        self.emit(Opcode::SetGlobal, &[symbol.index]);
                    } else {
                        self.emit(Opcode::SetLocal, &[symbol.index]);
                    }

                    self.compile(Node::Expr(iterator))?;
                    self.emit_op(Opcode::Start);

                    let start_pos = self.current_instructions().len();

                    let jump_iter_end_pos = self.emit(Opcode::JumpEnd, &[9999, 9999]);

                    self.emit_op(Opcode::Next);

                    if symbol.scope == SymbolScope::Global {
                        self.emit(Opcode::SetGlobal, &[symbol.index]);
                    } else {
                        self.emit(Opcode::SetLocal, &[symbol.index]);
                    }

                    self.compile_block_statements(body)?;

                    self.emit(Opcode::Jump, &[start_pos]);

                    self.emit_op(Opcode::Pop);

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
                Expression::Infix(Infix {
                    left,
                    operator,
                    right,
                    ..
                }) => {
                    if operator == Operator::Lt {
                        self.compile(Node::Expr(*right))?;
                        self.compile(Node::Expr(*left))?;

                        self.emit_op(Opcode::GreaterThan);

                        return Ok(());
                    } else if operator == Operator::LtEq {
                        self.compile(Node::Expr(*right))?;
                        self.compile(Node::Expr(*left))?;

                        self.emit_op(Opcode::GreaterThanEqual);

                        return Ok(());
                    }

                    self.compile(Node::Expr(*left))?;
                    self.compile(Node::Expr(*right))?;

                    match operator {
                        Operator::Add => self.emit_op(Opcode::Add),
                        Operator::Sub => self.emit_op(Opcode::Sub),
                        Operator::Mul => self.emit_op(Opcode::Mul),
                        Operator::Div => self.emit_op(Opcode::Div),
                        Operator::BitXor => self.emit_op(Opcode::BitXor),
                        Operator::BitAnd => self.emit_op(Opcode::BitAnd),
                        Operator::BitOr => self.emit_op(Opcode::BitOr),
                        Operator::Shr => self.emit_op(Opcode::Shr),
                        Operator::Shl => self.emit_op(Opcode::Shl),
                        Operator::Gt => self.emit_op(Opcode::GreaterThan),
                        Operator::GtEq => self.emit_op(Opcode::GreaterThanEqual),
                        Operator::Eq => self.emit_op(Opcode::Equal),
                        Operator::NotEq => self.emit_op(Opcode::NotEqual),
                        Operator::And => self.emit_op(Opcode::And),
                        Operator::Or => self.emit_op(Opcode::Or),
                        _ => return Err(format!("unknown operator: \"{operator}\"")),
                    };
                }

                Expression::Prefix(Prefix {
                    operator, right, ..
                }) => {
                    self.compile(Node::Expr(*right))?;

                    match operator {
                        Operator::Bang => self.emit_op(Opcode::Bang),
                        Operator::Sub => self.emit_op(Opcode::Minus),
                        _ => return Err(format!("unknown operator: {operator}")),
                    };
                }

                Expression::Literal(Literal { lit, .. }) => match lit {
                    Lit::Int { value } => {
                        let integer = Object::Int(Int { value });
                        let operand = self.add_constant(integer);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Lit::Float { value } => {
                        let float = Object::Float(Float { value });
                        let operand = self.add_constant(float);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Lit::Char { value } => {
                        let ch = Object::Char(Char { value });
                        let operand = self.add_constant(ch);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Lit::Str { value } => {
                        let str = Object::Str(Str { value });
                        let operand = self.add_constant(str);
                        self.emit(Opcode::Constant, &[operand]);
                    }

                    Lit::Bool { value } => {
                        if value {
                            self.emit_op(Opcode::True)
                        } else {
                            self.emit_op(Opcode::False)
                        };
                    }

                    Lit::Null => {
                        self.emit_op(Opcode::Null);
                    }

                    Lit::Array { elements } => {
                        let n = elements.len();
                        for el in elements {
                            self.compile(Node::Expr(el))?;
                        }

                        self.emit(Opcode::Array, &[n]);
                    }

                    Lit::Hash { pairs } => {
                        let n = pairs.len();

                        for (k, v) in pairs {
                            self.compile(Node::Expr(k))?;
                            self.compile(Node::Expr(v))?;
                        }

                        self.emit(Opcode::Hash, &[n]);
                    }
                },

                Expression::If(If {
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
                        self.emit_op(Opcode::Null);
                    }

                    let after_alternative_pos = self.current_instructions().len();
                    self.change_operand(jump_pos, after_alternative_pos);
                }

                Expression::Identifier(Identifier { value, .. }) => {
                    let symbol = self
                        .symbol_table
                        .resolve(&value)
                        .ok_or_else(|| format!("undefined variable {value}"))?;

                    self.load_symbol(&symbol);
                }

                Expression::Index(Index { left, index, .. }) => {
                    self.compile(Node::Expr(*left))?;
                    self.compile(Node::Expr(*index))?;

                    self.emit_op(Opcode::Index);
                }

                Expression::Range(Range {
                    start, end, step, ..
                }) => {
                    if let Some(step) = step {
                        self.compile(Node::Expr(*step))?;
                        self.emit(Opcode::Range, &[3]);
                        return Ok(());
                    }

                    self.compile(Node::Expr(*start))?;
                    self.compile(Node::Expr(*end))?;
                    self.emit(Opcode::Range, &[2]);
                }

                Expression::Lambda(Lambda {
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
                        self.emit_op(Opcode::Return);
                    }

                    let free_symbols = self.symbol_table.free_symbols.clone();
                    let num_locals = self.symbol_table.num_definitions;
                    let instructions = self.leave_scope();

                    for symbol in &free_symbols {
                        self.load_symbol(symbol);
                    }

                    let compiled_fn = Object::CompiledFunction(CompiledFunction {
                        instructions,
                        num_locals,
                        num_parameters,
                    });

                    let idx = self.add_constant(compiled_fn);
                    self.emit(Opcode::Closure, &[idx, free_symbols.len()]);
                }

                Expression::Call(Call {
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

                Expression::Assign(Assign { to, value, .. }) => match to {
                    Assignable::Identifier(Identifier { value: name, .. }) => {
                        self.compile(Node::Expr(*value))?;

                        let symbol = self
                            .symbol_table
                            .resolve(&name)
                            .ok_or_else(|| format!("undefined variable {name}"))?;

                        self.emit_op(Opcode::Dup);

                        if symbol.scope == SymbolScope::Global {
                            self.emit(Opcode::SetGlobal, &[symbol.index]);
                        } else {
                            self.emit(Opcode::SetLocal, &[symbol.index]);
                        }
                    }

                    _ => todo!(),
                },

                Expression::Method(Method {
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

                Expression::Scope(Scope { module, .. }) => {
                    let (_, pos) = self
                        .symbol_table
                        .resolve_import(&module)
                        .ok_or_else(|| format!("no module named {module} found."))?;

                    self.emit(Opcode::Scope, &[pos]);

                    todo!();
                }

                Expression::Constructor(Constructor { constructable, .. }) => {
                    match constructable {
                        Constructable::Identifier(Identifier { ref value, .. }) => {
                            let class = self
                                .symbol_table
                                .resolve_type(value)
                                .ok_or_else(|| format!("no class named \"{value}\" found."))?;

                            (!class.initializers.is_empty())
                                .then_some(())
                                .ok_or_else(|| {
                                    format!(
                                        "cannot initialize class with 0 variables. required: {}",
                                        class.initializers.len()
                                    )
                                })?;

                            for stmt in class.body {
                                match stmt {
                                    ClassStatement::Declaration(decl) => {
                                        if let Some(value) = decl.value {
                                            self.compile(Node::Expr(value))?;
                                        } else {
                                            self.emit_op(Opcode::Null);
                                        }

                                        self.emit(
                                            Opcode::ClassMember,
                                            &[
                                                hash_method_name(&decl.name) as usize,
                                                0,
                                                usize::from(decl.mutable),
                                            ],
                                        );
                                    }

                                    ClassStatement::Function(func) => {
                                        self.enter_scope();

                                        self.symbol_table.define_function_name(&func.ident);

                                        let num_parameters = func.parameters.len();

                                        for p in func.parameters {
                                            self.symbol_table.define(&p, false);
                                        }

                                        self.compile_block_statements(func.body)?;

                                        if self.last_instruction_is(Opcode::Pop) {
                                            self.replace_last_pop_with(Opcode::ReturnValue);
                                        }

                                        if self.last_instruction_is(Opcode::PopNoRet) {
                                            self.replace_last_pop_with(Opcode::Return);
                                        }

                                        if !self.last_instruction_is(Opcode::ReturnValue)
                                            && !self.last_instruction_is(Opcode::Return)
                                        {
                                            self.emit_op(Opcode::Return);
                                        }

                                        let free_symbols = self.symbol_table.free_symbols.clone();
                                        let num_locals = self.symbol_table.num_definitions;
                                        let instructions = self.leave_scope();

                                        for symbol in &free_symbols {
                                            self.load_symbol(symbol);
                                        }

                                        let compiled_fn =
                                            Object::CompiledFunction(CompiledFunction {
                                                instructions,
                                                num_locals,
                                                num_parameters,
                                            });

                                        let idx = self.add_constant(compiled_fn);
                                        self.emit(Opcode::Closure, &[idx, free_symbols.len()]);

                                        self.emit(
                                            Opcode::ClassMember,
                                            &[hash_method_name(&func.ident) as usize, 1, 0],
                                        );
                                    }
                                }
                            }
                        }

                        Constructable::Call(_) | Constructable::Scope(_) => {}
                    };

                    todo!();
                }
            },
        }

        Ok(())
    }

    fn load_symbol(&mut self, symbol: &Symbol) {
        match symbol.scope {
            SymbolScope::Global => self.emit(Opcode::GetGlobal, &[symbol.index]),
            SymbolScope::Local => self.emit(Opcode::GetLocal, &[symbol.index]),
            SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, &[symbol.index]),
            SymbolScope::Free => self.emit(Opcode::GetFree, &[symbol.index]),
            SymbolScope::Function => self.emit_op(Opcode::CurrentClosure),
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

    fn emit_op(&mut self, op: Opcode) -> usize {
        let ins = make(op, &[]);
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
