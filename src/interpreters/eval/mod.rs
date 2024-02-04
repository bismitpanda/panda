use std::{
    collections::{hash_map::Entry, HashMap},
    path::PathBuf,
};

pub mod environment;
use environment::Environment;

use crate::{
    ast::{
        Assign, Assignable, BlockStatement, Call, ClassStatement, Constructable, Constructor,
        Declaration, Delete, Expression, ExpressionStmt, For, Function, Identifier, If, Import,
        Index, Infix, Lambda, Lit, Literal, Method, Node, Operator, Prefix, Range, Return, Scope,
        Statement, While,
    },
    lexer::Lexer,
    object::{
        allowed_in_array, builtins::get_builtin_by_name, hash_method_name, Array, Bool, Builtin,
        Char, Class, ClassMember, Dict, DictPair, Error, EvaluatedFunction, EvaluatedModule, Float,
        Hashable, Int, Iterable, Object, Range as RangeObj, ReturnValue, Str, Type,
        DIR_ENV_VAR_NAME,
    },
    parser::Parser,
};

#[derive(Default, Debug)]
struct LoopState {
    in_loop: bool,
    break_loop: bool,
    continue_loop: bool,
}

pub struct Evaluator {
    environment: Environment,
    loop_state: LoopState,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
            loop_state: LoopState::default(),
        }
    }

    pub fn eval(&mut self, node: Node) -> Option<Object> {
        match node {
            Node::Program { statements } => {
                return self.eval_program(&statements);
            }

            Node::Stmt(stmt) => match stmt {
                Statement::ExpressionStmt(ExpressionStmt {
                    expression,
                    returns,
                    ..
                }) => {
                    let val = self.eval(Node::Expr(expression))?;

                    if is_error(&val) || returns {
                        return Some(val);
                    }
                }

                Statement::Return(Return { return_value }) => {
                    let value = self.eval(Node::Expr(return_value))?;

                    if is_error(&value) {
                        return Some(value);
                    }

                    return Some(Object::ReturnValue(ReturnValue {
                        value: Box::new(value),
                    }));
                }

                Statement::Declaration(Declaration {
                    name,
                    mutable,
                    value,
                    ..
                }) => {
                    let mut val = if let Some(value) = value {
                        self.eval(Node::Expr(value))?
                    } else {
                        Object::Nil
                    };

                    if is_error(&val) {
                        return Some(val);
                    }

                    if let Object::EvaluatedFunction(ref mut func) = val {
                        func.name = name.clone();
                    }

                    self.environment.set(name, val, mutable);
                }

                Statement::Function(Function {
                    ident,
                    parameters,
                    body,
                    ..
                }) => {
                    self.environment.set(
                        ident.clone(),
                        Object::EvaluatedFunction(EvaluatedFunction {
                            name: ident,
                            parameters,
                            environment: self.environment.clone(),
                            body,
                        }),
                        false,
                    );
                }

                Statement::While(While { condition, body }) => {
                    let mut condition_obj = self.eval(Node::Expr(condition.clone()))?;

                    if is_error(&condition_obj) {
                        return Some(condition_obj);
                    }

                    if body.is_empty() {
                        return None;
                    }

                    self.loop_state.in_loop = true;

                    while is_truthy(&condition_obj) && self.loop_state.in_loop {
                        if let Some(obj) = self.eval_loop_block_statement(&body) {
                            if is_error(&obj) {
                                return Some(obj);
                            }
                        }

                        if self.loop_state.break_loop {
                            break;
                        }

                        condition_obj = self.eval(Node::Expr(condition.clone()))?;

                        if is_error(&condition_obj) {
                            return Some(condition_obj);
                        }
                    }

                    self.loop_state = LoopState::default();
                }

                Statement::For(For {
                    ident,
                    iterator,
                    body,
                    ..
                }) => {
                    let obj = self.eval(Node::Expr(iterator))?;

                    if is_error(&obj) {
                        return Some(obj);
                    }

                    let Some(iterator) = Iterable::from_object(obj.clone()) else {
                        return Some(Object::error(format!("{} is not iterable", obj.kind())));
                    };

                    self.loop_state.in_loop = true;

                    return self.eval_for_statement(&iterator, &ident, &body);
                }

                Statement::ClassDecl(ast_node) => {
                    self.environment.set_type(ast_node.ident.clone(), ast_node);
                }

                Statement::Import(Import { path, alias, class }) => {
                    let path_buf = PathBuf::from(&path);
                    let start_dir = std::env::var(DIR_ENV_VAR_NAME).ok()?;

                    if let Some(ext) = path_buf.extension() {
                        if ext != "pnd" {
                            return Some(Object::error(
                                "cannot import non panda files".to_string(),
                            ));
                        }

                        let import_file =
                            std::fs::read_to_string(PathBuf::from(start_dir).join(&path_buf))
                                .ok()?;

                        let mut lexer = Lexer::new(&import_file);
                        let mut parser = Parser::new(&mut lexer);

                        let program = parser.parse_program();

                        if !parser.errors.is_empty() {
                            println!("parser errors:");
                            for msg in &parser.errors {
                                println!("\t{msg}");
                            }
                            return Some(Object::error(format!(
                                "could not import \"{path}\" as it had errors."
                            )));
                        }

                        let mut evaluator = Self::new();

                        let evaluated = evaluator.eval(program.unwrap());

                        if let Some(evaluated) = evaluated {
                            if matches!(evaluated, Object::Error { .. }) {
                                println!("{}", evaluated.inspect());
                                return None;
                            }
                        }

                        let module_name = alias.unwrap_or_else(|| {
                            path_buf.file_stem().unwrap().to_str().unwrap().to_string()
                        });

                        self.environment.set_import(
                            module_name.clone(),
                            EvaluatedModule {
                                environment: evaluator.environment,
                                name: module_name,
                                class,
                            },
                        );
                    }
                }

                Statement::Break => {
                    if !self.loop_state.in_loop {
                        return Some(Object::error("cannot use break outside loop".to_string()));
                    }

                    self.loop_state.break_loop = true;
                }

                Statement::Continue => {
                    if !self.loop_state.in_loop {
                        return Some(Object::error(
                            "cannot use continue outside loop".to_string(),
                        ));
                    }

                    self.loop_state.continue_loop = true;
                }

                Statement::Delete(Delete { delete_ident }) => {
                    return self.environment.delete(&delete_ident).map_or_else(
                        || {
                            Some(Object::error(format!(
                                "no identifier named \"{delete_ident}\" found."
                            )))
                        },
                        Some,
                    )
                }
            },

            Node::Expr(expr) => match expr {
                Expression::Prefix(Prefix { right, operator }) => {
                    let right = self.eval(Node::Expr(*right))?;

                    if is_error(&right) {
                        return Some(right);
                    }

                    return Some(Self::eval_prefix_expression(operator, &right));
                }

                Expression::Infix(Infix {
                    left,
                    operator,
                    right,
                    ..
                }) => {
                    let left = self.eval(Node::Expr(*left))?;

                    if is_error(&left) {
                        return Some(left);
                    }

                    let right = self.eval(Node::Expr(*right))?;

                    if is_error(&right) {
                        return Some(right);
                    }

                    return Some(Self::eval_infix_expression(operator, left, right));
                }

                Expression::If(If {
                    condition,
                    consequence,
                    alternative,
                    ..
                }) => {
                    return self.eval_if_expression(*condition, &consequence, &alternative);
                }

                Expression::Identifier(Identifier { value }) => {
                    return Some(self.eval_identifier(value));
                }

                Expression::Lambda(Lambda {
                    parameters, body, ..
                }) => {
                    return Some(Object::EvaluatedFunction(EvaluatedFunction {
                        name: String::new(),
                        parameters,
                        environment: self.environment.clone(),
                        body,
                    }));
                }

                Expression::Call(Call {
                    function,
                    arguments,
                    ..
                }) => {
                    let function = self.eval(Node::Expr(*function))?;

                    if is_error(&function) {
                        return Some(function);
                    }

                    let args = self.eval_expressions(&arguments)?;

                    if args.len() == 1 && is_error(&args[0]) {
                        return Some(args[0].clone());
                    }

                    return Some(self.eval_call_expression(&function, &args));
                }

                Expression::Index(Index { left, index }) => {
                    let left = self.eval(Node::Expr(*left))?;

                    if is_error(&left) {
                        return Some(left);
                    }

                    let index = self.eval(Node::Expr(*index))?;

                    if is_error(&index) {
                        return Some(index);
                    }

                    return Some(Self::eval_index_expression(&left, &index));
                }

                Expression::Assign(Assign { to, value }) => {
                    return self.eval_assign_expression(to, &value);
                }

                Expression::Method(Method {
                    left,
                    name: method,
                    arguments,
                    ..
                }) => {
                    let left = self.eval(Node::Expr(*left))?;

                    if is_error(&left) {
                        return Some(left);
                    }

                    return Some(self.eval_method_expression(arguments, left, &method));
                }

                Expression::Constructor(Constructor { constructable }) => {
                    return Some(self.eval_constructor_expression(constructable));
                }

                Expression::Range(Range {
                    start: node_start,
                    end: node_end,
                    step: node_step,
                    ..
                }) => {
                    let start = self.eval(Node::Expr(*node_start))?;

                    if is_error(&start) {
                        return Some(start);
                    }

                    let end = self.eval(Node::Expr(*node_end))?;

                    if is_error(&end) {
                        return Some(end);
                    }

                    let mut step = None;
                    if let Some(s) = node_step {
                        let evaluated = self.eval(Node::Expr(*s))?;

                        if is_error(&evaluated) {
                            return Some(evaluated);
                        }

                        step = Some(evaluated);
                    }

                    let Object::Int(Int { value: start }) = start else {
                        return Some(Object::error(format!(
                            "cannot use {} as start in range. expected: INT",
                            start.kind()
                        )));
                    };

                    let Object::Int(Int { value: end }) = end else {
                        return Some(Object::error(format!(
                            "cannot use {} as end in range. expected: INT",
                            end.kind()
                        )));
                    };

                    let rev = start > end;

                    let step = if let Some(step) = step {
                        match step {
                            Object::Int(Int { value }) => value,
                            _ => {
                                return Some(Object::error(format!(
                                    "cannot use {} as step in range. expected: INT",
                                    step.kind()
                                )))
                            }
                        }
                    } else if rev {
                        -1
                    } else {
                        1
                    };

                    return Some(Object::Range(RangeObj { start, end, step }));
                }

                Expression::Scope(Scope { module, member }) => {
                    let Some(import) = self.environment.get_import(&module) else {
                        return Some(Object::error(format!("no module named \"{module}\" found")));
                    };

                    match *member {
                        Expression::Identifier(Identifier { ref value }) => {
                            let Some((member, _)) = import.environment.get(value.clone()) else {
                                return Some(Object::error(format!(
                                    "member '{member}' not found in module '{module}'"
                                )));
                            };

                            return Some(member);
                        }
                        Expression::Call(Call {
                            ref function,
                            ref arguments,
                            ..
                        }) => {
                            let Expression::Identifier(Identifier { value: member_name }) =
                                *function.clone()
                            else {
                                return Some(Object::error(
                                    "expected Identifier in scope expression".to_string(),
                                ));
                            };

                            let Some((member, _)) = import.environment.get(member_name) else {
                                return Some(Object::error(format!(
                                    "member '{member}' not found in module '{module}'"
                                )));
                            };

                            if !matches!(member, Object::EvaluatedFunction { .. }) {
                                return Some(Object::error(format!(
                                    "\"{member}\" is not callable"
                                )));
                            }

                            let args = self.eval_expressions(arguments)?;

                            if args.len() == 1 && is_error(&args[0]) {
                                return Some(args[0].clone());
                            }

                            return Some(self.eval_call_expression(&member, &args));
                        }
                        _ => {
                            return Some(Object::error("invalid scope expression".to_string()));
                        }
                    };
                }

                Expression::Literal(Literal { lit }) => match lit {
                    Lit::Int { value } => return Some(Object::int(value)),

                    Lit::Float { value } => return Some(Object::float(value)),

                    Lit::Bool { value } => {
                        return Some(if value { Object::TRUE } else { Object::FALSE })
                    }

                    Lit::Str { value } => return Some(Object::Str(Str { value })),

                    Lit::Array { elements } => {
                        let elements = self.eval_array_expressions(&elements)?;
                        if elements.len() == 1 && is_error(&elements[0]) {
                            return Some(elements[0].clone());
                        }

                        return Some(Object::array(elements));
                    }

                    Lit::Char { value } => return Some(Object::char(value)),

                    Lit::Dict { pairs } => {
                        return self.eval_dict_literal(&pairs);
                    }

                    Lit::Nil => return Some(Object::Nil),
                },
            },
        };

        None
    }

    fn eval_constructor_expression(&mut self, constructable: Constructable) -> Object {
        match constructable {
            Constructable::Identifier(Identifier { ref value }) => {
                let Some(class) = self.environment.get_type(value) else {
                    return Object::error(format!("no class named '{value}' found."));
                };

                if !class.initializers.is_empty() {
                    return Object::error(format!(
                        "cannot initialize class with 0 variables. required: {}",
                        class.initializers.len()
                    ));
                }

                let mut members = HashMap::new();
                for stmt in class.body {
                    match stmt {
                        ClassStatement::Variable(decl) => {
                            let obj = decl
                                .value
                                .map_or(Object::Nil, |val| self.eval(Node::Expr(val)).unwrap());
                            members.insert(
                                hash_method_name(&decl.name),
                                ClassMember::new(decl.name, obj),
                            );
                        }

                        ClassStatement::Method(func) => {
                            let obj = Object::EvaluatedFunction(EvaluatedFunction {
                                name: func.name.clone(),
                                parameters: func.parameters,
                                environment: self.environment.clone(),
                                body: func.body,
                            });

                            members.insert(
                                hash_method_name(&func.name),
                                ClassMember::new(func.name, obj),
                            );
                        }
                    }
                }

                Object::Class(Class {
                    name: value.clone(),
                    members,
                })
            }

            Constructable::Call(Call {
                function,
                arguments,
                ..
            }) => {
                let Expression::Identifier(Identifier { value: member }) = *function else {
                    return Object::error(String::new());
                };

                let Some(class) = self.environment.get_type(&member) else {
                    return Object::error(format!("no class named '{member}' found"));
                };

                let received_initializers = self.eval_expressions(&arguments).unwrap();

                if class.initializers.len() != received_initializers.len() {
                    return Object::error(format!(
                        "invalid length of initializers. required: {}, got: {}",
                        class.initializers.len(),
                        received_initializers.len()
                    ));
                }

                let mut members = HashMap::new();

                for stmt in class.body {
                    match stmt {
                        ClassStatement::Variable(decl) => {
                            let obj = decl
                                .value
                                .map_or(Object::Nil, |val| self.eval(Node::Expr(val)).unwrap());

                            members.insert(
                                hash_method_name(&decl.name),
                                ClassMember::new(decl.name, obj),
                            );
                        }

                        ClassStatement::Method(func) => {
                            let obj = Object::EvaluatedFunction(EvaluatedFunction {
                                name: func.name.clone(),
                                parameters: func.parameters,
                                environment: self.environment.clone(),
                                body: func.body,
                            });

                            members.insert(
                                hash_method_name(&func.name),
                                ClassMember::new(func.name, obj),
                            );
                        }
                    }
                }

                for (name, value) in class.initializers.iter().zip(received_initializers.iter()) {
                    members.insert(
                        hash_method_name(name),
                        ClassMember::new(name.to_string(), value.clone()),
                    );
                }

                Object::Class(Class {
                    name: member,
                    members,
                })
            }

            Constructable::Scope(Scope {
                ref module,
                ref member,
                ..
            }) => {
                let Some(module) = self.environment.get_import(module) else {
                    return Object::error(format!("no module named '{module}' found"));
                };

                match *member.clone() {
                    Expression::Identifier(Identifier { value }) => {
                        let Some(class) = module.environment.get_type(&value) else {
                            return Object::error(format!(
                                "no class named '{}' found in module '{}'",
                                member, module.name
                            ));
                        };

                        if !class.initializers.is_empty() {
                            return Object::error(format!(
                                "cannot initialize class with 0 variables. required: {}",
                                class.initializers.len()
                            ));
                        }

                        let mut members = HashMap::new();
                        for stmt in class.body {
                            match stmt {
                                ClassStatement::Variable(decl) => {
                                    let obj = decl.value.map_or(Object::Nil, |val| {
                                        self.eval(Node::Expr(val)).unwrap()
                                    });
                                    members.insert(
                                        hash_method_name(&decl.name),
                                        ClassMember::new(decl.name, obj),
                                    );
                                }

                                ClassStatement::Method(func) => {
                                    let obj = Object::EvaluatedFunction(EvaluatedFunction {
                                        name: func.name.clone(),
                                        parameters: func.parameters,
                                        environment: self.environment.clone(),
                                        body: func.body,
                                    });

                                    members.insert(
                                        hash_method_name(&func.name),
                                        ClassMember::new(func.name, obj),
                                    );
                                }
                            }
                        }

                        Object::Class(Class {
                            name: value,
                            members,
                        })
                    }

                    Expression::Call(Call {
                        function,
                        arguments,
                        ..
                    }) => {
                        let Expression::Identifier(Identifier { value: member }) = *function else {
                            return Object::error(String::new());
                        };

                        let Some(class) = module.environment.get_type(&member) else {
                            return Object::error(format!(
                                "no class named '{}' found in module '{}'",
                                member, module.name
                            ));
                        };

                        let received_initializers = self.eval_expressions(&arguments).unwrap();

                        if class.initializers.len() != received_initializers.len() {
                            return Object::error(format!(
                                "invalid length of initializers. required: {}, got: {}",
                                class.initializers.len(),
                                received_initializers.len()
                            ));
                        }

                        let mut members = HashMap::new();

                        for stmt in class.body {
                            match stmt {
                                ClassStatement::Variable(decl) => {
                                    let obj = decl.value.map_or(Object::Nil, |val| {
                                        self.eval(Node::Expr(val)).unwrap()
                                    });
                                    members.insert(
                                        hash_method_name(&decl.name),
                                        ClassMember::new(decl.name, obj),
                                    );
                                }

                                ClassStatement::Method(func) => {
                                    let obj = Object::EvaluatedFunction(EvaluatedFunction {
                                        name: func.name.clone(),
                                        parameters: func.parameters,
                                        environment: self.environment.clone(),
                                        body: func.body,
                                    });

                                    members.insert(
                                        hash_method_name(&func.name),
                                        ClassMember::new(func.name, obj),
                                    );
                                }
                            }
                        }

                        for (name, value) in
                            class.initializers.iter().zip(received_initializers.iter())
                        {
                            members.insert(
                                hash_method_name(name),
                                ClassMember::new(name.clone(), value.clone()),
                            );
                        }

                        Object::Class(Class {
                            name: member,
                            members,
                        })
                    }

                    _ => Object::error("invalid constructor".to_string()),
                }
            }
        }
    }

    fn eval_method_expression(
        &mut self,
        arguments: Option<Vec<Expression>>,
        left: Object,
        method: &str,
    ) -> Object {
        let mut arg_objs = Vec::new();
        if let Some(args) = &arguments {
            for arg in args {
                let Some(evaluated) = self.eval(Node::Expr(arg.clone())) else {
                    return Object::error("cannot evaluate arguments".to_string());
                };

                if is_error(&evaluated) {
                    return evaluated;
                }

                arg_objs.push(evaluated);
            }
        }

        let arg_objs = arg_objs.as_slice();
        let evaluated = left.call_method(hash_method_name(method), arguments.map(|_| arg_objs));

        if let (Object::Class(_), func @ Object::EvaluatedFunction(_)) = (left, &evaluated) {
            self.eval_call_expression(func, arg_objs)
        } else {
            evaluated
        }
    }

    fn eval_for_statement(
        &mut self,
        iterator: &Iterable,
        ident: &str,
        body: &BlockStatement,
    ) -> Option<Object> {
        let iter_len = iterator.count();

        if let Iterable::Range(obj) = &iterator {
            if let Err(err) = validate_range(obj) {
                return Some(Object::Error(err));
            }
        }

        if body.is_empty() {
            return None;
        }

        for idx in 0..iter_len {
            let value = iterator.get(idx);
            self.environment
                .set(ident.to_string(), value.clone(), false);

            if let Some(obj) = self.eval_loop_block_statement(body) {
                if is_error(&obj) {
                    return Some(obj);
                }

                if self.loop_state.break_loop {
                    self.loop_state = LoopState::default();

                    break;
                }
            }
        }

        self.environment.delete(ident)
    }

    fn eval_program(&mut self, stmts: &[Statement]) -> Option<Object> {
        let mut result = None;

        for stmt in stmts {
            result = self.eval(Node::Stmt(stmt.clone()));

            if let Some(ref result) = result {
                match result {
                    Object::ReturnValue(ReturnValue { value }) => return Some(*value.clone()),
                    Object::Error(_) => return Some(result.clone()),
                    _ => {}
                }
            }
        }

        result
    }

    fn eval_block_statement(&mut self, stmts: &[Statement]) -> Option<Object> {
        let mut result = None;

        for stmt in stmts {
            result = self.eval(Node::Stmt(stmt.clone()));

            if let Some(result) = result.clone() {
                if matches!(result, Object::ReturnValue(_) | Object::Error(_)) {
                    return Some(result);
                }
            }
        }

        result
    }

    fn eval_loop_block_statement(&mut self, stmts: &[Statement]) -> Option<Object> {
        let mut result = None;

        for stmt in stmts {
            result = self.eval(Node::Stmt(stmt.clone()));

            if self.loop_state.break_loop {
                break;
            }

            if self.loop_state.continue_loop {
                self.loop_state.continue_loop = false;
                break;
            }

            if let Some(result) = result.clone() {
                if matches!(result, Object::ReturnValue(_)) {
                    return Some(result);
                }
            }
        }

        result
    }

    fn eval_prefix_expression(operator: Operator, right: &Object) -> Object {
        match operator {
            Operator::Bang => {
                if is_truthy(right) {
                    Object::FALSE
                } else {
                    Object::TRUE
                }
            }
            Operator::Sub => match right {
                Object::Int(Int { value }) => Object::int(-value),
                Object::Float(Float { value }) => Object::float(-value),
                _ => Object::error(format!("unsupported type for negation: {}", right.kind())),
            },
            _ => Object::error(format!("unknown operator: {}{}", operator, right.kind())),
        }
    }

    fn eval_infix_expression(operator: Operator, left: Object, right: Object) -> Object {
        match (left.clone(), right.clone()) {
            (Object::Nil, Object::Nil) => match operator {
                Operator::Eq => Object::TRUE,
                Operator::NotEq => Object::FALSE,
                _ => Object::error(format!(
                    "unknown operator: {} {} {}",
                    left.kind(),
                    operator,
                    right.kind()
                )),
            },
            (Object::Nil, _) | (_, Object::Nil) => match operator {
                Operator::Eq => Object::FALSE,
                Operator::NotEq => Object::TRUE,
                _ => Object::error(format!(
                    "unknown operator: {} {} {}",
                    left.kind(),
                    operator,
                    right.kind()
                )),
            },
            _ if operator == Operator::And => {
                if is_truthy(&left) {
                    right
                } else {
                    left
                }
            }
            _ if operator == Operator::Or => {
                if is_truthy(&left) {
                    left
                } else {
                    right
                }
            }
            (Object::Int(Int { value: left }), Object::Int(Int { value: right })) => {
                Self::eval_integer_infix_expression(operator, left, right)
            }
            (Object::Float(Float { value: left }), Object::Float(Float { value: right })) => {
                Self::eval_float_infix_expression(operator, left, right)
            }
            (Object::Char(Char { value: left }), Object::Char(Char { value: right })) => {
                Self::eval_char_infix_expression(operator, left, right)
            }
            (Object::Bool(Bool { value: left }), Object::Bool(Bool { value: right })) => {
                match operator {
                    Operator::Eq => native_bool_boolean_object(left == right),
                    Operator::NotEq => native_bool_boolean_object(left != right),
                    _ => Object::error(format!(
                        "unsupported types for binary operation: BOOLEAN {operator} BOOLEAN",
                    )),
                }
            }
            (Object::Type(Type { id: left, .. }), Object::Type(Type { id: right, .. })) => {
                match operator {
                    Operator::Eq => native_bool_boolean_object(left == right),
                    Operator::NotEq => native_bool_boolean_object(left != right),
                    _ => Object::error(format!("unknown operator: TYPE {operator} TYPE",)),
                }
            }
            (Object::Str(Str { value: left }), Object::Str(Str { value: right })) => {
                Self::eval_string_infix_expression(operator, &left, &right)
            }
            _ if left.kind() != right.kind() => Object::error(format!(
                "unsupported types for binary operation: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
            _ => Object::error(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
        }
    }

    fn eval_integer_infix_expression(operator: Operator, left: isize, right: isize) -> Object {
        match operator {
            Operator::Add => Object::int(left + right),
            Operator::Sub => Object::int(left - right),
            Operator::Mul => Object::int(left * right),
            Operator::Div => Object::int(left / right),
            Operator::Mod => Object::int(left % right),
            Operator::BitXor => Object::int(left ^ right),
            Operator::BitAnd => Object::int(left & right),
            Operator::BitOr => Object::int(left | right),
            Operator::Shr => Object::int(left >> right),
            Operator::Shl => Object::int(left << right),
            Operator::Lt => native_bool_boolean_object(left < right),
            Operator::Gt => native_bool_boolean_object(left > right),
            Operator::Eq => native_bool_boolean_object(left == right),
            Operator::NotEq => native_bool_boolean_object(left != right),
            Operator::LtEq => native_bool_boolean_object(left <= right),
            Operator::GtEq => native_bool_boolean_object(left >= right),
            _ => Object::error(format!("unknown operator: INT {operator} INT",)),
        }
    }

    fn eval_float_infix_expression(operator: Operator, left: f64, right: f64) -> Object {
        match operator {
            Operator::Add => Object::float(left + right),
            Operator::Sub => Object::float(left - right),
            Operator::Mul => Object::float(left * right),
            Operator::Div => Object::float(left / right),
            Operator::Mod => Object::float(left % right),
            Operator::Lt => native_bool_boolean_object(left < right),
            Operator::Gt => native_bool_boolean_object(left > right),
            Operator::Eq => native_bool_boolean_object((left - right).abs() < f64::EPSILON),
            Operator::NotEq => native_bool_boolean_object((left - right).abs() > f64::EPSILON),
            Operator::LtEq => native_bool_boolean_object(left <= right),
            Operator::GtEq => native_bool_boolean_object(left >= right),
            _ => Object::error(format!("unknown operator: FLOAT {operator} FLOAT",)),
        }
    }

    fn eval_char_infix_expression(operator: Operator, left: char, right: char) -> Object {
        match operator {
            Operator::Lt => native_bool_boolean_object(left < right),
            Operator::Gt => native_bool_boolean_object(left > right),
            Operator::LtEq => native_bool_boolean_object(left <= right),
            Operator::GtEq => native_bool_boolean_object(left >= right),
            Operator::Eq => native_bool_boolean_object(left == right),
            Operator::NotEq => native_bool_boolean_object(left != right),
            Operator::Add => Object::Str(Str {
                value: format!("{left}{right}"),
            }),
            _ => Object::error(format!("unknown operator: CHAR {operator} CHAR",)),
        }
    }

    fn eval_string_infix_expression(operator: Operator, left: &str, right: &str) -> Object {
        match operator {
            Operator::Add => {
                let mut new_val = left.to_string();
                new_val.push_str(right);
                Object::Str(Str { value: new_val })
            }
            Operator::Eq => Object::bool(left == right),
            Operator::NotEq => Object::bool(left != right),
            _ => Object::error(format!("unknown operator: STR {operator} STR",)),
        }
    }

    fn eval_if_expression(
        &mut self,
        condition: Expression,
        consequence: &[Statement],
        alternative: &Option<BlockStatement>,
    ) -> Option<Object> {
        let condition = self.eval(Node::Expr(condition))?;

        if is_error(&condition) {
            return Some(condition);
        }

        if is_truthy(&condition) {
            self.eval_block_statement(consequence)
        } else if let Some(alternative) = &alternative {
            self.eval_block_statement(alternative)
        } else {
            Some(Object::Nil)
        }
    }

    fn eval_identifier(&self, value: String) -> Object {
        if let Some((val, _)) = self.environment.get(value.clone()) {
            val
        } else if let Some(func) = get_builtin_by_name(&value) {
            Object::Builtin(Builtin {
                name: value,
                func,
                caller: None,
            })
        } else {
            Object::error(format!("undefined variable {value}"))
        }
    }

    fn eval_array_expressions(&mut self, exprs: &[Expression]) -> Option<Vec<Object>> {
        let mut result = Vec::new();

        for expr in exprs {
            let evaluated = self.eval(Node::Expr(expr.clone()))?;
            if is_error(&evaluated) {
                return Some([evaluated].to_vec());
            }

            if !allowed_in_array(&evaluated) {
                return Some(
                    [
                        Object::error(format!("ARRAY cannot contain {}", evaluated.kind()))
                            as Object,
                    ]
                    .to_vec(),
                );
            }

            result.push(evaluated);
        }

        Some(result)
    }

    fn eval_expressions(&mut self, exprs: &[Expression]) -> Option<Vec<Object>> {
        let mut result = Vec::new();

        for expr in exprs {
            let evaluated = self.eval(Node::Expr(expr.clone()))?;
            if is_error(&evaluated) {
                return Some([evaluated].to_vec());
            }

            result.push(evaluated);
        }

        Some(result)
    }

    pub fn eval_call_expression(&mut self, func: &Object, args: &[Object]) -> Object {
        match func {
            Object::EvaluatedFunction(func) => {
                if func.parameters.len() != args.len() {
                    return Object::error(format!(
                        "wrong number of arguments. got: {}, want: {}",
                        args.len(),
                        func.parameters.len()
                    ));
                }

                let extended_env = extend_function_env(func.clone(), args);

                let old_env = self.environment.clone();
                self.environment = extended_env;

                let evaluated = self.eval_block_statement(&func.body).unwrap_or(Object::Nil);

                self.environment = old_env;

                if is_error(&evaluated) {
                    return evaluated;
                }

                unwrap_return_value(evaluated)
            }

            Object::Builtin(Builtin { func, caller, .. }) => func(
                &(caller.clone().unwrap_or_else(|| Box::new(Object::Nil))),
                args,
            ),

            _ => Object::error(format!("not a function: {}", func.kind())),
        }
    }

    fn eval_index_expression(left: &Object, index: &Object) -> Object {
        match (left, index) {
            (Object::Array(Array { elements }), Object::Int(Int { value })) => {
                Self::eval_array_index_expression(elements, *value)
            }
            (Object::Str(Str { value: left }), Object::Int(Int { value })) => {
                Self::eval_string_index_expression(left, *value)
            }
            (Object::Array(Array { elements }), Object::Range(RangeObj { start, end, step })) => {
                Self::eval_array_slice_expression(elements, *start, *end, *step)
            }
            (Object::Str(Str { value }), Object::Range(RangeObj { start, end, step })) => {
                Self::eval_string_slice_expression(value, *start, *end, *step)
            }
            (Object::Dict(Dict { pairs }), _) => Self::eval_dict_index_expression(pairs, index),
            _ => Object::error(format!(
                "index operator not supported: {}[{}]",
                left.kind(),
                index.kind()
            )),
        }
    }

    fn eval_array_index_expression(array: &[Object], idx: isize) -> Object {
        let max = array.len();
        let idx = normalize_index(idx, max);

        if idx >= max {
            return Object::error(format!("index out of bounds. got: {idx}"));
        }

        array[idx].clone()
    }

    fn eval_string_index_expression(string: &str, idx: isize) -> Object {
        let max = string.len();
        let idx = normalize_index(idx, max);

        if idx >= max {
            return Object::error(format!("index out of bounds. got: {idx}"));
        }

        Object::char(string.chars().nth(idx).unwrap())
    }

    fn eval_array_slice_expression(
        array: &[Object],
        start: isize,
        end: isize,
        step: isize,
    ) -> Object {
        let max = (array.len() - 1).try_into().unwrap();

        if start > max || end > max || start < 0 || end < 0 || start > end {
            return Object::error("cannot slice ARRAY using this range".to_string());
        }

        let mut elements = Vec::new();

        let mut i = start;
        while i < end {
            elements.push(array[TryInto::<usize>::try_into(i).unwrap()].clone());
            i += step;
        }

        Object::array(elements)
    }

    fn eval_string_slice_expression(string: &str, start: isize, end: isize, step: isize) -> Object {
        let max = (string.len() - 1).try_into().unwrap();

        if start > max || end > max || start < 0 || end < 0 || start > end {
            return Object::error("cannot slice ARRAY using this range".to_string());
        }

        let mut value = String::new();

        let mut i = start;
        while i < end {
            value.push(
                string
                    .chars()
                    .nth(TryInto::<usize>::try_into(i).unwrap())
                    .unwrap(),
            );
            i += step;
        }

        Object::Str(Str { value })
    }

    fn eval_dict_index_expression(pairs: &HashMap<u64, DictPair>, index: &Object) -> Object {
        let Some(hashable) = Hashable::from_object(index) else {
            return Object::error(format!("unusable as hash key: {}", index.kind()));
        };

        pairs.get(&hashable.hash()).map_or_else(
            || Object::error(format!("key error. got: {index}")),
            |pair| pair.value.clone(),
        )
    }

    fn eval_dict_literal(&mut self, pairs: &[(Expression, Expression)]) -> Option<Object> {
        let mut obj_pairs = HashMap::new();

        for (key_node, value_node) in pairs {
            let key = self.eval(Node::Expr(key_node.clone()))?;

            if is_error(&key) {
                return Some(key);
            }

            let value = self.eval(Node::Expr(value_node.clone()))?;

            if is_error(&value) {
                return Some(value);
            }

            let Some(hashable) = Hashable::from_object(&key) else {
                return Some(Object::error(format!(
                    "unusable as hash key: {}",
                    key.kind()
                )));
            };

            obj_pairs.insert(
                hashable.hash(),
                DictPair {
                    key: hashable,
                    value,
                },
            );
        }

        Some(Object::dict(obj_pairs))
    }

    fn eval_assign_expression(&mut self, to: Assignable, value: &Expression) -> Option<Object> {
        let val = self.eval(Node::Expr(value.clone()))?;

        if is_error(&val) {
            return Some(val);
        }

        match to {
            Assignable::Identifier(Identifier { value }) => {
                if let Some((_, mutable)) = self.environment.get(value.clone()) {
                    if mutable {
                        self.environment.set(value, val.clone(), mutable);
                        Some(val)
                    } else {
                        Some(Object::error(format!("identifier is not mutable: {value}")))
                    }
                } else {
                    Some(Object::error(format!("undefined variable {value}")))
                }
            }

            Assignable::Index(Index { left, index }) => {
                let index = self.eval(Node::Expr(*index))?;

                if is_error(&index) {
                    return Some(index);
                }

                if let Expression::Identifier(Identifier { value: index_ident }) = *left {
                    if let Some((data, mutable)) = self.environment.get(index_ident.clone()) {
                        if mutable {
                            match (data.clone(), index.clone()) {
                                (
                                    Object::Array(Array { elements }),
                                    Object::Int(Int { value: idx }),
                                ) => {
                                    let mut new_data = elements.clone();
                                    let max = elements.len();
                                    let idx = normalize_index(idx, max);
                                    if idx < max {
                                        new_data[idx] = val.clone();
                                    }

                                    self.environment.set(
                                        index_ident,
                                        Object::array(new_data),
                                        true,
                                    );
                                }

                                (Object::Str(Str { value }), Object::Int(Int { value: idx })) => {
                                    let mut new_data = value.chars().collect::<Vec<_>>();
                                    let max = value.len();
                                    let idx = normalize_index(idx, max);

                                    if let Object::Char(Char { value: ch }) = val {
                                        if idx < max {
                                            new_data[idx] = ch;
                                        }
                                        self.environment.set(
                                            index_ident,
                                            Object::Str(Str {
                                                value: new_data.iter().collect(),
                                            }),
                                            true,
                                        );
                                    } else {
                                        return Some(Object::error(format!(
                                            "cannot assign {} to STR, expected CHAR",
                                            val.kind()
                                        )));
                                    }
                                }

                                (Object::Dict(Dict { pairs }), _) => {
                                    let mut new_data = pairs;

                                    let Some(hashable) = Hashable::from_object(&index) else {
                                        return Some(Object::error(format!(
                                            "unusable as hash key: {}",
                                            index.kind()
                                        )));
                                    };

                                    new_data.insert(
                                        hashable.hash(),
                                        DictPair {
                                            key: hashable,
                                            value: val.clone(),
                                        },
                                    );

                                    self.environment
                                        .set(index_ident, Object::dict(new_data), true);
                                }

                                _ => {
                                    return Some(Object::error(format!(
                                        "cannot assign to index expression: {}[{}]",
                                        data.kind(),
                                        index.kind()
                                    )))
                                }
                            }
                            return Some(val);
                        }
                        return Some(Object::error(format!("identifier is not mutable: {value}")));
                    }
                    return Some(Object::error(format!("undefined variable {value}")));
                }

                Some(Object::error("cannot assign".to_string()))
            }

            Assignable::Method(Method {
                left, name: method, ..
            }) => {
                if let Expression::Identifier(Identifier { value }) = *left {
                    if let Some((data, mutable)) = self.environment.get(value.clone()) {
                        if mutable {
                            let mut new_obj = data.clone();
                            let Object::Class(ast_node) = &mut new_obj else {
                                return Some(Object::error(format!(
                                    "cannot assign to non-class instances. got: {}",
                                    data.kind()
                                )));
                            };

                            match ast_node.members.entry(hash_method_name(&method)) {
                                Entry::Vacant(entry) => {
                                    entry.insert(ClassMember::new(method, val.clone()));
                                }
                                Entry::Occupied(mut entry) => {
                                    entry.insert(ClassMember::new(method, val.clone()));
                                }
                            };

                            self.environment.set(value, new_obj, true);
                            return Some(val);
                        }
                        return Some(Object::error(format!("identifier is not mutable: {value}")));
                    }
                    return Some(Object::error(format!("undefined variable {value}")));
                }

                Some(Object::error(format!("cannot assign to method '{method}'")))
            }
        }
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Nil => false,
        Object::Bool(Bool { value }) => *value,
        Object::Int(Int { value }) => *value != 0,
        Object::Str(Str { value }) => !value.is_empty(),
        Object::Char(Char { value }) => *value != '\0',
        Object::Array(Array { elements }) => !elements.is_empty(),
        Object::Dict(Dict { pairs }) => !pairs.is_empty(),
        Object::Float(Float { value }) => !(value.is_nan() || *value == 0f64),
        _ => true,
    }
}

fn extend_function_env(func: EvaluatedFunction, args: &[Object]) -> Environment {
    let mut environment = Environment::new_enclosed(func.environment.clone());

    for (param_idx, param) in func.parameters.iter().enumerate() {
        environment.set(param.clone(), args[param_idx].clone(), false);
    }

    environment.set(func.name.clone(), Object::EvaluatedFunction(func), false);

    environment
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(ReturnValue { value }) = obj {
        return *value;
    }
    obj
}

fn native_bool_boolean_object(b: bool) -> Object {
    if b {
        Object::TRUE
    } else {
        Object::FALSE
    }
}

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error { .. })
}

fn validate_range(range: &RangeObj) -> Result<(), Error> {
    let rev = range.start > range.end;

    match (rev, range.step.is_negative()) {
        (true, false) => Err(Error {
            value: "start must be less than end in range".to_string(),
        }),
        (false, true) => Err(Error {
            value: "step cannot be negative when start is less than end".to_string(),
        }),
        _ => Ok(()),
    }
}

fn normalize_index(idx: isize, max: usize) -> usize {
    let max: isize = max.try_into().unwrap();

    usize::try_from(if idx.is_negative() { max + idx } else { idx }).unwrap()
}
