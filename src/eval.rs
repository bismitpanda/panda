use std::path::PathBuf;

use hashbrown::{hash_map::Entry, HashMap};

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
        Char, Class, ClassMember, ControlFlow, Dict, Error, EvaluatedFunction, EvaluatedModule,
        Float, HashPair, Hashable, Int, Iterable, Object, Range as RangeObj, ReturnValue, Str,
        Type, DIR_ENV_VAR_NAME, FALSE, NULL_OBJ, TRUE,
    },
    parser::Parser,
};

mod environment;
pub use environment::*;

#[cfg(test)]
mod tests;

pub fn eval(node: Node, environment: &mut Environment) -> Option<Object> {
    match node {
        Node::Program { statements, .. } => {
            return eval_program(&statements, environment);
        }

        Node::Stmt(stmt) => match stmt {
            Statement::ExpressionStmt(ExpressionStmt {
                expression,
                returns,
                ..
            }) => {
                let val = eval(Node::Expr(expression), environment)?;

                if is_error(&val) || returns {
                    return Some(val);
                }
            }

            Statement::Return(Return { return_value, .. }) => {
                let value = eval(Node::Expr(return_value), environment)?;

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
                let val = if let Some(value) = value {
                    eval(Node::Expr(value), environment)?
                } else {
                    NULL_OBJ
                };

                if is_error(&val) {
                    return Some(val);
                }

                environment.set(name, val, mutable);
            }

            Statement::Function(Function {
                ident,
                parameters,
                body,
                ..
            }) => {
                environment.set(
                    ident,
                    Object::EvaluatedFunction(EvaluatedFunction {
                        parameters,
                        environment: environment.clone(),
                        body,
                    }),
                    false,
                );
            }

            Statement::While(While {
                condition, body, ..
            }) => {
                let mut condition_obj = eval(Node::Expr(condition.clone()), environment)?;

                if is_error(&condition_obj) {
                    return Some(condition_obj);
                }

                while is_truthy(&condition_obj) {
                    if let Some(obj) = eval_loop_block_statement(&body, environment) {
                        if is_error(&obj) {
                            return Some(obj);
                        } else if matches!(obj, Object::ControlFlow(ControlFlow::Continue)) {
                            continue;
                        } else if matches!(obj, Object::ControlFlow(ControlFlow::Break)) {
                            break;
                        }
                    }

                    condition_obj = eval(Node::Expr(condition.clone()), environment)?;

                    if is_error(&condition_obj) {
                        return Some(condition_obj);
                    }
                }
            }

            Statement::For(For {
                ident,
                iterator,
                body,
                ..
            }) => {
                let obj = eval(Node::Expr(iterator), environment)?;

                if is_error(&obj) {
                    return Some(obj);
                }

                let Some(iterator) = Iterable::from_object(obj.clone()) else {
                    return Some(new_error(format!("{} is not iterable", obj.kind())));
                };

                return eval_for_statement(&iterator, &ident, &body, environment);
            }

            Statement::ClassDecl(ast_node) => {
                environment.set_type(ast_node.ident.clone(), ast_node);
            }

            Statement::Import(Import { path, alias, .. }) => {
                let path_buf = PathBuf::from(&path);
                let start_dir = std::env::var(DIR_ENV_VAR_NAME).ok()?;

                if let Some(ext) = path_buf.extension() {
                    if ext != "pd" {
                        return Some(new_error("cannot import non panda files".to_string()));
                    }
                    let import_file =
                        std::fs::read_to_string(PathBuf::from(start_dir).join(&path_buf)).ok()?;

                    let mut module_env = Environment::new();

                    let mut lexer = Lexer::new(&import_file);
                    let mut parser = Parser::new(&mut lexer);

                    let program = parser.parse_program();

                    if !parser.errors.is_empty() {
                        println!("parser errors:");
                        for msg in &parser.errors {
                            println!("\t{msg}");
                        }
                        return Some(new_error(format!(
                            "could not import \"{path}\" as it had errors."
                        )));
                    }

                    let evaluated = eval(program.unwrap(), &mut module_env);

                    if let Some(evaluated) = evaluated {
                        if matches!(evaluated, Object::Error { .. }) {
                            println!("{}", evaluated.inspect());
                        }
                    }

                    let module_name = alias.unwrap_or_else(|| {
                        path_buf.file_stem().unwrap().to_str().unwrap().to_string()
                    });

                    environment.set_import(
                        module_name.clone(),
                        EvaluatedModule {
                            environment: module_env,
                            name: module_name,
                        },
                    );
                }
            }

            Statement::Break => {
                return Some(Object::ControlFlow(ControlFlow::Break));
            }

            Statement::Continue => {
                return Some(Object::ControlFlow(ControlFlow::Continue));
            }

            Statement::Delete(Delete { delete_ident, .. }) => {
                return environment.delete(&delete_ident).map_or_else(
                    || {
                        Some(new_error(format!(
                            "no identifier named \"{delete_ident}\" found."
                        )))
                    },
                    Some,
                )
            }
        },

        Node::Expr(expr) => match expr {
            Expression::Prefix(Prefix {
                right, operator, ..
            }) => {
                let right = eval(Node::Expr(*right), environment)?;

                if is_error(&right) {
                    return Some(right);
                }

                return Some(eval_prefix_expression(operator, &right));
            }

            Expression::Infix(Infix {
                left,
                operator,
                right,
                ..
            }) => {
                let left = eval(Node::Expr(*left), environment)?;

                if is_error(&left) {
                    return Some(left);
                }

                let right = eval(Node::Expr(*right), environment)?;

                if is_error(&right) {
                    return Some(right);
                }

                return Some(eval_infix_expression(operator, left, right));
            }

            Expression::If(If {
                condition,
                consequence,
                alternative,
                ..
            }) => {
                return eval_if_expression(*condition, &consequence, alternative, environment);
            }

            Expression::Identifier(Identifier { value, .. }) => {
                return Some(eval_identifier(value, environment));
            }

            Expression::Lambda(Lambda {
                parameters, body, ..
            }) => {
                return Some(Object::EvaluatedFunction(EvaluatedFunction {
                    parameters,
                    environment: environment.clone(),
                    body,
                }));
            }

            Expression::Call(Call {
                function,
                arguments,
                ..
            }) => {
                let function = eval(Node::Expr(*function), environment)?;

                if is_error(&function) {
                    return Some(function);
                }

                let args = eval_expressions(&arguments, environment)?;

                if args.len() == 1 && is_error(&args[0]) {
                    return Some(args[0].clone());
                }

                return Some(apply_function(&function, &args));
            }

            Expression::Index(Index {
                left, expr: index, ..
            }) => {
                let left = eval(Node::Expr(*left), environment)?;

                if is_error(&left) {
                    return Some(left);
                }

                let index = eval(Node::Expr(*index), environment)?;

                if is_error(&index) {
                    return Some(index);
                }

                return Some(eval_index_expression(&left, &index));
            }

            Expression::Assign(Assign { to, value, .. }) => {
                return eval_assign_expression(to, &value, environment);
            }

            Expression::Method(Method {
                left,
                name: method,
                arguments,
                ..
            }) => {
                let left = eval(Node::Expr(*left), environment)?;

                if is_error(&left) {
                    return Some(left);
                }

                return Some(eval_method_expression(
                    arguments,
                    left,
                    &method,
                    environment,
                ));
            }

            Expression::Constructor(Constructor { constructable, .. }) => {
                return Some(eval_constructor_expression(constructable, environment));
            }

            Expression::Range(Range {
                start: node_start,
                end: node_end,
                step: node_step,
                ..
            }) => {
                let start = eval(Node::Expr(*node_start), environment)?;

                if is_error(&start) {
                    return Some(start);
                }

                let end = eval(Node::Expr(*node_end), environment)?;

                if is_error(&end) {
                    return Some(end);
                }

                let mut step = None;
                if let Some(s) = node_step {
                    let evaluated = eval(Node::Expr(*s), environment)?;

                    if is_error(&evaluated) {
                        return Some(evaluated);
                    }

                    step = Some(evaluated);
                }

                let Object::Int(Int { value: start }) = start else {
                    return Some(new_error(format!(
                        "cannot use {} as start in range. expected: INT",
                        start.kind()
                    )));
                };

                let Object::Int(Int { value: end }) = end else {
                    return Some(new_error(format!(
                        "cannot use {} as end in range. expected: INT",
                        end.kind()
                    )));
                };

                let rev = start > end;

                let step = if let Some(step) = step {
                    match step {
                        Object::Int(Int { value }) => value,
                        _ => {
                            return Some(new_error(format!(
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

            Expression::Scope(Scope { module, member, .. }) => {
                let Some(import) = environment.get_import(&module) else {
                    return Some(new_error(format!("no module named \"{module}\" found")));
                };

                match *member {
                    Expression::Identifier(Identifier { ref value, .. }) => {
                        let Some((member, _)) = import.environment.get(value.clone()) else {
                            return Some(new_error(format!(
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
                        let Expression::Identifier(Identifier {
                            value: member_name, ..
                        }) = *function.clone()
                        else {
                            return Some(new_error(
                                "expected Identifier in scope expression".to_string(),
                            ));
                        };

                        let Some((member, _)) = import.environment.get(member_name) else {
                            return Some(new_error(format!(
                                "member '{member}' not found in module '{module}'"
                            )));
                        };

                        if !matches!(member, Object::EvaluatedFunction { .. }) {
                            return Some(new_error(format!("\"{member}\" is not callable")));
                        }

                        let args = eval_expressions(arguments, environment)?;

                        if args.len() == 1 && is_error(&args[0]) {
                            return Some(args[0].clone());
                        }

                        return Some(apply_function(&member, &args));
                    }
                    _ => {
                        return Some(new_error("invalid scope expression".to_string()));
                    }
                };
            }

            Expression::Literal(Literal { lit, .. }) => match lit {
                Lit::Int { value } => return Some(Object::Int(Int { value })),

                Lit::Float { value } => return Some(Object::Float(Float { value })),

                Lit::Bool { value } => return Some(if value { TRUE } else { FALSE }),

                Lit::Str { value } => return Some(Object::Str(Str { value })),

                Lit::Array { elements } => {
                    let elements = eval_array_expressions(&elements, environment)?;
                    if elements.len() == 1 && is_error(&elements[0]) {
                        return Some(elements[0].clone());
                    }

                    return Some(Object::Array(Array { elements }));
                }

                Lit::Char { value } => return Some(Object::Char(Char { value })),

                Lit::Hash { pairs } => {
                    return eval_hash_literal(&pairs, environment);
                }

                Lit::Null => return Some(NULL_OBJ),
            },
        },
    };

    None
}

fn eval_constructor_expression(
    constructable: Constructable,
    environment: &mut Environment,
) -> Object {
    match constructable {
        Constructable::Identifier(Identifier { ref value, .. }) => {
            let Some(class) = environment.get_type(value) else {
                return new_error(format!("no class named '{value}' found."));
            };

            if !class.initializers.is_empty() {
                return new_error(format!(
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
                            .map_or(NULL_OBJ, |val| eval(Node::Expr(val), environment).unwrap());
                        members.insert(
                            hash_method_name(&decl.name),
                            ClassMember::new(decl.name, obj),
                        );
                    }

                    ClassStatement::Method(func) => {
                        let obj = Object::EvaluatedFunction(EvaluatedFunction {
                            parameters: func.parameters,
                            environment: environment.clone(),
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
            let Expression::Identifier(Identifier { value: member, .. }) = *function else {
                return new_error(String::new());
            };

            let Some(class) = environment.get_type(&member) else {
                return new_error(format!("no class named '{member}' found"));
            };

            let received_initializers = eval_expressions(&arguments, environment).unwrap();

            if class.initializers.len() != received_initializers.len() {
                return new_error(format!(
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
                            .map_or(NULL_OBJ, |val| eval(Node::Expr(val), environment).unwrap());
                        members.insert(
                            hash_method_name(&decl.name),
                            ClassMember::new(decl.name, obj),
                        );
                    }

                    ClassStatement::Method(func) => {
                        let obj = Object::EvaluatedFunction(EvaluatedFunction {
                            parameters: func.parameters,
                            environment: environment.clone(),
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
            let Some(module) = environment.get_import(module) else {
                return new_error(format!("no module named '{module}' found"));
            };

            match *member.clone() {
                Expression::Identifier(Identifier { value, .. }) => {
                    let Some(class) = module.environment.get_type(&value) else {
                        return new_error(format!(
                            "no class named '{}' found in module '{}'",
                            member, module.name
                        ));
                    };

                    if !class.initializers.is_empty() {
                        return new_error(format!(
                            "cannot initialize class with 0 variables. required: {}",
                            class.initializers.len()
                        ));
                    }

                    let mut members = HashMap::new();
                    for stmt in class.body {
                        match stmt {
                            ClassStatement::Variable(decl) => {
                                let obj = decl.value.map_or(NULL_OBJ, |val| {
                                    eval(Node::Expr(val), environment).unwrap()
                                });
                                members.insert(
                                    hash_method_name(&decl.name),
                                    ClassMember::new(decl.name, obj),
                                );
                            }

                            ClassStatement::Method(func) => {
                                let obj = Object::EvaluatedFunction(EvaluatedFunction {
                                    parameters: func.parameters,
                                    environment: environment.clone(),
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
                    let Expression::Identifier(Identifier { value: member, .. }) = *function else {
                        return new_error(String::new());
                    };

                    let Some(class) = module.environment.get_type(&member) else {
                        return new_error(format!(
                            "no class named '{}' found in module '{}'",
                            member, module.name
                        ));
                    };

                    let received_initializers = eval_expressions(&arguments, environment).unwrap();

                    if class.initializers.len() != received_initializers.len() {
                        return new_error(format!(
                            "invalid length of initializers. required: {}, got: {}",
                            class.initializers.len(),
                            received_initializers.len()
                        ));
                    }

                    let mut members = HashMap::new();

                    for stmt in class.body {
                        match stmt {
                            ClassStatement::Variable(decl) => {
                                let obj = decl.value.map_or(NULL_OBJ, |val| {
                                    eval(Node::Expr(val), environment).unwrap()
                                });
                                members.insert(decl.name, obj);
                            }

                            ClassStatement::Method(func) => {
                                let obj = Object::EvaluatedFunction(EvaluatedFunction {
                                    parameters: func.parameters,
                                    environment: environment.clone(),
                                    body: func.body,
                                });
                                members.insert(func.name, obj);
                            }
                        }
                    }

                    for (name, value) in class.initializers.iter().zip(received_initializers.iter())
                    {
                        members.insert(name.clone(), value.clone());
                    }

                    Object::Class(Class {
                        name: member,
                        members: HashMap::new(),
                    })
                }

                _ => new_error("invalid constructor".to_string()),
            }
        }
    }
}

fn eval_method_expression(
    arguments: Option<Vec<Expression>>,
    left: Object,
    method: &str,
    environment: &mut Environment,
) -> Object {
    let mut arg_objs = Vec::new();
    if let Some(args) = &arguments {
        for arg in args {
            let Some(evaluated) = eval(Node::Expr(arg.clone()), environment) else {
                return new_error("cannot evaluate arguments".to_string());
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
        apply_function(func, arg_objs)
    } else {
        evaluated
    }
}

fn eval_for_statement(
    iterator: &Iterable,
    ident: &str,
    body: &BlockStatement,
    environment: &mut Environment,
) -> Option<Object> {
    let iter_len = iterator.count();

    if let Iterable::Range(obj) = &iterator {
        if let Err(err) = validate_range(obj) {
            return Some(Object::Error(err));
        }
    }

    for idx in 0..iter_len {
        let value = iterator.get(idx);
        environment.set(ident.to_string(), value.clone(), false);
        if let Some(obj) = eval_loop_block_statement(body, environment) {
            if is_error(&obj) {
                return Some(obj);
            } else if matches!(obj, Object::ControlFlow(ControlFlow::Continue)) {
                continue;
            } else if matches!(obj, Object::ControlFlow(ControlFlow::Break)) {
                break;
            }
        }
    }

    environment.delete(ident)
}

fn eval_program(stmts: &[Statement], environment: &mut Environment) -> Option<Object> {
    let mut result = None;

    for stmt in stmts {
        result = eval(Node::Stmt(stmt.clone()), environment);

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

fn eval_block_statement(stmts: &[Statement], environment: &mut Environment) -> Option<Object> {
    let mut result = None;

    for stmt in stmts {
        result = eval(Node::Stmt(stmt.clone()), environment);

        if let Some(result) = result.clone() {
            if matches!(result, Object::ReturnValue(_) | Object::Error(_)) {
                return Some(result);
            } else if matches!(result, Object::ControlFlow(_)) {
                return Some(new_error(
                    "cannot use control flow statements outside loops".to_string(),
                ));
            }
        }
    }

    result
}

fn eval_loop_block_statement(stmts: &[Statement], environment: &mut Environment) -> Option<Object> {
    let mut result = None;

    for stmt in stmts {
        result = eval(Node::Stmt(stmt.clone()), environment);

        if let Some(result) = result.clone() {
            if matches!(
                result,
                Object::ReturnValue(_) | Object::ControlFlow(_) | Object::Error(_)
            ) {
                return Some(result);
            }
        }
    }

    result
}

fn eval_prefix_expression(operator: Operator, right: &Object) -> Object {
    match operator {
        Operator::Bang => eval_bang_operator_expression(right),
        Operator::Sub => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!("unknown operator: {}{}", operator, right.kind())),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    if is_truthy(right) {
        FALSE
    } else {
        TRUE
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    match right {
        Object::Int(Int { value }) => Object::Int(Int { value: -value }),
        Object::Float(Float { value }) => Object::Float(Float { value: -value }),
        _ => new_error(format!("unknown operator: -{}", right.kind())),
    }
}

fn eval_infix_expression(operator: Operator, left: Object, right: Object) -> Object {
    match (left.clone(), right.clone()) {
        (Object::Null, Object::Null) => match operator {
            Operator::Eq => TRUE,
            Operator::NotEq => FALSE,
            _ => new_error(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
        },
        (Object::Null, _) | (_, Object::Null) => match operator {
            Operator::Eq => FALSE,
            Operator::NotEq => TRUE,
            _ => new_error(format!(
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
            eval_integer_infix_expression(operator, left, right)
        }
        (Object::Float(Float { value: left }), Object::Float(Float { value: right })) => {
            eval_float_infix_expression(operator, left, right)
        }
        (Object::Char(Char { value: left }), Object::Char(Char { value: right })) => {
            eval_char_infix_expression(operator, left, right)
        }
        (Object::Bool(Bool { value: left }), Object::Bool(Bool { value: right })) => match operator
        {
            Operator::Eq => native_bool_boolean_object(left == right),
            Operator::NotEq => native_bool_boolean_object(left != right),
            _ => new_error(format!("unknown operator: BOOL {operator} BOOL",)),
        },
        (Object::Type(Type { id: left, .. }), Object::Type(Type { id: right, .. })) => {
            match operator {
                Operator::Eq => native_bool_boolean_object(left == right),
                Operator::NotEq => native_bool_boolean_object(left != right),
                _ => new_error(format!("unknown operator: TYPE {operator} TYPE",)),
            }
        }
        (Object::Str(Str { value: left }), Object::Str(Str { value: right })) => {
            eval_string_infix_expression(operator, &left, &right)
        }
        _ if left.kind() != right.kind() => new_error(format!(
            "type mismatch: {} {} {}",
            left.kind(),
            operator,
            right.kind()
        )),
        _ => new_error(format!(
            "unknown operator: {} {} {}",
            left.kind(),
            operator,
            right.kind()
        )),
    }
}

fn eval_integer_infix_expression(operator: Operator, left: isize, right: isize) -> Object {
    match operator {
        Operator::Add => Object::Int(Int {
            value: left + right,
        }),
        Operator::Sub => Object::Int(Int {
            value: left - right,
        }),
        Operator::Mul => Object::Int(Int {
            value: left * right,
        }),
        Operator::Div => Object::Int(Int {
            value: left / right,
        }),
        Operator::BitXor => Object::Int(Int {
            value: left ^ right,
        }),
        Operator::BitAnd => Object::Int(Int {
            value: left & right,
        }),
        Operator::BitOr => Object::Int(Int {
            value: left | right,
        }),
        Operator::Shr => Object::Int(Int {
            value: left >> right,
        }),
        Operator::Shl => Object::Int(Int {
            value: left << right,
        }),
        Operator::Lt => native_bool_boolean_object(left < right),
        Operator::Gt => native_bool_boolean_object(left > right),
        Operator::Eq => native_bool_boolean_object(left == right),
        Operator::NotEq => native_bool_boolean_object(left != right),
        Operator::LtEq => native_bool_boolean_object(left <= right),
        Operator::GtEq => native_bool_boolean_object(left >= right),
        _ => new_error(format!("unknown operator: INT {operator} INT",)),
    }
}

fn eval_float_infix_expression(operator: Operator, left: f64, right: f64) -> Object {
    match operator {
        Operator::Add => Object::Float(Float {
            value: left + right,
        }),
        Operator::Sub => Object::Float(Float {
            value: left - right,
        }),
        Operator::Mul => Object::Float(Float {
            value: left * right,
        }),
        Operator::Div => Object::Float(Float {
            value: left / right,
        }),
        Operator::Lt => native_bool_boolean_object(left < right),
        Operator::Gt => native_bool_boolean_object(left > right),
        Operator::Eq => native_bool_boolean_object((left - right).abs() < f64::EPSILON),
        Operator::NotEq => native_bool_boolean_object((left - right).abs() > f64::EPSILON),
        Operator::LtEq => native_bool_boolean_object(left <= right),
        Operator::GtEq => native_bool_boolean_object(left >= right),
        _ => new_error(format!("unknown operator: FLOAT {operator} FLOAT",)),
    }
}

fn eval_char_infix_expression(operator: Operator, left: char, right: char) -> Object {
    match operator {
        Operator::Lt => native_bool_boolean_object(left < right),
        Operator::Gt => native_bool_boolean_object(left > right),
        Operator::Eq => native_bool_boolean_object(left == right),
        Operator::NotEq => native_bool_boolean_object(left != right),
        Operator::Add => Object::Str(Str {
            value: format!("{left}{right}"),
        }),
        _ => new_error(format!("unknown operator: CHAR {operator} CHAR",)),
    }
}

fn eval_string_infix_expression(operator: Operator, left: &str, right: &str) -> Object {
    match operator {
        Operator::Add => {
            let mut new_val = left.to_string();
            new_val.push_str(right);
            Object::Str(Str { value: new_val })
        }
        Operator::Eq => Object::Bool(Bool {
            value: left == right,
        }),
        Operator::NotEq => Object::Bool(Bool {
            value: left != right,
        }),
        _ => new_error(format!("unknown operator: STR {operator} STR",)),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: &[Statement],
    alternative: Option<BlockStatement>,
    environment: &mut Environment,
) -> Option<Object> {
    let condition = eval(Node::Expr(condition), environment)?;

    if is_error(&condition) {
        return Some(condition);
    }

    if is_truthy(&condition) {
        eval_block_statement(consequence, environment)
    } else if let Some(alternative) = alternative {
        eval_block_statement(&alternative, environment)
    } else {
        Some(NULL_OBJ)
    }
}

fn eval_identifier(value: String, environment: &Environment) -> Object {
    if let Some((val, _)) = environment.get(value.clone()) {
        val
    } else if let Some(func) = get_builtin_by_name(&value) {
        Object::Builtin(Builtin {
            name: value,
            func,
            caller: None,
        })
    } else {
        new_error(format!("identifier not found: {value}"))
    }
}

fn eval_array_expressions(
    exprs: &[Expression],
    environment: &mut Environment,
) -> Option<Vec<Object>> {
    let mut result = Vec::new();

    for expr in exprs {
        let evaluated = eval(Node::Expr(expr.clone()), environment)?;
        if is_error(&evaluated) {
            return Some([evaluated].to_vec());
        }

        if !allowed_in_array(&evaluated) {
            return Some(
                [new_error(format!("ARRAY cannot contain {}", evaluated.kind())) as Object]
                    .to_vec(),
            );
        }

        result.push(evaluated);
    }

    Some(result)
}

fn eval_expressions(exprs: &[Expression], environment: &mut Environment) -> Option<Vec<Object>> {
    let mut result = Vec::new();

    for expr in exprs {
        let evaluated = eval(Node::Expr(expr.clone()), environment)?;
        if is_error(&evaluated) {
            return Some([evaluated].to_vec());
        }

        result.push(evaluated);
    }

    Some(result)
}

pub fn apply_function(func: &Object, args: &[Object]) -> Object {
    match func {
        Object::EvaluatedFunction(func) => {
            let mut extended_env = extend_function_env(func.clone(), args);
            let evaluated = eval_block_statement(&func.body, &mut extended_env).unwrap_or(NULL_OBJ);

            if is_error(&evaluated) {
                return evaluated;
            }

            unwrap_return_value(evaluated)
        }

        Object::Builtin(Builtin { func, caller, .. }) => func(
            &(caller.clone().unwrap_or_else(|| Box::new(NULL_OBJ))),
            args,
        ),

        _ => new_error(format!("not a function: {}", func.kind())),
    }
}

fn extend_function_env(func: EvaluatedFunction, args: &[Object]) -> Environment {
    let mut environment = Environment::new_enclosed(func.environment);

    for (param_idx, param) in func.parameters.iter().enumerate() {
        environment.set(param.clone(), args[param_idx].clone(), false);
    }

    environment
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(ReturnValue { value }) = obj {
        return *value;
    }
    obj
}

fn eval_index_expression(left: &Object, index: &Object) -> Object {
    match (left, index) {
        (Object::Array(Array { elements }), Object::Int(Int { value })) => {
            eval_array_index_expression(elements, *value)
        }
        (Object::Str(Str { value: left }), Object::Int(Int { value })) => {
            eval_string_index_expression(left, *value)
        }
        (Object::Array(Array { elements }), Object::Range(RangeObj { start, end, step })) => {
            eval_array_slice_expression(elements, *start, *end, *step)
        }
        (Object::Str(Str { value }), Object::Range(RangeObj { start, end, step })) => {
            eval_string_slice_expression(value, *start, *end, *step)
        }
        (Object::Dict(Dict { pairs }), _) => eval_hash_index_expression(pairs, index),
        _ => new_error(format!(
            "index operator not supported: {}[{}]",
            left.kind(),
            index.kind()
        )),
    }
}

fn eval_array_index_expression(array: &[Object], idx: isize) -> Object {
    let max = (array.len() - 1) as isize;

    if idx < 0 || idx > max {
        return NULL_OBJ;
    }

    array[idx as usize].clone()
}

fn eval_string_index_expression(string: &str, idx: isize) -> Object {
    let max = (string.len() - 1) as isize;

    if idx < 0 || idx > max {
        return NULL_OBJ;
    }

    Object::Char(Char {
        value: string.chars().nth(idx as usize).unwrap(),
    })
}

fn eval_array_slice_expression(array: &[Object], start: isize, end: isize, step: isize) -> Object {
    let max = (array.len() - 1).try_into().unwrap();

    if start > max || end > max || start < 0 || end < 0 || start > end {
        return new_error("cannot slice ARRAY using this range".to_string());
    }

    let mut elements = Vec::new();

    let mut i = start;
    while i < end {
        elements.push(array[TryInto::<usize>::try_into(i).unwrap()].clone());
        i += step;
    }

    Object::Array(Array { elements })
}

fn eval_string_slice_expression(string: &str, start: isize, end: isize, step: isize) -> Object {
    let max = (string.len() - 1).try_into().unwrap();

    if start > max || end > max || start < 0 || end < 0 || start > end {
        return new_error("cannot slice ARRAY using this range".to_string());
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

fn eval_hash_index_expression(pairs: &HashMap<u64, HashPair>, index: &Object) -> Object {
    let Some(hashable) = Hashable::from_object(index) else {
        return new_error(format!("unusable as hash key: {}", index.kind()));
    };

    pairs
        .get(&hashable.hash_key())
        .map_or(NULL_OBJ, |pair| pair.value.clone())
}

fn eval_hash_literal(
    pairs: &[(Expression, Expression)],
    environment: &mut Environment,
) -> Option<Object> {
    let mut obj_pairs = HashMap::new();

    for (key_node, value_node) in pairs {
        let key = eval(Node::Expr(key_node.clone()), environment)?;

        if is_error(&key) {
            return Some(key);
        }

        let value = eval(Node::Expr(value_node.clone()), environment)?;

        if is_error(&value) {
            return Some(value);
        }

        let Some(hashable) = Hashable::from_object(&key) else {
            return Some(new_error(format!("unusable as hash key: {}", key.kind())));
        };

        obj_pairs.insert(
            hashable.hash_key(),
            HashPair {
                key: hashable,
                value,
            },
        );
    }

    Some(Object::Dict(Dict { pairs: obj_pairs }))
}

fn eval_assign_expression(
    to: Assignable,
    value: &Expression,
    environment: &mut Environment,
) -> Option<Object> {
    let val = eval(Node::Expr(value.clone()), environment)?;

    if is_error(&val) {
        return Some(val);
    }

    match to {
        Assignable::Identifier(Identifier { value, .. }) => {
            if let Some((_, mutable)) = environment.get(value.clone()) {
                if mutable {
                    environment.set(value, val.clone(), mutable);
                    Some(val)
                } else {
                    Some(new_error(format!("identifier is not mutable: {value}")))
                }
            } else {
                Some(new_error(format!("identifier not found: {value}")))
            }
        }
        Assignable::Index(Index {
            left, expr: index, ..
        }) => {
            let index = eval(Node::Expr(*index), environment)?;

            if is_error(&index) {
                return Some(index);
            }

            if let Expression::Identifier(Identifier {
                value: index_ident, ..
            }) = *left
            {
                if let Some((data, mutable)) = environment.get(index_ident.clone()) {
                    if mutable {
                        match (data.clone(), index.clone()) {
                            (
                                Object::Array(Array { elements }),
                                Object::Int(Int { value: idx }),
                            ) => {
                                let mut new_data = elements.clone();
                                let max = (elements.len() - 1).try_into().unwrap();
                                if idx > 0 && idx < max {
                                    new_data[usize::try_from(idx).unwrap()] = val.clone();
                                }

                                environment.set(
                                    index_ident,
                                    Object::Array(Array { elements: new_data }),
                                    true,
                                );
                            }

                            (Object::Str(Str { value }), Object::Int(Int { value: idx })) => {
                                let mut new_data = value.chars().collect::<Vec<_>>();
                                if let Object::Char(Char { value: ch }) = val {
                                    new_data[usize::try_from(idx).unwrap()] = ch;
                                    environment.set(
                                        index_ident,
                                        Object::Str(Str {
                                            value: new_data.iter().collect(),
                                        }),
                                        true,
                                    );
                                } else {
                                    return Some(new_error(format!(
                                        "cannot assign {} to STR, expected CHAR",
                                        val.kind()
                                    )));
                                }
                            }

                            (Object::Dict(Dict { pairs }), _) => {
                                let mut new_data = pairs;

                                let Some(hashable) = Hashable::from_object(&index) else {
                                    return Some(new_error(format!(
                                        "unusable as hash key: {}",
                                        index.kind()
                                    )));
                                };

                                new_data.insert(
                                    hashable.hash_key(),
                                    HashPair {
                                        key: hashable,
                                        value: val.clone(),
                                    },
                                );
                                environment.set(
                                    index_ident,
                                    Object::Dict(Dict { pairs: new_data }),
                                    true,
                                );
                            }

                            _ => {
                                return Some(new_error(format!(
                                    "cannot assign to index expression: {}[{}]",
                                    data.kind(),
                                    index.kind()
                                )))
                            }
                        }
                        return Some(val);
                    }
                    return Some(new_error(format!("identifier is not mutable: {value}")));
                }
                return Some(new_error(format!("identifier not found: {value}")));
            }

            Some(new_error("cannot assign".to_string()))
        }

        Assignable::Method(Method {
            left, name: method, ..
        }) => {
            if let Expression::Identifier(Identifier { value, .. }) = *left {
                if let Some((data, mutable)) = environment.get(value.clone()) {
                    if mutable {
                        let mut new_obj = data.clone();
                        let Object::Class(ast_node) = &mut new_obj else {
                            return Some(new_error(format!(
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

                        environment.set(value, new_obj, true);
                        return Some(val);
                    }
                    return Some(new_error(format!("identifier is not mutable: {value}")));
                }
                return Some(new_error(format!("identifier not found: {value}")));
            }

            Some(new_error(format!("cannot assign to method '{method}'")))
        }
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
        _ => true,
    }
}

fn native_bool_boolean_object(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

fn new_error(message: String) -> Object {
    Object::Error(Error { message })
}

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error { .. })
}

fn validate_range(range: &RangeObj) -> Result<(), Error> {
    let rev = range.start > range.end;

    match (rev, range.step.is_negative()) {
        (true, false) => Err(Error {
            message: "start must be less than end in range".to_string(),
        }),
        (false, true) => Err(Error {
            message: "step cannot be negative when start is less than end".to_string(),
        }),
        _ => Ok(()),
    }
}
