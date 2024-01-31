use pretty_assertions::assert_eq;

use super::*;
use crate::{code::*, lexer::Lexer, parser::Parser};

struct TestCase {
    input: String,
    expected_constants: Vec<Object>,
    expected_instructions: Vec<Instructions>,
}

#[test]
fn test_integer_arithmetic() {
    run_compiler_tests(&[
        TestCase {
            input: "1".to_string(),
            expected_constants: Vec::from([Object::int(1)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 + 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Add, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1; 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::PopNoRet, &[]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 - 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Sub, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 * 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Mul, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 / 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Div, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 % 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Mod, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 ^ 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::BitXor, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 & 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::BitAnd, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 | 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::BitOr, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 >> 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Shr, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 << 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Shl, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "-1".to_string(),
            expected_constants: Vec::from([Object::int(1)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Minus, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "-1.0".to_string(),
            expected_constants: Vec::from([Object::float(1.0)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Minus, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_boolean_expressions() {
    run_compiler_tests(&[
        TestCase {
            input: "true".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([make(Opcode::True, &[]), make(Opcode::Pop, &[])]),
        },
        TestCase {
            input: "false".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([make(Opcode::False, &[]), make(Opcode::Pop, &[])]),
        },
        TestCase {
            input: "1 < 2".to_string(),
            expected_constants: Vec::from([Object::int(2), Object::int(1)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThan, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 > 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThan, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 <= 2".to_string(),
            expected_constants: Vec::from([Object::int(2), Object::int(1)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThanEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 >= 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThanEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 == 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Equal, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "1 != 2".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::NotEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "true == false".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::False, &[]),
                make(Opcode::Equal, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "true != false".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::False, &[]),
                make(Opcode::NotEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "!true".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::Bang, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_nil_literal() {
    run_compiler_tests(&[TestCase {
        input: "nil".to_string(),
        expected_constants: Vec::new(),
        expected_instructions: Vec::from([make(Opcode::Nil, &[]), make(Opcode::Pop, &[])]),
    }]);
}

#[test]
fn test_conditionals() {
    run_compiler_tests(&[
        TestCase {
            input: "if (true) { 10 }; 3333;".to_string(),
            expected_constants: Vec::from([Object::int(10), Object::int(3333)]),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::JumpNotTruthy, &[10]),
                make(Opcode::Constant, &[0]),
                make(Opcode::Jump, &[11]),
                make(Opcode::Nil, &[]),
                make(Opcode::PopNoRet, &[]),
                make(Opcode::Constant, &[1]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
        TestCase {
            input: "if (true) { 10 } else { 20 }; 3333;".to_string(),
            expected_constants: Vec::from([Object::int(10), Object::int(20), Object::int(3333)]),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::JumpNotTruthy, &[10]),
                make(Opcode::Constant, &[0]),
                make(Opcode::Jump, &[13]),
                make(Opcode::Constant, &[1]),
                make(Opcode::PopNoRet, &[]),
                make(Opcode::Constant, &[2]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_while_loop() {
    run_compiler_tests(&[TestCase {
        input: "while (true) { 10; }".to_string(),
        expected_constants: Vec::from([Object::int(10)]),
        expected_instructions: Vec::from([
            make(Opcode::True, &[]),
            make(Opcode::JumpNotTruthy, &[12]),
            make(Opcode::Constant, &[0]),
            make(Opcode::PopNoRet, &[]),
            make(Opcode::Jump, &[0]),
            make(Opcode::Pop, &[]),
        ]),
    }]);
}

#[test]
fn test_global_declaration_expressions() {
    run_compiler_tests(&[TestCase {
        input: "var one = 1; one = 2;".to_string(),
        expected_constants: Vec::from([Object::int(1), Object::int(2)]),
        expected_instructions: Vec::from([
            make(Opcode::Constant, &[0]),
            make(Opcode::SetGlobal, &[0]),
            make(Opcode::Constant, &[1]),
            make(Opcode::Dup, &[]),
            make(Opcode::SetGlobal, &[0]),
            make(Opcode::PopNoRet, &[]),
        ]),
    }]);
}

#[test]
fn test_string_expressions() {
    run_compiler_tests(&[
        TestCase {
            input: "\"panda\"".to_string(),
            expected_constants: Vec::from([Object::str("panda".to_string())]),
            expected_instructions: [make(Opcode::Constant, &[0]), make(Opcode::Pop, &[])].to_vec(),
        },
        TestCase {
            input: "\"mon\" + \"key\"".to_string(),
            expected_constants: Vec::from([
                Object::str("mon".to_string()),
                Object::str("key".to_string()),
            ]),
            expected_instructions: [
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Add, &[]),
                make(Opcode::Pop, &[]),
            ]
            .to_vec(),
        },
    ]);
}

#[test]
fn test_char_expressions() {
    run_compiler_tests(&[
        TestCase {
            input: "'a'".to_string(),
            expected_constants: Vec::from([Object::char('a')]),
            expected_instructions: [make(Opcode::Constant, &[0]), make(Opcode::Pop, &[])].to_vec(),
        },
        TestCase {
            input: "'h' + 'i'".to_string(),
            expected_constants: Vec::from([Object::char('h'), Object::char('i')]),
            expected_instructions: [
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Add, &[]),
                make(Opcode::Pop, &[]),
            ]
            .to_vec(),
        },
    ]);
}

#[test]
fn test_array_literals() {
    run_compiler_tests(&[
        TestCase {
            input: "[]".to_string(),
            expected_constants: Vec::from([]),
            expected_instructions: Vec::from([make(Opcode::Array, &[0]), make(Opcode::Pop, &[])]),
        },
        TestCase {
            input: "[1, 2, 3]".to_string(),
            expected_constants: Vec::from([Object::int(1), Object::int(2), Object::int(3)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Array, &[3]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "[1 + 2, 3 - 4, 5 * 6]".to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::int(2),
                Object::int(3),
                Object::int(4),
                Object::int(5),
                Object::int(6),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Add, &[]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Sub, &[]),
                make(Opcode::Constant, &[4]),
                make(Opcode::Constant, &[5]),
                make(Opcode::Mul, &[]),
                make(Opcode::Array, &[3]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_dict_literals() {
    run_compiler_tests(&[
        TestCase {
            input: "{}".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([make(Opcode::Dict, &[0]), make(Opcode::Pop, &[])]),
        },
        TestCase {
            input: "{1: 2, 3: 4, 5: 6}".to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::int(2),
                Object::int(3),
                Object::int(4),
                Object::int(5),
                Object::int(6),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Constant, &[4]),
                make(Opcode::Constant, &[5]),
                make(Opcode::Dict, &[3]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "{1: 2 + 3, 4: 5 * 6}".to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::int(2),
                Object::int(3),
                Object::int(4),
                Object::int(5),
                Object::int(6),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Add, &[]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Constant, &[4]),
                make(Opcode::Constant, &[5]),
                make(Opcode::Mul, &[]),
                make(Opcode::Dict, &[2]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_index_expression() {
    run_compiler_tests(&[
        TestCase {
            input: "[1, 2, 3][1 + 1]".to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::int(2),
                Object::int(3),
                Object::int(1),
                Object::int(1),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Array, &[3]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Constant, &[4]),
                make(Opcode::Add, &[]),
                make(Opcode::Index, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "{1: 2}[2 - 1]".to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::int(2),
                Object::int(2),
                Object::int(1),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Dict, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Sub, &[]),
                make(Opcode::Index, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_lambdas() {
    run_compiler_tests(&[
        TestCase {
            input: "fn() { return 5 + 10 }".to_string(),
            expected_constants: Vec::from([
                Object::int(5),
                Object::int(10),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[0]),
                        make(Opcode::Constant, &[1]),
                        make(Opcode::Add, &[]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "fn() { 5 + 10 }".to_string(),
            expected_constants: Vec::from([
                Object::int(5),
                Object::int(10),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[0]),
                        make(Opcode::Constant, &[1]),
                        make(Opcode::Add, &[]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "fn() { 1; 2 }".to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::int(2),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[0]),
                        make(Opcode::PopNoRet, &[]),
                        make(Opcode::Constant, &[1]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_compiler_scopes() {
    let mut comp = Compiler::new();

    assert_eq!(comp.scope_index, 0);

    let global_symbol_table = comp.symbol_table.clone();

    comp.emit(Opcode::Mul, &[]);
    comp.enter_scope();

    assert_eq!(comp.scope_index, 1);

    comp.emit(Opcode::Sub, &[]);

    assert_eq!(
        comp.symbol_table.outer,
        Some(Box::new(global_symbol_table.clone()))
    );

    assert_eq!(comp.current_instructions().len(), 1);

    let last = comp.scopes[comp.scope_index].last_instruction;

    assert_eq!(last.opcode, Opcode::Sub);

    comp.leave_scope();

    assert_eq!(comp.scope_index, 0);
    assert_eq!(comp.symbol_table, global_symbol_table);
    assert_eq!(comp.symbol_table.outer, None);

    comp.emit(Opcode::Add, &[]);

    assert_eq!(comp.current_instructions().len(), 2);

    let last = comp.scopes[comp.scope_index].last_instruction;
    assert_eq!(last.opcode, Opcode::Add);

    let prev = comp.scopes[comp.scope_index].previous_instruction;
    assert_eq!(prev.opcode, Opcode::Mul);
}

#[test]
fn test_functions_without_return_value() {
    run_compiler_tests(&[TestCase {
        input: "fn() { }".to_string(),
        expected_constants: Vec::from([Object::compiled_fn(
            [make(Opcode::Return, &[])].concat(),
            0,
            0,
        )]),
        expected_instructions: Vec::from([make(Opcode::Closure, &[0, 0]), make(Opcode::Pop, &[])]),
    }]);
}

#[test]
fn test_function_calls() {
    run_compiler_tests(&[
        TestCase {
            input: "fn() { 24 }()".to_string(),
            expected_constants: Vec::from([
                Object::int(24),
                Object::compiled_fn(
                    [make(Opcode::Constant, &[0]), make(Opcode::ReturnValue, &[])].concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Call, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "const noArg = fn() { 24 }; noArg()".to_string(),
            expected_constants: Vec::from([
                Object::int(24),
                Object::compiled_fn(
                    [make(Opcode::Constant, &[0]), make(Opcode::ReturnValue, &[])].concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Call, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "const fn1 = fn() { 24 }; const fn2 = fn() { 24 }; fn1() + fn2()".to_string(),
            expected_constants: Vec::from([
                Object::int(24),
                Object::compiled_fn(
                    [make(Opcode::Constant, &[0]), make(Opcode::ReturnValue, &[])].concat(),
                    0,
                    0,
                ),
                Object::int(24),
                Object::compiled_fn(
                    [make(Opcode::Constant, &[2]), make(Opcode::ReturnValue, &[])].concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::Closure, &[3, 0]),
                make(Opcode::SetGlobal, &[1]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Call, &[0]),
                make(Opcode::GetGlobal, &[1]),
                make(Opcode::Call, &[0]),
                make(Opcode::Add, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "fn noArg() { 24 }; noArg()".to_string(),
            expected_constants: Vec::from([
                Object::int(24),
                Object::compiled_fn(
                    [make(Opcode::Constant, &[0]), make(Opcode::ReturnValue, &[])].concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Call, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "const oneArg = fn(a) { }; oneArg(24);".to_string(),
            expected_constants: Vec::from([
                Object::compiled_fn([make(Opcode::Return, &[])].concat(), 1, 1),
                Object::int(24),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[0, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Call, &[1]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
        TestCase {
            input: "const oneArg = fn(a) { a }; oneArg(24);".to_string(),
            expected_constants: Vec::from([
                Object::compiled_fn(
                    [make(Opcode::GetLocal, &[0]), make(Opcode::ReturnValue, &[])].concat(),
                    1,
                    1,
                ),
                Object::int(24),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[0, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Call, &[1]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
        TestCase {
            input: "const manyArgs = fn(a, b, c) { }; manyArgs(24, 25, 26)".to_string(),
            expected_constants: Vec::from([
                Object::compiled_fn([make(Opcode::Return, &[])].concat(), 3, 3),
                Object::int(24),
                Object::int(25),
                Object::int(26),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[0, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Call, &[3]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "const manyArgs = fn(a, b, c) { a; b; c }; manyArgs(24, 25, 26);".to_string(),
            expected_constants: Vec::from([
                Object::compiled_fn(
                    [
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::PopNoRet, &[]),
                        make(Opcode::GetLocal, &[1]),
                        make(Opcode::PopNoRet, &[]),
                        make(Opcode::GetLocal, &[2]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    3,
                    3,
                ),
                Object::int(24),
                Object::int(25),
                Object::int(26),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[0, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Call, &[3]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_declaration_statement_scopes() {
    run_compiler_tests(&[
        TestCase {
            input: "var num = 55; fn() { num }".to_string(),
            expected_constants: Vec::from([
                Object::int(55),
                Object::compiled_fn(
                    [
                        make(Opcode::GetGlobal, &[0]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    0,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "fn() { const num = 55; num }".to_string(),
            expected_constants: Vec::from([
                Object::int(55),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[0]),
                        make(Opcode::SetLocal, &[0]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "fn() { const a = 55; const b = 77; a + b }".to_string(),
            expected_constants: Vec::from([
                Object::int(55),
                Object::int(77),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[0]),
                        make(Opcode::SetLocal, &[0]),
                        make(Opcode::Constant, &[1]),
                        make(Opcode::SetLocal, &[1]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::GetLocal, &[1]),
                        make(Opcode::Add, &[]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    2,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_builtins() {
    run_compiler_tests(&[
        TestCase {
            input: "print([]); print(\"\")".to_string(),
            expected_constants: Vec::from([Object::str(String::new())]),
            expected_instructions: Vec::from([
                make(Opcode::GetBuiltin, &[3]),
                make(Opcode::Array, &[0]),
                make(Opcode::Call, &[1]),
                make(Opcode::PopNoRet, &[]),
                make(Opcode::GetBuiltin, &[3]),
                make(Opcode::Constant, &[0]),
                make(Opcode::Call, &[1]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "fn() { print([]) }".to_string(),
            expected_constants: Vec::from([Object::compiled_fn(
                [
                    make(Opcode::GetBuiltin, &[3]),
                    make(Opcode::Array, &[0]),
                    make(Opcode::Call, &[1]),
                    make(Opcode::ReturnValue, &[]),
                ]
                .concat(),
                0,
                0,
            )]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[0, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_closures() {
    run_compiler_tests(&[
        TestCase {
            input: "fn(a) { fn(b) { a + b } }".to_string(),
            expected_constants: Vec::from([
                Object::compiled_fn(
                    [
                        make(Opcode::GetFree, &[0]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Add, &[]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    1,
                ),
                Object::compiled_fn(
                    [
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Closure, &[0, 1]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    1,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        TestCase {
            input: "fn(a) {
    fn(b) {
        fn(c) {
            a + b + c
        }
    }
};"
            .to_string(),
            expected_constants: Vec::from([
                Object::compiled_fn(
                    [
                        make(Opcode::GetFree, &[0]),
                        make(Opcode::GetFree, &[1]),
                        make(Opcode::Add, &[]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Add, &[]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    1,
                ),
                Object::compiled_fn(
                    [
                        make(Opcode::GetFree, &[0]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Closure, &[0, 2]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    1,
                ),
                Object::compiled_fn(
                    [
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Closure, &[1, 1]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    1,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
        TestCase {
            input: "
var global = 55;
fn() {
    var a = 66;
    fn() {
        var b = 77;
        fn() {
            var c = 88;
            global + a + b + c
        }
    }
}
            "
            .to_string(),
            expected_constants: Vec::from([
                Object::int(55),
                Object::int(66),
                Object::int(77),
                Object::int(88),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[3]),
                        make(Opcode::SetLocal, &[0]),
                        make(Opcode::GetGlobal, &[0]),
                        make(Opcode::GetFree, &[0]),
                        make(Opcode::Add, &[]),
                        make(Opcode::GetFree, &[1]),
                        make(Opcode::Add, &[]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Add, &[]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    0,
                ),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[2]),
                        make(Opcode::SetLocal, &[0]),
                        make(Opcode::GetFree, &[0]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Closure, &[4, 2]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    0,
                ),
                Object::compiled_fn(
                    [
                        make(Opcode::Constant, &[1]),
                        make(Opcode::SetLocal, &[0]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Closure, &[5, 1]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::Closure, &[6, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ]);
}

#[test]
fn test_recursive_functions() {
    run_compiler_tests(&[
        TestCase {
            input: "var countDown = fn(x) { countDown(x - 1) };
            countDown(1);"
                .to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::compiled_fn(
                    [
                        make(Opcode::CurrentClosure, &[]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Constant, &[0]),
                        make(Opcode::Sub, &[]),
                        make(Opcode::Call, &[1]),
                        make(Opcode::ReturnValue, &[]),
                    ]
                    .concat(),
                    1,
                    1,
                ),
                Object::int(1),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Call, &[1]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
        TestCase {
            input: "
var wrapper = fn() {
    var countDown = fn(x) { countDown(x - 1); };
    countDown(1);
};
wrapper();
            "
            .to_string(),
            expected_constants: Vec::from([
                Object::int(1),
                Object::compiled_fn(
                    [
                        make(Opcode::CurrentClosure, &[]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Constant, &[0]),
                        make(Opcode::Sub, &[]),
                        make(Opcode::Call, &[1]),
                        make(Opcode::Return, &[]),
                    ]
                    .concat(),
                    1,
                    1,
                ),
                Object::int(1),
                Object::compiled_fn(
                    [
                        make(Opcode::Closure, &[1, 0]),
                        make(Opcode::SetLocal, &[0]),
                        make(Opcode::GetLocal, &[0]),
                        make(Opcode::Constant, &[2]),
                        make(Opcode::Call, &[1]),
                        make(Opcode::Return, &[]),
                    ]
                    .concat(),
                    1,
                    0,
                ),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[3, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Call, &[0]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
    ]);
}

fn run_compiler_tests(test_cases: &[TestCase]) {
    for test_case in test_cases {
        let program = parse(&test_case.input);

        let mut compiler = Compiler::new();

        let err = compiler.compile(program.unwrap());
        if let Err(e) = err {
            panic!("compiler error: {e}");
        }

        let bytecode = compiler.bytecode();

        test_instructions(&test_case.expected_instructions, &bytecode.instructions).unwrap();

        test_constants(&test_case.expected_constants, bytecode.constants);
    }
}

fn parse(input: &str) -> Option<Node> {
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    p.parse_program()
}

fn test_instructions(expected: &[Instructions], actual: &[u8]) -> Result<(), String> {
    let concatted = expected.concat();
    if actual.len() != concatted.len() {
        return Err(format!(
            "wrong instructions length.\n\twant: {}\n\tgot: {}",
            instructions_to_string(&concatted),
            instructions_to_string(actual)
        ));
    }

    for (i, ins) in concatted.iter().enumerate() {
        if actual[i] != *ins {
            return Err(format!(
                "wrong instruction at {}.\n\twant: {}\n\tgot: {}",
                i,
                instructions_to_string(&concatted),
                instructions_to_string(actual)
            ));
        }
    }
    Ok(())
}

fn test_constants(expected: &[Object], actual: Vec<Object>) {
    assert_eq!(expected.len(), actual.len());

    for (i, constant) in expected.iter().enumerate() {
        assert_eq!(constant, &actual[i]);
    }
}
