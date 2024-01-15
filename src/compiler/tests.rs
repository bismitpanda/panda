use std::any::Any;

use pretty_assertions::assert_eq;

use super::*;
use crate::{
    code::*,
    lexer::Lexer,
    object::{Bool, Char, CompiledFunction, Float, Int, Str},
    parser::Parser,
};

struct CompilerTestCase {
    input: String,
    expected_constants: Vec<Box<dyn Any>>,
    expected_instructions: Vec<Instructions>,
}

#[test]
fn test_integer_arithmetic() {
    let test_cases = [
        CompilerTestCase {
            input: "1".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 + 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Add, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1; 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::PopNoRet, &[]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 - 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Sub, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 * 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Mul, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 / 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Div, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 ^ 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::BitXor, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 & 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::BitAnd, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 | 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::BitOr, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 >> 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Shr, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 << 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Shl, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "-1".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Minus, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "-1.0".to_string(),
            expected_constants: Vec::from([Box::new(1.0) as Box<dyn Any>]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Minus, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_boolean_expressions() {
    let test_cases = [
        CompilerTestCase {
            input: "true".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([make(Opcode::True, &[]), make(Opcode::Pop, &[])]),
        },
        CompilerTestCase {
            input: "false".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([make(Opcode::False, &[]), make(Opcode::Pop, &[])]),
        },
        CompilerTestCase {
            input: "1 < 2".to_string(),
            expected_constants: Vec::from([Box::new(2) as Box<dyn Any>, Box::new(1)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThan, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 > 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThan, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 <= 2".to_string(),
            expected_constants: Vec::from([Box::new(2) as Box<dyn Any>, Box::new(1)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThanEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 >= 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::GreaterThanEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 == 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Equal, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "1 != 2".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::NotEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "true == false".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::False, &[]),
                make(Opcode::Equal, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "true != false".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::False, &[]),
                make(Opcode::NotEqual, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "!true".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::Bang, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_null_literal() {
    let test_cases = [CompilerTestCase {
        input: "null".to_string(),
        expected_constants: Vec::new(),
        expected_instructions: Vec::from([make(Opcode::Null, &[]), make(Opcode::Pop, &[])]),
    }];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_conditionals() {
    let test_cases = [
        CompilerTestCase {
            input: "if (true) { 10 }; 3333;".to_string(),
            expected_constants: Vec::from([Box::new(10) as Box<dyn Any>, Box::new(3333)]),
            expected_instructions: Vec::from([
                make(Opcode::True, &[]),
                make(Opcode::JumpNotTruthy, &[10]),
                make(Opcode::Constant, &[0]),
                make(Opcode::Jump, &[11]),
                make(Opcode::Null, &[]),
                make(Opcode::PopNoRet, &[]),
                make(Opcode::Constant, &[1]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
        CompilerTestCase {
            input: "if (true) { 10 } else { 20 }; 3333;".to_string(),
            expected_constants: Vec::from([
                Box::new(10) as Box<dyn Any>,
                Box::new(20),
                Box::new(3333),
            ]),
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
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_while_loop() {
    let test_cases = [CompilerTestCase {
        input: "while (true) { 10; }".to_string(),
        expected_constants: Vec::from([Box::new(10) as Box<dyn Any>]),
        expected_instructions: Vec::from([
            make(Opcode::True, &[]),
            make(Opcode::JumpNotTruthy, &[12]),
            make(Opcode::Constant, &[0]),
            make(Opcode::PopNoRet, &[]),
            make(Opcode::Jump, &[0]),
            make(Opcode::Pop, &[]),
        ]),
    }];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_global_declaration_expressions() {
    let test_cases = [CompilerTestCase {
        input: "var one = 1; one = 2;".to_string(),
        expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2)]),
        expected_instructions: Vec::from([
            make(Opcode::Constant, &[0]),
            make(Opcode::SetGlobal, &[0]),
            make(Opcode::Constant, &[1]),
            make(Opcode::Dup, &[]),
            make(Opcode::SetGlobal, &[0]),
            make(Opcode::PopNoRet, &[]),
        ]),
    }];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_string_expressions() {
    let test_cases = [
        CompilerTestCase {
            input: "\"panda\"".to_string(),
            expected_constants: Vec::from([Box::new("panda".to_string()) as Box<dyn Any>]),
            expected_instructions: [make(Opcode::Constant, &[0]), make(Opcode::Pop, &[])].to_vec(),
        },
        CompilerTestCase {
            input: "\"mon\" + \"key\"".to_string(),
            expected_constants: Vec::from([
                Box::new("mon".to_string()) as Box<dyn Any>,
                Box::new("key".to_string()),
            ]),
            expected_instructions: [
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Add, &[]),
                make(Opcode::Pop, &[]),
            ]
            .to_vec(),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_char_expressions() {
    let test_cases = [
        CompilerTestCase {
            input: "'a'".to_string(),
            expected_constants: Vec::from([Box::new('a') as Box<dyn Any>]),
            expected_instructions: [make(Opcode::Constant, &[0]), make(Opcode::Pop, &[])].to_vec(),
        },
        CompilerTestCase {
            input: "'h' + 'i'".to_string(),
            expected_constants: Vec::from([Box::new('h') as Box<dyn Any>, Box::new('i')]),
            expected_instructions: [
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Add, &[]),
                make(Opcode::Pop, &[]),
            ]
            .to_vec(),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_array_literals() {
    let test_cases = [
        CompilerTestCase {
            input: "[]".to_string(),
            expected_constants: Vec::from([]),
            expected_instructions: Vec::from([make(Opcode::Array, &[0]), make(Opcode::Pop, &[])]),
        },
        CompilerTestCase {
            input: "[1, 2, 3]".to_string(),
            expected_constants: Vec::from([Box::new(1) as Box<dyn Any>, Box::new(2), Box::new(3)]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Array, &[3]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "[1 + 2, 3 - 4, 5 * 6]".to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(2),
                Box::new(3),
                Box::new(4),
                Box::new(5),
                Box::new(6),
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
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_hash_literals() {
    let test_cases = [
        CompilerTestCase {
            input: "{}".to_string(),
            expected_constants: Vec::new(),
            expected_instructions: Vec::from([make(Opcode::Hash, &[0]), make(Opcode::Pop, &[])]),
        },
        CompilerTestCase {
            input: "{1: 2, 3: 4, 5: 6}".to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(2),
                Box::new(3),
                Box::new(4),
                Box::new(5),
                Box::new(6),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Constant, &[4]),
                make(Opcode::Constant, &[5]),
                make(Opcode::Hash, &[3]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "{1: 2 + 3, 4: 5 * 6}".to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(2),
                Box::new(3),
                Box::new(4),
                Box::new(5),
                Box::new(6),
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
                make(Opcode::Hash, &[2]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_index_expression() {
    let test_cases = [
        CompilerTestCase {
            input: "[1, 2, 3][1 + 1]".to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(2),
                Box::new(3),
                Box::new(1),
                Box::new(1),
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
        CompilerTestCase {
            input: "{1: 2}[2 - 1]".to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(2),
                Box::new(2),
                Box::new(1),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Hash, &[1]),
                make(Opcode::Constant, &[2]),
                make(Opcode::Constant, &[3]),
                make(Opcode::Sub, &[]),
                make(Opcode::Index, &[]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_lambdas() {
    let test_cases = [
        CompilerTestCase {
            input: "fn() { return 5 + 10 }".to_string(),
            expected_constants: Vec::from([
                Box::new(5) as Box<dyn Any>,
                Box::new(10),
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "fn() { 5 + 10 }".to_string(),
            expected_constants: Vec::from([
                Box::new(5) as Box<dyn Any>,
                Box::new(10),
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "fn() { 1; 2 }".to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(2),
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::PopNoRet, &[]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
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
    let test_cases = [CompilerTestCase {
        input: "fn() { }".to_string(),
        expected_constants: Vec::from([
            Box::new(Vec::from([make(Opcode::Return, &[])])) as Box<dyn Any>
        ]),
        expected_instructions: Vec::from([make(Opcode::Closure, &[0, 0]), make(Opcode::Pop, &[])]),
    }];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_function_calls() {
    let test_cases = [
        CompilerTestCase {
            input: "fn() { 24 }()".to_string(),
            expected_constants: Vec::from([
                Box::new(24) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Call, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "const noArg = fn() { 24 }; noArg()".to_string(),
            expected_constants: Vec::from([
                Box::new(24) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Call, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "const fn1 = fn() { 24 }; const fn2 = fn() { 24 }; fn1() + fn2()".to_string(),
            expected_constants: Vec::from([
                Box::new(24) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::ReturnValue, &[]),
                ])),
                Box::new(24),
                Box::new(Vec::from([
                    make(Opcode::Constant, &[2]),
                    make(Opcode::ReturnValue, &[]),
                ])),
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
        CompilerTestCase {
            input: "fn noArg() { 24 }; noArg()".to_string(),
            expected_constants: Vec::from([
                Box::new(24) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Call, &[0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "const oneArg = fn(a) { }; oneArg(24);".to_string(),
            expected_constants: Vec::from([
                Box::new(Vec::from([make(Opcode::Return, &[])])) as Box<dyn Any>,
                Box::new(24),
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
        CompilerTestCase {
            input: "const oneArg = fn(a) { a }; oneArg(24);".to_string(),
            expected_constants: Vec::from([
                Box::new(Vec::from([
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::ReturnValue, &[]),
                ])) as Box<dyn Any>,
                Box::new(24),
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
        CompilerTestCase {
            input: "const manyArgs = fn(a, b, c) { }; manyArgs(24, 25, 26)".to_string(),
            expected_constants: Vec::from([
                Box::new(Vec::from([make(Opcode::Return, &[])])) as Box<dyn Any>,
                Box::new(24),
                Box::new(25),
                Box::new(26),
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
        CompilerTestCase {
            input: "const manyArgs = fn(a, b, c) { a; b; c }; manyArgs(24, 25, 26);".to_string(),
            expected_constants: Vec::from([
                Box::new(Vec::from([
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::PopNoRet, &[]),
                    make(Opcode::GetLocal, &[1]),
                    make(Opcode::PopNoRet, &[]),
                    make(Opcode::GetLocal, &[2]),
                    make(Opcode::ReturnValue, &[]),
                ])) as Box<dyn Any>,
                Box::new(24),
                Box::new(25),
                Box::new(26),
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
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_declaration_statement_scopes() {
    let test_cases = [
        CompilerTestCase {
            input: "var num = 55; fn() { num }".to_string(),
            expected_constants: Vec::from([
                Box::new(55) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::GetGlobal, &[0]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "fn() { const num = 55; num }".to_string(),
            expected_constants: Vec::from([
                Box::new(55) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::SetLocal, &[0]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "fn() { const a = 55; const b = 77; a + b }".to_string(),
            expected_constants: Vec::from([
                Box::new(55) as Box<dyn Any>,
                Box::new(77),
                Box::new(Vec::from([
                    make(Opcode::Constant, &[0]),
                    make(Opcode::SetLocal, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::SetLocal, &[1]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::GetLocal, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_builtins() {
    let test_cases = [
        CompilerTestCase {
            input: "print([]); print(\"\")".to_string(),
            expected_constants: Vec::from([Box::new("") as Box<dyn Any>]),
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
        CompilerTestCase {
            input: "fn() { print([]) }".to_string(),
            expected_constants: Vec::from([Box::new(Vec::from([
                make(Opcode::GetBuiltin, &[3]),
                make(Opcode::Array, &[0]),
                make(Opcode::Call, &[1]),
                make(Opcode::ReturnValue, &[]),
            ])) as Box<dyn Any>]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[0, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_closures() {
    let test_cases = [
        CompilerTestCase {
            input: "fn(a) { fn(b) { a + b } }".to_string(),
            expected_constants: Vec::from([
                Box::new(Vec::from([
                    make(Opcode::GetFree, &[0]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Add, &[]),
                    make(Opcode::ReturnValue, &[]),
                ])) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Closure, &[0, 1]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[1, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
        CompilerTestCase {
            input: "fn(a) {
    fn(b) {
        fn(c) {
            a + b + c
        }
    }
};"
            .to_string(),
            expected_constants: Vec::from([
                Box::new(Vec::from([
                    make(Opcode::GetFree, &[0]),
                    make(Opcode::GetFree, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Add, &[]),
                    make(Opcode::ReturnValue, &[]),
                ])) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::GetFree, &[0]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Closure, &[0, 2]),
                    make(Opcode::ReturnValue, &[]),
                ])),
                Box::new(Vec::from([
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Closure, &[1, 1]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[2, 0]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
        CompilerTestCase {
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
                Box::new(55) as Box<dyn Any>,
                Box::new(66),
                Box::new(77),
                Box::new(88),
                Box::new(Vec::from([
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
                ])),
                Box::new(Vec::from([
                    make(Opcode::Constant, &[2]),
                    make(Opcode::SetLocal, &[0]),
                    make(Opcode::GetFree, &[0]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Closure, &[4, 2]),
                    make(Opcode::ReturnValue, &[]),
                ])),
                Box::new(Vec::from([
                    make(Opcode::Constant, &[1]),
                    make(Opcode::SetLocal, &[0]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Closure, &[5, 1]),
                    make(Opcode::ReturnValue, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Constant, &[0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::Closure, &[6, 0]),
                make(Opcode::Pop, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

#[test]
fn test_recursive_functions() {
    let test_cases = [
        CompilerTestCase {
            input: "var countDown = fn(x) { countDown(x - 1) };
            countDown(1);"
                .to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::CurrentClosure, &[]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Sub, &[]),
                    make(Opcode::Call, &[1]),
                    make(Opcode::ReturnValue, &[]),
                ])),
                Box::new(1),
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
        CompilerTestCase {
            input: "
var wrapper = fn() {
    var countDown = fn(x) { countDown(x - 1); };
    countDown(1);
};
wrapper();
            "
            .to_string(),
            expected_constants: Vec::from([
                Box::new(1) as Box<dyn Any>,
                Box::new(Vec::from([
                    make(Opcode::CurrentClosure, &[]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Sub, &[]),
                    make(Opcode::Call, &[1]),
                    make(Opcode::Return, &[]),
                ])),
                Box::new(1),
                Box::new(Vec::from([
                    make(Opcode::Closure, &[1, 0]),
                    make(Opcode::SetLocal, &[0]),
                    make(Opcode::GetLocal, &[0]),
                    make(Opcode::Constant, &[2]),
                    make(Opcode::Call, &[1]),
                    make(Opcode::Return, &[]),
                ])),
            ]),
            expected_instructions: Vec::from([
                make(Opcode::Closure, &[3, 0]),
                make(Opcode::SetGlobal, &[0]),
                make(Opcode::GetGlobal, &[0]),
                make(Opcode::Call, &[0]),
                make(Opcode::PopNoRet, &[]),
            ]),
        },
    ];

    run_compiler_tests(&test_cases);
}

fn run_compiler_tests(test_cases: &[CompilerTestCase]) {
    for test_case in test_cases {
        let program = parse(&test_case.input);

        let mut compiler = Compiler::new();

        let err = compiler.compile(program.unwrap());
        if let Err(e) = err {
            panic!("compiler error: {e}");
        }

        let bytecode = compiler.byte_code();

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
    let concatted = concat_instructions(expected);
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

fn concat_instructions(s: &[Instructions]) -> Instructions {
    s.concat()
}

fn test_constants(expected: &[Box<dyn Any>], actual: Vec<Object>) {
    assert_eq!(expected.len(), actual.len());

    for (i, constant) in expected.iter().enumerate() {
        if let Some(constant) = (*constant).downcast_ref::<i32>() {
            test_integer_object(*constant as isize, &actual[i]);
        } else if let Some(constant) = (*constant).downcast_ref::<bool>() {
            test_boolean_object(*constant, &actual[i]);
        } else if let Some(constant) = (*constant).downcast_ref::<f64>() {
            test_float_object(*constant, &actual[i]);
        } else if let Some(constant) = (*constant).downcast_ref::<char>() {
            test_char_object(*constant, &actual[i]);
        } else if let Some(constant) = (*constant).downcast_ref::<String>() {
            test_string_object(constant, &actual[i]);
        } else if let Some(constant) = (*constant).downcast_ref::<&str>() {
            test_string_object(constant, &actual[i]);
        } else if let Some(constant) = (*constant).downcast_ref::<Vec<Vec<u8>>>() {
            test_function_object(constant, &actual[i]);
        } else {
            assert_eq!(&Object::Null, &actual[i]);
        }
    }
}

fn test_integer_object(expected: isize, actual: &Object) {
    assert_eq!(&Object::Int(Int { value: expected }), actual);
}

fn test_boolean_object(expected: bool, actual: &Object) {
    assert_eq!(&Object::Bool(Bool { value: expected }), actual);
}

fn test_float_object(expected: f64, actual: &Object) {
    assert_eq!(&Object::Float(Float { value: expected }), actual);
}

fn test_char_object(expected: char, actual: &Object) {
    assert_eq!(&Object::Char(Char { value: expected }), actual);
}

fn test_string_object(expected: &str, actual: &Object) {
    assert_eq!(
        &Object::Str(Str {
            value: expected.to_string()
        }),
        actual
    );
}

fn test_function_object(expected: &[Instructions], actual: &Object) {
    let Object::CompiledFunction(CompiledFunction { instructions, .. }) = actual else {
        panic!("not a function")
    };

    test_instructions(expected, instructions).unwrap();
}
