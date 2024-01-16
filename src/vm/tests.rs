use std::{any::Any, collections::HashMap};

use pretty_assertions::assert_eq;

use super::VM;
use crate::{
    ast::Node,
    code::instructions_to_string,
    compiler::Compiler,
    lexer::Lexer,
    object::{Array, Bool, Char, Dict, Float, Hashable, Int, Object, Str, NULL_OBJ},
    parser::Parser,
};

struct VMTestCase {
    input: String,
    expected: Box<dyn Any>,
}

#[test]
fn test_integer_arithmetic() {
    let test_cases = [
        VMTestCase {
            input: "1".to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "2".to_string(),
            expected: Box::new(2),
        },
        VMTestCase {
            input: "1 + 2".to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "1 - 2".to_string(),
            expected: Box::new(-1),
        },
        VMTestCase {
            input: "1 * 2".to_string(),
            expected: Box::new(2),
        },
        VMTestCase {
            input: "4 / 2".to_string(),
            expected: Box::new(2),
        },
        VMTestCase {
            input: "50 / 2 * 2 + 10 - 5".to_string(),
            expected: Box::new(55),
        },
        VMTestCase {
            input: "5 + 5 + 5 + 5 - 10".to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "2 * 2 * 2 * 2 * 2".to_string(),
            expected: Box::new(32),
        },
        VMTestCase {
            input: "5 * 2 + 10".to_string(),
            expected: Box::new(20),
        },
        VMTestCase {
            input: "5 & 10".to_string(),
            expected: Box::new(0),
        },
        VMTestCase {
            input: "5 & (2 | 10)".to_string(),
            expected: Box::new(0),
        },
        VMTestCase {
            input: "5 ^ (2 & 10)".to_string(),
            expected: Box::new(7),
        },
        VMTestCase {
            input: "5 | (2 | 10)".to_string(),
            expected: Box::new(15),
        },
        VMTestCase {
            input: "-5".to_string(),
            expected: Box::new(-5),
        },
        VMTestCase {
            input: "-10".to_string(),
            expected: Box::new(-10),
        },
        VMTestCase {
            input: "-50 + 100 + -50".to_string(),
            expected: Box::new(0),
        },
        VMTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
            expected: Box::new(50),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_float_arithmetic() {
    let test_cases = [
        VMTestCase {
            input: "-7.84 + -5.89".to_string(),
            expected: Box::new(-13.73),
        },
        VMTestCase {
            input: "-5.94 * -8.94".to_string(),
            expected: Box::new(53.1036),
        },
        VMTestCase {
            input: "-8.65 - 0.65".to_string(),
            expected: Box::new(-9.3),
        },
        VMTestCase {
            input: "5.17 / -9.39".to_string(),
            expected: Box::new(-0.550_585_729_499_467_5),
        },
        VMTestCase {
            input: "2.25 - -9.86 / -6.43".to_string(),
            expected: Box::new(0.716_562_986_003_110_4),
        },
        VMTestCase {
            input: "-1.33 * -8.74 + 9.55".to_string(),
            expected: Box::new(21.1742),
        },
        VMTestCase {
            input: "-9.07 + 5.55 - -8.99".to_string(),
            expected: Box::new(5.47),
        },
        VMTestCase {
            input: "-7.73 / -1.28 * 7.35".to_string(),
            expected: Box::new(44.387_109_374_999_994),
        },
        VMTestCase {
            input: "-6.24 * -5.34 / -2.9".to_string(),
            expected: Box::new(-11.490_206_896_551_726),
        },
        VMTestCase {
            input: "1.4 / 6.1 - -4.25".to_string(),
            expected: Box::new(4.479_508_196_721_311),
        },
        VMTestCase {
            input: "2.31 / 0.79 * 7.31".to_string(),
            expected: Box::new(21.374_810_126_582_28),
        },
        VMTestCase {
            input: "-5.01 + -3.01 * 6.63".to_string(),
            expected: Box::new(-24.966_299_999_999_997),
        },
        VMTestCase {
            input: "3.87 / -4.74 + 2.99".to_string(),
            expected: Box::new(2.173_544_303_797_468_7),
        },
        VMTestCase {
            input: "-6.52 + -10.37 / -6.36".to_string(),
            expected: Box::new(-4.889_496_855_345_912),
        },
        VMTestCase {
            input: "8.2 - -1.6 * 8.86".to_string(),
            expected: Box::new(22.375_999_999_999_998),
        },
        VMTestCase {
            input: "10.24 + -8.92 + -5.85".to_string(),
            expected: Box::new(-4.529_999_999_999_999),
        },
        VMTestCase {
            input: "-4.34 * 4.28 / -6.41".to_string(),
            expected: Box::new(2.897_847_113_884_555),
        },
        VMTestCase {
            input: "3.53 + -2.38 / 0.56".to_string(),
            expected: Box::new(-0.719_999_999_999_999_3),
        },
        VMTestCase {
            input: "5.07 / -4.03 / -3.23".to_string(),
            expected: Box::new(0.389_493_658_244_282_45),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_boolean_expressions() {
    let test_cases = [
        VMTestCase {
            input: "true".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "false".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 < 2".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "1 > 2".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 < 1".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 > 1".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 <= 2".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "1 >= 2".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 <= 1".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "1 >= 1".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "1 == 1".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "1 != 1".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 == 2".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 != 2".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "true == true".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "false == false".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "true == false".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "true != false".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "false != true".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "(1 < 2) == true".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "(1 < 2) == false".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "(1 > 2) == true".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "(1 > 2) == false".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "!true".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "!false".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "!5".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "!!true".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "!!false".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "!!5".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "-5.67 == -7.04".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "-7.04 == -5.67".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "0.01 + 0.02 == 0.03".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "-0.32 + 0.22 == -0.11".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "-0.32 + 0.22 == -0.10".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "-8.54 != 10.29".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "10.29 != -8.54".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "-7.52 > -10.18".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "-10.18 > -7.52".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "9.72 < -2.42".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "-2.42 < 9.72".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "-2.66 >= -7.71".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "-7.71 >= -2.66".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "6.94 <= 3.59".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "3.59 <= 6.94".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'w' == 'p'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'p' == 'w'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'g' != 'c'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'c' != 'g'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'g' > 'r'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'r' > 'g'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'a' < 't'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'t' < 'a'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'v' >= 'q'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'q' >= 'v'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'m' <= 'f'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'f' <= 'm'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'w' == 'w'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'w' != 'w'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'h' > 'q'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'q' > 'h'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'p' < 'w'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'w' < 'p'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'y' >= 'a'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "'a' >= 'y'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'k' <= 'g'".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "'g' <= 'k'".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "!(if (false) { 5; })".to_string(),
            expected: Box::new(true),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_null_expressions() {
    let test_cases = [VMTestCase {
        input: "null".to_string(),
        expected: Box::new(None::<u8>),
    }];

    run_vm_tests(&test_cases);
}

#[test]
fn test_boolean_operation() {
    let test_cases = [
        VMTestCase {
            input: "true && 1".to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "false && 20".to_string(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "0 && 2".to_string(),
            expected: Box::new(0),
        },
        VMTestCase {
            input: "1 && 0".to_string(),
            expected: Box::new(0),
        },
        VMTestCase {
            input: "0 || 1".to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "1 || 0".to_string(),
            expected: Box::new(1),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_conditionals() {
    let test_cases = [
        VMTestCase {
            input: "if (false) { 10 }".to_string(),
            expected: Box::new(None::<u8>),
        },
        VMTestCase {
            input: "if (true) { 10 } else { 20 }".to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "if (false) { 10 } else { 20 } ".to_string(),
            expected: Box::new(20),
        },
        VMTestCase {
            input: "if (1) { 10 }".to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "if (1 < 2) { 10 }".to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "if (1 < 2) { 10 } else { 20 }".to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "if (1 > 2) { 10 } else { 20 }".to_string(),
            expected: Box::new(20),
        },
        VMTestCase {
            input: "if ((if (false) { 10 })) { 10 } else { 20 }".to_string(),
            expected: Box::new(20),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_global_declaration_statements() {
    let test_cases = [
        VMTestCase {
            input: "var one = 1; one".to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "var one = 1; var two = 2; one + two".to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "var one = 1; var two = one + one; one + two".to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "const one = 1; one".to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "const one = 1; const two = 2; one + two".to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "const one = 1; const two = one + one; one + two".to_string(),
            expected: Box::new(3),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_string_expressions() {
    let test_cases = [
        VMTestCase {
            input: "\"panda\"".to_string(),
            expected: Box::new(String::from("panda")),
        },
        VMTestCase {
            input: "\"pan\" + \"da\"".to_string(),
            expected: Box::new(String::from("panda")),
        },
        VMTestCase {
            input: "\"pa\" + \"nda\" + \" \" + \"bamboo\"".to_string(),
            expected: Box::new(String::from("panda bamboo")),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_array_literals() {
    let test_cases = [
        VMTestCase {
            input: "[]".to_string(),
            expected: Box::<Vec<i32>>::default(),
        },
        VMTestCase {
            input: "[1, 2, 3]".to_string(),
            expected: Box::new(Vec::from([1, 2, 3])),
        },
        VMTestCase {
            input: "[1 + 2, 3 * 4, 5 + 6]".to_string(),
            expected: Box::new(Vec::from([3, 12, 11])),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_dict_literals() {
    let test_cases = [
        VMTestCase {
            input: "{}".to_string(),
            expected: Box::<HashMap<u64, i32>>::default(),
        },
        VMTestCase {
            input: "{1: 2, 2: 3}".to_string(),
            expected: Box::new(HashMap::from([
                (
                    Hashable::from_object(&Object::Int(Int { value: 1 }))
                        .unwrap()
                        .hash(),
                    2,
                ),
                (
                    Hashable::from_object(&Object::Int(Int { value: 2 }))
                        .unwrap()
                        .hash(),
                    3,
                ),
            ])),
        },
        VMTestCase {
            input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
            expected: Box::new(HashMap::from([
                (
                    Hashable::from_object(&Object::Int(Int { value: 2 }))
                        .unwrap()
                        .hash(),
                    4,
                ),
                (
                    Hashable::from_object(&Object::Int(Int { value: 6 }))
                        .unwrap()
                        .hash(),
                    16,
                ),
            ])),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_index_expression() {
    let test_cases = [
        VMTestCase {
            input: "[1, 2, 3][1]".to_string(),
            expected: Box::new(2),
        },
        VMTestCase {
            input: "[1, 2, 3][0 + 2]".to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "[[1, 1, 1]][0][0]".to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "{1: 1, 2: 2}[1]".to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "{1: 1, 2: 2}[2]".to_string(),
            expected: Box::new(2),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_without_arguments() {
    let test_cases = [
        VMTestCase {
            input: "var fivePlusTen = fn() { 5 + 10 }; fivePlusTen()".to_string(),
            expected: Box::new(15),
        },
        VMTestCase {
            input: "
var a = fn() {
    1
};
var b = fn() {
    a() + 1
};
var c = fn() {
    b() + 1
};
c()"
            .to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "var one = fn() { 1 }; var two = fn() { 2 }; one() + two()".to_string(),
            expected: Box::new(3),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_functions_with_return_statement() {
    let test_cases = [
        VMTestCase {
            input: "var earlyExit = fn() { return 99; 100; }; earlyExit()".to_string(),
            expected: Box::new(99),
        },
        VMTestCase {
            input: "var earlyExit = fn() { return 99; return 100; }; earlyExit()".to_string(),
            expected: Box::new(99),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_functions_without_return_value() {
    let test_cases = [
        VMTestCase {
            input: "var noReturn = fn() { }; noReturn();".to_string(),
            expected: Box::new(None::<u8>),
        },
        VMTestCase {
            input: "
var noReturn = fn() { };
var noReturnTwo = fn() {
    noReturn();
};
noReturn();
noReturnTwo();"
                .to_string(),
            expected: Box::new(None::<u8>),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_first_class_functions() {
    let test_cases = [
        VMTestCase {
            input: "var returnsOne = fn() { 1 };
            var returnsOneReturner = fn() { returnsOne };
            returnsOneReturner()()"
                .to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "var returnsOneReturner = fn() {
            var returnsOne = fn() { 1 };
            returnsOne
            };
            returnsOneReturner()()"
                .to_string(),
            expected: Box::new(1),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_with_bindings() {
    let test_cases = [
        VMTestCase {
            input: "var one = fn() { var one = 1; one };
        one()"
                .to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "var oneAndTwo = fn() {
                var one = 1;
                var two = 2;
                one + two
            };
            oneAndTwo()"
                .to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "fn oneAndTwo() {
                var one = 1;
                var two = 2;
                one + two
            };
            fn threeAndFour() {
                var three = 3;
                var four = 4;
                three + four
            };
            oneAndTwo() + threeAndFour()"
                .to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "
var firstFoobar = fn() {
    var foobar = 50;
    foobar
};
var secondFoobar = fn() {
    var foobar = 100
    foobar
};
firstFoobar() + secondFoobar()"
                .to_string(),
            expected: Box::new(150),
        },
        VMTestCase {
            input: "
var globalSeed = 50;
var minusOne = fn() {
    var num = 1;
    globalSeed - num
};

var minusTwo = fn() {
    var num = 2;
    globalSeed - num
}
minusOne() + minusTwo()
            "
            .to_string(),
            expected: Box::new(97),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
    let test_cases = [
        VMTestCase {
            input: "var identity = fn(a) { a };
            identity(4)"
                .to_string(),
            expected: Box::new(4),
        },
        VMTestCase {
            input: "var sum = fn(a, b) { a + b };
            sum(1, 2)"
                .to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "
var sum = fn(a, b) {
    var c = a + b;
    c
};
sum(1, 2)"
                .to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "
var sum = fn(a, b) {
    var c = a + b;
    c
};
var outer = fn() {
    sum(1, 2) + sum(3, 4)
};
outer()"
                .to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "var sum = fn(a, b) {
                var c = a + b;
                c
                };
                sum(1, 2) + sum(3, 4)"
                .to_string(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "var globalNum = 10;
            var sum = fn(a, b) {
            var c = a + b;
            c + globalNum
            };
            var outer = fn() {
            sum(1, 2) + sum(3, 4) + globalNum
            };
            outer() + globalNum"
                .to_string(),
            expected: Box::new(50),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
    let test_cases = [
        VMTestCase {
            input: "fn() { 1; }(1);".to_string(),
            expected: Box::new("wrong number of arguments. got: 1, want: 0"),
        },
        VMTestCase {
            input: "fn(a) { a; }();".to_string(),
            expected: Box::new("wrong number of arguments. got: 0, want: 1"),
        },
        VMTestCase {
            input: "fn(a, b) { a + b; }(1);".to_string(),
            expected: Box::new("wrong number of arguments. got: 1, want: 2"),
        },
    ];

    for test_case in test_cases {
        let program = parse(&test_case.input);

        let mut comp = Compiler::new();

        if let Err(err) = comp.compile(program.unwrap()) {
            panic!("compiler error: {err}")
        }

        let byte_code = comp.bytecode();
        let mut vm = VM::new(&byte_code);

        if let Err(err) = vm.run() {
            assert_eq!(err, *test_case.expected.downcast_ref::<&str>().unwrap());
        } else {
            panic!("expected error but resulted in Ok(())")
        }
    }
}

#[test]
fn test_closures() {
    let test_cases = [
        VMTestCase {
            input: "
var newClosure = fn(a) {
    fn() { a }
};

var closure = newClosure(99);
closure()"
                .to_string(),
            expected: Box::new(99),
        },
        VMTestCase {
            input: "
var newAdder = fn(a, b) {
    fn(c) { a + b + c }
};
var adder = newAdder(1, 2);
adder(8)"
                .to_string(),
            expected: Box::new(11),
        },
        VMTestCase {
            input: "
var newAdder = fn(a, b) {
    var c = a + b;
    fn(d) { c + d }
};
var adder = newAdder(1, 2);
adder(8)"
                .to_string(),
            expected: Box::new(11),
        },
        VMTestCase {
            input: "
var newAdderOuter = fn(a, b) {
    var c = a + b;
    fn(d) {
        var e = d + c;
        fn(f) { e + f }
    }
};

var newAdderInner = newAdderOuter(1, 2);
var adder = newAdderInner(3);
adder(8)
            "
            .to_string(),
            expected: Box::new(14),
        },
        VMTestCase {
            input: "
var a = 1;
var newAdderOuter = fn(b) {
    fn(c) {
        fn(d) { a + b + c + d }
    }
};

var newAdderInner = newAdderOuter(2);
var adder = newAdderInner(3);
adder(8)"
                .to_string(),
            expected: Box::new(14),
        },
        VMTestCase {
            input: "
var  newClosure = fn(a, b) {
    var  one = fn() { a };
    var  two = fn() { b };
    fn() { one() + two() }
};

var  closure = newClosure(9, 90);
closure()"
                .to_string(),
            expected: Box::new(99),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_recursive_functions() {
    let test_cases = [
        VMTestCase {
            input: "
fn countDown(x) {
    if (x == 1) {
        return 1
    } else {
        countDown(x - 1)
    }
};
countDown(2)"
                .to_string(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "
var wrapper = fn() {
    var countDown = fn(x) {
        if (x == 0) {
            return 0
        } else {
            return countDown(x - 1)
        }
    }
    countDown(1)
}
wrapper()"
                .to_string(),
            expected: Box::new(0),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_while_statement() {
    let test_cases = [
        VMTestCase {
            input: "var i = 10; while (i > 0) { i = i - 1 }; i".to_string(),
            expected: Box::new(0),
        },
        VMTestCase {
            input: "
var i = 10;
while (i > 0) {
    i = i - 1;
    if (i == 5) {
        break;
    }
};
i"
            .to_string(),
            expected: Box::new(5),
        },
        VMTestCase {
            input: "
var i = 10;
var j = 0;
while (i > 0) {
    i = i - 1;
    if (i < 5) {
        continue;
    }
    j = i;
};
j"
            .to_string(),
            expected: Box::new(5),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_for_statement() {
    let test_cases = [
        VMTestCase {
            input: "
var x = 0;
for (i in [1, 2, 3, 4, 5, 6, 7, 8, 9]) {
    x = x + i;
}
x
"
            .to_string(),
            expected: Box::new(45),
        },
        VMTestCase {
            input: "
var x = 0;
for (i in {1:0, 2:'a', 3:\"\", 4:3.4, 5:[1,2], 6:{1:1}, 7:1..2, 8:fn(){}, 9:(-23 + 345)}) {
    x = x + i;
}
x
"
            .to_string(),
            expected: Box::new(45),
        },
        VMTestCase {
            input: "
var x = 0;
for (i in 0..10) {
    x = x + i;
}
x
"
            .to_string(),
            expected: Box::new(45),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_assign_expressions() {
    let test_cases = [VMTestCase {
        input: "var i = 10; i = i - 1 ; i".to_string(),
        expected: Box::new(9),
    }];

    run_vm_tests(&test_cases);
}

#[test]
fn test_builtin_method_expressions() {
    let test_cases = [
        VMTestCase {
            input: "[2, 3].contains(2)".to_string(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "(-3).abs()".to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "{1:1, 2:2, 3:3}.len()".to_string(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "var isDigit = 'a'.isDigit; isDigit(16)".to_string(),
            expected: Box::new(true),
        },
    ];

    run_vm_tests(&test_cases);
}

fn parse(input: &str) -> Option<Node> {
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    p.parse_program()
}

fn run_vm_tests(test_cases: &[VMTestCase]) {
    for test_case in test_cases {
        let program = parse(&test_case.input);

        let mut comp = Compiler::new();

        if let Err(err) = comp.compile(program.unwrap()) {
            panic!("compiler error: {err}")
        }

        let byte_code = comp.bytecode();

        println!("{}", instructions_to_string(&byte_code.instructions));

        let mut vm = VM::new(&byte_code);

        if let Err(err) = vm.run() {
            panic!("vm error: {err}")
        }

        let stack_elem = vm.last_popped_stack_elem;

        test_expected_object(&test_case.expected, &stack_elem.unwrap_or(NULL_OBJ));
    }
}

fn test_expected_object(expected: &Box<dyn Any>, actual: &Object) {
    if let Some(expected) = (*expected).downcast_ref::<i32>() {
        test_integer_object(*expected as isize, actual);
    } else if let Some(expected) = (*expected).downcast_ref::<bool>() {
        test_boolean_object(*expected, actual);
    } else if let Some(expected) = (*expected).downcast_ref::<f64>() {
        test_float_object(*expected, actual);
    } else if let Some(expected) = (*expected).downcast_ref::<char>() {
        test_char_object(*expected, actual);
    } else if let Some(expected) = (*expected).downcast_ref::<String>() {
        test_string_object(expected, actual);
    } else if let Some(expected) = (*expected).downcast_ref::<Vec<i32>>() {
        test_array_object(expected, actual);
    } else if let Some(expected) = (*expected).downcast_ref::<HashMap<u64, i32>>() {
        test_dict_object(expected, actual);
    } else {
        assert_eq!(&Object::Null, actual);
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

fn test_array_object(expected: &[i32], actual: &Object) {
    assert_eq!(
        &Object::Array(Array {
            elements: expected
                .iter()
                .map(|el| Object::Int(Int {
                    value: (*el).try_into().unwrap()
                }))
                .collect()
        }),
        actual
    );
}

fn test_dict_object(expected: &HashMap<u64, i32>, actual: &Object) {
    if let Object::Dict(Dict { pairs }) = actual {
        assert_eq!(expected.len(), pairs.len());

        for (&expected_key, &expected_value) in expected {
            let value = &pairs[&expected_key].value;
            assert_eq!(
                value,
                &Object::Int(Int {
                    value: expected_value.try_into().unwrap()
                })
            );
        }
    }
}
