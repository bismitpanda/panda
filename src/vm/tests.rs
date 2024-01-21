use std::collections::HashMap;

use pretty_assertions::assert_eq;

use super::VM;
use crate::{
    ast::Node,
    code::instructions_to_string,
    compiler::Compiler,
    lexer::Lexer,
    object::{DictPair, Hashable, Object},
    parser::Parser,
};

struct TestCase {
    input: String,
    expected: Object,
}

#[test]
fn test_integer_arithmetic() {
    let test_cases = [
        TestCase {
            input: "1".to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "2".to_string(),
            expected: Object::int(2),
        },
        TestCase {
            input: "1 + 2".to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "1 - 2".to_string(),
            expected: Object::int(-1),
        },
        TestCase {
            input: "1 * 2".to_string(),
            expected: Object::int(2),
        },
        TestCase {
            input: "4 / 2".to_string(),
            expected: Object::int(2),
        },
        TestCase {
            input: "50 / 2 * 2 + 10 - 5".to_string(),
            expected: Object::int(55),
        },
        TestCase {
            input: "5 + 5 + 5 + 5 - 10".to_string(),
            expected: Object::int(10),
        },
        TestCase {
            input: "2 * 2 * 2 * 2 * 2".to_string(),
            expected: Object::int(32),
        },
        TestCase {
            input: "5 * 2 + 10".to_string(),
            expected: Object::int(20),
        },
        TestCase {
            input: "5 & 10".to_string(),
            expected: Object::int(0),
        },
        TestCase {
            input: "5 & (2 | 10)".to_string(),
            expected: Object::int(0),
        },
        TestCase {
            input: "5 ^ (2 & 10)".to_string(),
            expected: Object::int(7),
        },
        TestCase {
            input: "5 | (2 | 10)".to_string(),
            expected: Object::int(15),
        },
        TestCase {
            input: "-5".to_string(),
            expected: Object::int(-5),
        },
        TestCase {
            input: "-10".to_string(),
            expected: Object::int(-10),
        },
        TestCase {
            input: "-50 + 100 + -50".to_string(),
            expected: Object::int(0),
        },
        TestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
            expected: Object::int(50),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_float_arithmetic() {
    let test_cases = [
        TestCase {
            input: "-7.84 + -5.89".to_string(),
            expected: Object::float(-13.73),
        },
        TestCase {
            input: "-5.94 * -8.94".to_string(),
            expected: Object::float(53.1036),
        },
        TestCase {
            input: "-8.65 - 0.65".to_string(),
            expected: Object::float(-9.3),
        },
        TestCase {
            input: "5.17 / -9.39".to_string(),
            expected: Object::float(-0.550_585_729_499_467_5),
        },
        TestCase {
            input: "2.25 - -9.86 / -6.43".to_string(),
            expected: Object::float(0.716_562_986_003_110_4),
        },
        TestCase {
            input: "-1.33 * -8.74 + 9.55".to_string(),
            expected: Object::float(21.1742),
        },
        TestCase {
            input: "-9.07 + 5.55 - -8.99".to_string(),
            expected: Object::float(5.47),
        },
        TestCase {
            input: "-7.73 / -1.28 * 7.35".to_string(),
            expected: Object::float(44.387_109_374_999_994),
        },
        TestCase {
            input: "-6.24 * -5.34 / -2.9".to_string(),
            expected: Object::float(-11.490_206_896_551_726),
        },
        TestCase {
            input: "1.4 / 6.1 - -4.25".to_string(),
            expected: Object::float(4.479_508_196_721_311),
        },
        TestCase {
            input: "2.31 / 0.79 * 7.31".to_string(),
            expected: Object::float(21.374_810_126_582_28),
        },
        TestCase {
            input: "-5.01 + -3.01 * 6.63".to_string(),
            expected: Object::float(-24.966_299_999_999_997),
        },
        TestCase {
            input: "3.87 / -4.74 + 2.99".to_string(),
            expected: Object::float(2.173_544_303_797_468_7),
        },
        TestCase {
            input: "-6.52 + -10.37 / -6.36".to_string(),
            expected: Object::float(-4.889_496_855_345_912),
        },
        TestCase {
            input: "8.2 - -1.6 * 8.86".to_string(),
            expected: Object::float(22.375_999_999_999_998),
        },
        TestCase {
            input: "10.24 + -8.92 + -5.85".to_string(),
            expected: Object::float(-4.529_999_999_999_999),
        },
        TestCase {
            input: "-4.34 * 4.28 / -6.41".to_string(),
            expected: Object::float(2.897_847_113_884_555),
        },
        TestCase {
            input: "3.53 + -2.38 / 0.56".to_string(),
            expected: Object::float(-0.719_999_999_999_999_3),
        },
        TestCase {
            input: "5.07 / -4.03 / -3.23".to_string(),
            expected: Object::float(0.389_493_658_244_282_45),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_boolean_expressions() {
    let test_cases = [
        TestCase {
            input: "true".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "false".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 < 2".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "1 > 2".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 < 1".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 > 1".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 <= 2".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "1 >= 2".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 <= 1".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "1 >= 1".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "1 == 1".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "1 != 1".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 == 2".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 != 2".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "true == true".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "false == false".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "true == false".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "true != false".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "false != true".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "(1 < 2) == true".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "(1 < 2) == false".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "(1 > 2) == true".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "(1 > 2) == false".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "!true".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "!false".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "!5".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "!!true".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "!!false".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "!!5".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "-5.67 == -7.04".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "-7.04 == -5.67".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "0.01 + 0.02 == 0.03".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "-0.32 + 0.22 == -0.11".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "-0.32 + 0.22 == -0.10".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "-8.54 != 10.29".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "10.29 != -8.54".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "-7.52 > -10.18".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "-10.18 > -7.52".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "9.72 < -2.42".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "-2.42 < 9.72".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "-2.66 >= -7.71".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "-7.71 >= -2.66".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "6.94 <= 3.59".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "3.59 <= 6.94".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' == 'p'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'p' == 'w'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'g' != 'c'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'c' != 'g'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'g' > 'r'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'r' > 'g'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'a' < 't'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'t' < 'a'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'v' >= 'q'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'q' >= 'v'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'m' <= 'f'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'f' <= 'm'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' == 'w'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' != 'w'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'h' > 'q'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'q' > 'h'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'p' < 'w'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' < 'p'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'y' >= 'a'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "'a' >= 'y'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'k' <= 'g'".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "'g' <= 'k'".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "!(if (false) { 5; })".to_string(),
            expected: Object::bool(true),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_nil_expressions() {
    let test_cases = [TestCase {
        input: "nil".to_string(),
        expected: Object::Nil,
    }];

    run_vm_tests(&test_cases);
}

#[test]
fn test_boolean_operation() {
    let test_cases = [
        TestCase {
            input: "true && 1".to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "false && 20".to_string(),
            expected: Object::bool(false),
        },
        TestCase {
            input: "0 && 2".to_string(),
            expected: Object::int(0),
        },
        TestCase {
            input: "1 && 0".to_string(),
            expected: Object::int(0),
        },
        TestCase {
            input: "0 || 1".to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "1 || 0".to_string(),
            expected: Object::int(1),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_conditionals() {
    let test_cases = [
        TestCase {
            input: "if (false) { 10 }".to_string(),
            expected: Object::Nil,
        },
        TestCase {
            input: "if (true) { 10 } else { 20 }".to_string(),
            expected: Object::int(10),
        },
        TestCase {
            input: "if (false) { 10 } else { 20 } ".to_string(),
            expected: Object::int(20),
        },
        TestCase {
            input: "if (1) { 10 }".to_string(),
            expected: Object::int(10),
        },
        TestCase {
            input: "if (1 < 2) { 10 }".to_string(),
            expected: Object::int(10),
        },
        TestCase {
            input: "if (1 < 2) { 10 } else { 20 }".to_string(),
            expected: Object::int(10),
        },
        TestCase {
            input: "if (1 > 2) { 10 } else { 20 }".to_string(),
            expected: Object::int(20),
        },
        TestCase {
            input: "if ((if (false) { 10 })) { 10 } else { 20 }".to_string(),
            expected: Object::int(20),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_global_declaration_statements() {
    let test_cases = [
        TestCase {
            input: "var one = 1; one".to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "var one = 1; var two = 2; one + two".to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "var one = 1; var two = one + one; one + two".to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "const one = 1; one".to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "const one = 1; const two = 2; one + two".to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "const one = 1; const two = one + one; one + two".to_string(),
            expected: Object::int(3),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_string_expressions() {
    let test_cases = [
        TestCase {
            input: "\"panda\"".to_string(),
            expected: Object::str("panda".to_string()),
        },
        TestCase {
            input: "\"pan\" + \"da\"".to_string(),
            expected: Object::str("panda".to_string()),
        },
        TestCase {
            input: "\"pa\" + \"nda\" + \" \" + \"bamboo\"".to_string(),
            expected: Object::str("panda bamboo".to_string()),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_array_literals() {
    let test_cases = [
        TestCase {
            input: "[]".to_string(),
            expected: Object::array(Vec::new()),
        },
        TestCase {
            input: "[1, 2, 3]".to_string(),
            expected: Object::array(Vec::from([Object::int(1), Object::int(2), Object::int(3)])),
        },
        TestCase {
            input: "[1 + 2, 3 * 4, 5 + 6]".to_string(),
            expected: Object::array(Vec::from([
                Object::int(3),
                Object::int(12),
                Object::int(11),
            ])),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_dict_literals() {
    let test_cases = [
        TestCase {
            input: "{}".to_string(),
            expected: Object::dict(HashMap::new()),
        },
        TestCase {
            input: "{1: 2, 2: 3}".to_string(),
            expected: Object::dict(HashMap::from([
                (
                    Hashable::Int(1).hash(),
                    DictPair {
                        key: Hashable::Int(1),
                        value: Object::int(2),
                    },
                ),
                (
                    Hashable::Int(2).hash(),
                    DictPair {
                        key: Hashable::Int(2),
                        value: Object::int(3),
                    },
                ),
            ])),
        },
        TestCase {
            input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
            expected: Object::dict(HashMap::from([
                (
                    Hashable::Int(2).hash(),
                    DictPair {
                        key: Hashable::Int(2),
                        value: Object::int(4),
                    },
                ),
                (
                    Hashable::Int(6).hash(),
                    DictPair {
                        key: Hashable::Int(6),
                        value: Object::int(16),
                    },
                ),
            ])),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_index_expression() {
    let test_cases = [
        TestCase {
            input: "[1, 2, 3][1]".to_string(),
            expected: Object::int(2),
        },
        TestCase {
            input: "[1, 2, 3][0 + 2]".to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "[[1, 1, 1]][0][0]".to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "{1: 1, 2: 2}[1]".to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "{1: 1, 2: 2}[2]".to_string(),
            expected: Object::int(2),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_without_arguments() {
    let test_cases = [
        TestCase {
            input: "var fivePlusTen = fn() { 5 + 10 }; fivePlusTen()".to_string(),
            expected: Object::int(15),
        },
        TestCase {
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
            expected: Object::int(3),
        },
        TestCase {
            input: "var one = fn() { 1 }; var two = fn() { 2 }; one() + two()".to_string(),
            expected: Object::int(3),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_functions_with_return_statement() {
    let test_cases = [
        TestCase {
            input: "var earlyExit = fn() { return 99; 100; }; earlyExit()".to_string(),
            expected: Object::int(99),
        },
        TestCase {
            input: "var earlyExit = fn() { return 99; return 100; }; earlyExit()".to_string(),
            expected: Object::int(99),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_functions_without_return_value() {
    let test_cases = [
        TestCase {
            input: "var noReturn = fn() { }; noReturn();".to_string(),
            expected: Object::Nil,
        },
        TestCase {
            input: "
var noReturn = fn() { };
var noReturnTwo = fn() {
    noReturn();
};
noReturn();
noReturnTwo();"
                .to_string(),
            expected: Object::Nil,
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_first_class_functions() {
    let test_cases = [
        TestCase {
            input: "var returnsOne = fn() { 1 };
            var returnsOneReturner = fn() { returnsOne };
            returnsOneReturner()()"
                .to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "var returnsOneReturner = fn() {
            var returnsOne = fn() { 1 };
            returnsOne
            };
            returnsOneReturner()()"
                .to_string(),
            expected: Object::int(1),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_with_bindings() {
    let test_cases = [
        TestCase {
            input: "var one = fn() { var one = 1; one };
        one()"
                .to_string(),
            expected: Object::int(1),
        },
        TestCase {
            input: "var oneAndTwo = fn() {
                var one = 1;
                var two = 2;
                one + two
            };
            oneAndTwo()"
                .to_string(),
            expected: Object::int(3),
        },
        TestCase {
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
            expected: Object::int(10),
        },
        TestCase {
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
            expected: Object::int(150),
        },
        TestCase {
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
            expected: Object::int(97),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
    let test_cases = [
        TestCase {
            input: "var identity = fn(a) { a };
            identity(4)"
                .to_string(),
            expected: Object::int(4),
        },
        TestCase {
            input: "var sum = fn(a, b) { a + b };
            sum(1, 2)"
                .to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "
var sum = fn(a, b) {
    var c = a + b;
    c
};
sum(1, 2)"
                .to_string(),
            expected: Object::int(3),
        },
        TestCase {
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
            expected: Object::int(10),
        },
        TestCase {
            input: "var sum = fn(a, b) {
                var c = a + b;
                c
                };
                sum(1, 2) + sum(3, 4)"
                .to_string(),
            expected: Object::int(10),
        },
        TestCase {
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
            expected: Object::int(50),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
    let test_cases = [
        TestCase {
            input: "fn() { 1; }(1);".to_string(),
            expected: Object::error("wrong number of arguments. got: 1, want: 0".to_string()),
        },
        TestCase {
            input: "fn(a) { a; }();".to_string(),
            expected: Object::error("wrong number of arguments. got: 0, want: 1".to_string()),
        },
        TestCase {
            input: "fn(a, b) { a + b; }(1);".to_string(),
            expected: Object::error("wrong number of arguments. got: 1, want: 2".to_string()),
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
            assert_eq!(Object::error(err), test_case.expected);
        } else {
            panic!("expected error but resulted in Ok(())")
        }
    }
}

#[test]
fn test_closures() {
    let test_cases = [
        TestCase {
            input: "
var newClosure = fn(a) {
    fn() { a }
};

var closure = newClosure(99);
closure()"
                .to_string(),
            expected: Object::int(99),
        },
        TestCase {
            input: "
var newAdder = fn(a, b) {
    fn(c) { a + b + c }
};
var adder = newAdder(1, 2);
adder(8)"
                .to_string(),
            expected: Object::int(11),
        },
        TestCase {
            input: "
var newAdder = fn(a, b) {
    var c = a + b;
    fn(d) { c + d }
};
var adder = newAdder(1, 2);
adder(8)"
                .to_string(),
            expected: Object::int(11),
        },
        TestCase {
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
            expected: Object::int(14),
        },
        TestCase {
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
            expected: Object::int(14),
        },
        TestCase {
            input: "
var  newClosure = fn(a, b) {
    var  one = fn() { a };
    var  two = fn() { b };
    fn() { one() + two() }
};

var  closure = newClosure(9, 90);
closure()"
                .to_string(),
            expected: Object::int(99),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_recursive_functions() {
    let test_cases = [
        TestCase {
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
            expected: Object::int(1),
        },
        TestCase {
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
            expected: Object::int(0),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_while_statement() {
    let test_cases = [
        TestCase {
            input: "var i = 10; while (i > 0) { i = i - 1 }; i".to_string(),
            expected: Object::int(0),
        },
        TestCase {
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
            expected: Object::int(5),
        },
        TestCase {
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
            expected: Object::int(5),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_for_statement() {
    let test_cases = [
        TestCase {
            input: "
var x = 0;
for (i in [1, 2, 3, 4, 5, 6, 7, 8, 9]) {
    x = x + i;
}
x
"
            .to_string(),
            expected: Object::int(45),
        },
        TestCase {
            input: "
var x = 0;
for (i in {1:0, 2:'a', 3:\"\", 4:3.4, 5:[1,2], 6:{1:1}, 7:1..2, 8:fn(){}, 9:(-23 + 345)}) {
    x = x + i;
}
x
"
            .to_string(),
            expected: Object::int(45),
        },
        TestCase {
            input: "
var x = 0;
for (i in 0..10) {
    x = x + i;
}
x
"
            .to_string(),
            expected: Object::int(45),
        },
    ];

    run_vm_tests(&test_cases);
}

#[test]
fn test_assign_expressions() {
    let test_cases = [TestCase {
        input: "var i = 10; i = i - 1 ; i".to_string(),
        expected: Object::int(9),
    }];

    run_vm_tests(&test_cases);
}

#[test]
fn test_builtin_method_expressions() {
    let test_cases = [
        TestCase {
            input: "{1:1, 2:2, 3:3}.len()".to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "var isDigit = 'a'.isDigit; isDigit(16)".to_string(),
            expected: Object::bool(true),
        },
        TestCase {
            input: "(-3).abs()".to_string(),
            expected: Object::int(3),
        },
        TestCase {
            input: "[2, 3].contains(2)".to_string(),
            expected: Object::bool(true),
        },
    ];

    run_vm_tests(&test_cases);
}

fn parse(input: &str) -> Option<Node> {
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    p.parse_program()
}

fn run_vm_tests(test_cases: &[TestCase]) {
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

        assert_eq!(test_case.expected, stack_elem.unwrap_or(Object::Nil));
    }
}
