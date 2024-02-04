use std::collections::HashMap;

use pretty_assertions::assert_eq;

use super::{eval::Evaluator, vm::VM};
use crate::{
    compiler::Compiler,
    lexer::Lexer,
    object::{DictPair, Hashable, Object},
    parser::Parser,
};

struct TestCase {
    input: &'static str,
    expected: Object,
}

fn run_tests(test_cases: &[TestCase]) {
    for test_case in test_cases {
        let mut l = Lexer::new(test_case.input);
        let mut p = Parser::new(&mut l);

        if !p.errors.is_empty() {
            for err in p.errors {
                println!("parser error: {err}");
            }

            panic!()
        }

        let program = p.parse_program().unwrap();

        // Evaluator

        let mut evaluator = Evaluator::new();

        let evaluated = evaluator.eval(program.clone()).unwrap_or(Object::Nil);

        assert_eq!(test_case.expected, evaluated);

        // Compiler + VM
        let mut comp = Compiler::new();

        if let Err(err) = comp.compile(program) {
            assert_eq!(test_case.expected, Object::error(err));
            return;
        }

        let byte_code = comp.bytecode();

        let mut vm = VM::new(&byte_code);

        let evaluated = if let Err(err) = vm.run() {
            Object::error(err)
        } else {
            vm.last_popped_stack_elem.unwrap_or(Object::Nil)
        };

        assert_eq!(test_case.expected, evaluated);
    }
}

#[test]
fn test_errors() {
    run_tests(&[
        TestCase {
            input: "fn() { 1; }(1);",
            expected: Object::error("wrong number of arguments. got: 1, want: 0".to_string()),
        },
        TestCase {
            input: "fn(a) { a; }();",
            expected: Object::error("wrong number of arguments. got: 0, want: 1".to_string()),
        },
        TestCase {
            input: "fn(a, b) { a + b; }(1);",
            expected: Object::error("wrong number of arguments. got: 1, want: 2".to_string()),
        },
        TestCase {
            input: "5 + true;",
            expected: Object::error(
                "unsupported types for binary operation: INT + BOOLEAN".to_string(),
            ),
        },
        TestCase {
            input: "5 + true; 5;",
            expected: Object::error(
                "unsupported types for binary operation: INT + BOOLEAN".to_string(),
            ),
        },
        TestCase {
            input: "-true",
            expected: Object::error("unsupported type for negation: BOOLEAN".to_string()),
        },
        TestCase {
            input: "true + false;",
            expected: Object::error(
                "unsupported types for binary operation: BOOLEAN + BOOLEAN".to_string(),
            ),
        },
        TestCase {
            input: "5; true + false; 5",
            expected: Object::error(
                "unsupported types for binary operation: BOOLEAN + BOOLEAN".to_string(),
            ),
        },
        TestCase {
            input: "if (10 > 1) { true + false; }",
            expected: Object::error(
                "unsupported types for binary operation: BOOLEAN + BOOLEAN".to_string(),
            ),
        },
        TestCase {
            input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
            expected: Object::error(
                "unsupported types for binary operation: BOOLEAN + BOOLEAN".to_string(),
            ),
        },
        TestCase {
            input: "foobar",
            expected: Object::error("undefined variable foobar".to_string()),
        },
        TestCase {
            input: r#""Hello" - "World""#,
            expected: Object::error("unknown operator: STR - STR".to_string()),
        },
        TestCase {
            input: r#"{"name": "Panda"}[fn(x) { x }];"#,
            expected: Object::error("unusable as hash key: FUNCTION".to_string()),
        },
        TestCase {
            input: "[1, 2, 3][3]",
            expected: Object::error("index out of bounds. got: 3".to_string()),
        },
        TestCase {
            input: r#"{"name": "Panda"}["foo"];"#,
            expected: Object::error(r#"key error. got: "foo""#.to_string()),
        },
    ]);
}

#[test]
fn test_eval_integer_expression() {
    run_tests(&[
        TestCase {
            input: "5",
            expected: Object::int(5),
        },
        TestCase {
            input: "10",
            expected: Object::int(10),
        },
        TestCase {
            input: "-5",
            expected: Object::int(-5),
        },
        TestCase {
            input: "-10",
            expected: Object::int(-10),
        },
        TestCase {
            input: "5 + 5 + 5 + 5 - 10",
            expected: Object::int(10),
        },
        TestCase {
            input: "2 * 2 * 2 * 2 * 2",
            expected: Object::int(32),
        },
        TestCase {
            input: "-50 + 100 + -50",
            expected: Object::int(0),
        },
        TestCase {
            input: "5 * 2 + 10",
            expected: Object::int(20),
        },
        TestCase {
            input: "5 % 2 + 10",
            expected: Object::int(11),
        },
        TestCase {
            input: "5 + 2 * 10",
            expected: Object::int(25),
        },
        TestCase {
            input: "20 + 2 * -10",
            expected: Object::int(0),
        },
        TestCase {
            input: "50 / 2 * 2 + 10",
            expected: Object::int(60),
        },
        TestCase {
            input: "2 * (5 + 10)",
            expected: Object::int(30),
        },
        TestCase {
            input: "3 * 3 * 3 + 10",
            expected: Object::int(37),
        },
        TestCase {
            input: "3 * (3 * 3) + 10",
            expected: Object::int(37),
        },
        TestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: Object::int(50),
        },
        TestCase {
            input: "4 / 2",
            expected: Object::int(2),
        },
        TestCase {
            input: "50 / 2 * 2 + 10 - 5",
            expected: Object::int(55),
        },
        TestCase {
            input: "5 & 10",
            expected: Object::int(0),
        },
        TestCase {
            input: "5 & (2 | 10)",
            expected: Object::int(0),
        },
        TestCase {
            input: "5 ^ (2 & 10)",
            expected: Object::int(7),
        },
        TestCase {
            input: "5 | (2 | 10)",
            expected: Object::int(15),
        },
        TestCase {
            input: "-5",
            expected: Object::int(-5),
        },
        TestCase {
            input: "-10",
            expected: Object::int(-10),
        },
    ]);
}

#[test]
fn test_eval_boolean_expression() {
    run_tests(&[
        TestCase {
            input: "true",
            expected: Object::bool(true),
        },
        TestCase {
            input: "false",
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 < 2",
            expected: Object::bool(true),
        },
        TestCase {
            input: "1 > 2",
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 < 1",
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 > 1",
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 == 1",
            expected: Object::bool(true),
        },
        TestCase {
            input: "1 != 1",
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 == 2",
            expected: Object::bool(false),
        },
        TestCase {
            input: "1 != 2",
            expected: Object::bool(true),
        },
        TestCase {
            input: "true == true",
            expected: Object::bool(true),
        },
        TestCase {
            input: "false == false",
            expected: Object::bool(true),
        },
        TestCase {
            input: "true == false",
            expected: Object::bool(false),
        },
        TestCase {
            input: "true != false",
            expected: Object::bool(true),
        },
        TestCase {
            input: "false != true",
            expected: Object::bool(true),
        },
        TestCase {
            input: "(1 < 2) == true",
            expected: Object::bool(true),
        },
        TestCase {
            input: "(1 < 2) == false",
            expected: Object::bool(false),
        },
        TestCase {
            input: "(1 > 2) == true",
            expected: Object::bool(false),
        },
        TestCase {
            input: "(1 > 2) == false",
            expected: Object::bool(true),
        },
        TestCase {
            input: "!true",
            expected: Object::bool(false),
        },
        TestCase {
            input: "!false",
            expected: Object::bool(true),
        },
        TestCase {
            input: "!5",
            expected: Object::bool(false),
        },
        TestCase {
            input: "!!true",
            expected: Object::bool(true),
        },
        TestCase {
            input: "!!false",
            expected: Object::bool(false),
        },
        TestCase {
            input: "!!5",
            expected: Object::bool(true),
        },
        TestCase {
            input: "-5.67 == -7.04",
            expected: Object::bool(false),
        },
        TestCase {
            input: "-7.04 == -5.67",
            expected: Object::bool(false),
        },
        TestCase {
            input: "0.01 + 0.02 == 0.03",
            expected: Object::bool(true),
        },
        TestCase {
            input: "-0.32 + 0.22 == -0.11",
            expected: Object::bool(false),
        },
        TestCase {
            input: "-0.32 + 0.22 == -0.10",
            expected: Object::bool(true),
        },
        TestCase {
            input: "-8.54 != 10.29",
            expected: Object::bool(true),
        },
        TestCase {
            input: "10.29 != -8.54",
            expected: Object::bool(true),
        },
        TestCase {
            input: "-7.52 > -10.18",
            expected: Object::bool(true),
        },
        TestCase {
            input: "-10.18 > -7.52",
            expected: Object::bool(false),
        },
        TestCase {
            input: "9.72 < -2.42",
            expected: Object::bool(false),
        },
        TestCase {
            input: "-2.42 < 9.72",
            expected: Object::bool(true),
        },
        TestCase {
            input: "-2.66 >= -7.71",
            expected: Object::bool(true),
        },
        TestCase {
            input: "-7.71 >= -2.66",
            expected: Object::bool(false),
        },
        TestCase {
            input: "6.94 <= 3.59",
            expected: Object::bool(false),
        },
        TestCase {
            input: "3.59 <= 6.94",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' == 'p'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'p' == 'w'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'g' != 'c'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'c' != 'g'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'g' > 'r'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'r' > 'g'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'a' < 't'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'t' < 'a'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'v' >= 'q'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'q' >= 'v'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'m' <= 'f'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'f' <= 'm'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' == 'w'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' != 'w'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'h' > 'q'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'q' > 'h'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'p' < 'w'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'w' < 'p'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'y' >= 'a'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "'a' >= 'y'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'k' <= 'g'",
            expected: Object::bool(false),
        },
        TestCase {
            input: "'g' <= 'k'",
            expected: Object::bool(true),
        },
        TestCase {
            input: "!(if (false) { 5; })",
            expected: Object::bool(true),
        },
    ]);
}

#[test]
fn test_boolean_operation() {
    run_tests(&[
        TestCase {
            input: "true && 1",
            expected: Object::int(1),
        },
        TestCase {
            input: "false && 20",
            expected: Object::bool(false),
        },
        TestCase {
            input: "0 && 2",
            expected: Object::int(0),
        },
        TestCase {
            input: "1 && 0",
            expected: Object::int(0),
        },
        TestCase {
            input: "0 || 1",
            expected: Object::int(1),
        },
        TestCase {
            input: "1 || 0",
            expected: Object::int(1),
        },
    ]);
}

#[test]
fn test_eval_float_expression() {
    run_tests(&[
        TestCase {
            input: "-7.84 + -5.89",
            expected: Object::float(-13.73),
        },
        TestCase {
            input: "-5.94 * -8.94",
            expected: Object::float(53.1036),
        },
        TestCase {
            input: "-8.65 - 0.65",
            expected: Object::float(-9.3),
        },
        TestCase {
            input: "5.17 / -9.39",
            expected: Object::float(-0.550_585_729_499_467_5),
        },
        TestCase {
            input: "2.25 - -9.86 / -6.43",
            expected: Object::float(0.716_562_986_003_110_4),
        },
        TestCase {
            input: "-1.33 * -8.74 + 9.55",
            expected: Object::float(21.1742),
        },
        TestCase {
            input: "-9.07 + 5.55 - -8.99",
            expected: Object::float(5.47),
        },
        TestCase {
            input: "-7.73 / -1.28 * 7.35",
            expected: Object::float(44.387_109_374_999_994),
        },
        TestCase {
            input: "-6.24 * -5.34 / -2.9",
            expected: Object::float(-11.490_206_896_551_726),
        },
        TestCase {
            input: "1.4 / 6.1 - -4.25",
            expected: Object::float(4.479_508_196_721_311),
        },
        TestCase {
            input: "2.31 / 0.79 * 7.31",
            expected: Object::float(21.374_810_126_582_28),
        },
        TestCase {
            input: "-5.01 + -3.01 * 6.63",
            expected: Object::float(-24.966_299_999_999_997),
        },
        TestCase {
            input: "3.87 / -4.74 + 2.99",
            expected: Object::float(2.173_544_303_797_468_7),
        },
        TestCase {
            input: "-6.52 + -10.37 / -6.36",
            expected: Object::float(-4.889_496_855_345_912),
        },
        TestCase {
            input: "8.2 - -1.6 * 8.86",
            expected: Object::float(22.375_999_999_999_998),
        },
        TestCase {
            input: "10.24 + -8.92 + -5.85",
            expected: Object::float(-4.529_999_999_999_999),
        },
        TestCase {
            input: "-4.34 * 4.28 / -6.41",
            expected: Object::float(2.897_847_113_884_555),
        },
        TestCase {
            input: "3.53 + -2.38 / 0.56",
            expected: Object::float(-0.719_999_999_999_999_3),
        },
        TestCase {
            input: "5.07 / -4.03 / -3.23",
            expected: Object::float(0.389_493_658_244_282_45),
        },
    ]);
}

#[test]
fn test_bang_operator() {
    run_tests(&[
        TestCase {
            input: "!true",
            expected: Object::bool(false),
        },
        TestCase {
            input: "!false",
            expected: Object::bool(true),
        },
        TestCase {
            input: "!5",
            expected: Object::bool(false),
        },
        TestCase {
            input: "!!true",
            expected: Object::bool(true),
        },
        TestCase {
            input: "!!false",
            expected: Object::bool(false),
        },
        TestCase {
            input: "!!5",
            expected: Object::bool(true),
        },
    ]);
}

#[test]
fn test_if_else_expression() {
    run_tests(&[
        TestCase {
            input: "if (true) { 10 }",
            expected: Object::int(10),
        },
        TestCase {
            input: "if (1 > 2) { 10 }",
            expected: Object::Nil,
        },
        TestCase {
            input: "if (false) { 10 }",
            expected: Object::Nil,
        },
        TestCase {
            input: "if (true) { 10 } else { 20 }",
            expected: Object::int(10),
        },
        TestCase {
            input: "if (false) { 10 } else { 20 } ",
            expected: Object::int(20),
        },
        TestCase {
            input: "if (1) { 10 }",
            expected: Object::int(10),
        },
        TestCase {
            input: "if (1 < 2) { 10 }",
            expected: Object::int(10),
        },
        TestCase {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Object::int(10),
        },
        TestCase {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Object::int(20),
        },
        TestCase {
            input: "if ((if (false) { 10 })) { 10 } else { 20 }",
            expected: Object::int(20),
        },
    ]);
}

#[test]
fn test_declaration_statement() {
    run_tests(&[
        TestCase {
            input: "var a = 5; a",
            expected: Object::int(5),
        },
        TestCase {
            input: "var a = 5 * 5; a",
            expected: Object::int(25),
        },
        TestCase {
            input: "var a = 5; var b = a; b",
            expected: Object::int(5),
        },
        TestCase {
            input: "var a = 5; var b = a; var c = a + b + 5; c",
            expected: Object::int(15),
        },
        TestCase {
            input: "const a = 5; a",
            expected: Object::int(5),
        },
        TestCase {
            input: "const a = 5 * 5; a",
            expected: Object::int(25),
        },
        TestCase {
            input: "const a = 5; const b = a; b",
            expected: Object::int(5),
        },
        TestCase {
            input: "const a = 5; var b = a; const c = a + b + 5; c",
            expected: Object::int(15),
        },
    ]);
}

#[test]
fn test_functions() {
    run_tests(&[
        TestCase {
            input: "fn addTwo(x) { x + 2 }; addTwo(10)",
            expected: Object::int(12),
        },
        TestCase {
            input: "var identity = fn(x) { x }; identity(5)",
            expected: Object::int(5),
        },
        TestCase {
            input: "var identity = fn(x) { return x; }; identity(5)",
            expected: Object::int(5),
        },
        TestCase {
            input: "var double = fn(x) { x * 2 }; double(5)",
            expected: Object::int(10),
        },
        TestCase {
            input: "var add = fn(x, y) { x + y }; add(5, 5)",
            expected: Object::int(10),
        },
        TestCase {
            input: "var add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))",
            expected: Object::int(20),
        },
        TestCase {
            input: "fn(x) { x }(5)",
            expected: Object::int(5),
        },
        TestCase {
            input: "var fivePlusTen = fn() { 5 + 10 }; fivePlusTen()",
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

c()",
            expected: Object::int(3),
        },
        TestCase {
            input: "var one = fn() { 1 }; var two = fn() { 2 }; one() + two()",
            expected: Object::int(3),
        },
        TestCase {
            input: "var earlyExit = fn() { return 99; 100; }; earlyExit()",
            expected: Object::int(99),
        },
        TestCase {
            input: "var earlyExit = fn() { return 99; return 100; }; earlyExit()",
            expected: Object::int(99),
        },
        TestCase {
            input: "var noReturn = fn() { }; noReturn();",
            expected: Object::Nil,
        },
        TestCase {
            input: "
var noReturn = fn() { };
var noReturnTwo = fn() {
    noReturn();
};

noReturn();
noReturnTwo();",
            expected: Object::Nil,
        },
        TestCase {
            input: "var returnsOne = fn() { 1 };
var returnsOneReturner = fn() { returnsOne };

returnsOneReturner()()",
            expected: Object::int(1),
        },
        TestCase {
            input: "var returnsOneReturner = fn() {
    var returnsOne = fn() { 1 };
    returnsOne
};

returnsOneReturner()()",
            expected: Object::int(1),
        },
        TestCase {
            input: "var one = fn() { var one = 1; one }; one()",
            expected: Object::int(1),
        },
        TestCase {
            input: "var oneAndTwo = fn() {
    var one = 1;
    var two = 2;
    one + two
};

oneAndTwo()",
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

oneAndTwo() + threeAndFour()",
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
firstFoobar() + secondFoobar()",
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
            ",
            expected: Object::int(97),
        },
        TestCase {
            input: "var identity = fn(a) { a }; identity(4)",
            expected: Object::int(4),
        },
        TestCase {
            input: "var sum = fn(a, b) { a + b }; sum(1, 2)",
            expected: Object::int(3),
        },
        TestCase {
            input: "
var sum = fn(a, b) {
    var c = a + b;
    c
};

sum(1, 2)",
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

outer()",
            expected: Object::int(10),
        },
        TestCase {
            input: "var sum = fn(a, b) {
    var c = a + b;
    c
};

sum(1, 2) + sum(3, 4)",
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
            outer() + globalNum",
            expected: Object::int(50),
        },
        TestCase {
            input: "
        var newAdder = fn(x) {
            fn(y) {
                x + y
            }
        };

        var addTwo = newAdder(2);
        addTwo(2)",
            expected: Object::int(4),
        },
        TestCase {
            input: "
var newClosure = fn(a) {
fn() { a }
};

var closure = newClosure(99);
closure()",
            expected: Object::int(99),
        },
        TestCase {
            input: "
var newAdder = fn(a, b) {
fn(c) { a + b + c }
};
var adder = newAdder(1, 2);
adder(8)",
            expected: Object::int(11),
        },
        TestCase {
            input: "
var newAdder = fn(a, b) {
var c = a + b;
fn(d) { c + d }
};
var adder = newAdder(1, 2);
adder(8)",
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
        ",
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
adder(8)",
            expected: Object::int(14),
        },
        TestCase {
            input: "
var  newClosure = fn(a, b) {
var  one = fn() { a };
var  two = fn() { b };
fn() { one() + two() }
};

var closure = newClosure(9, 90);
closure()",
            expected: Object::int(99),
        },
        TestCase {
            input: "
fn countDown(x) {
    if (x == 1) {
        return 1
    } else {
        countDown(x - 1)
    }
};

countDown(2)",
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
wrapper()",
            expected: Object::int(0),
        },
    ]);
}

#[test]
fn test_char_literal() {
    run_tests(&[TestCase {
        input: "'a'",
        expected: Object::char('a'),
    }]);
}

#[test]
fn test_nil_literal() {
    run_tests(&[TestCase {
        input: "nil",
        expected: Object::Nil,
    }]);
}

#[test]
fn test_string_expressions() {
    run_tests(&[
        TestCase {
            input: r#""panda""#,
            expected: Object::str("panda".to_string()),
        },
        TestCase {
            input: r#""pan" + "da""#,
            expected: Object::str("panda".to_string()),
        },
        TestCase {
            input: r#""pa" + "nda" + " " + "bamboo""#,
            expected: Object::str("panda bamboo".to_string()),
        },
    ]);
}

#[test]
fn test_array_literals() {
    run_tests(&[
        TestCase {
            input: "[]",
            expected: Object::array(Vec::new()),
        },
        TestCase {
            input: "[1, 2, 3]",
            expected: Object::array(Vec::from([Object::int(1), Object::int(2), Object::int(3)])),
        },
        TestCase {
            input: "[1 + 2, 3 * 4, 5 + 6]",
            expected: Object::array(Vec::from([
                Object::int(3),
                Object::int(12),
                Object::int(11),
            ])),
        },
    ]);
}

#[test]
fn test_array_index_expressions() {
    run_tests(&[
        TestCase {
            input: "[1, 2, 3][0]",
            expected: Object::int(1),
        },
        TestCase {
            input: "[1, 2, 3][1]",
            expected: Object::int(2),
        },
        TestCase {
            input: "[1, 2, 3][2]",
            expected: Object::int(3),
        },
        TestCase {
            input: "var i = 0; [1][i]",
            expected: Object::int(1),
        },
        TestCase {
            input: "[1, 2, 3][1 + 1]",
            expected: Object::int(3),
        },
        TestCase {
            input: "var myArray = [1, 2, 3]; myArray[2]",
            expected: Object::int(3),
        },
        TestCase {
            input: "var myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
            expected: Object::int(6),
        },
        TestCase {
            input: "var myArray = [1, 2, 3]; var i = myArray[0]; myArray[i]",
            expected: Object::int(2),
        },
        TestCase {
            input: "[1, 2, 3][-1]",
            expected: Object::int(3),
        },
        TestCase {
            input: "[[1, 1, 1]][0][0]",
            expected: Object::int(1),
        },
        TestCase {
            input: "{1: 1, 2: 2}[1]",
            expected: Object::int(1),
        },
        TestCase {
            input: "{1: 1, 2: 2}[2]",
            expected: Object::int(2),
        },
        TestCase {
            input: r#"{"foo": 5}["foo"]"#,
            expected: Object::int(5),
        },
        TestCase {
            input: r#"var key = "foo"; {"foo": 5}[key]"#,
            expected: Object::int(5),
        },
        TestCase {
            input: "{5: 5}[5]",
            expected: Object::int(5),
        },
        TestCase {
            input: "{true: 5}[true]",
            expected: Object::int(5),
        },
        TestCase {
            input: "{false: 5}[false]",
            expected: Object::int(5),
        },
    ]);
}

#[test]
fn test_dict_literals() {
    run_tests(&[
        TestCase {
            input: "{}",
            expected: Object::dict(HashMap::new()),
        },
        TestCase {
            input: "{1: 2, 2: 3}",
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
            input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
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
        TestCase {
            input: r#"var two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }"#,
            expected: Object::dict(HashMap::from([
                (
                    Hashable::Str("one".to_string()).hash(),
                    DictPair {
                        key: Hashable::Str("one".to_string()),
                        value: Object::int(1),
                    },
                ),
                (
                    Hashable::Str("two".to_string()).hash(),
                    DictPair {
                        key: Hashable::Str("two".to_string()),
                        value: Object::int(2),
                    },
                ),
                (
                    Hashable::Str("three".to_string()).hash(),
                    DictPair {
                        key: Hashable::Str("three".to_string()),
                        value: Object::int(3),
                    },
                ),
                (
                    Hashable::Int(4).hash(),
                    DictPair {
                        key: Hashable::Int(4),
                        value: Object::int(4),
                    },
                ),
                (
                    Hashable::Bool(true).hash(),
                    DictPair {
                        key: Hashable::Bool(true),
                        value: Object::int(5),
                    },
                ),
                (
                    Hashable::Bool(false).hash(),
                    DictPair {
                        key: Hashable::Bool(false),
                        value: Object::int(6),
                    },
                ),
            ])),
        },
    ]);
}

#[test]
fn test_builtin_methods() {
    run_tests(&[
        TestCase {
            input: "[2, 3].contains(2)",
            expected: Object::bool(true),
        },
        TestCase {
            input: "(-3).abs()",
            expected: Object::int(3),
        },
        TestCase {
            input: "{1:1, 2:2, 3:3}.len()",
            expected: Object::int(3),
        },
        TestCase {
            input: "var isDigit = 'a'.isDigit; isDigit(16)",
            expected: Object::bool(true),
        },
        TestCase {
            input: "var isDigit = 'g'.isDigit; isDigit(16)",
            expected: Object::bool(false),
        },
        TestCase {
            input: "var c = 'g'; c.isDigit(16)",
            expected: Object::bool(false),
        },
    ]);
}

#[test]
fn test_delete_statement() {
    run_tests(&[TestCase {
        input: "var i = 10;
    delete i",
        expected: Object::int(10),
    }]);
}

#[test]
fn test_for_statement() {
    run_tests(&[
        TestCase {
            input: "var i = 0;
for (j in 0..10) {
    i = i + j;
}

i",
            expected: Object::int(45),
        },
        TestCase {
            input: "var arr = [];
for (i in 0..2) {
    arr = arr.push(i)
}

arr",
            expected: Object::array(Vec::from([Object::int(0), Object::int(1)])),
        },
        TestCase {
            input: "
var x = 0;
for (i in [1, 2, 3, 4, 5, 6, 7, 8, 9]) {
    x = x + i;
}
x
",
            expected: Object::int(45),
        },
        TestCase {
            input: "
var x = 0;
for (i in {1:0, 2:'a', 3:\"\", 4:3.4, 5:[1,2], 6:{1:1}, 7:1..2, 8:fn(){}, 9:(-23 + 345)}) {
    x = x + i;
}
x
",
            expected: Object::int(45),
        },
        TestCase {
            input: "
var x = 0;
for (i in 0..10) {
    x = x + i;
}
x
",
            expected: Object::int(45),
        },
    ]);
}

#[test]
fn test_while_statement() {
    run_tests(&[
        TestCase {
            input: "var i = 0;
var j = 0;
while (j < 10) {
    i = i + j;
    j = j + 1;
}

i",
            expected: Object::int(45),
        },
        TestCase {
            input: "var arr = [];
var i = 0;
while (i < 2) {
    arr = arr.push(i)
    i = i + 1;
}

arr",
            expected: Object::array(Vec::from([Object::int(0), Object::int(1)])),
        },
        TestCase {
            input: "var i = 10; while (i > 0) { i = i - 1 }; i",
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
i",
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
j",
            expected: Object::int(5),
        },
    ]);
}

#[test]
fn test_assign_expressions() {
    run_tests(&[
        TestCase {
            input: "var i = 10; i = i - 1 ; i",
            expected: Object::int(9),
        },
        {
            TestCase {
                input: "var arr = [1, 1, 1, 1];
arr[0] = 0;
arr[0]",
                expected: Object::int(0),
            }
        },
    ]);
}
