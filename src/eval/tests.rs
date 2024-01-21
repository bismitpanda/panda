use std::collections::HashMap;

use pretty_assertions::assert_eq;

use super::*;
use crate::{
    ast::{ExpressionStmt, Infix, Lit},
    lexer::Lexer,
    parser::Parser,
};

struct TestCase {
    input: &'static str,
    expected: Object,
}

#[test]
fn test_eval_integer_expression() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_eval_float_expression() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_bang_operator() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_if_else_expression() {
    let test_cases = [
        TestCase {
            input: "if (true) { 10 }",
            expected: Object::int(10),
        },
        TestCase {
            input: "if (false) { 10 }",
            expected: Object::Nil,
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
            input: "if (1 > 2) { 10 }",
            expected: Object::Nil,
        },
        TestCase {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Object::int(20),
        },
        TestCase {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Object::int(10),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_return_statements() {
    let test_cases = [
        TestCase {
            input: "return 10;",
            expected: Object::int(10),
        },
        TestCase {
            input: "return 10; 9;",
            expected: Object::int(10),
        },
        TestCase {
            input: "return 2 * 5; 9;",
            expected: Object::int(10),
        },
        TestCase {
            input: "9; return 2 * 5; 9;",
            expected: Object::int(10),
        },
        TestCase {
            input: "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
            expected: Object::int(10),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_error_handling() {
    let test_cases = [
        TestCase {
            input: "5 + true;",
            expected: Object::error("type mismatch: INT + BOOLEAN".to_string()),
        },
        TestCase {
            input: "5 + true; 5;",
            expected: Object::error("type mismatch: INT + BOOLEAN".to_string()),
        },
        TestCase {
            input: "-true",
            expected: Object::error("unknown operator: -BOOLEAN".to_string()),
        },
        TestCase {
            input: "true + false;",
            expected: Object::error("unknown operator: BOOLEAN + BOOLEAN".to_string()),
        },
        TestCase {
            input: "5; true + false; 5",
            expected: Object::error("unknown operator: BOOLEAN + BOOLEAN".to_string()),
        },
        TestCase {
            input: "if (10 > 1) { true + false; }",
            expected: Object::error("unknown operator: BOOLEAN + BOOLEAN".to_string()),
        },
        TestCase {
            input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
            expected: Object::error("unknown operator: BOOLEAN + BOOLEAN".to_string()),
        },
        TestCase {
            input: "foobar",
            expected: Object::error("identifier not found: foobar".to_string()),
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_declaration_statement() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_lambda_object() {
    let input = "fn(x) { x + 2 }";

    let evaluated = test_eval(input);
    assert_eq!(
        Object::EvaluatedFunction(EvaluatedFunction {
            parameters: Vec::from(["x".to_string()]),
            body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Infix(Infix {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "x".to_string()
                    })),
                    operator: Operator::Add,
                    right: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int { value: 2 }
                    }))
                })
            })]),
            environment: Environment {
                store: HashMap::new(),
                outer: None,
                types: HashMap::new(),
                imports: HashMap::new()
            }
        }),
        evaluated
    );
}

#[test]
fn test_function_statement() {
    let input = "fn addTwo(x) { x + 2 }; addTwo(10)";

    let evaluated = test_eval(input);
    assert_eq!(Object::int(12), evaluated);
}

#[test]
fn test_function_application() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_closures() {
    let input = "
var newAdder = fn(x) {
    fn(y) {
        x + y
    }
};

var addTwo = newAdder(2);
addTwo(2)";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::int(4));
}

#[test]
fn test_string_literal() {
    let input = r#""Hello World!""#;
    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::str("Hello World!".to_string()));
}

#[test]
fn test_char_literal() {
    let input = "'a'";
    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::char('a'));
}

#[test]
fn test_nil_literal() {
    let input = "nil";
    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Nil);
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::str("Hello World!".to_string()));
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);
    assert_eq!(
        evaluated,
        Object::array(Vec::from([Object::int(1), Object::int(4), Object::int(6)]))
    );
}

#[test]
fn test_array_index_expressions() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_dict_literals() {
    let input = r#"var two = "two";
{
    "one": 10 - 9,
    two: 1 + 1,
    "thr" + "ee": 6 / 2,
    4: 4,
    true: 5,
    false: 6
}"#;
    let evaluated = test_eval(input);
    let key1 = Hashable::Str("one".to_string());
    let key2 = Hashable::Str("two".to_string());
    let key3 = Hashable::Str("three".to_string());
    let key4 = Hashable::Int(4);
    let key5 = Hashable::from_object(&Object::TRUE).unwrap();
    let key6 = Hashable::from_object(&Object::FALSE).unwrap();
    assert_eq!(
        evaluated,
        Object::dict(HashMap::from([
            (
                key1.hash(),
                DictPair {
                    key: key1,
                    value: Object::int(1)
                }
            ),
            (
                key2.hash(),
                DictPair {
                    key: key2,
                    value: Object::int(2)
                }
            ),
            (
                key3.hash(),
                DictPair {
                    key: key3,
                    value: Object::int(3)
                }
            ),
            (
                key4.hash(),
                DictPair {
                    key: key4,
                    value: Object::int(4)
                }
            ),
            (
                key5.hash(),
                DictPair {
                    key: key5,
                    value: Object::int(5)
                }
            ),
            (
                key6.hash(),
                DictPair {
                    key: key6,
                    value: Object::int(6)
                }
            ),
        ]))
    );
}

#[test]
fn test_dict_index_expressions() {
    let test_cases = [
        TestCase {
            input: r#"{"foo": 5}["foo"]"#,
            expected: Object::int(5),
        },
        TestCase {
            input: r#"{"foo": 5}["bar"]"#,
            expected: Object::Nil,
        },
        TestCase {
            input: r#"var key = "foo"; {"foo": 5}[key]"#,
            expected: Object::int(5),
        },
        TestCase {
            input: r#"{}["foo"]"#,
            expected: Object::Nil,
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_builtin_methods() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_class_method() {
    let input = "class a { b() { 10 } };
var c = new a();
c.b()";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::int(10));
}

#[test]
fn test_delete_statement() {
    let input = "var i = 10;
delete i";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::int(10));
}

#[test]
fn test_for_statement() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

#[test]
fn test_while_statement() {
    let test_cases = [
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
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

fn test_eval(input: &str) -> Object {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);

    let program = parser.parse_program().unwrap();

    let mut env = Environment::new();

    eval(program, &mut env).unwrap()
}
