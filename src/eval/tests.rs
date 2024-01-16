use hashbrown::HashMap;
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
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "10",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "-5",
            expected: Object::Int(Int { value: -5 }),
        },
        TestCase {
            input: "-10",
            expected: Object::Int(Int { value: -10 }),
        },
        TestCase {
            input: "5 + 5 + 5 + 5 - 10",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "2 * 2 * 2 * 2 * 2",
            expected: Object::Int(Int { value: 32 }),
        },
        TestCase {
            input: "-50 + 100 + -50",
            expected: Object::Int(Int { value: 0 }),
        },
        TestCase {
            input: "5 * 2 + 10",
            expected: Object::Int(Int { value: 20 }),
        },
        TestCase {
            input: "5 + 2 * 10",
            expected: Object::Int(Int { value: 25 }),
        },
        TestCase {
            input: "20 + 2 * -10",
            expected: Object::Int(Int { value: 0 }),
        },
        TestCase {
            input: "50 / 2 * 2 + 10",
            expected: Object::Int(Int { value: 60 }),
        },
        TestCase {
            input: "2 * (5 + 10)",
            expected: Object::Int(Int { value: 30 }),
        },
        TestCase {
            input: "3 * 3 * 3 + 10",
            expected: Object::Int(Int { value: 37 }),
        },
        TestCase {
            input: "3 * (3 * 3) + 10",
            expected: Object::Int(Int { value: 37 }),
        },
        TestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: Object::Int(Int { value: 50 }),
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
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "false",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "1 < 2",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "1 > 2",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "1 < 1",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "1 > 1",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "1 == 1",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "1 != 1",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "1 == 2",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "1 != 2",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "true == true",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "false == false",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "true == false",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "true != false",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "false != true",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "(1 < 2) == true",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "(1 < 2) == false",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "(1 > 2) == true",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "(1 > 2) == false",
            expected: Object::Bool(Bool { value: true }),
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
            expected: Object::Float(Float { value: -13.73 }),
        },
        TestCase {
            input: "-5.94 * -8.94",
            expected: Object::Float(Float { value: 53.1036 }),
        },
        TestCase {
            input: "-8.65 - 0.65",
            expected: Object::Float(Float { value: -9.3 }),
        },
        TestCase {
            input: "5.17 / -9.39",
            expected: Object::Float(Float {
                value: -0.550_585_729_499_467_5,
            }),
        },
        TestCase {
            input: "2.25 - -9.86 / -6.43",
            expected: Object::Float(Float {
                value: 0.716_562_986_003_110_4,
            }),
        },
        TestCase {
            input: "-1.33 * -8.74 + 9.55",
            expected: Object::Float(Float { value: 21.1742 }),
        },
        TestCase {
            input: "-9.07 + 5.55 - -8.99",
            expected: Object::Float(Float { value: 5.47 }),
        },
        TestCase {
            input: "-7.73 / -1.28 * 7.35",
            expected: Object::Float(Float {
                value: 44.387_109_374_999_994,
            }),
        },
        TestCase {
            input: "-6.24 * -5.34 / -2.9",
            expected: Object::Float(Float {
                value: -11.490_206_896_551_726,
            }),
        },
        TestCase {
            input: "1.4 / 6.1 - -4.25",
            expected: Object::Float(Float {
                value: 4.479_508_196_721_311,
            }),
        },
        TestCase {
            input: "2.31 / 0.79 * 7.31",
            expected: Object::Float(Float {
                value: 21.374_810_126_582_28,
            }),
        },
        TestCase {
            input: "-5.01 + -3.01 * 6.63",
            expected: Object::Float(Float {
                value: -24.966_299_999_999_997,
            }),
        },
        TestCase {
            input: "3.87 / -4.74 + 2.99",
            expected: Object::Float(Float {
                value: 2.173_544_303_797_468_7,
            }),
        },
        TestCase {
            input: "-6.52 + -10.37 / -6.36",
            expected: Object::Float(Float {
                value: -4.889_496_855_345_912,
            }),
        },
        TestCase {
            input: "8.2 - -1.6 * 8.86",
            expected: Object::Float(Float {
                value: 22.375_999_999_999_998,
            }),
        },
        TestCase {
            input: "10.24 + -8.92 + -5.85",
            expected: Object::Float(Float {
                value: -4.529_999_999_999_999,
            }),
        },
        TestCase {
            input: "-4.34 * 4.28 / -6.41",
            expected: Object::Float(Float {
                value: 2.897_847_113_884_555,
            }),
        },
        TestCase {
            input: "3.53 + -2.38 / 0.56",
            expected: Object::Float(Float {
                value: -0.719_999_999_999_999_3,
            }),
        },
        TestCase {
            input: "5.07 / -4.03 / -3.23",
            expected: Object::Float(Float {
                value: 0.389_493_658_244_282_45,
            }),
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
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "!false",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "!5",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "!!true",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "!!false",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "!!5",
            expected: Object::Bool(Bool { value: true }),
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
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "if (false) { 10 }",
            expected: Object::Null,
        },
        TestCase {
            input: "if (1) { 10 }",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "if (1 < 2) { 10 }",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "if (1 > 2) { 10 }",
            expected: Object::Null,
        },
        TestCase {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Object::Int(Int { value: 20 }),
        },
        TestCase {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Object::Int(Int { value: 10 }),
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
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "return 10; 9;",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "return 2 * 5; 9;",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "9; return 2 * 5; 9;",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
            expected: Object::Int(Int { value: 10 }),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(test_case.input);
        assert_eq!(evaluated, test_case.expected);
    }
}

struct ErrorHandlingTestCase {
    input: String,
    expected_message: String,
}

#[test]
fn test_error_handling() {
    let test_cases = [
        ErrorHandlingTestCase {
            input: "5 + true;".to_string(),
            expected_message: "type mismatch: INT + BOOL".to_string(),
        },
        ErrorHandlingTestCase {
            input: "5 + true; 5;".to_string(),
            expected_message: "type mismatch: INT + BOOL".to_string(),
        },
        ErrorHandlingTestCase {
            input: "-true".to_string(),
            expected_message: "unknown operator: -BOOL".to_string(),
        },
        ErrorHandlingTestCase {
            input: "true + false;".to_string(),
            expected_message: "unknown operator: BOOL + BOOL".to_string(),
        },
        ErrorHandlingTestCase {
            input: "5; true + false; 5".to_string(),
            expected_message: "unknown operator: BOOL + BOOL".to_string(),
        },
        ErrorHandlingTestCase {
            input: "if (10 > 1) { true + false; }".to_string(),
            expected_message: "unknown operator: BOOL + BOOL".to_string(),
        },
        ErrorHandlingTestCase {
            input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }".to_string(),
            expected_message: "unknown operator: BOOL + BOOL".to_string(),
        },
        ErrorHandlingTestCase {
            input: "foobar".to_string(),
            expected_message: "identifier not found: foobar".to_string(),
        },
        ErrorHandlingTestCase {
            input: r#""Hello" - "World""#.to_string(),
            expected_message: "unknown operator: STR - STR".to_string(),
        },
        ErrorHandlingTestCase {
            input: r#"{"name": "Panda"}[fn(x) { x }];"#.to_string(),
            expected_message: "unusable as hash key: FUNCTION".to_string(),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        assert_eq!(
            evaluated,
            Object::Error(Error {
                message: test_case.expected_message.clone()
            })
        );
    }
}

#[test]
fn test_declaration_statement() {
    let test_cases = [
        TestCase {
            input: "var a = 5; a",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "var a = 5 * 5; a",
            expected: Object::Int(Int { value: 25 }),
        },
        TestCase {
            input: "var a = 5; var b = a; b",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "var a = 5; var b = a; var c = a + b + 5; c",
            expected: Object::Int(Int { value: 15 }),
        },
        TestCase {
            input: "const a = 5; a",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "const a = 5 * 5; a",
            expected: Object::Int(Int { value: 25 }),
        },
        TestCase {
            input: "const a = 5; const b = a; b",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "const a = 5; var b = a; const c = a + b + 5; c",
            expected: Object::Int(Int { value: 15 }),
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
    assert_eq!(Object::Int(Int { value: 12 }), evaluated);
}

#[test]
fn test_function_application() {
    let test_cases = [
        TestCase {
            input: "var identity = fn(x) { x }; identity(5)",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "var identity = fn(x) { return x; }; identity(5)",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "var double = fn(x) { x * 2 }; double(5)",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "var add = fn(x, y) { x + y }; add(5, 5)",
            expected: Object::Int(Int { value: 10 }),
        },
        TestCase {
            input: "var add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))",
            expected: Object::Int(Int { value: 20 }),
        },
        TestCase {
            input: "fn(x) { x }(5)",
            expected: Object::Int(Int { value: 5 }),
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
    assert_eq!(evaluated, Object::Int(Int { value: 4 }));
}

#[test]
fn test_string_literal() {
    let input = r#""Hello World!""#;
    let evaluated = test_eval(input);
    assert_eq!(
        evaluated,
        Object::Str(Str {
            value: "Hello World!".to_string()
        })
    );
}

#[test]
fn test_char_literal() {
    let input = "'a'";
    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Char(Char { value: 'a' }));
}

#[test]
fn test_null_literal() {
    let input = "null";
    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Null);
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;

    let evaluated = test_eval(input);
    assert_eq!(
        evaluated,
        Object::Str(Str {
            value: "Hello World!".to_string()
        })
    );
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);
    assert_eq!(
        evaluated,
        Object::Array(Array {
            elements: Vec::from([
                Object::Int(Int { value: 1 }),
                Object::Int(Int { value: 4 }),
                Object::Int(Int { value: 6 })
            ])
        })
    );
}

#[test]
fn test_array_index_expressions() {
    let test_cases = [
        TestCase {
            input: "[1, 2, 3][0]",
            expected: Object::Int(Int { value: 1 }),
        },
        TestCase {
            input: "[1, 2, 3][1]",
            expected: Object::Int(Int { value: 2 }),
        },
        TestCase {
            input: "[1, 2, 3][2]",
            expected: Object::Int(Int { value: 3 }),
        },
        TestCase {
            input: "var i = 0; [1][i]",
            expected: Object::Int(Int { value: 1 }),
        },
        TestCase {
            input: "[1, 2, 3][1 + 1]",
            expected: Object::Int(Int { value: 3 }),
        },
        TestCase {
            input: "var myArray = [1, 2, 3]; myArray[2]",
            expected: Object::Int(Int { value: 3 }),
        },
        TestCase {
            input: "var myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
            expected: Object::Int(Int { value: 6 }),
        },
        TestCase {
            input: "var myArray = [1, 2, 3]; var i = myArray[0]; myArray[i]",
            expected: Object::Int(Int { value: 2 }),
        },
        TestCase {
            input: "[1, 2, 3][3]",
            expected: Object::Null,
        },
        TestCase {
            input: "[1, 2, 3][-1]",
            expected: Object::Null,
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
    let key1 = Hashable::Str(Str {
        value: "one".to_string(),
    });
    let key2 = Hashable::Str(Str {
        value: "two".to_string(),
    });
    let key3 = Hashable::Str(Str {
        value: "three".to_string(),
    });
    let key4 = Hashable::Int(Int { value: 4 });
    let key5 = Hashable::from_object(&TRUE).unwrap();
    let key6 = Hashable::from_object(&FALSE).unwrap();
    assert_eq!(
        evaluated,
        Object::Dict(Dict {
            pairs: HashMap::from([
                (
                    key1.hash(),
                    DictPair {
                        key: key1,
                        value: Object::Int(Int { value: 1 })
                    }
                ),
                (
                    key2.hash(),
                    DictPair {
                        key: key2,
                        value: Object::Int(Int { value: 2 })
                    }
                ),
                (
                    key3.hash(),
                    DictPair {
                        key: key3,
                        value: Object::Int(Int { value: 3 })
                    }
                ),
                (
                    key4.hash(),
                    DictPair {
                        key: key4,
                        value: Object::Int(Int { value: 4 })
                    }
                ),
                (
                    key5.hash(),
                    DictPair {
                        key: key5,
                        value: Object::Int(Int { value: 5 })
                    }
                ),
                (
                    key6.hash(),
                    DictPair {
                        key: key6,
                        value: Object::Int(Int { value: 6 })
                    }
                ),
            ])
        })
    );
}

#[test]
fn test_dict_index_expressions() {
    let test_cases = [
        TestCase {
            input: r#"{"foo": 5}["foo"]"#,
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: r#"{"foo": 5}["bar"]"#,
            expected: Object::Null,
        },
        TestCase {
            input: r#"var key = "foo"; {"foo": 5}[key]"#,
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: r#"{}["foo"]"#,
            expected: Object::Null,
        },
        TestCase {
            input: "{5: 5}[5]",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "{true: 5}[true]",
            expected: Object::Int(Int { value: 5 }),
        },
        TestCase {
            input: "{false: 5}[false]",
            expected: Object::Int(Int { value: 5 }),
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
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "(-3).abs()",
            expected: Object::Int(Int { value: 3 }),
        },
        TestCase {
            input: "{1:1, 2:2, 3:3}.len()",
            expected: Object::Int(Int { value: 3 }),
        },
        TestCase {
            input: "var isDigit = 'a'.isDigit; isDigit(16)",
            expected: Object::Bool(Bool { value: true }),
        },
        TestCase {
            input: "var isDigit = 'g'.isDigit; isDigit(16)",
            expected: Object::Bool(Bool { value: false }),
        },
        TestCase {
            input: "var c = 'g'; c.isDigit(16)",
            expected: Object::Bool(Bool { value: false }),
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
    assert_eq!(evaluated, Object::Int(Int { value: 10 }));
}

#[test]
fn test_delete_statement() {
    let input = "var i = 10;
delete i";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Int(Int { value: 10 }));
}

#[test]
fn test_for_statement() {
    let input = "var i = 0;
for (j in 0..10) {
    i = i + j;
}

i";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Int(Int { value: 45 }));
}

fn test_eval(input: &str) -> Object {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);

    let program = parser.parse_program().unwrap();

    let mut env = Environment::new();

    eval(program, &mut env).unwrap()
}
