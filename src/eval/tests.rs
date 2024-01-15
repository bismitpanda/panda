use std::any::Any;

use hashbrown::HashMap;
use pretty_assertions::assert_eq;

use super::*;
use crate::{
    ast::{ExpressionStmt, Infix, Lit, Span},
    lexer::Lexer,
    parser::Parser,
    token::Position,
};

struct EvalIntegerTestCase {
    input: String,
    expected: isize,
}

#[test]
fn test_eval_integer_expression() {
    let test_cases = [
        EvalIntegerTestCase {
            input: "5".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "10".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "-5".to_string(),
            expected: -5,
        },
        EvalIntegerTestCase {
            input: "-10".to_string(),
            expected: -10,
        },
        EvalIntegerTestCase {
            input: "5 + 5 + 5 + 5 - 10".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "2 * 2 * 2 * 2 * 2".to_string(),
            expected: 32,
        },
        EvalIntegerTestCase {
            input: "-50 + 100 + -50".to_string(),
            expected: 0,
        },
        EvalIntegerTestCase {
            input: "5 * 2 + 10".to_string(),
            expected: 20,
        },
        EvalIntegerTestCase {
            input: "5 + 2 * 10".to_string(),
            expected: 25,
        },
        EvalIntegerTestCase {
            input: "20 + 2 * -10".to_string(),
            expected: 0,
        },
        EvalIntegerTestCase {
            input: "50 / 2 * 2 + 10".to_string(),
            expected: 60,
        },
        EvalIntegerTestCase {
            input: "2 * (5 + 10)".to_string(),
            expected: 30,
        },
        EvalIntegerTestCase {
            input: "3 * 3 * 3 + 10".to_string(),
            expected: 37,
        },
        EvalIntegerTestCase {
            input: "3 * (3 * 3) + 10".to_string(),
            expected: 37,
        },
        EvalIntegerTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
            expected: 50,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_integer_object(evaluated, test_case.expected);
    }
}

struct EvalBooleanTestCase {
    input: String,
    expected: bool,
}

#[test]
fn test_eval_boolean_expression() {
    let test_cases = [
        EvalBooleanTestCase {
            input: "true".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "false".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "1 < 2".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "1 > 2".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "1 < 1".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "1 > 1".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "1 == 1".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "1 != 1".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "1 == 2".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "1 != 2".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "true == true".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "false == false".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "true == false".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "true != false".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "false != true".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "(1 < 2) == true".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "(1 < 2) == false".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "(1 > 2) == true".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "(1 > 2) == false".to_string(),
            expected: true,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_boolean_object(evaluated, test_case.expected);
    }
}

struct EvalFloatTestCase {
    input: String,
    expected: f64,
}

#[test]
fn test_eval_float_expression() {
    let test_cases = [
        EvalFloatTestCase {
            input: "-7.84 + -5.89".to_string(),
            expected: -13.73,
        },
        EvalFloatTestCase {
            input: "-5.94 * -8.94".to_string(),
            expected: 53.1036,
        },
        EvalFloatTestCase {
            input: "-8.65 - 0.65".to_string(),
            expected: -9.3,
        },
        EvalFloatTestCase {
            input: "5.17 / -9.39".to_string(),
            expected: -0.550_585_729_499_467_5,
        },
        EvalFloatTestCase {
            input: "2.25 - -9.86 / -6.43".to_string(),
            expected: 0.716_562_986_003_110_4,
        },
        EvalFloatTestCase {
            input: "-1.33 * -8.74 + 9.55".to_string(),
            expected: 21.1742,
        },
        EvalFloatTestCase {
            input: "-9.07 + 5.55 - -8.99".to_string(),
            expected: 5.47,
        },
        EvalFloatTestCase {
            input: "-7.73 / -1.28 * 7.35".to_string(),
            expected: 44.387_109_374_999_994,
        },
        EvalFloatTestCase {
            input: "-6.24 * -5.34 / -2.9".to_string(),
            expected: -11.490_206_896_551_726,
        },
        EvalFloatTestCase {
            input: "1.4 / 6.1 - -4.25".to_string(),
            expected: 4.479_508_196_721_311,
        },
        EvalFloatTestCase {
            input: "2.31 / 0.79 * 7.31".to_string(),
            expected: 21.374_810_126_582_28,
        },
        EvalFloatTestCase {
            input: "-5.01 + -3.01 * 6.63".to_string(),
            expected: -24.966_299_999_999_997,
        },
        EvalFloatTestCase {
            input: "3.87 / -4.74 + 2.99".to_string(),
            expected: 2.173_544_303_797_468_7,
        },
        EvalFloatTestCase {
            input: "-6.52 + -10.37 / -6.36".to_string(),
            expected: -4.889_496_855_345_912,
        },
        EvalFloatTestCase {
            input: "8.2 - -1.6 * 8.86".to_string(),
            expected: 22.375_999_999_999_998,
        },
        EvalFloatTestCase {
            input: "10.24 + -8.92 + -5.85".to_string(),
            expected: -4.529_999_999_999_999,
        },
        EvalFloatTestCase {
            input: "-4.34 * 4.28 / -6.41".to_string(),
            expected: 2.897_847_113_884_555,
        },
        EvalFloatTestCase {
            input: "3.53 + -2.38 / 0.56".to_string(),
            expected: -0.719_999_999_999_999_3,
        },
        EvalFloatTestCase {
            input: "5.07 / -4.03 / -3.23".to_string(),
            expected: 0.389_493_658_244_282_45,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_float_object(evaluated, test_case.expected);
    }
}

#[test]
fn test_bang_operator() {
    let test_cases = [
        EvalBooleanTestCase {
            input: "!true".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "!false".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "!5".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "!!true".to_string(),
            expected: true,
        },
        EvalBooleanTestCase {
            input: "!!false".to_string(),
            expected: false,
        },
        EvalBooleanTestCase {
            input: "!!5".to_string(),
            expected: true,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_boolean_object(evaluated, test_case.expected);
    }
}

struct IfElseExpressionTestCase {
    input: String,
    expected: Option<isize>,
}

#[test]
fn test_if_else_expression() {
    let test_cases = [
        IfElseExpressionTestCase {
            input: "if (true) { 10 }".to_string(),
            expected: Some(10),
        },
        IfElseExpressionTestCase {
            input: "if (false) { 10 }".to_string(),
            expected: None,
        },
        IfElseExpressionTestCase {
            input: "if (1) { 10 }".to_string(),
            expected: Some(10),
        },
        IfElseExpressionTestCase {
            input: "if (1 < 2) { 10 }".to_string(),
            expected: Some(10),
        },
        IfElseExpressionTestCase {
            input: "if (1 > 2) { 10 }".to_string(),
            expected: None,
        },
        IfElseExpressionTestCase {
            input: "if (1 > 2) { 10 } else { 20 }".to_string(),
            expected: Some(20),
        },
        IfElseExpressionTestCase {
            input: "if (1 < 2) { 10 } else { 20 }".to_string(),
            expected: Some(10),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        if let Some(integer) = test_case.expected {
            test_integer_object(evaluated, integer);
        } else {
            test_null_object(evaluated);
        }
    }
}
#[test]
fn test_return_statements() {
    let test_cases = [
        EvalIntegerTestCase {
            input: "return 10;".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "return 10; 9;".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "return 2 * 5; 9;".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "9; return 2 * 5; 9;".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "if (10 > 1) { if (10 > 1) { return 10; } return 1; }".to_string(),
            expected: 10,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_integer_object(evaluated, test_case.expected);
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
        EvalIntegerTestCase {
            input: "var a = 5; a".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "var a = 5 * 5; a".to_string(),
            expected: 25,
        },
        EvalIntegerTestCase {
            input: "var a = 5; var b = a; b".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "var a = 5; var b = a; var c = a + b + 5; c".to_string(),
            expected: 15,
        },
        EvalIntegerTestCase {
            input: "const a = 5; a".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "const a = 5 * 5; a".to_string(),
            expected: 25,
        },
        EvalIntegerTestCase {
            input: "const a = 5; const b = a; b".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "const a = 5; var b = a; const c = a + b + 5; c".to_string(),
            expected: 15,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_integer_object(evaluated, test_case.expected);
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
                span: Span {
                    start: Position::new(0, 9),
                    end: Position::new(0, 14)
                },
                returns: true,
                expression: Expression::Infix(Infix {
                    span: Span {
                        start: Position::new(0, 9),
                        end: Position::new(0, 14)
                    },
                    left: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 9),
                            end: Position::new(0, 10)
                        },
                        value: "x".to_string()
                    })),
                    operator: Operator::Add,
                    right: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 13),
                            end: Position::new(0, 14)
                        },
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
        EvalIntegerTestCase {
            input: "var identity = fn(x) { x }; identity(5)".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "var identity = fn(x) { return x; }; identity(5)".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "var double = fn(x) { x * 2 }; double(5)".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "var add = fn(x, y) { x + y }; add(5, 5)".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "var add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))".to_string(),
            expected: 20,
        },
        EvalIntegerTestCase {
            input: "fn(x) { x }(5)".to_string(),
            expected: 5,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_integer_object(evaluated, test_case.expected);
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
    test_integer_object(evaluated, 4);
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

struct IndexExpressionTestCase {
    input: String,
    expected: Option<isize>,
}

#[test]
fn test_array_index_expressions() {
    let test_cases = [
        IndexExpressionTestCase {
            input: "[1, 2, 3][0]".to_string(),
            expected: Some(1),
        },
        IndexExpressionTestCase {
            input: "[1, 2, 3][1]".to_string(),
            expected: Some(2),
        },
        IndexExpressionTestCase {
            input: "[1, 2, 3][2]".to_string(),
            expected: Some(3),
        },
        IndexExpressionTestCase {
            input: "var i = 0; [1][i]".to_string(),
            expected: Some(1),
        },
        IndexExpressionTestCase {
            input: "[1, 2, 3][1 + 1]".to_string(),
            expected: Some(3),
        },
        IndexExpressionTestCase {
            input: "var myArray = [1, 2, 3]; myArray[2]".to_string(),
            expected: Some(3),
        },
        IndexExpressionTestCase {
            input: "var myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]".to_string(),
            expected: Some(6),
        },
        IndexExpressionTestCase {
            input: "var myArray = [1, 2, 3]; var i = myArray[0]; myArray[i]".to_string(),
            expected: Some(2),
        },
        IndexExpressionTestCase {
            input: "[1, 2, 3][3]".to_string(),
            expected: None,
        },
        IndexExpressionTestCase {
            input: "[1, 2, 3][-1]".to_string(),
            expected: None,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);

        match test_case.expected {
            Some(i) => test_integer_object(evaluated, i),
            None => test_null_object(evaluated),
        }
    }
}

#[test]
fn test_hash_literals() {
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
                    key1.hash_key(),
                    HashPair {
                        key: key1,
                        value: Object::Int(Int { value: 1 })
                    }
                ),
                (
                    key2.hash_key(),
                    HashPair {
                        key: key2,
                        value: Object::Int(Int { value: 2 })
                    }
                ),
                (
                    key3.hash_key(),
                    HashPair {
                        key: key3,
                        value: Object::Int(Int { value: 3 })
                    }
                ),
                (
                    key4.hash_key(),
                    HashPair {
                        key: key4,
                        value: Object::Int(Int { value: 4 })
                    }
                ),
                (
                    key5.hash_key(),
                    HashPair {
                        key: key5,
                        value: Object::Int(Int { value: 5 })
                    }
                ),
                (
                    key6.hash_key(),
                    HashPair {
                        key: key6,
                        value: Object::Int(Int { value: 6 })
                    }
                ),
            ])
        })
    );
}

#[test]
fn test_hash_index_expressions() {
    let test_cases = [
        IndexExpressionTestCase {
            input: r#"{"foo": 5}["foo"]"#.to_string(),
            expected: Some(5),
        },
        IndexExpressionTestCase {
            input: r#"{"foo": 5}["bar"]"#.to_string(),
            expected: None,
        },
        IndexExpressionTestCase {
            input: r#"var key = "foo"; {"foo": 5}[key]"#.to_string(),
            expected: Some(5),
        },
        IndexExpressionTestCase {
            input: r#"{}["foo"]"#.to_string(),
            expected: None,
        },
        IndexExpressionTestCase {
            input: "{5: 5}[5]".to_string(),
            expected: Some(5),
        },
        IndexExpressionTestCase {
            input: "{true: 5}[true]".to_string(),
            expected: Some(5),
        },
        IndexExpressionTestCase {
            input: "{false: 5}[false]".to_string(),
            expected: Some(5),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);

        if let Some(i) = test_case.expected {
            test_integer_object(evaluated, i);
        } else {
            test_null_object(evaluated);
        }
    }
}

struct BuiltinTestCase {
    input: String,
    expected: Box<dyn Any>,
}

#[test]
fn test_builtin_methods() {
    let test_cases = [
        BuiltinTestCase {
            input: "[2, 3].contains(2)".to_string(),
            expected: Box::new(true),
        },
        BuiltinTestCase {
            input: "(-3).abs()".to_string(),
            expected: Box::new(3),
        },
        BuiltinTestCase {
            input: "{1:1, 2:2, 3:3}.len()".to_string(),
            expected: Box::new(3),
        },
        BuiltinTestCase {
            input: "var isDigit = 'a'.isDigit; isDigit(16)".to_string(),
            expected: Box::new(true),
        },
        BuiltinTestCase {
            input: "var isDigit = 'g'.isDigit; isDigit(16)".to_string(),
            expected: Box::new(false),
        },
        BuiltinTestCase {
            input: "var c = 'g'; c.isDigit(16)".to_string(),
            expected: Box::new(false),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);

        let expected = &*test_case.expected;

        if let Some(expected) = expected.downcast_ref::<i32>() {
            test_integer_object(evaluated, *expected as isize);
        } else if let Some(expected) = expected.downcast_ref::<bool>() {
            test_boolean_object(evaluated, *expected);
        }
    }
}

#[test]
fn test_class_method() {
    let input = "class a { fn b() { 10 } };
var c = new a();
c.b()";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Int(Int { value: 10 }));
}

#[test]
fn test_delete_statement() {
    let input = "
var i = 10;
delete i";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Int(Int { value: 10 }));
}

fn test_eval(input: &str) -> Object {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);

    let program = parser.parse_program().unwrap();

    let mut env = Environment::new();

    eval(program, &mut env).unwrap()
}

fn test_integer_object(obj: Object, expected: isize) {
    assert_eq!(obj, Object::Int(Int { value: expected }));
}

fn test_boolean_object(obj: Object, expected: bool) {
    assert_eq!(obj, Object::Bool(Bool { value: expected }));
}

fn test_null_object(obj: Object) {
    assert_eq!(obj, Object::Null);
}

fn test_float_object(obj: Object, expected: f64) {
    assert_eq!(obj, Object::Float(Float { value: expected }));
}
