use crate::{
    ast::{ExpressionStmtAst, IdentifierAst, InfixAst, LiteralAst, Span},
    lexer::Lexer,
    parser::Parser,
    token::Position,
};
use hashbrown::HashMap;
use pretty_assertions::assert_eq;
use std::any::Any;

use super::*;

struct EvalIntegerTestCase {
    input: String,
    expected: isize,
}

struct EvalBooleanTestCase {
    input: String,
    expected: bool,
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
        test_boolean_object(evaluated, test_case.expected)
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
            test_integer_object(evaluated, integer)
        } else {
            test_null_object(evaluated)
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
        test_integer_object(evaluated, test_case.expected)
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
            Object::Error(ErrorObject {
                message: test_case.expected_message.clone()
            })
        );
    }
}

#[test]
fn test_declaration_statement() {
    let test_cases = [
        EvalIntegerTestCase {
            input: "let a = 5; a".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "let a = 5 * 5; a".to_string(),
            expected: 25,
        },
        EvalIntegerTestCase {
            input: "let a = 5; let b = a; b".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "let a = 5; let b = a; let c = a + b + 5; c".to_string(),
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
            input: "const a = 5; let b = a; const c = a + b + 5; c".to_string(),
            expected: 15,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_integer_object(evaluated, test_case.expected)
    }
}

#[test]
fn test_lambda_object() {
    let input = "fn(x) { x + 2 }";

    let evaluated = test_eval(input);
    assert_eq!(
        Object::EvaluatedFunction(EvaluatedFunctionObject {
            parameters: Vec::from(["x".to_string()]),
            body: Vec::from([Statement::ExpressionStmt(ExpressionStmtAst {
                span: Span {
                    start: Position::new(0, 9),
                    end: Position::new(0, 14)
                },
                returns: true,
                expression: Expression::Infix(InfixAst {
                    span: Span {
                        start: Position::new(0, 9),
                        end: Position::new(0, 14)
                    },
                    left: Box::new(Expression::Identifier(IdentifierAst {
                        span: Span {
                            start: Position::new(0, 9),
                            end: Position::new(0, 10)
                        },
                        value: "x".to_string()
                    })),
                    operator: Operator::Add,
                    right: Box::new(Expression::Literal(LiteralAst {
                        span: Span {
                            start: Position::new(0, 13),
                            end: Position::new(0, 14)
                        },
                        lit: Literal::Int { value: 2.into() }
                    }))
                })
            })]),
            env: Environment {
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
fn test_function_application() {
    let test_cases = [
        EvalIntegerTestCase {
            input: "let identity = fn(x) { x }; identity(5)".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "let identity = fn(x) { return x; }; identity(5)".to_string(),
            expected: 5,
        },
        EvalIntegerTestCase {
            input: "let double = fn(x) { x * 2 }; double(5)".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "let add = fn(x, y) { x + y }; add(5, 5)".to_string(),
            expected: 10,
        },
        EvalIntegerTestCase {
            input: "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))".to_string(),
            expected: 20,
        },
        EvalIntegerTestCase {
            input: "fn(x) { x }(5)".to_string(),
            expected: 5,
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);
        test_integer_object(evaluated, test_case.expected)
    }
}

#[test]
fn test_closures() {
    let input = "
let newAdder = fn(x) {
    fn(y) {
        x + y
    }
};

let addTwo = newAdder(2);
addTwo(2)";

    let evaluated = test_eval(input);
    test_integer_object(evaluated, 4)
}

#[test]
fn test_string_literal() {
    let input = r#""Hello World!""#;
    let evaluated = test_eval(input);
    assert_eq!(
        evaluated,
        Object::Str(StrObject {
            value: "Hello World!".to_string()
        })
    )
}

#[test]
fn test_char_literal() {
    let input = "'a'";
    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Char(CharObject { value: 'a' }))
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;

    let evaluated = test_eval(input);
    assert_eq!(
        evaluated,
        Object::Str(StrObject {
            value: "Hello World!".to_string()
        })
    )
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);
    assert_eq!(
        evaluated,
        Object::Array(ArrayObject {
            elements: Vec::from([
                Object::Int(IntObject { value: 1.into() }),
                Object::Int(IntObject { value: 4.into() }),
                Object::Int(IntObject { value: 6.into() })
            ])
        })
    )
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
            input: "let i = 0; [1][i]".to_string(),
            expected: Some(1),
        },
        IndexExpressionTestCase {
            input: "[1, 2, 3][1 + 1]".to_string(),
            expected: Some(3),
        },
        IndexExpressionTestCase {
            input: "let myArray = [1, 2, 3]; myArray[2]".to_string(),
            expected: Some(3),
        },
        IndexExpressionTestCase {
            input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]".to_string(),
            expected: Some(6),
        },
        IndexExpressionTestCase {
            input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]".to_string(),
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
    let input = r#"let two = "two";
{
    "one": 10 - 9,
    two: 1 + 1,
    "thr" + "ee": 6 / 2,
    4: 4,
    true: 5,
    false: 6
}"#;
    let evaluated = test_eval(input);
    let key1 = Hashable::Str(StrObject {
        value: "one".to_string(),
    });
    let key2 = Hashable::Str(StrObject {
        value: "two".to_string(),
    });
    let key3 = Hashable::Str(StrObject {
        value: "three".to_string(),
    });
    let key4 = Hashable::Int(IntObject { value: 4.into() });
    let key5 = Hashable::from_object(&TRUE).unwrap();
    let key6 = Hashable::from_object(&FALSE).unwrap();
    assert_eq!(
        evaluated,
        Object::Hash(HashObject {
            pairs: HashMap::from([
                (
                    key1.hash_key(),
                    HashPair {
                        key: key1,
                        value: Object::Int(IntObject { value: 1.into() })
                    }
                ),
                (
                    key2.hash_key(),
                    HashPair {
                        key: key2,
                        value: Object::Int(IntObject { value: 2.into() })
                    }
                ),
                (
                    key3.hash_key(),
                    HashPair {
                        key: key3,
                        value: Object::Int(IntObject { value: 3.into() })
                    }
                ),
                (
                    key4.hash_key(),
                    HashPair {
                        key: key4,
                        value: Object::Int(IntObject { value: 4.into() })
                    }
                ),
                (
                    key5.hash_key(),
                    HashPair {
                        key: key5,
                        value: Object::Int(IntObject { value: 5.into() })
                    }
                ),
                (
                    key6.hash_key(),
                    HashPair {
                        key: key6,
                        value: Object::Int(IntObject { value: 6.into() })
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
            input: r#"let key = "foo"; {"foo": 5}[key]"#.to_string(),
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
            test_integer_object(evaluated, i)
        } else {
            test_null_object(evaluated)
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
            input: "let isDigit = 'a'.isDigit; isDigit(16)".to_string(),
            expected: Box::new(true),
        },
        BuiltinTestCase {
            input: "let isDigit = 'g'.isDigit; isDigit(16)".to_string(),
            expected: Box::new(false),
        },
        BuiltinTestCase {
            input: "let c = 'g'; c.isDigit(16)".to_string(),
            expected: Box::new(false),
        },
    ];

    for test_case in test_cases {
        let evaluated = test_eval(&test_case.input);

        let expected = &*test_case.expected;

        if let Some(expected) = expected.downcast_ref::<i32>() {
            test_integer_object(evaluated, *expected as isize)
        } else if let Some(expected) = expected.downcast_ref::<bool>() {
            test_boolean_object(evaluated, *expected)
        }
    }
}

#[test]
fn test_class_method() {
    let input = "class a { fn b() { 10 } };
let c = new a();
c.b()";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Int(IntObject { value: 10.into() }));
}

#[test]
fn test_delete_statement() {
    let input = "
let i = 10;
delete i";

    let evaluated = test_eval(input);
    assert_eq!(evaluated, Object::Int(IntObject { value: 10.into() }));
}

fn test_eval(input: &str) -> Object {
    let mut lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(&mut lexer);

    let program = parser.parse_program().unwrap();

    let mut env = Environment::new();

    eval(program, &mut env).unwrap()
}

fn test_integer_object(obj: Object, expected: isize) {
    assert_eq!(
        obj,
        Object::Int(IntObject {
            value: expected.into()
        })
    );
}

fn test_boolean_object(obj: Object, expected: bool) {
    assert_eq!(obj, Object::Bool(BoolObject { value: expected }));
}

fn test_null_object(obj: Object) {
    assert_eq!(obj, Object::Null);
}
