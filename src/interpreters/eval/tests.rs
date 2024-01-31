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
fn test_class_method() {
    run_interpreter_tests(&[TestCase {
        input: "class a { b() { 10 } };
        var c = new a();
        c.b()",
        expected: Object::int(10),
    }]);
}

#[test]
fn test_lambda_object() {
    run_interpreter_tests(&[TestCase {
        input: "fn(x) { x + 2 }",
        expected: Object::EvaluatedFunction(EvaluatedFunction {
            parameters: Vec::from(["x".to_string()]),
            body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Infix(Infix {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "x".to_string(),
                    })),
                    operator: Operator::Add,
                    right: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int { value: 2 },
                    })),
                }),
            })]),
            environment: Environment {
                store: HashMap::new(),
                outer: None,
                types: HashMap::new(),
                imports: HashMap::new(),
            },
        }),
    }]);
}

#[test]
fn test_error_handling() {
    run_interpreter_tests(&[
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
        TestCase {
            input: r#"{"name": "Panda"}["foo"];"#,
            expected: Object::error(r#"key error. got: "foo""#.to_string()),
        },
    ]);
}

fn parse(input: &str) -> Node {
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    p.parse_program().unwrap()
}

fn run_evaluator_tests(test_cases: &[TestCase]) {
    for test_case in test_cases {
        let program = parse(test_case.input);

        let mut env = Environment::new();

        let evaluated = eval(program, &mut env).unwrap();

        assert_eq!(test_case.expected, evaluated);
    }
}
