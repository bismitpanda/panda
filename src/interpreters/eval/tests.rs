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
fn test_error_handling() {
    run_interpreter_tests(&[]);
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
