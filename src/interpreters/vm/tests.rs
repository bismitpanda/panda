use pretty_assertions::assert_eq;

use super::VM;
use crate::{ast::Node, compiler::Compiler, lexer::Lexer, object::Object, parser::Parser};

struct TestCase {
    input: &'static str,
    expected: Object,
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
    let test_cases = [
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

fn parse(input: &str) -> Option<Node> {
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    p.parse_program()
}
