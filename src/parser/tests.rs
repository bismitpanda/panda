use pretty_assertions::assert_eq;

use super::*;
use crate::ast::*;

#[test]
fn test_declaration_statement() {
    let input = "
var x = 5;
var foobar = 838383;
const y = 10;
const barbaz = 121212;
";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    let test_cases = [
        ("x", true, 5),
        ("foobar", true, 838_383),
        ("y", false, 10),
        ("barbaz", false, 121_212),
    ];

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 4);
        for (i, &(name, mutable, value)) in test_cases.iter().enumerate() {
            assert_eq!(
                Statement::Declaration(Declaration {
                    name: name.to_string(),
                    mutable,
                    value: Some(Expression::Literal(Literal {
                        lit: Lit::Int { value }
                    })),
                }),
                statements[i]
            );
        }
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_return_statement() {
    let input = "
return 5;
return 10;
return 838383;
";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    let test_cases = [5, 10, 838_383];

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 3);

        for (i, &test_case) in test_cases.iter().enumerate() {
            assert_eq!(
                Statement::Return(Return {
                    return_value: Expression::Literal(Literal {
                        lit: Lit::Int { value: test_case }
                    }),
                }),
                statements[i]
            );
        }
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_function_statement() {
    let input = "fn add() { x + y }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Function(Function {
                ident: "add".to_string(),
                parameters: Vec::new(),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: Expression::Infix(Infix {
                        left: Box::new(Expression::Identifier(Identifier {
                            value: "x".to_string(),
                        })),
                        operator: Operator::Add,
                        right: Box::new(Expression::Identifier(Identifier {
                            value: "y".to_string(),
                        })),
                    }),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_while_statement() {
    let input = "while (i < n) { i = i + 1 }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::While(While {
                condition: Expression::Infix(Infix {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "i".to_string(),
                    })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier {
                        value: "n".to_string(),
                    })),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: Expression::Assign(Assign {
                        to: Assignable::Identifier(Identifier {
                            value: "i".to_string(),
                        }),
                        value: Box::new(Expression::Infix(Infix {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "i".to_string(),
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 1 },
                            })),
                        })),
                    }),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_while_with_break_statement() {
    let input = "while (i < n) { if (i == 3) { break; }}";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::While(While {
                condition: Expression::Infix(Infix {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "i".to_string(),
                    })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier {
                        value: "n".to_string(),
                    })),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: Expression::If(If {
                        condition: Box::new(Expression::Infix(Infix {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 3 }
                            }))
                        })),
                        consequence: Vec::from([Statement::Break]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_while_with_continue_statement() {
    let input = "while (i < n) { if (i == 3) { continue; }}";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::While(While {
                condition: Expression::Infix(Infix {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "i".to_string(),
                    })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier {
                        value: "n".to_string(),
                    })),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: Expression::If(If {
                        condition: Box::new(Expression::Infix(Infix {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 3 }
                            }))
                        })),
                        consequence: Vec::from([Statement::Continue]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_for_statement() {
    let input = "for (i in arr) { i = i + 1 }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::For(For {
                ident: "i".to_string(),
                iterator: Expression::Identifier(Identifier {
                    value: "arr".to_string(),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: Expression::Assign(Assign {
                        to: Assignable::Identifier(Identifier {
                            value: "i".to_string(),
                        }),
                        value: Box::new(Expression::Infix(Infix {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "i".to_string(),
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 1 },
                            })),
                        })),
                    }),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_for_with_break_statement() {
    let input = "for (i in arr) { if (i == 3) { break; } }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::For(For {
                ident: "i".to_string(),
                iterator: Expression::Identifier(Identifier {
                    value: "arr".to_string(),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: Expression::If(If {
                        condition: Box::new(Expression::Infix(Infix {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 3 }
                            }))
                        })),
                        consequence: Vec::from([Statement::Break]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_for_with_continue_statement() {
    let input = "for (i in arr) { if (i == 3) { continue; } }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::For(For {
                ident: "i".to_string(),
                iterator: Expression::Identifier(Identifier {
                    value: "arr".to_string(),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: Expression::If(If {
                        condition: Box::new(Expression::Infix(Infix {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 3 }
                            }))
                        })),
                        consequence: Vec::from([Statement::Continue]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_class_statement() {
    let input = "class MyClass(i1, i2) { a = 32; }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ClassDecl(ClassDecl {
                ident: "MyClass".to_string(),
                initializers: Vec::from(["i1".to_string(), "i2".to_string()]),
                body: Vec::from([ClassStatement::Variable(ClassVariable {
                    name: "a".to_string(),
                    value: Some(Expression::Literal(Literal {
                        lit: Lit::Int { value: 32 },
                    })),
                })]),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

struct ImportStatementTestCase {
    input: &'static str,
    expected: Statement,
}

#[test]
fn test_import_statement() {
    let test_cases = [
        ImportStatementTestCase {
            input: r#"import "fs""#,
            expected: Statement::Import(Import {
                path: "fs".to_string(),
                alias: None,
            }),
        },
        ImportStatementTestCase {
            input: r#"import "std/datetime/duration""#,
            expected: Statement::Import(Import {
                path: "std/datetime/duration".to_string(),
                alias: None,
            }),
        },
        ImportStatementTestCase {
            input: r#"import "std/datetime/duration" as duration"#,
            expected: Statement::Import(Import {
                path: "std/datetime/duration".to_string(),
                alias: Some("duration".to_string()),
            }),
        },
    ];

    for test_case in test_cases {
        let mut l = Lexer::new(test_case.input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program();

        check_parser_errors(p);

        if let Some(Node::Program { statements, .. }) = program {
            assert_eq!(statements.len(), 1);
            assert_eq!(test_case.expected, statements[0]);
        } else {
            panic!("p.parse_program() returned None")
        }
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foo_bar123";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Identifier(Identifier {
                    value: "foo_bar123".to_string()
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_boolean_literal() {
    let input = "true";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Bool { value: true }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_integer_literal() {
    let input = 5.to_string();

    let mut l = Lexer::new(&input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Int { value: 5 }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_float_literal() {
    let input = 5.103.to_string();

    let mut l = Lexer::new(&input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Float { value: 5.103 }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

struct PrefixExpressionsTestCase {
    input: &'static str,
    expected: Statement,
}

#[test]
fn test_parsing_prefix_expressions() {
    let test_cases = [
        PrefixExpressionsTestCase {
            input: "!5",
            expected: Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Prefix(Prefix {
                    operator: Operator::Bang,
                    right: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int { value: 5 },
                    })),
                }),
            }),
        },
        PrefixExpressionsTestCase {
            input: "-15",
            expected: Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Prefix(Prefix {
                    operator: Operator::Sub,
                    right: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int { value: 15 },
                    })),
                }),
            }),
        },
    ];

    for test_case in test_cases {
        let mut l = Lexer::new(test_case.input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program();

        check_parser_errors(p);

        if let Some(Node::Program { statements, .. }) = program {
            assert_eq!(statements.len(), 1);
            assert_eq!(test_case.expected, statements[0]);
        } else {
            panic!("p.parse_program() returned None")
        }
    }
}

struct InfixExpressionsTestCase {
    input: &'static str,
    left_value: isize,
    operator: Operator,
    right_value: isize,
}

#[test]
fn test_parsing_infix_expressions() {
    let test_cases = [
        InfixExpressionsTestCase {
            input: "5 + 4;",
            left_value: 5,
            operator: Operator::Add,
            right_value: 4,
        },
        InfixExpressionsTestCase {
            input: "5 - 4;",
            left_value: 5,
            operator: Operator::Sub,
            right_value: 4,
        },
        InfixExpressionsTestCase {
            input: "5 * 4;",
            left_value: 5,
            operator: Operator::Mul,
            right_value: 4,
        },
        InfixExpressionsTestCase {
            input: "5 / 4;",
            left_value: 5,
            operator: Operator::Div,
            right_value: 4,
        },
        InfixExpressionsTestCase {
            input: "5 > 4;",
            left_value: 5,
            operator: Operator::Gt,
            right_value: 4,
        },
        InfixExpressionsTestCase {
            input: "5 < 4;",
            left_value: 5,
            operator: Operator::Lt,
            right_value: 4,
        },
        InfixExpressionsTestCase {
            input: "5 == 4;",
            left_value: 5,
            operator: Operator::Eq,
            right_value: 4,
        },
        InfixExpressionsTestCase {
            input: "5 != 4;",
            left_value: 5,
            operator: Operator::NotEq,
            right_value: 4,
        },
    ];

    for test_case in test_cases {
        let mut l = Lexer::new(test_case.input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program();

        check_parser_errors(p);

        if let Some(Node::Program { statements, .. }) = program {
            assert_eq!(statements.len(), 1);
            assert_eq!(
                Statement::ExpressionStmt(ExpressionStmt {
                    expression: Expression::Infix(Infix {
                        left: Box::new(Expression::Literal(Literal {
                            lit: Lit::Int {
                                value: test_case.left_value
                            }
                        })),
                        operator: test_case.operator,
                        right: Box::new(Expression::Literal(Literal {
                            lit: Lit::Int {
                                value: test_case.right_value
                            }
                        }))
                    }),
                    returns: false
                }),
                statements[0]
            );
        } else {
            panic!("p.parse_program() returned None")
        }
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                expression: Expression::If(If {
                    condition: Box::new(Expression::Infix(Infix {
                        left: Box::new(Expression::Identifier(Identifier {
                            value: "x".to_string(),
                        })),
                        operator: Operator::Lt,
                        right: Box::new(Expression::Identifier(Identifier {
                            value: "y".to_string(),
                        })),
                    })),
                    consequence: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        expression: Expression::Identifier(Identifier {
                            value: "x".to_string(),
                        }),
                        returns: true,
                    })]),
                    alternative: None,
                }),
                returns: true,
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                expression: Expression::If(If {
                    condition: Box::new(Expression::Infix(Infix {
                        left: Box::new(Expression::Identifier(Identifier {
                            value: "x".to_string(),
                        })),
                        operator: Operator::Lt,
                        right: Box::new(Expression::Identifier(Identifier {
                            value: "y".to_string(),
                        })),
                    })),
                    consequence: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        expression: Expression::Identifier(Identifier {
                            value: "x".to_string(),
                        }),
                        returns: true,
                    })]),
                    alternative: Some(Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        expression: Expression::Identifier(Identifier {
                            value: "y".to_string(),
                        }),
                        returns: true,
                    })])),
                }),
                returns: true,
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_lambda_expression() {
    let input = "fn (x, y) { x + y; }";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Lambda(Lambda {
                    parameters: Vec::from(["x".to_string(), "y".to_string()]),
                    body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        returns: false,
                        expression: Expression::Infix(Infix {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "x".to_string(),
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Identifier(Identifier {
                                value: "y".to_string(),
                            })),
                        })
                    })]),
                    name: String::new()
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

struct LambdaParameterTestCase {
    input: &'static str,
    expected_params: Vec<String>,
}

#[test]
fn test_lambda_parameter_parsing() {
    let test_cases = [
        LambdaParameterTestCase {
            input: "fn() {};",
            expected_params: [].into(),
        },
        LambdaParameterTestCase {
            input: "fn(x) {};",
            expected_params: ["x".to_string()].into(),
        },
        LambdaParameterTestCase {
            input: "fn(x, y, z) {};",
            expected_params: ["x".to_string(), "y".to_string(), "z".to_string()].into(),
        },
    ];

    for test_case in test_cases {
        let mut l = Lexer::new(test_case.input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program();

        check_parser_errors(p);

        if let Some(Node::Program { statements, .. }) = program {
            assert_eq!(statements.len(), 1);
            assert_eq!(
                Statement::ExpressionStmt(ExpressionStmt {
                    returns: false,
                    expression: Expression::Lambda(Lambda {
                        parameters: test_case.expected_params,
                        body: Vec::new(),
                        name: String::new()
                    })
                }),
                statements[0]
            );
        } else {
            panic!("p.parse_program() returned None")
        }
    }
}

#[test]
fn test_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: false,
                expression: Expression::Call(Call {
                    function: Box::new(Expression::Identifier(Identifier {
                        value: "add".to_string()
                    })),
                    arguments: Vec::from([
                        Expression::Literal(Literal {
                            lit: Lit::Int { value: 1 }
                        }),
                        Expression::Infix(Infix {
                            left: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 2 }
                            })),
                            operator: Operator::Mul,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 3 }
                            }))
                        }),
                        Expression::Infix(Infix {
                            left: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 4 }
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Literal(Literal {
                                lit: Lit::Int { value: 5 }
                            }))
                        })
                    ])
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_method_call_expression() {
    let input = "3.add(4 + 5);";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: false,
                expression: Expression::Method(Method {
                    left: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int { value: 3 }
                    })),
                    name: "add".to_string(),
                    arguments: Some(Vec::from([Expression::Infix(Infix {
                        left: Box::new(Expression::Literal(Literal {
                            lit: Lit::Int { value: 4 }
                        })),
                        operator: Operator::Add,
                        right: Box::new(Expression::Literal(Literal {
                            lit: Lit::Int { value: 5 }
                        }))
                    })]))
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_method_ident_expression() {
    let input = r#""Hello, World!".length"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Method(Method {
                    left: Box::new(Expression::Literal(Literal {
                        lit: Lit::Str {
                            value: "Hello, World!".to_string()
                        }
                    })),
                    name: "length".to_string(),
                    arguments: None
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_constructor_expression() {
    let input = "var myClass = new MyClass(a, b, c);";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    constructable: Constructable::Call(Call {
                        function: Box::new(Expression::Identifier(Identifier {
                            value: "MyClass".to_string()
                        })),
                        arguments: Vec::from([
                            Expression::Identifier(Identifier {
                                value: "a".to_string()
                            }),
                            Expression::Identifier(Identifier {
                                value: "b".to_string()
                            }),
                            Expression::Identifier(Identifier {
                                value: "c".to_string()
                            })
                        ])
                    })
                }))
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_constructor_expression_empty_initializer() {
    let input = "var myClass = new MyClass;";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    constructable: Constructable::Identifier(Identifier {
                        value: "MyClass".to_string()
                    })
                }))
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_scope_constructor_expression() {
    let input = "var myClass = new module::MyClass(a, b, c);";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    constructable: Constructable::Scope(Scope {
                        module: "module".to_string(),
                        member: Box::new(Expression::Call(Call {
                            function: Box::new(Expression::Identifier(Identifier {
                                value: "MyClass".to_string()
                            })),
                            arguments: Vec::from([
                                Expression::Identifier(Identifier {
                                    value: "a".to_string()
                                }),
                                Expression::Identifier(Identifier {
                                    value: "b".to_string()
                                }),
                                Expression::Identifier(Identifier {
                                    value: "c".to_string()
                                })
                            ])
                        }))
                    })
                }))
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_scope_constructor_expression_empty_initializer() {
    let input = "var myClass = new module::MyClass;";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    constructable: Constructable::Scope(Scope {
                        module: "module".to_string(),
                        member: Box::new(Expression::Identifier(Identifier {
                            value: "MyClass".to_string()
                        }))
                    })
                }))
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_string_literal_expression() {
    let input = r#""hello world";"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: false,
                expression: Expression::Literal(Literal {
                    lit: Lit::Str {
                        value: "hello world".to_string()
                    }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_char_literal_expression() {
    let input = "'a'";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Char { value: 'a' }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_null_literal_expression() {
    let input = "null";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal { lit: Lit::Null })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 3, 4 + 5]";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Array {
                        elements: Vec::from([
                            Expression::Literal(Literal {
                                lit: Lit::Int { value: 1 }
                            }),
                            Expression::Infix(Infix {
                                left: Box::new(Expression::Literal(Literal {
                                    lit: Lit::Int { value: 2 }
                                })),
                                operator: Operator::Mul,
                                right: Box::new(Expression::Literal(Literal {
                                    lit: Lit::Int { value: 3 }
                                }))
                            }),
                            Expression::Infix(Infix {
                                left: Box::new(Expression::Literal(Literal {
                                    lit: Lit::Int { value: 4 }
                                })),
                                operator: Operator::Add,
                                right: Box::new(Expression::Literal(Literal {
                                    lit: Lit::Int { value: 5 }
                                }))
                            })
                        ])
                    }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[2 * 3]";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Index(Index {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "myArray".to_string()
                    })),
                    expr: Box::new(Expression::Infix(Infix {
                        left: Box::new(Expression::Literal(Literal {
                            lit: Lit::Int { value: 2 }
                        })),
                        operator: Operator::Mul,
                        right: Box::new(Expression::Literal(Literal {
                            lit: Lit::Int { value: 3 }
                        }))
                    }))
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_hash_literal_string_keys() {
    let input = r#"{"one": 1, "two": 2, "three": 3}"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Hash {
                        pairs: Vec::from([
                            (
                                Expression::Literal(Literal {
                                    lit: Lit::Str {
                                        value: "one".to_string()
                                    }
                                }),
                                Expression::Literal(Literal {
                                    lit: Lit::Int { value: 1 }
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    lit: Lit::Str {
                                        value: "two".to_string()
                                    }
                                }),
                                Expression::Literal(Literal {
                                    lit: Lit::Int { value: 2 }
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    lit: Lit::Str {
                                        value: "three".to_string()
                                    }
                                }),
                                Expression::Literal(Literal {
                                    lit: Lit::Int { value: 3 }
                                })
                            )
                        ])
                    }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Hash { pairs: Vec::new() }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_hash_literal_with_expressions() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Literal(Literal {
                    lit: Lit::Hash {
                        pairs: Vec::from([
                            (
                                Expression::Literal(Literal {
                                    lit: Lit::Str {
                                        value: "one".to_string()
                                    }
                                }),
                                Expression::Infix(Infix {
                                    left: Box::new(Expression::Literal(Literal {
                                        lit: Lit::Int { value: 0 }
                                    })),
                                    operator: Operator::Add,
                                    right: Box::new(Expression::Literal(Literal {
                                        lit: Lit::Int { value: 1 }
                                    }))
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    lit: Lit::Str {
                                        value: "two".to_string()
                                    }
                                }),
                                Expression::Infix(Infix {
                                    left: Box::new(Expression::Literal(Literal {
                                        lit: Lit::Int { value: 10 }
                                    })),
                                    operator: Operator::Sub,
                                    right: Box::new(Expression::Literal(Literal {
                                        lit: Lit::Int { value: 8 }
                                    }))
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    lit: Lit::Str {
                                        value: "three".to_string()
                                    }
                                }),
                                Expression::Infix(Infix {
                                    left: Box::new(Expression::Literal(Literal {
                                        lit: Lit::Int { value: 15 }
                                    })),
                                    operator: Operator::Div,
                                    right: Box::new(Expression::Literal(Literal {
                                        lit: Lit::Int { value: 5 }
                                    }))
                                })
                            )
                        ])
                    }
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

struct RangeExpressionTestCase {
    input: &'static str,
    start: isize,
    end: isize,
    step: Option<isize>,
}

// TODO: add range test cases
#[test]
fn test_range_expression() {
    let test_case = RangeExpressionTestCase {
        input: "0..100",
        start: 0,
        end: 100,
        step: None,
    };

    let mut l = Lexer::new(test_case.input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Range(Range {
                    start: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int {
                            value: test_case.start
                        }
                    })),
                    end: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int {
                            value: test_case.end
                        }
                    })),
                    step: None
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }

    let test_case = RangeExpressionTestCase {
        input: "0..100..10",
        start: 0,
        end: 100,
        step: Some(10),
    };

    let mut l = Lexer::new(test_case.input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Range(Range {
                    start: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int {
                            value: test_case.start
                        }
                    })),
                    end: Box::new(Expression::Literal(Literal {
                        lit: Lit::Int {
                            value: test_case.end
                        }
                    })),
                    step: test_case
                        .step
                        .map(|expected| Box::new(Expression::Literal(Literal {
                            lit: Lit::Int { value: expected }
                        })))
                })
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_scope_var_expression() {
    let input = r#"fs::MAX_FILE_PATH"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Scope(Scope {
                    member: Box::new(Expression::Identifier(Identifier {
                        value: "MAX_FILE_PATH".to_string()
                    })),
                    module: "fs".to_string(),
                }),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_scope_fn_expression() {
    let input = r#"fs::readFile(fname)"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                returns: true,
                expression: Expression::Scope(Scope {
                    member: Box::new(Expression::Call(Call {
                        function: Box::new(Expression::Identifier(Identifier {
                            value: "readFile".to_string(),
                        })),
                        arguments: Vec::from([Expression::Identifier(Identifier {
                            value: "fname".to_string(),
                        })]),
                    })),
                    module: "fs".to_string(),
                }),
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

struct AssignExpressionTestCase {
    input: &'static str,
    expected: Expression,
}

#[test]
fn test_assign_expression() {
    let test_cases = [
        AssignExpressionTestCase {
            input: "a = 45",
            expected: Expression::Assign(Assign {
                to: Assignable::Identifier(Identifier {
                    value: "a".to_string(),
                }),
                value: Box::new(Expression::Literal(Literal {
                    lit: Lit::Int { value: 45 },
                })),
            }),
        },
        AssignExpressionTestCase {
            input: "a.b = 45",
            expected: Expression::Assign(Assign {
                to: Assignable::Method(Method {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "a".to_string(),
                    })),
                    name: "b".to_string(),
                    arguments: None,
                }),
                value: Box::new(Expression::Literal(Literal {
                    lit: Lit::Int { value: 45 },
                })),
            }),
        },
        AssignExpressionTestCase {
            input: "a[b] = 45",
            expected: Expression::Assign(Assign {
                to: Assignable::Index(Index {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "a".to_string(),
                    })),
                    expr: Box::new(Expression::Identifier(Identifier {
                        value: "b".to_string(),
                    })),
                }),
                value: Box::new(Expression::Literal(Literal {
                    lit: Lit::Int { value: 45 },
                })),
            }),
        },
    ];

    for test_case in test_cases {
        let mut l = Lexer::new(test_case.input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program();

        check_parser_errors(p);

        if let Some(Node::Program { statements, .. }) = program {
            assert_eq!(statements.len(), 1);
            assert_eq!(
                Statement::ExpressionStmt(ExpressionStmt {
                    returns: true,
                    expression: test_case.expected
                }),
                statements[0]
            );
        } else {
            panic!("p.parse_program() returned None")
        }
    }
}

#[test]
fn test_function_literal_with_name() {
    let input = "var myFunction = fn() { };";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                name: "myFunction".to_string(),
                mutable: true,
                value: Some(Expression::Lambda(Lambda {
                    parameters: Vec::new(),
                    body: Vec::new(),
                    name: "myFunction".to_string()
                }))
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_delete_statement() {
    let input = "delete foo";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);

        assert_eq!(
            Statement::Delete(Delete {
                delete_ident: "foo".to_string()
            }),
            statements[0]
        );
    } else {
        panic!("p.parse_program() returned None")
    }
}

fn check_parser_errors(p: Parser) {
    if !p.errors.is_empty() {
        println!("parser has {} errors", p.errors.len());
        for err in p.errors {
            println!("parser error: {err}");
        }

        panic!()
    }
}
