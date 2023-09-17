use pretty_assertions::assert_eq;

use crate::{ast::*, token::Position};

use super::*;

#[test]
fn test_declaration_statement() {
    let input = "
var x = 5;
var foobar = 838383;
const y = 10;
const barbaz = 121212;
";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    let test_cases = [
        ("x", true, 5, 9),
        ("foobar", true, 838_383, 14),
        ("y", false, 10, 11),
        ("barbaz", false, 121_212, 16),
    ];

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 4);
        for (i, &(name, mutable, value, end_pos)) in test_cases.iter().enumerate() {
            assert_eq!(
                Statement::Declaration(Declaration {
                    span: Span {
                        start: Position::new(i + 1, 1),
                        end: Position::new(i + 1, end_pos + value.to_string().len())
                    },
                    name: name.to_string(),
                    mutable,
                    value: Some(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(i + 1, end_pos),
                            end: Position::new(i + 1, end_pos + value.to_string().len())
                        },
                        lit: Lit::Int {
                            value: value.into()
                        }
                    })),
                }),
                statements[i]
            )
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    let test_cases = [(5, 9), (10, 10), (838_383, 14)];

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 3);

        for (i, &(test_case, end)) in test_cases.iter().enumerate() {
            assert_eq!(
                Statement::Return(Return {
                    span: Span {
                        start: Position::new(i + 1, 1),
                        end: Position::new(i + 1, 8 + test_case.to_string().len())
                    },
                    return_value: Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(i + 1, 8),
                            end: Position::new(i + 1, end)
                        },
                        lit: Lit::Int {
                            value: test_case.into()
                        }
                    }),
                }),
                statements[i]
            )
        }
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_function_statement() {
    let input = "fn add() { x + y }";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Function(Function {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 18)
                },
                ident: "add".to_string(),
                parameters: Vec::new(),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 12),
                        end: Position::new(0, 17)
                    },
                    returns: true,
                    expression: Expression::Infix(Infix {
                        span: Span {
                            start: Position::new(0, 12),
                            end: Position::new(0, 17)
                        },
                        left: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 12),
                                end: Position::new(0, 13)
                            },
                            value: "x".to_string(),
                        })),
                        operator: Operator::Add,
                        right: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 16),
                                end: Position::new(0, 17)
                            },
                            value: "y".to_string(),
                        })),
                    }),
                })]),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_while_statement() {
    let input = "while (i < n) { i = i + 1 }";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::While(While {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 27)
                },
                condition: Expression::Infix(Infix {
                    span: Span {
                        start: Position::new(0, 8),
                        end: Position::new(0, 13)
                    },
                    left: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 8),
                            end: Position::new(0, 9)
                        },
                        value: "i".to_string(),
                    })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 12),
                            end: Position::new(0, 13)
                        },
                        value: "n".to_string(),
                    })),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 17),
                        end: Position::new(0, 26)
                    },
                    returns: true,
                    expression: Expression::Assign(Assign {
                        span: Span {
                            start: Position::new(0, 17),
                            end: Position::new(0, 26)
                        },
                        to: Assignable::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 17),
                                end: Position::new(0, 18)
                            },
                            value: "i".to_string(),
                        }),
                        value: Box::new(Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 21),
                                end: Position::new(0, 26)
                            },
                            left: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 21),
                                    end: Position::new(0, 22)
                                },
                                value: "i".to_string(),
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 25),
                                    end: Position::new(0, 26)
                                },
                                lit: Lit::Int { value: 1.into() },
                            })),
                        })),
                    }),
                })]),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_while_with_break_statement() {
    let input = "while (i < n) { if (i == 3) { break; }}";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::While(While {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 39)
                },
                condition: Expression::Infix(Infix {
                    span: Span {
                        start: Position::new(0, 8),
                        end: Position::new(0, 13)
                    },
                    left: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 8),
                            end: Position::new(0, 9)
                        },
                        value: "i".to_string(),
                    })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 12),
                            end: Position::new(0, 13)
                        },
                        value: "n".to_string(),
                    })),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 17),
                        end: Position::new(0, 38)
                    },
                    returns: true,
                    expression: Expression::If(If {
                        span: Span {
                            start: Position::new(0, 17),
                            end: Position::new(0, 38)
                        },
                        condition: Box::new(Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 21),
                                end: Position::new(0, 27)
                            },
                            left: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 21),
                                    end: Position::new(0, 22)
                                },
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 26),
                                    end: Position::new(0, 27)
                                },
                                lit: Lit::Int { value: 3.into() }
                            }))
                        })),
                        consequence: Vec::from([Statement::Break(Span {
                            start: Position::new(0, 31),
                            end: Position::new(0, 36)
                        })]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_while_with_continue_statement() {
    let input = "while (i < n) { if (i == 3) { continue; }}";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::While(While {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 42)
                },
                condition: Expression::Infix(Infix {
                    span: Span {
                        start: Position::new(0, 8),
                        end: Position::new(0, 13)
                    },
                    left: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 8),
                            end: Position::new(0, 9)
                        },
                        value: "i".to_string(),
                    })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 12),
                            end: Position::new(0, 13)
                        },
                        value: "n".to_string(),
                    })),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 17),
                        end: Position::new(0, 41)
                    },
                    returns: true,
                    expression: Expression::If(If {
                        span: Span {
                            start: Position::new(0, 17),
                            end: Position::new(0, 41)
                        },
                        condition: Box::new(Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 21),
                                end: Position::new(0, 27)
                            },
                            left: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 21),
                                    end: Position::new(0, 22)
                                },
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 26),
                                    end: Position::new(0, 27)
                                },
                                lit: Lit::Int { value: 3.into() }
                            }))
                        })),
                        consequence: Vec::from([Statement::Continue(Span {
                            start: Position::new(0, 31),
                            end: Position::new(0, 39)
                        })]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_for_statement() {
    let input = "for (i in arr) { i = i + 1 }";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::For(For {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 28)
                },
                ident: "i".to_string(),
                iterator: Expression::Identifier(Identifier {
                    span: Span {
                        start: Position::new(0, 11),
                        end: Position::new(0, 14)
                    },
                    value: "arr".to_string(),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 18),
                        end: Position::new(0, 27)
                    },
                    returns: true,
                    expression: Expression::Assign(Assign {
                        span: Span {
                            start: Position::new(0, 18),
                            end: Position::new(0, 27)
                        },
                        to: Assignable::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 18),
                                end: Position::new(0, 19)
                            },
                            value: "i".to_string(),
                        }),
                        value: Box::new(Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 22),
                                end: Position::new(0, 27)
                            },
                            left: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 22),
                                    end: Position::new(0, 23)
                                },
                                value: "i".to_string(),
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 26),
                                    end: Position::new(0, 27)
                                },
                                lit: Lit::Int { value: 1.into() },
                            })),
                        })),
                    }),
                })]),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_for_with_break_statement() {
    let input = "for (i in arr) { if (i == 3) { break; } }";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::For(For {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 41)
                },
                ident: "i".to_string(),
                iterator: Expression::Identifier(Identifier {
                    span: Span {
                        start: Position::new(0, 11),
                        end: Position::new(0, 14)
                    },
                    value: "arr".to_string(),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 18),
                        end: Position::new(0, 39)
                    },
                    returns: true,
                    expression: Expression::If(If {
                        span: Span {
                            start: Position::new(0, 18),
                            end: Position::new(0, 39)
                        },
                        condition: Box::new(Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 22),
                                end: Position::new(0, 28)
                            },
                            left: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 22),
                                    end: Position::new(0, 23)
                                },
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 27),
                                    end: Position::new(0, 28)
                                },
                                lit: Lit::Int { value: 3.into() }
                            }))
                        })),
                        consequence: Vec::from([Statement::Break(Span {
                            start: Position::new(0, 32),
                            end: Position::new(0, 37)
                        })]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_for_with_continue_statement() {
    let input = "for (i in arr) { if (i == 3) { continue; } }";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::For(For {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 44)
                },
                ident: "i".to_string(),
                iterator: Expression::Identifier(Identifier {
                    span: Span {
                        start: Position::new(0, 11),
                        end: Position::new(0, 14)
                    },
                    value: "arr".to_string(),
                }),
                body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 18),
                        end: Position::new(0, 42)
                    },
                    returns: true,
                    expression: Expression::If(If {
                        span: Span {
                            start: Position::new(0, 18),
                            end: Position::new(0, 42)
                        },
                        condition: Box::new(Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 22),
                                end: Position::new(0, 28)
                            },
                            left: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 22),
                                    end: Position::new(0, 23)
                                },
                                value: "i".to_string()
                            })),
                            operator: Operator::Eq,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 27),
                                    end: Position::new(0, 28)
                                },
                                lit: Lit::Int { value: 3.into() }
                            }))
                        })),
                        consequence: Vec::from([Statement::Continue(Span {
                            start: Position::new(0, 32),
                            end: Position::new(0, 40)
                        })]),
                        alternative: None
                    }),
                })]),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_class_statement() {
    let input = "class MyClass(i1, i2) { var a = 32; }";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ClassDecl(ClassDecl {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 37)
                },
                ident: "MyClass".to_string(),
                initializers: Vec::from(["i1".to_string(), "i2".to_string()]),
                body: Vec::from([ClassStatement::Declaration(Declaration {
                    span: Span {
                        start: Position::new(0, 25),
                        end: Position::new(0, 35)
                    },
                    name: "a".to_string(),
                    mutable: true,
                    value: Some(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 33),
                            end: Position::new(0, 35)
                        },
                        lit: Lit::Int { value: 32.into() },
                    })),
                })]),
            }),
            statements[0]
        )
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
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 10),
                },
                path: "fs".to_string(),
                alias: None,
            }),
        },
        ImportStatementTestCase {
            input: r#"import "std/datetime/duration""#,
            expected: Statement::Import(Import {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 29),
                },
                path: "std/datetime/duration".to_string(),
                alias: None,
            }),
        },
        ImportStatementTestCase {
            input: r#"import "std/datetime/duration" as duration"#,
            expected: Statement::Import(Import {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 43),
                },
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 11)
                },
                returns: true,
                expression: Expression::Identifier(Identifier {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 11)
                    },
                    value: "foo_bar123".to_string()
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_boolean_literal() {
    let input = "true";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 5)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 5)
                    },
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
    let input = 5;

    let mut l = Lexer::new(&input.to_string());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 2)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 2)
                    },
                    lit: Lit::Int { value: 5.into() }
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_float_literal() {
    let input = 5.103;

    let mut l = Lexer::new(&input.to_string());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 6)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 6)
                    },
                    lit: Lit::Float { value: 5.103 }
                })
            }),
            statements[0]
        )
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
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 3),
                },
                returns: true,
                expression: Expression::Prefix(Prefix {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 3),
                    },
                    operator: Operator::Bang,
                    right: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 2),
                            end: Position::new(0, 3),
                        },
                        lit: Lit::Int { value: 5.into() },
                    })),
                }),
            }),
        },
        PrefixExpressionsTestCase {
            input: "-15",
            expected: Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 4),
                },
                returns: true,
                expression: Expression::Prefix(Prefix {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 4),
                    },
                    operator: Operator::Sub,
                    right: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 2),
                            end: Position::new(0, 4),
                        },
                        lit: Lit::Int { value: 15.into() },
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
            let end_pos = test_case.operator.to_string().len() + 4;
            assert_eq!(statements.len(), 1);
            assert_eq!(
                Statement::ExpressionStmt(ExpressionStmt {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, end_pos + 1)
                    },
                    expression: Expression::Infix(Infix {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, end_pos + 1)
                        },
                        left: Box::new(Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, 1),
                                end: Position::new(0, 2)
                            },
                            lit: Lit::Int {
                                value: test_case.left_value.into()
                            }
                        })),
                        operator: test_case.operator,
                        right: Box::new(Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, end_pos),
                                end: Position::new(0, end_pos + 1)
                            },
                            lit: Lit::Int {
                                value: test_case.right_value.into()
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 16)
                },
                expression: Expression::If(If {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 16)
                    },
                    condition: Box::new(Expression::Infix(Infix {
                        span: Span {
                            start: Position::new(0, 5),
                            end: Position::new(0, 10)
                        },
                        left: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 5),
                                end: Position::new(0, 6)
                            },
                            value: "x".to_string(),
                        })),
                        operator: Operator::Lt,
                        right: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 9),
                                end: Position::new(0, 10)
                            },
                            value: "y".to_string(),
                        })),
                    })),
                    consequence: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        span: Span {
                            start: Position::new(0, 14),
                            end: Position::new(0, 15)
                        },
                        expression: Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 14),
                                end: Position::new(0, 15)
                            },
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 27)
                },
                expression: Expression::If(If {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 27)
                    },
                    condition: Box::new(Expression::Infix(Infix {
                        span: Span {
                            start: Position::new(0, 5),
                            end: Position::new(0, 10)
                        },
                        left: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 5),
                                end: Position::new(0, 6)
                            },
                            value: "x".to_string(),
                        })),
                        operator: Operator::Lt,
                        right: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 9),
                                end: Position::new(0, 10)
                            },
                            value: "y".to_string(),
                        })),
                    })),
                    consequence: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        span: Span {
                            start: Position::new(0, 14),
                            end: Position::new(0, 15)
                        },
                        expression: Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 14),
                                end: Position::new(0, 15)
                            },
                            value: "x".to_string(),
                        }),
                        returns: true,
                    })]),
                    alternative: Some(Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        span: Span {
                            start: Position::new(0, 25),
                            end: Position::new(0, 26)
                        },
                        expression: Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 25),
                                end: Position::new(0, 26)
                            },
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 20)
                },
                returns: true,
                expression: Expression::Lambda(Lambda {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 20)
                    },
                    parameters: Vec::from(["x".to_string(), "y".to_string()]),
                    body: Vec::from([Statement::ExpressionStmt(ExpressionStmt {
                        span: Span {
                            start: Position::new(0, 13),
                            end: Position::new(0, 18)
                        },
                        returns: false,
                        expression: Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 13),
                                end: Position::new(0, 18)
                            },
                            left: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 13),
                                    end: Position::new(0, 14)
                                },
                                value: "x".to_string(),
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 17),
                                    end: Position::new(0, 18)
                                },
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
    end_pos: usize,
}

#[test]
fn test_lambda_parameter_parsing() {
    let test_cases = [
        LambdaParameterTestCase {
            input: "fn() {};",
            expected_params: [].into(),
            end_pos: 7,
        },
        LambdaParameterTestCase {
            input: "fn(x) {};",
            expected_params: ["x".to_string()].into(),
            end_pos: 8,
        },
        LambdaParameterTestCase {
            input: "fn(x, y, z) {};",
            expected_params: ["x".to_string(), "y".to_string(), "z".to_string()].into(),
            end_pos: 14,
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
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, test_case.end_pos)
                    },
                    returns: false,
                    expression: Expression::Lambda(Lambda {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, test_case.end_pos)
                        },
                        parameters: test_case.expected_params,
                        body: Vec::new(),
                        name: String::new()
                    })
                }),
                statements[0]
            )
        } else {
            panic!("p.parse_program() returned None")
        }
    }
}

#[test]
fn test_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 20)
                },
                returns: false,
                expression: Expression::Call(Call {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 20)
                    },
                    function: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 4)
                        },
                        value: "add".to_string()
                    })),
                    arguments: Vec::from([
                        Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, 5),
                                end: Position::new(0, 6)
                            },
                            lit: Lit::Int { value: 1.into() }
                        }),
                        Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 8),
                                end: Position::new(0, 13)
                            },
                            left: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 8),
                                    end: Position::new(0, 9)
                                },
                                lit: Lit::Int { value: 2.into() }
                            })),
                            operator: Operator::Mul,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 12),
                                    end: Position::new(0, 13)
                                },
                                lit: Lit::Int { value: 3.into() }
                            }))
                        }),
                        Expression::Infix(Infix {
                            span: Span {
                                start: Position::new(0, 15),
                                end: Position::new(0, 20)
                            },
                            left: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 15),
                                    end: Position::new(0, 16)
                                },
                                lit: Lit::Int { value: 4.into() }
                            })),
                            operator: Operator::Add,
                            right: Box::new(Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 19),
                                    end: Position::new(0, 20)
                                },
                                lit: Lit::Int { value: 5.into() }
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 15)
                },
                returns: false,
                expression: Expression::Method(Method {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 15)
                    },
                    left: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 2)
                        },
                        lit: Lit::Int { value: 3.into() }
                    })),
                    method: "add".to_string(),
                    arguments: Some(Vec::from([Expression::Infix(Infix {
                        span: Span {
                            start: Position::new(0, 7),
                            end: Position::new(0, 12)
                        },
                        left: Box::new(Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, 7),
                                end: Position::new(0, 8)
                            },
                            lit: Lit::Int { value: 4.into() }
                        })),
                        operator: Operator::Add,
                        right: Box::new(Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, 11),
                                end: Position::new(0, 12)
                            },
                            lit: Lit::Int { value: 5.into() }
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 17)
                },
                returns: true,
                expression: Expression::Method(Method {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 17)
                    },
                    left: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 14)
                        },
                        lit: Lit::Str {
                            value: "Hello, World!".to_string()
                        }
                    })),
                    method: "length".to_string(),
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 34)
                },
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    span: Span {
                        start: Position::new(0, 15),
                        end: Position::new(0, 34)
                    },
                    constructable: Constructable::Call(Call {
                        span: Span {
                            start: Position::new(0, 19),
                            end: Position::new(0, 34)
                        },
                        function: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 19),
                                end: Position::new(0, 26)
                            },
                            value: "MyClass".to_string()
                        })),
                        arguments: Vec::from([
                            Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 27),
                                    end: Position::new(0, 28)
                                },
                                value: "a".to_string()
                            }),
                            Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 30),
                                    end: Position::new(0, 31)
                                },
                                value: "b".to_string()
                            }),
                            Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 33),
                                    end: Position::new(0, 34)
                                },
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 26)
                },
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    span: Span {
                        start: Position::new(0, 15),
                        end: Position::new(0, 26)
                    },
                    constructable: Constructable::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 19),
                            end: Position::new(0, 26)
                        },
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 42)
                },
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    span: Span {
                        start: Position::new(0, 15),
                        end: Position::new(0, 42)
                    },
                    constructable: Constructable::Scope(Scope {
                        span: Span {
                            start: Position::new(0, 19),
                            end: Position::new(0, 42)
                        },
                        module: "module".to_string(),
                        member: Box::new(Expression::Call(Call {
                            span: Span {
                                start: Position::new(0, 27),
                                end: Position::new(0, 42)
                            },
                            function: Box::new(Expression::Identifier(Identifier {
                                span: Span {
                                    start: Position::new(0, 27),
                                    end: Position::new(0, 34)
                                },
                                value: "MyClass".to_string()
                            })),
                            arguments: Vec::from([
                                Expression::Identifier(Identifier {
                                    span: Span {
                                        start: Position::new(0, 35),
                                        end: Position::new(0, 36)
                                    },
                                    value: "a".to_string()
                                }),
                                Expression::Identifier(Identifier {
                                    span: Span {
                                        start: Position::new(0, 38),
                                        end: Position::new(0, 39)
                                    },
                                    value: "b".to_string()
                                }),
                                Expression::Identifier(Identifier {
                                    span: Span {
                                        start: Position::new(0, 41),
                                        end: Position::new(0, 42)
                                    },
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 34)
                },
                name: "myClass".to_string(),
                mutable: true,
                value: Some(Expression::Constructor(Constructor {
                    span: Span {
                        start: Position::new(0, 15),
                        end: Position::new(0, 34)
                    },
                    constructable: Constructable::Scope(Scope {
                        span: Span {
                            start: Position::new(0, 19),
                            end: Position::new(0, 34)
                        },
                        module: "module".to_string(),
                        member: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 27),
                                end: Position::new(0, 34)
                            },
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 12)
                },
                returns: false,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 12)
                    },
                    lit: Lit::Str {
                        value: "hello world".to_string()
                    }
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_char_literal_expression() {
    let input = "'a'";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 2)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 2)
                    },
                    lit: Lit::Char { value: 'a' }
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 3, 4 + 5]";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 17)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 17)
                    },
                    lit: Lit::Array {
                        elements: Vec::from([
                            Expression::Literal(Literal {
                                span: Span {
                                    start: Position::new(0, 2),
                                    end: Position::new(0, 3)
                                },
                                lit: Lit::Int { value: 1.into() }
                            }),
                            Expression::Infix(Infix {
                                span: Span {
                                    start: Position::new(0, 5),
                                    end: Position::new(0, 10)
                                },
                                left: Box::new(Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 5),
                                        end: Position::new(0, 6)
                                    },
                                    lit: Lit::Int { value: 2.into() }
                                })),
                                operator: Operator::Mul,
                                right: Box::new(Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 9),
                                        end: Position::new(0, 10)
                                    },
                                    lit: Lit::Int { value: 3.into() }
                                }))
                            }),
                            Expression::Infix(Infix {
                                span: Span {
                                    start: Position::new(0, 12),
                                    end: Position::new(0, 17)
                                },
                                left: Box::new(Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 12),
                                        end: Position::new(0, 13)
                                    },
                                    lit: Lit::Int { value: 4.into() }
                                })),
                                operator: Operator::Add,
                                right: Box::new(Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 16),
                                        end: Position::new(0, 17)
                                    },
                                    lit: Lit::Int { value: 5.into() }
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

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 14)
                },
                returns: true,
                expression: Expression::Index(Index {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 14)
                    },
                    left: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 8)
                        },
                        value: "myArray".to_string()
                    })),
                    index: Box::new(Expression::Infix(Infix {
                        span: Span {
                            start: Position::new(0, 9),
                            end: Position::new(0, 14)
                        },
                        left: Box::new(Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, 9),
                                end: Position::new(0, 10)
                            },
                            lit: Lit::Int { value: 2.into() }
                        })),
                        operator: Operator::Mul,
                        right: Box::new(Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, 13),
                                end: Position::new(0, 14)
                            },
                            lit: Lit::Int { value: 3.into() }
                        }))
                    }))
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_hash_literal_string_keys() {
    let input = r#"{"one": 1, "two": 2, "three": 3}"#;

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 32)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 32)
                    },
                    lit: Lit::Hash {
                        pairs: Vec::from([
                            (
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 2),
                                        end: Position::new(0, 5)
                                    },
                                    lit: Lit::Str {
                                        value: "one".to_string()
                                    }
                                }),
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 9),
                                        end: Position::new(0, 10)
                                    },
                                    lit: Lit::Int { value: 1.into() }
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 12),
                                        end: Position::new(0, 15)
                                    },
                                    lit: Lit::Str {
                                        value: "two".to_string()
                                    }
                                }),
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 19),
                                        end: Position::new(0, 20)
                                    },
                                    lit: Lit::Int { value: 2.into() }
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 22),
                                        end: Position::new(0, 27)
                                    },
                                    lit: Lit::Str {
                                        value: "three".to_string()
                                    }
                                }),
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 31),
                                        end: Position::new(0, 32)
                                    },
                                    lit: Lit::Int { value: 3.into() }
                                })
                            )
                        ])
                    }
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 2)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 2)
                    },
                    lit: Lit::Hash { pairs: Vec::new() }
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_parsing_hash_literal_with_expressions() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 46)
                },
                returns: true,
                expression: Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 46)
                    },
                    lit: Lit::Hash {
                        pairs: Vec::from([
                            (
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 2),
                                        end: Position::new(0, 5)
                                    },
                                    lit: Lit::Str {
                                        value: "one".to_string()
                                    }
                                }),
                                Expression::Infix(Infix {
                                    span: Span {
                                        start: Position::new(0, 9),
                                        end: Position::new(0, 14)
                                    },
                                    left: Box::new(Expression::Literal(Literal {
                                        span: Span {
                                            start: Position::new(0, 9),
                                            end: Position::new(0, 10)
                                        },
                                        lit: Lit::Int { value: 0.into() }
                                    })),
                                    operator: Operator::Add,
                                    right: Box::new(Expression::Literal(Literal {
                                        span: Span {
                                            start: Position::new(0, 13),
                                            end: Position::new(0, 14)
                                        },
                                        lit: Lit::Int { value: 1.into() }
                                    }))
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 16),
                                        end: Position::new(0, 19)
                                    },
                                    lit: Lit::Str {
                                        value: "two".to_string()
                                    }
                                }),
                                Expression::Infix(Infix {
                                    span: Span {
                                        start: Position::new(0, 23),
                                        end: Position::new(0, 29)
                                    },
                                    left: Box::new(Expression::Literal(Literal {
                                        span: Span {
                                            start: Position::new(0, 23),
                                            end: Position::new(0, 25)
                                        },
                                        lit: Lit::Int { value: 10.into() }
                                    })),
                                    operator: Operator::Sub,
                                    right: Box::new(Expression::Literal(Literal {
                                        span: Span {
                                            start: Position::new(0, 28),
                                            end: Position::new(0, 29)
                                        },
                                        lit: Lit::Int { value: 8.into() }
                                    }))
                                })
                            ),
                            (
                                Expression::Literal(Literal {
                                    span: Span {
                                        start: Position::new(0, 31),
                                        end: Position::new(0, 36)
                                    },
                                    lit: Lit::Str {
                                        value: "three".to_string()
                                    }
                                }),
                                Expression::Infix(Infix {
                                    span: Span {
                                        start: Position::new(0, 40),
                                        end: Position::new(0, 46)
                                    },
                                    left: Box::new(Expression::Literal(Literal {
                                        span: Span {
                                            start: Position::new(0, 40),
                                            end: Position::new(0, 42)
                                        },
                                        lit: Lit::Int { value: 15.into() }
                                    })),
                                    operator: Operator::Div,
                                    right: Box::new(Expression::Literal(Literal {
                                        span: Span {
                                            start: Position::new(0, 45),
                                            end: Position::new(0, 46)
                                        },
                                        lit: Lit::Int { value: 5.into() }
                                    }))
                                })
                            )
                        ])
                    }
                })
            }),
            statements[0]
        )
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
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 4)
                },
                returns: true,
                expression: Expression::Range(Range {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 4)
                    },
                    start: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 2)
                        },
                        lit: Lit::Int {
                            value: test_case.start.into()
                        }
                    })),
                    end: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 4),
                            end: Position::new(0, 7)
                        },
                        lit: Lit::Int {
                            value: test_case.end.into()
                        }
                    })),
                    step: None
                })
            }),
            statements[0]
        )
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
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 9)
                },
                returns: true,
                expression: Expression::Range(Range {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 9)
                    },
                    start: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 2)
                        },
                        lit: Lit::Int {
                            value: test_case.start.into()
                        }
                    })),
                    end: Box::new(Expression::Literal(Literal {
                        span: Span {
                            start: Position::new(0, 4),
                            end: Position::new(0, 7)
                        },
                        lit: Lit::Int {
                            value: test_case.end.into()
                        }
                    })),
                    step: test_case
                        .step
                        .map(|expected| Box::new(Expression::Literal(Literal {
                            span: Span {
                                start: Position::new(0, 9),
                                end: Position::new(0, 11)
                            },
                            lit: Lit::Int {
                                value: expected.into()
                            }
                        })))
                })
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_scope_var_expression() {
    let input = r#"fs::MAX_FILE_PATH"#;

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 18)
                },
                returns: true,
                expression: Expression::Scope(Scope {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 18)
                    },
                    member: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 5),
                            end: Position::new(0, 18)
                        },
                        value: "MAX_FILE_PATH".to_string()
                    })),
                    module: "fs".to_string(),
                }),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_scope_fn_expression() {
    let input = r#"fs::readFile(fname)"#;

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::ExpressionStmt(ExpressionStmt {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 19)
                },
                returns: true,
                expression: Expression::Scope(Scope {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 19)
                    },
                    member: Box::new(Expression::Call(Call {
                        span: Span {
                            start: Position::new(0, 5),
                            end: Position::new(0, 19)
                        },
                        function: Box::new(Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 5),
                                end: Position::new(0, 13)
                            },
                            value: "readFile".to_string(),
                        })),
                        arguments: Vec::from([Expression::Identifier(Identifier {
                            span: Span {
                                start: Position::new(0, 14),
                                end: Position::new(0, 19)
                            },
                            value: "fname".to_string(),
                        })]),
                    })),
                    module: "fs".to_string(),
                }),
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

struct AssignExpressionTestCase {
    input: &'static str,
    expected: Expression,
    expr_stmt_end: usize,
}

#[test]
fn test_assign_expression() {
    let test_cases = [
        AssignExpressionTestCase {
            input: "a = 45",
            expected: Expression::Assign(Assign {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 7),
                },
                to: Assignable::Identifier(Identifier {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 2),
                    },
                    value: "a".to_string(),
                }),
                value: Box::new(Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 5),
                        end: Position::new(0, 7),
                    },
                    lit: Lit::Int { value: 45.into() },
                })),
            }),
            expr_stmt_end: 7,
        },
        AssignExpressionTestCase {
            input: "a.b = 45",
            expected: Expression::Assign(Assign {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 9),
                },
                to: Assignable::Method(Method {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 3),
                    },
                    left: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 2),
                        },
                        value: "a".to_string(),
                    })),
                    method: "b".to_string(),
                    arguments: None,
                }),
                value: Box::new(Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 7),
                        end: Position::new(0, 9),
                    },
                    lit: Lit::Int { value: 45.into() },
                })),
            }),
            expr_stmt_end: 9,
        },
        AssignExpressionTestCase {
            input: "a[b] = 45",
            expected: Expression::Assign(Assign {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 10),
                },
                to: Assignable::Index(Index {
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, 4),
                    },
                    left: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 1),
                            end: Position::new(0, 2),
                        },
                        value: "a".to_string(),
                    })),
                    index: Box::new(Expression::Identifier(Identifier {
                        span: Span {
                            start: Position::new(0, 3),
                            end: Position::new(0, 4),
                        },
                        value: "b".to_string(),
                    })),
                }),
                value: Box::new(Expression::Literal(Literal {
                    span: Span {
                        start: Position::new(0, 8),
                        end: Position::new(0, 10),
                    },
                    lit: Lit::Int { value: 45.into() },
                })),
            }),
            expr_stmt_end: 10,
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
                    span: Span {
                        start: Position::new(0, 1),
                        end: Position::new(0, test_case.expr_stmt_end)
                    },
                    returns: true,
                    expression: test_case.expected
                }),
                statements[0]
            )
        } else {
            panic!("p.parse_program() returned None")
        }
    }
}

#[test]
fn test_function_literal_with_name() {
    let input = "var myFunction = fn() { };";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);
        assert_eq!(
            Statement::Declaration(Declaration {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 25)
                },
                name: "myFunction".to_string(),
                mutable: true,
                value: Some(Expression::Lambda(Lambda {
                    span: Span {
                        start: Position::new(0, 18),
                        end: Position::new(0, 25)
                    },
                    parameters: Vec::new(),
                    body: Vec::new(),
                    name: "myFunction".to_string()
                }))
            }),
            statements[0]
        )
    } else {
        panic!("p.parse_program() returned None")
    }
}

#[test]
fn test_delete_statement() {
    let input = "delete foo";

    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();

    check_parser_errors(p);

    if let Some(Node::Program { statements, .. }) = program {
        assert_eq!(statements.len(), 1);

        assert_eq!(
            Statement::Delete(Delete {
                span: Span {
                    start: Position::new(0, 1),
                    end: Position::new(0, 11)
                },
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
