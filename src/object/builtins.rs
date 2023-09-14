use std::process::exit;

use super::{
    allowed_in_array, intersperse, new_error, AHasher, ArrayObject, BoolObject, BuiltinFunction,
    CharObject, ClassObject, FloatObject, HashObject, HashPair, Hashable, Hasher, IntObject,
    Object, Signed, StdHash, StrObject, ToPrimitive, TypeObject, Write, NULL,
};

pub const BUILTINS: &[(&str, BuiltinFunction)] = &[
    ("exit", |_, _| exit(0)),
    ("type", |_, args| {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got: {}, want: 1",
                args.len()
            ));
        }

        get_type(&args[0])
    }),
    ("input", |_, args| {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got: {}, want: 1",
                args.len()
            ));
        }

        if let Object::Str(StrObject { value }) = &args[0] {
            print!("{}", value);
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();

            Object::Str(StrObject {
                value: input.trim().to_string(),
            })
        } else {
            new_error(format!(
                "cannot use {} as prompt in `input`. expected STR",
                args[0].kind()
            ))
        }
    }),
    ("print", |_, args| {
        let mut str_args = Vec::new();
        for arg in args {
            if let Some(s) = escape_string(&arg.to_string()) {
                str_args.push(s);
            } else {
                return new_error(
                    "cannot print as representation contains invalid escapes".to_string(),
                );
            }
        }

        print!("{}", str_args.join(" "));
        NULL
    }),
    ("println", |_, args| {
        let mut str_args = Vec::new();
        for arg in args {
            if let Some(s) = escape_string(&arg.to_string()) {
                str_args.push(s);
            } else {
                return new_error(
                    "cannot print as representation contains invalid escapes".to_string(),
                );
            }
        }

        println!("{}", str_args.join(" "));
        NULL
    }),
];

fn get_type(obj: &Object) -> Object {
    match obj {
        Object::Class(ClassObject { name, .. }) => {
            let mut hasher = AHasher::default();
            name.hash(&mut hasher);
            Object::Type(TypeObject {
                id: hasher.finish() as usize,
                lit: name.to_string(),
            })
        }
        _ => Object::Type(TypeObject {
            id: 0,
            lit: String::new(),
        }),
    }
}

fn escape_string(s: &str) -> Option<String> {
    let mut chars = s.chars().peekable();
    let mut out = String::new();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.peek() {
                Some(c) => match c {
                    'n' => {
                        chars.next();
                        out.push('\n');
                    }
                    'r' => {
                        chars.next();
                        out.push('\r');
                    }
                    't' => {
                        chars.next();
                        out.push('\t');
                    }
                    '0' => {
                        chars.next();
                        out.push('\0');
                    }
                    '\\' => {
                        chars.next();
                        out.push('\\');
                    }
                    '"' => {
                        chars.next();
                        out.push('"');
                    }
                    _ => return None,
                },
                None => return None,
            }
        } else {
            out.push(c);
        }
    }

    Some(out)
}

pub fn get_builtin_by_name(name: &str) -> Option<BuiltinFunction> {
    for &(func_name, func) in BUILTINS {
        if name == func_name {
            return Some(func);
        }
    }
    None
}

pub const BUILTIN_METHODS: &[&[(&str, BuiltinFunction)]] = &[
    &[
        ("bits", |caller, params| {
            let Object::Int(IntObject { value }) = caller else {
                return new_error(format!("expected INT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Str(StrObject {
                    value: format!("{value:b}"),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("abs", |caller, params| {
            let Object::Int(IntObject { value }) = caller else {
                return new_error(format!("expected INT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Int(IntObject { value: value.abs() })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
    ],
    &[
        ("bits", |caller, params| {
            let Object::Float(FloatObject { value }) = caller else {
                return new_error(format!("expected FLOAT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Str(StrObject {
                    value: format!("{:b}", value.to_bits()),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("abs", |caller, params| {
            let Object::Float(FloatObject { value }) = caller else {
                return new_error(format!("expected FLOAT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Float(FloatObject { value: value.abs() })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
    ],
    &[
        ("len", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Int(IntObject {
                    value: value.len().into(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("contains", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if params.len() != 1 {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            } else {
                match params[0] {
                    Object::Char(CharObject { value: ch }) => Object::Bool(BoolObject {
                        value: value.contains(ch),
                    }),
                    _ => new_error(format!(
                        "STR cannot contain {}, expected: CHAR",
                        params[0].kind()
                    )),
                }
            }
        }),
        ("push", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if params.len() != 1 {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            } else {
                match params[0] {
                    Object::Char(CharObject { value: ch }) => {
                        let mut new_value = value.clone();
                        new_value.push(ch);

                        Object::Str(StrObject { value: new_value })
                    }
                    _ => new_error(format!(
                        "STR cannot contain {}, expected: CHAR",
                        params[0].kind()
                    )),
                }
            }
        }),
        ("split", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if params.len() != 1 {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            } else {
                match params[0] {
                    Object::Char(CharObject { value: ch }) => {
                        let mut new_value = value.clone();
                        new_value.push(ch);

                        Object::Array(ArrayObject {
                            elements: value
                                .split(|c| c == ch)
                                .map(|part| {
                                    Object::Str(StrObject {
                                        value: part.to_string(),
                                    })
                                })
                                .collect(),
                        })
                    }
                    _ => new_error(format!(
                        "STR cannot be split using {}, expected: CHAR",
                        params[0].kind()
                    )),
                }
            }
        }),
        ("toAsciiLowercase", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Str(StrObject {
                    value: value.chars().map(|ch| ch.to_ascii_lowercase()).collect(),
                })
            }
        }),
        ("toAsciiUppercase", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Str(StrObject {
                    value: value.chars().map(|ch| ch.to_ascii_uppercase()).collect(),
                })
            }
        }),
        ("chars", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Array(ArrayObject {
                    elements: value
                        .chars()
                        .map(|value| Object::Char(CharObject { value }))
                        .collect(),
                })
            }
        }),
        ("trimWhitespace", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Str(StrObject {
                    value: value.trim().to_string(),
                })
            }
        }),
        ("isAscii", |caller, params| {
            let Object::Str(StrObject { value }) = caller else {
                return new_error(format!("expected STR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.chars().all(|ch| ch.is_ascii()),
                })
            }
        }),
    ],
    &[
        ("isAlphabetic", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_alphabetic(),
                })
            }
        }),
        ("isAlphanumeric", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_alphanumeric(),
                })
            }
        }),
        ("isAscii", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii(),
                })
            }
        }),
        ("isAsciiAlphabetic", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_alphabetic(),
                })
            }
        }),
        ("isAsciiAlphanumeric", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_alphanumeric(),
                })
            }
        }),
        ("isAsciiControl", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_control(),
                })
            }
        }),
        ("isAsciiDigit", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_digit(),
                })
            }
        }),
        ("isDecDigit", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_digit(),
                })
            }
        }),
        ("isAsciiGraphic", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_graphic(),
                })
            }
        }),
        ("isAsciiLowercase", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_lowercase(),
                })
            }
        }),
        ("isAsciiPunctuation", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_punctuation(),
                })
            }
        }),
        ("isAsciiUppercase", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_uppercase(),
                })
            }
        }),
        ("isAsciiWhitespace", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_whitespace(),
                })
            }
        }),
        ("isControl", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_control(),
                })
            }
        }),
        ("isHexDigit", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_ascii_hexdigit(),
                })
            }
        }),
        ("isOctDigit", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_digit(8),
                })
            }
        }),
        ("isBinDigit", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_digit(2),
                })
            }
        }),
        ("isDigit", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected: CHAR, got: {}", caller.kind()));
            };
            if params.len() != 1 {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            } else {
                match &params[0] {
                    Object::Int(IntObject { value: i }) => Object::Bool(BoolObject {
                        value: value.is_digit(i.to_u32().unwrap()),
                    }),
                    _ => new_error(format!(
                        "expected INT as argument. got: {}",
                        params[0].kind()
                    )),
                }
            }
        }),
        ("isLowercase", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_lowercase(),
                })
            }
        }),
        ("isNumeric", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_numeric(),
                })
            }
        }),
        ("isUppercase", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if !params.is_empty() {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            } else {
                Object::Bool(BoolObject {
                    value: value.is_uppercase(),
                })
            }
        }),
        ("isWhitespace", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(BoolObject {
                    value: value.is_whitespace(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("toAsciiLowercase", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Char(CharObject {
                    value: value.to_ascii_lowercase(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("toAsciiUppercase", |caller, params| {
            let Object::Char(CharObject { value }) = caller else {
                return new_error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Char(CharObject {
                    value: value.to_ascii_uppercase(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
    ],
    &[
        ("len", |caller, params| {
            let Object::Array(ArrayObject { elements }) = caller else {
                return new_error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Int(IntObject {
                    value: elements.len().into(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("contains", |caller, params| {
            let Object::Array(ArrayObject { elements }) = caller else {
                return new_error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() == 1 {
                for elem in elements {
                    if elem.kind() != params[0].kind() {
                        continue;
                    }

                    let contains = match (&params[0], elem) {
                        (
                            Object::Int(IntObject { value: lhs }),
                            Object::Int(IntObject { value: rhs }),
                        ) => *lhs == *rhs,
                        (
                            Object::Float(FloatObject { value: lhs }),
                            Object::Float(FloatObject { value: rhs }),
                        ) => (*lhs - *rhs).abs() < f64::EPSILON,
                        (
                            Object::Bool(BoolObject { value: lhs }),
                            Object::Bool(BoolObject { value: rhs }),
                        ) => *lhs == *rhs,
                        (
                            Object::Char(CharObject { value: lhs }),
                            Object::Char(CharObject { value: rhs }),
                        ) => *lhs == *rhs,
                        (
                            Object::Str(StrObject { value: lhs }),
                            Object::Str(StrObject { value: rhs }),
                        ) => *lhs == *rhs,
                        (Object::Null, Object::Null) => true,
                        (Object::Array(_) | Object::Hash(_), _) => {
                            return new_error(format!(
                                "{} is not comparable. the array may contain same value",
                                params[0].kind()
                            ))
                        }
                        _ => {
                            return new_error(format!("ARRAY cannot contain {}", params[0].kind()))
                        }
                    };

                    if contains {
                        return Object::Bool(BoolObject { value: contains });
                    }
                }

                Object::Bool(BoolObject { value: false })
            } else {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("push", |caller, params| {
            let Object::Array(ArrayObject { elements }) = caller else {
                return new_error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() == 1 {
                if !allowed_in_array(&params[0]) {
                    return new_error(format!("ARRAY cannot contain {}", params[0].kind()));
                }
                let mut new_elements = Vec::with_capacity(elements.len() + 1);
                new_elements.clone_from(elements);

                new_elements.push(params[0].clone());

                Object::Array(ArrayObject {
                    elements: new_elements,
                })
            } else {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("first", |caller, params| {
            let Object::Array(ArrayObject { elements }) = caller else {
                return new_error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() != 1 {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            } else if let Some(first) = elements.first() {
                first.clone()
            } else {
                NULL
            }
        }),
        ("last", |caller, params| {
            let Object::Array(ArrayObject { elements }) = caller else {
                return new_error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() != 1 {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            } else if let Some(last) = elements.last() {
                last.clone()
            } else {
                NULL
            }
        }),
        ("rest", |caller, params| {
            let Object::Array(ArrayObject { elements }) = caller else {
                return new_error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() != 1 {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            } else if let Some((_, rest)) = elements.split_first() {
                Object::Array(ArrayObject {
                    elements: rest.to_vec(),
                })
            } else {
                NULL
            }
        }),
        ("join", |caller, params| {
            let Object::Array(ArrayObject { elements }) = caller else {
                return new_error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() == 1 {
                match &params[0] {
                    Object::Char(CharObject { value }) => {
                        if elements.iter().all(|elem| matches!(elem, Object::Char(_))) {
                            Object::Str(StrObject {
                                value: intersperse(
                                    elements.iter().map(|elem| match elem {
                                        Object::Char(CharObject { value }) => *value,
                                        _ => unreachable!(),
                                    }),
                                    *value,
                                )
                                .iter()
                                .collect(),
                            })
                        } else {
                            new_error("to join, the elements must be CHAR".to_string())
                        }
                    }
                    Object::Str(StrObject { value }) => {
                        if elements.iter().all(|elem| matches!(elem, Object::Str(_))) {
                            Object::Str(StrObject {
                                value: elements
                                    .iter()
                                    .map(|elem| match elem {
                                        Object::Str(StrObject { value }) => value.as_str(),
                                        _ => unreachable!(),
                                    })
                                    .collect::<Vec<_>>()
                                    .join(value.as_str())
                                    .chars()
                                    .collect(),
                            })
                        } else {
                            new_error(String::new())
                        }
                    }
                    _ => new_error(format!(
                        "expected STR or CHAR parameter. got: {}",
                        params[0].kind()
                    )),
                }
            } else {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
    ],
    &[
        ("len", |caller, params| {
            let Object::Hash(HashObject { pairs }) = caller else {
                return new_error(format!("expected HASH, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Int(IntObject {
                    value: pairs.len().into(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("keys", |caller, params| {
            let Object::Hash(HashObject { pairs }) = caller else {
                return new_error(format!("expected HASH, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Array(ArrayObject {
                    elements: pairs.values().map(|pair| pair.key.to_object()).collect(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("values", |caller, params| {
            let Object::Hash(HashObject { pairs }) = caller else {
                return new_error(format!("expected HASH, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Array(ArrayObject {
                    elements: pairs.values().map(|pair| pair.value.clone()).collect(),
                })
            } else {
                new_error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("insert", |caller, params| {
            let Object::Hash(HashObject { pairs }) = caller else {
                return new_error(format!("expected HASH, got {}", caller.kind()));
            };
            if params.len() == 2 {
                let mut pairs = pairs.clone();
                let Some(hashable) = Hashable::from_object(&params[0]) else {
                    return new_error(format!("unusable as hash key: {}", params[0].kind()));
                };
                pairs.insert(
                    hashable.hash_key(),
                    HashPair {
                        key: hashable,
                        value: params[1].clone(),
                    },
                );
                Object::Hash(HashObject { pairs })
            } else {
                new_error(format!("expected 2 parameters. got: {}", params.len()))
            }
        }),
        ("contains", |caller, params| {
            let Object::Hash(HashObject { pairs }) = caller else {
                return new_error(format!("expected HASH, got {}", caller.kind()));
            };
            if params.len() == 1 {
                let Some(hashable) = Hashable::from_object(&params[0]) else {
                    return new_error(format!("unusable as hash key: {}", params[0].kind()));
                };

                Object::Bool(BoolObject {
                    value: pairs.contains_key(&hashable.hash_key()),
                })
            } else {
                new_error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
    ],
];
