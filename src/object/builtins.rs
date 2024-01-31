use std::{io::Write, process::exit};

use super::{
    allowed_in_array, intersperse, AHasher, Array, Bool, BuiltinFunction, Char, Class, Dict,
    DictPair, Float, Hashable, Hasher, Int, Object, StdHash, Str, Type,
};

pub const BUILTINS: &[(&str, BuiltinFunction)] = &[
    ("exit", |_, _| exit(0)),
    ("type", |_, args| {
        if args.len() != 1 {
            return Object::error(format!(
                "wrong number of arguments. got: {}, want: 1",
                args.len()
            ));
        }

        get_type(&args[0])
    }),
    ("input", |_, args| {
        if args.len() != 1 {
            return Object::error(format!(
                "wrong number of arguments. got: {}, want: 1",
                args.len()
            ));
        }

        if let Object::Str(Str { value }) = &args[0] {
            print!("{value}");
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();

            Object::Str(Str {
                value: input.trim().to_string(),
            })
        } else {
            Object::error(format!(
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
                return Object::error(
                    "cannot print as representation contains invalid escapes".to_string(),
                );
            }
        }

        print!("{}", str_args.join(" "));
        Object::Nil
    }),
    ("println", |_, args| {
        let mut str_args = Vec::new();
        for arg in args {
            if let Some(s) = escape_string(&arg.to_string()) {
                str_args.push(s);
            } else {
                return Object::error(
                    "cannot print as representation contains invalid escapes".to_string(),
                );
            }
        }

        println!("{}", str_args.join(" "));
        Object::Nil
    }),
];

fn get_type(obj: &Object) -> Object {
    match obj {
        Object::Class(Class { name, .. }) => {
            let mut hasher = AHasher::default();
            name.hash(&mut hasher);
            Object::Type(Type {
                id: usize::try_from(hasher.finish()).unwrap(),
                lit: name.to_string(),
            })
        }
        _ => Object::Type(Type {
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
    // Int
    &[
        ("bits", |caller, params| {
            let Object::Int(Int { value }) = caller else {
                return Object::error(format!("expected INT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Str(Str {
                    value: format!("{value:b}"),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("abs", |caller, params| {
            let Object::Int(Int { value }) = caller else {
                return Object::error(format!("expected INT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::int(value.abs())
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
    ],
    // Float
    &[
        ("bits", |caller, params| {
            let Object::Float(Float { value }) = caller else {
                return Object::error(format!("expected FLOAT, got {}", caller.kind()));
            };

            if params.is_empty() {
                Object::Str(Str {
                    value: format!("{:b}", value.to_bits()),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("abs", |caller, params| {
            let Object::Float(Float { value }) = caller else {
                return Object::error(format!("expected FLOAT, got {}", caller.kind()));
            };

            if params.is_empty() {
                Object::float(value.abs())
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isInf", |caller, params| {
            let Object::Float(Float { value }) = caller else {
                return Object::error(format!("expected FLOAT, got {}", caller.kind()));
            };

            if params.is_empty() {
                Object::Bool(Bool {
                    value: *value == f64::INFINITY,
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isNegInf", |caller, params| {
            let Object::Float(Float { value }) = caller else {
                return Object::error(format!("expected FLOAT, got {}", caller.kind()));
            };

            if params.is_empty() {
                Object::Bool(Bool {
                    value: *value == f64::NEG_INFINITY,
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isNaN", |caller, params| {
            let Object::Float(Float { value }) = caller else {
                return Object::error(format!("expected FLOAT, got {}", caller.kind()));
            };

            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_nan(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
    ],
    // Str
    &[
        ("len", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::int(value.len() as isize)
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("contains", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.len() == 1 {
                match params[0] {
                    Object::Char(Char { value: ch }) => Object::Bool(Bool {
                        value: value.contains(ch),
                    }),
                    _ => Object::error(format!(
                        "STR cannot contain {}, expected: CHAR",
                        params[0].kind()
                    )),
                }
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("push", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.len() == 1 {
                match params[0] {
                    Object::Char(Char { value: ch }) => {
                        let mut new_value = value.clone();
                        new_value.push(ch);

                        Object::Str(Str { value: new_value })
                    }
                    _ => Object::error(format!(
                        "STR cannot contain {}, expected: CHAR",
                        params[0].kind()
                    )),
                }
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("split", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.len() == 1 {
                match params[0] {
                    Object::Char(Char { value: ch }) => Object::array(
                        value
                            .split(|c| c == ch)
                            .map(|part| {
                                Object::Str(Str {
                                    value: part.to_string(),
                                })
                            })
                            .collect(),
                    ),
                    _ => Object::error(format!(
                        "STR cannot be split using {}, expected: CHAR",
                        params[0].kind()
                    )),
                }
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("toAsciiLowercase", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Str(Str {
                    value: value.chars().map(|ch| ch.to_ascii_lowercase()).collect(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("toAsciiUppercase", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Str(Str {
                    value: value.chars().map(|ch| ch.to_ascii_uppercase()).collect(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("chars", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::array(value.chars().map(Object::char).collect())
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("trimWhitespace", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Str(Str {
                    value: value.trim().to_string(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAscii", |caller, params| {
            let Object::Str(Str { value }) = caller else {
                return Object::error(format!("expected STR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.chars().all(|ch| ch.is_ascii()),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
    ],
    // Char
    &[
        ("isAlphabetic", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_alphabetic(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAlphanumeric", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_alphanumeric(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAscii", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiAlphabetic", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_alphabetic(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiAlphanumeric", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_alphanumeric(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiControl", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_control(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiDigit", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_digit(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isDecDigit", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_digit(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiGraphic", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_graphic(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiLowercase", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_lowercase(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiPunctuation", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_punctuation(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiUppercase", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_uppercase(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isAsciiWhitespace", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_whitespace(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isControl", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_control(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isHexDigit", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_ascii_hexdigit(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isOctDigit", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_digit(8),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isBinDigit", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_digit(2),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isDigit", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected: CHAR, got: {}", caller.kind()));
            };
            if params.len() == 1 {
                match &params[0] {
                    Object::Int(Int { value: i }) => Object::Bool(Bool {
                        value: value.is_digit((*i).try_into().unwrap()),
                    }),
                    _ => Object::error(format!(
                        "expected INT as argument. got: {}",
                        params[0].kind()
                    )),
                }
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("isLowercase", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_lowercase(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isNumeric", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_numeric(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isUppercase", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_uppercase(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("isWhitespace", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::Bool(Bool {
                    value: value.is_whitespace(),
                })
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("toAsciiLowercase", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::char(value.to_ascii_lowercase())
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("toAsciiUppercase", |caller, params| {
            let Object::Char(Char { value }) = caller else {
                return Object::error(format!("expected CHAR, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::char(value.to_ascii_uppercase())
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
    ],
    // Array
    &[
        ("len", |caller, params| {
            let Object::Array(Array { elements }) = caller else {
                return Object::error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::int(elements.len() as isize)
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("contains", |caller, params| {
            let Object::Array(Array { elements }) = caller else {
                return Object::error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() == 1 {
                for elem in elements {
                    if elem.kind() != params[0].kind() {
                        continue;
                    }

                    let contains = match (&params[0], elem) {
                        (Object::Int(Int { value: lhs }), Object::Int(Int { value: rhs })) => {
                            *lhs == *rhs
                        }
                        (
                            Object::Float(Float { value: lhs }),
                            Object::Float(Float { value: rhs }),
                        ) => (*lhs - *rhs).abs() < f64::EPSILON,
                        (Object::Bool(Bool { value: lhs }), Object::Bool(Bool { value: rhs })) => {
                            *lhs == *rhs
                        }
                        (Object::Char(Char { value: lhs }), Object::Char(Char { value: rhs })) => {
                            *lhs == *rhs
                        }
                        (Object::Str(Str { value: lhs }), Object::Str(Str { value: rhs })) => {
                            *lhs == *rhs
                        }
                        (Object::Nil, Object::Nil) => true,
                        (Object::Array(_) | Object::Dict(_), _) => {
                            return Object::error(format!(
                                "{} is not comparable. the array may contain same value",
                                params[0].kind()
                            ))
                        }
                        _ => {
                            return Object::error(format!(
                                "ARRAY cannot contain {}",
                                params[0].kind()
                            ))
                        }
                    };

                    if contains {
                        return Object::Bool(Bool { value: contains });
                    }
                }

                Object::Bool(Bool { value: false })
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("push", |caller, params| {
            let Object::Array(Array { elements }) = caller else {
                return Object::error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() == 1 {
                if !allowed_in_array(&params[0]) {
                    return Object::error(format!("ARRAY cannot contain {}", params[0].kind()));
                }

                let mut new_elements = Vec::with_capacity(elements.len() + 1);
                new_elements.clone_from(elements);

                new_elements.push(params[0].clone());

                Object::array(new_elements)
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
        ("first", |caller, params| {
            let Object::Array(Array { elements }) = caller else {
                return Object::error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() != 1 {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            } else if let Some(first) = elements.first() {
                first.clone()
            } else {
                Object::Nil
            }
        }),
        ("last", |caller, params| {
            let Object::Array(Array { elements }) = caller else {
                return Object::error(format!("expected ARRAY, got {}", caller.kind()));
            };
            if params.len() != 1 {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            } else if let Some(last) = elements.last() {
                last.clone()
            } else {
                Object::Nil
            }
        }),
        ("rest", |caller, params| {
            let Object::Array(Array { elements }) = caller else {
                return Object::error(format!("expected ARRAY, got {}", caller.kind()));
            };

            if params.len() != 1 {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            } else if let Some((_, rest)) = elements.split_first() {
                Object::array(rest.to_vec())
            } else {
                Object::Nil
            }
        }),
        ("join", |caller, params| {
            let Object::Array(Array { elements }) = caller else {
                return Object::error(format!("expected ARRAY, got {}", caller.kind()));
            };

            if params.len() == 1 {
                match &params[0] {
                    Object::Char(Char { value }) => {
                        if elements.iter().all(|elem| matches!(elem, Object::Char(_))) {
                            Object::Str(Str {
                                value: intersperse(
                                    elements.iter().map(|elem| match elem {
                                        Object::Char(Char { value }) => *value,
                                        _ => unsafe { std::hint::unreachable_unchecked() },
                                    }),
                                    *value,
                                )
                                .iter()
                                .collect(),
                            })
                        } else {
                            Object::error("to join, the elements must be CHAR".to_string())
                        }
                    }

                    Object::Str(Str { value }) => {
                        if elements.iter().all(|elem| matches!(elem, Object::Str(_))) {
                            Object::Str(Str {
                                value: elements
                                    .iter()
                                    .map(|elem| match elem {
                                        Object::Str(Str { value }) => value.as_str(),
                                        _ => unsafe { std::hint::unreachable_unchecked() },
                                    })
                                    .collect::<Vec<_>>()
                                    .join(value.as_str())
                                    .chars()
                                    .collect(),
                            })
                        } else {
                            Object::error(String::new())
                        }
                    }
                    _ => Object::error(format!(
                        "expected STR or CHAR parameter. got: {}",
                        params[0].kind()
                    )),
                }
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
    ],
    // HashMap
    &[
        ("len", |caller, params| {
            let Object::Dict(Dict { pairs }) = caller else {
                return Object::error(format!("expected DICT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::int(pairs.len() as isize)
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("keys", |caller, params| {
            let Object::Dict(Dict { pairs }) = caller else {
                return Object::error(format!("expected DICT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::array(pairs.values().map(|pair| pair.key.to_object()).collect())
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("values", |caller, params| {
            let Object::Dict(Dict { pairs }) = caller else {
                return Object::error(format!("expected DICT, got {}", caller.kind()));
            };
            if params.is_empty() {
                Object::array(pairs.values().map(|pair| pair.value.clone()).collect())
            } else {
                Object::error(format!("expected 0 parameters. got: {}", params.len()))
            }
        }),
        ("insert", |caller, params| {
            let Object::Dict(Dict { pairs }) = caller else {
                return Object::error(format!("expected DICT, got {}", caller.kind()));
            };

            if params.len() == 2 {
                let mut pairs = pairs.clone();
                let Some(hashable) = Hashable::from_object(&params[0]) else {
                    return Object::error(format!("unusable as hash key: {}", params[0].kind()));
                };

                pairs.insert(
                    hashable.hash(),
                    DictPair {
                        key: hashable,
                        value: params[1].clone(),
                    },
                );

                Object::dict(pairs)
            } else {
                Object::error(format!("expected 2 parameters. got: {}", params.len()))
            }
        }),
        ("contains", |caller, params| {
            let Object::Dict(Dict { pairs }) = caller else {
                return Object::error(format!("expected DICT, got {}", caller.kind()));
            };
            if params.len() == 1 {
                let Some(hashable) = Hashable::from_object(&params[0]) else {
                    return Object::error(format!("unusable as hash key: {}", params[0].kind()));
                };

                Object::Bool(Bool {
                    value: pairs.contains_key(&hashable.hash()),
                })
            } else {
                Object::error(format!("expected 1 parameters. got: {}", params.len()))
            }
        }),
    ],
];
