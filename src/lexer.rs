use crate::token::{lookup_ident, Kind, Token};

pub struct Lexer {
    input: Vec<char>,

    position: usize,
    read_position: usize,
    len: usize,

    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Self {
            input: input.chars().collect(),

            position: 0,
            read_position: 0,
            len: input.len(),

            ch: '\0',
        };

        l.read_char();

        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.len {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(Kind::Eq, "==".to_string())
                } else {
                    Token::new(Kind::Assign, self.ch.to_string())
                }
            }
            '+' => Token::new(Kind::Plus, self.ch.to_string()),
            '-' => Token::new(Kind::Minus, self.ch.to_string()),
            '!' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(Kind::NotEq, "!=".to_string())
                } else {
                    Token::new(Kind::Bang, self.ch.to_string())
                }
            }
            '/' => Token::new(Kind::Slash, self.ch.to_string()),
            '*' => Token::new(Kind::Asterisk, self.ch.to_string()),
            '<' => {
                if self.peek_char() == Some('<') {
                    self.read_char();
                    Token::new(Kind::Shl, "<<".to_string())
                } else if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(Kind::LtEq, "<=".to_string())
                } else {
                    Token::new(Kind::Lt, self.ch.to_string())
                }
            }
            '>' => {
                if self.peek_char() == Some('>') {
                    self.read_char();
                    Token::new(Kind::Shr, ">>".to_string())
                } else if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(Kind::GtEq, ">=".to_string())
                } else {
                    Token::new(Kind::Gt, self.ch.to_string())
                }
            }
            ';' => Token::new(Kind::Semicolon, self.ch.to_string()),
            '(' => Token::new(Kind::LParen, self.ch.to_string()),
            ')' => Token::new(Kind::RParen, self.ch.to_string()),
            ',' => Token::new(Kind::Comma, self.ch.to_string()),
            '{' => Token::new(Kind::LBrace, self.ch.to_string()),
            '}' => Token::new(Kind::RBrace, self.ch.to_string()),
            '"' => Token::new(Kind::StrLiteral, self.read_string()),
            '[' => Token::new(Kind::LBracket, self.ch.to_string()),
            ']' => Token::new(Kind::RBracket, self.ch.to_string()),
            ':' => {
                if self.peek_char() == Some(':') {
                    self.read_char();
                    Token::new(Kind::Scope, "::".to_string())
                } else {
                    Token::new(Kind::Colon, self.ch.to_string())
                }
            }
            '\'' => Token::new(Kind::CharLiteral, self.read_chars()),
            '^' => Token::new(Kind::Caret, self.ch.to_string()),
            '.' => {
                if self.peek_char() == Some('.') {
                    self.read_char();
                    Token::new(Kind::Range, "..".to_string())
                } else {
                    Token::new(Kind::Dot, self.ch.to_string())
                }
            }
            '&' => {
                if self.peek_char() == Some('&') {
                    self.read_char();
                    Token::new(Kind::And, "&&".to_string())
                } else {
                    Token::new(Kind::BitAnd, self.ch.to_string())
                }
            }
            '|' => {
                if self.peek_char() == Some('|') {
                    self.read_char();
                    Token::new(Kind::Or, "||".to_string())
                } else {
                    Token::new(Kind::BitOr, self.ch.to_string())
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.read_identifier();
                return Token::new(lookup_ident(&ident), ident);
            }
            '0'..='9' => {
                let (lit, num_type) = self.read_number();
                return Token::new(num_type, lit);
            }
            '\0' => Token::new(Kind::Eol, String::new()),
            _ => Token::new(Kind::Illegal, self.ch.to_string()),
        };

        self.read_char();
        tok
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.len {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    fn skip_whitespace(&mut self) {
        while let '\t' | '\n' | '\x0C' | '\r' | ' ' = self.ch {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;

        while let 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' = self.ch {
            self.read_char();
        }

        String::from_iter(&self.input[pos..self.position])
    }

    fn read_number(&mut self) -> (String, Kind) {
        let pos = self.position;

        let mut num_type = Kind::IntLiteral;

        loop {
            if self.ch.is_ascii_digit() {
                self.read_char();
            } else if self.ch == '.' && num_type == Kind::IntLiteral {
                if self
                    .peek_char()
                    .and_then(|ch| if ch.is_ascii_digit() { Some(()) } else { None })
                    .is_some()
                {
                    self.read_char();
                    num_type = Kind::FloatLiteral;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        (String::from_iter(&self.input[pos..self.position]), num_type)
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;

        loop {
            self.read_char();

            if self.ch == '"' {
                break;
            }

            if self.ch == '\\' && self.peek_char() == Some('"') {
                self.read_char();
            }
        }

        String::from_iter(&self.input[pos..self.position])
    }

    fn read_chars(&mut self) -> String {
        let pos = self.position + 1;

        loop {
            self.read_char();

            if self.ch == '\'' {
                break;
            }

            if self.ch == '\\' && self.peek_char() == Some('\'') {
                self.read_char();
            }
        }

        String::from_iter(&self.input[pos..self.position])
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    struct TestCase {
        expected_type: Kind,
        expected_lit: String,
    }

    macro_rules! case {
        (Eol) => {
            TestCase {
                expected_type: Kind::Eol,
                expected_lit: String::new(),
            }
        };

        ($typ:ident) => {
            TestCase {
                expected_type: Kind::$typ,
                expected_lit: stringify!($typ).to_lowercase(),
            }
        };

        ($typ:ident, $lit:literal) => {
            TestCase {
                expected_type: Kind::$typ,
                expected_lit: $lit.to_string(),
            }
        };
    }

    #[test]
    fn test_next_token() {
        let input = r#"var five = 5;
var ten = 10;
const five = 5;

var add = fn(x, y) {
    x + y;
};

var result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"foobar"
""
[1, 2];
{"foo": "bar"}
'c'
'\x7f'
'\u2022'
null
^&| >= <= >> <<
&& || . .. ::

delete foo;

class test() {
    a = 45;

    b() {};
}
"#;

        let test_cases = [
            case!(Var),
            case!(Ident, "five"),
            case!(Assign, "="),
            case!(IntLiteral, "5"),
            case!(Semicolon, ";"),
            case!(Var),
            case!(Ident, "ten"),
            case!(Assign, "="),
            case!(IntLiteral, "10"),
            case!(Semicolon, ";"),
            case!(Const),
            case!(Ident, "five"),
            case!(Assign, "="),
            case!(IntLiteral, "5"),
            case!(Semicolon, ";"),
            case!(Var),
            case!(Ident, "add"),
            case!(Assign, "="),
            case!(Function, "fn"),
            case!(LParen, "("),
            case!(Ident, "x"),
            case!(Comma, ","),
            case!(Ident, "y"),
            case!(RParen, ")"),
            case!(LBrace, "{"),
            case!(Ident, "x"),
            case!(Plus, "+"),
            case!(Ident, "y"),
            case!(Semicolon, ";"),
            case!(RBrace, "}"),
            case!(Semicolon, ";"),
            case!(Var),
            case!(Ident, "result"),
            case!(Assign, "="),
            case!(Ident, "add"),
            case!(LParen, "("),
            case!(Ident, "five"),
            case!(Comma, ","),
            case!(Ident, "ten"),
            case!(RParen, ")"),
            case!(Semicolon, ";"),
            case!(Bang, "!"),
            case!(Minus, "-"),
            case!(Slash, "/"),
            case!(Asterisk, "*"),
            case!(IntLiteral, "5"),
            case!(Semicolon, ";"),
            case!(IntLiteral, "5"),
            case!(Lt, "<"),
            case!(IntLiteral, "10"),
            case!(Gt, ">"),
            case!(IntLiteral, "5"),
            case!(Semicolon, ";"),
            case!(If),
            case!(LParen, "("),
            case!(IntLiteral, "5"),
            case!(Lt, "<"),
            case!(IntLiteral, "10"),
            case!(RParen, ")"),
            case!(LBrace, "{"),
            case!(Return),
            case!(True),
            case!(Semicolon, ";"),
            case!(RBrace, "}"),
            case!(Else),
            case!(LBrace, "{"),
            case!(Return),
            case!(False),
            case!(Semicolon, ";"),
            case!(RBrace, "}"),
            case!(IntLiteral, "10"),
            case!(Eq, "=="),
            case!(IntLiteral, "10"),
            case!(Semicolon, ";"),
            case!(IntLiteral, "10"),
            case!(NotEq, "!="),
            case!(IntLiteral, "9"),
            case!(Semicolon, ";"),
            case!(StrLiteral, "foobar"),
            case!(StrLiteral, ""),
            case!(LBracket, "["),
            case!(IntLiteral, "1"),
            case!(Comma, ","),
            case!(IntLiteral, "2"),
            case!(RBracket, "]"),
            case!(Semicolon, ";"),
            case!(LBrace, "{"),
            case!(StrLiteral, "foo"),
            case!(Colon, ":"),
            case!(StrLiteral, "bar"),
            case!(RBrace, "}"),
            case!(CharLiteral, "c"),
            case!(CharLiteral, "\\x7f"),
            case!(CharLiteral, "\\u2022"),
            case!(Null),
            case!(Caret, "^"),
            case!(BitAnd, "&"),
            case!(BitOr, "|"),
            case!(GtEq, ">="),
            case!(LtEq, "<="),
            case!(Shr, ">>"),
            case!(Shl, "<<"),
            case!(And, "&&"),
            case!(Or, "||"),
            case!(Dot, "."),
            case!(Range, ".."),
            case!(Scope, "::"),
            case!(Delete),
            case!(Ident, "foo"),
            case!(Semicolon, ";"),
            case!(Class),
            case!(Ident, "test"),
            case!(LParen, "("),
            case!(RParen, ")"),
            case!(LBrace, "{"),
            case!(Ident, "a"),
            case!(Assign, "="),
            case!(IntLiteral, "45"),
            case!(Semicolon, ";"),
            case!(Ident, "b"),
            case!(LParen, "("),
            case!(RParen, ")"),
            case!(LBrace, "{"),
            case!(RBrace, "}"),
            case!(Semicolon, ";"),
            case!(RBrace, "}"),
            case!(Eol),
        ];

        let mut l = Lexer::new(input);

        for (i, tt) in test_cases.iter().enumerate() {
            let tok = l.next_token();

            assert_eq!(
                tok.tok_type, tt.expected_type,
                "tests[{}] - tokentype wrong. expected: '{}', got: '{}'",
                i, tt.expected_type, tok.tok_type
            );
            assert_eq!(
                tok.tok_lit, tt.expected_lit,
                "tests[{}] - literal wrong. expected: '{}', got: '{}'",
                i, tt.expected_lit, tok.tok_lit
            );
        }
    }
}
