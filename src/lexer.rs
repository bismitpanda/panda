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
            '%' => Token::new(Kind::Modulo, self.ch.to_string()),
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

        String::from_iter(self.input[pos..self.position].to_vec())
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

        (
            String::from_iter(self.input[pos..self.position].to_vec()),
            num_type,
        )
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

        String::from_iter(self.input[pos..self.position].to_vec())
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

        String::from_iter(self.input[pos..self.position].to_vec())
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    struct TestCase {
        expected_type: Kind,
        expected_lit: &'static str,
    }

    impl TestCase {
        fn new(expected_type: Kind, expected_lit: &'static str) -> Self {
            Self {
                expected_type,
                expected_lit,
            }
        }
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
!-/*%5;
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
            TestCase::new(Kind::Var, "var"),
            TestCase::new(Kind::Ident, "five"),
            TestCase::new(Kind::Assign, "="),
            TestCase::new(Kind::IntLiteral, "5"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::Var, "var"),
            TestCase::new(Kind::Ident, "ten"),
            TestCase::new(Kind::Assign, "="),
            TestCase::new(Kind::IntLiteral, "10"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::Const, "const"),
            TestCase::new(Kind::Ident, "five"),
            TestCase::new(Kind::Assign, "="),
            TestCase::new(Kind::IntLiteral, "5"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::Var, "var"),
            TestCase::new(Kind::Ident, "add"),
            TestCase::new(Kind::Assign, "="),
            TestCase::new(Kind::Function, "fn"),
            TestCase::new(Kind::LParen, "("),
            TestCase::new(Kind::Ident, "x"),
            TestCase::new(Kind::Comma, ","),
            TestCase::new(Kind::Ident, "y"),
            TestCase::new(Kind::RParen, ")"),
            TestCase::new(Kind::LBrace, "{"),
            TestCase::new(Kind::Ident, "x"),
            TestCase::new(Kind::Plus, "+"),
            TestCase::new(Kind::Ident, "y"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::RBrace, "}"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::Var, "var"),
            TestCase::new(Kind::Ident, "result"),
            TestCase::new(Kind::Assign, "="),
            TestCase::new(Kind::Ident, "add"),
            TestCase::new(Kind::LParen, "("),
            TestCase::new(Kind::Ident, "five"),
            TestCase::new(Kind::Comma, ","),
            TestCase::new(Kind::Ident, "ten"),
            TestCase::new(Kind::RParen, ")"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::Bang, "!"),
            TestCase::new(Kind::Minus, "-"),
            TestCase::new(Kind::Slash, "/"),
            TestCase::new(Kind::Asterisk, "*"),
            TestCase::new(Kind::Modulo, "%"),
            TestCase::new(Kind::IntLiteral, "5"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::IntLiteral, "5"),
            TestCase::new(Kind::Lt, "<"),
            TestCase::new(Kind::IntLiteral, "10"),
            TestCase::new(Kind::Gt, ">"),
            TestCase::new(Kind::IntLiteral, "5"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::If, "if"),
            TestCase::new(Kind::LParen, "("),
            TestCase::new(Kind::IntLiteral, "5"),
            TestCase::new(Kind::Lt, "<"),
            TestCase::new(Kind::IntLiteral, "10"),
            TestCase::new(Kind::RParen, ")"),
            TestCase::new(Kind::LBrace, "{"),
            TestCase::new(Kind::Return, "return"),
            TestCase::new(Kind::True, "true"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::RBrace, "}"),
            TestCase::new(Kind::Else, "else"),
            TestCase::new(Kind::LBrace, "{"),
            TestCase::new(Kind::Return, "return"),
            TestCase::new(Kind::False, "false"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::RBrace, "}"),
            TestCase::new(Kind::IntLiteral, "10"),
            TestCase::new(Kind::Eq, "=="),
            TestCase::new(Kind::IntLiteral, "10"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::IntLiteral, "10"),
            TestCase::new(Kind::NotEq, "!="),
            TestCase::new(Kind::IntLiteral, "9"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::StrLiteral, "foobar"),
            TestCase::new(Kind::StrLiteral, ""),
            TestCase::new(Kind::LBracket, "["),
            TestCase::new(Kind::IntLiteral, "1"),
            TestCase::new(Kind::Comma, ","),
            TestCase::new(Kind::IntLiteral, "2"),
            TestCase::new(Kind::RBracket, "]"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::LBrace, "{"),
            TestCase::new(Kind::StrLiteral, "foo"),
            TestCase::new(Kind::Colon, ":"),
            TestCase::new(Kind::StrLiteral, "bar"),
            TestCase::new(Kind::RBrace, "}"),
            TestCase::new(Kind::CharLiteral, "c"),
            TestCase::new(Kind::CharLiteral, "\\x7f"),
            TestCase::new(Kind::CharLiteral, "\\u2022"),
            TestCase::new(Kind::Null, "null"),
            TestCase::new(Kind::Caret, "^"),
            TestCase::new(Kind::BitAnd, "&"),
            TestCase::new(Kind::BitOr, "|"),
            TestCase::new(Kind::GtEq, ">="),
            TestCase::new(Kind::LtEq, "<="),
            TestCase::new(Kind::Shr, ">>"),
            TestCase::new(Kind::Shl, "<<"),
            TestCase::new(Kind::And, "&&"),
            TestCase::new(Kind::Or, "||"),
            TestCase::new(Kind::Dot, "."),
            TestCase::new(Kind::Range, ".."),
            TestCase::new(Kind::Scope, "::"),
            TestCase::new(Kind::Delete, "delete"),
            TestCase::new(Kind::Ident, "foo"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::Class, "class"),
            TestCase::new(Kind::Ident, "test"),
            TestCase::new(Kind::LParen, "("),
            TestCase::new(Kind::RParen, ")"),
            TestCase::new(Kind::LBrace, "{"),
            TestCase::new(Kind::Ident, "a"),
            TestCase::new(Kind::Assign, "="),
            TestCase::new(Kind::IntLiteral, "45"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::Ident, "b"),
            TestCase::new(Kind::LParen, "("),
            TestCase::new(Kind::RParen, ")"),
            TestCase::new(Kind::LBrace, "{"),
            TestCase::new(Kind::RBrace, "}"),
            TestCase::new(Kind::Semicolon, ";"),
            TestCase::new(Kind::RBrace, "}"),
            TestCase::new(Kind::Eol, ""),
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
