use crate::token::{lookup_ident, Position, Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    input_position: Position,

    position: usize,
    read_position: usize,
    len: usize,

    ch: char,
}

macro_rules! token {
    ($tok:expr, $lit:expr, $pos:expr) => {
        Token::new($tok, $lit, $pos)
    };
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input: input.chars().collect(),
            input_position: Position::new(0, 0),

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

        if self.ch == '\n' {
            self.input_position.new_line();
        } else {
            self.input_position.move_forward();
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
                    token!(TokenType::Eq, "==".to_string(), self.input_position)
                } else {
                    token!(TokenType::Assign, self.ch.to_string(), self.input_position)
                }
            }
            '+' => token!(TokenType::Plus, self.ch.to_string(), self.input_position),
            '-' => token!(TokenType::Minus, self.ch.to_string(), self.input_position),
            '!' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    token!(TokenType::NotEq, "!=".to_string(), self.input_position)
                } else {
                    token!(TokenType::Bang, self.ch.to_string(), self.input_position)
                }
            }
            '/' => token!(TokenType::Slash, self.ch.to_string(), self.input_position),
            '*' => {
                token!(
                    TokenType::Asterisk,
                    self.ch.to_string(),
                    self.input_position
                )
            }
            '<' => {
                if self.peek_char() == Some('<') {
                    self.read_char();
                    token!(TokenType::Shl, "<<".to_string(), self.input_position)
                } else if self.peek_char() == Some('=') {
                    self.read_char();
                    token!(TokenType::LtEq, "<=".to_string(), self.input_position)
                } else {
                    token!(TokenType::Lt, self.ch.to_string(), self.input_position)
                }
            }
            '>' => {
                if self.peek_char() == Some('>') {
                    self.read_char();
                    token!(TokenType::Shr, ">>".to_string(), self.input_position)
                } else if self.peek_char() == Some('=') {
                    self.read_char();
                    token!(TokenType::GtEq, ">=".to_string(), self.input_position)
                } else {
                    token!(TokenType::Gt, self.ch.to_string(), self.input_position)
                }
            }
            ';' => {
                token!(
                    TokenType::Semicolon,
                    self.ch.to_string(),
                    self.input_position
                )
            }
            '(' => token!(TokenType::LParen, self.ch.to_string(), self.input_position),
            ')' => token!(TokenType::RParen, self.ch.to_string(), self.input_position),
            ',' => token!(TokenType::Comma, self.ch.to_string(), self.input_position),
            '{' => token!(TokenType::LBrace, self.ch.to_string(), self.input_position),
            '}' => token!(TokenType::RBrace, self.ch.to_string(), self.input_position),
            '"' => {
                let pos = self.input_position;
                token!(TokenType::StrLiteral, self.read_string(), pos)
            }
            '[' => {
                token!(
                    TokenType::LBracket,
                    self.ch.to_string(),
                    self.input_position
                )
            }
            ']' => {
                token!(
                    TokenType::RBracket,
                    self.ch.to_string(),
                    self.input_position
                )
            }
            ':' => {
                if self.peek_char() == Some(':') {
                    self.read_char();
                    token!(TokenType::Scope, "::".to_string(), self.input_position)
                } else {
                    token!(TokenType::Colon, self.ch.to_string(), self.input_position)
                }
            }
            '\'' => {
                let pos = self.input_position;
                token!(TokenType::CharLiteral, self.read_chars(), pos)
            }
            '^' => token!(TokenType::Caret, self.ch.to_string(), self.input_position),
            '.' => {
                if self.peek_char() == Some('.') {
                    self.read_char();
                    token!(TokenType::Range, "..".to_string(), self.input_position)
                } else {
                    token!(TokenType::Dot, self.ch.to_string(), self.input_position)
                }
            }
            '&' => {
                if self.peek_char() == Some('&') {
                    self.read_char();
                    token!(TokenType::And, "&&".to_string(), self.input_position)
                } else {
                    token!(TokenType::BitAnd, self.ch.to_string(), self.input_position)
                }
            }
            '|' => {
                if self.peek_char() == Some('|') {
                    self.read_char();
                    token!(TokenType::Or, "||".to_string(), self.input_position)
                } else {
                    token!(TokenType::BitOr, self.ch.to_string(), self.input_position)
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let pos = self.input_position;
                let ident = self.read_identifier();
                return token!(lookup_ident(&ident), ident, pos);
            }
            '0'..='9' => {
                let pos = self.input_position;
                let (lit, num_type) = self.read_number();
                return token!(num_type, lit, pos);
            }
            '\0' => token!(TokenType::Eol, String::new(), self.input_position),
            _ => {
                token!(TokenType::Illegal, self.ch.to_string(), self.input_position)
            }
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

    fn read_number(&mut self) -> (String, TokenType) {
        let pos = self.position;

        let mut num_type = TokenType::IntLiteral;

        loop {
            if self.ch.is_ascii_digit() {
                self.read_char();
            } else if self.ch == '.' && num_type == TokenType::IntLiteral {
                if self
                    .peek_char()
                    .and_then(|ch| if ch.is_ascii_digit() { Some(()) } else { None })
                    .is_some()
                {
                    self.read_char();
                    num_type = TokenType::FloatLiteral;
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
    use super::*;
    use pretty_assertions::assert_eq;

    struct TestCase {
        expected_type: TokenType,
        expected_lit: String,
    }

    macro_rules! case {
        ($typ:ident, $lit:literal) => {
            TestCase {
                expected_type: TokenType::$typ,
                expected_lit: $lit.to_string(),
            }
        };

        ($typ:ident) => {
            TestCase {
                expected_type: TokenType::$typ,
                expected_lit: String::new(),
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
"#
        .to_string();

        let test_cases = [
            case!(Var, "var"),
            case!(Ident, "five"),
            case!(Assign, "="),
            case!(IntLiteral, "5"),
            case!(Semicolon, ";"),
            case!(Var, "var"),
            case!(Ident, "ten"),
            case!(Assign, "="),
            case!(IntLiteral, "10"),
            case!(Semicolon, ";"),
            case!(Const, "const"),
            case!(Ident, "five"),
            case!(Assign, "="),
            case!(IntLiteral, "5"),
            case!(Semicolon, ";"),
            case!(Var, "var"),
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
            case!(Var, "var"),
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
            case!(If, "if"),
            case!(LParen, "("),
            case!(IntLiteral, "5"),
            case!(Lt, "<"),
            case!(IntLiteral, "10"),
            case!(RParen, ")"),
            case!(LBrace, "{"),
            case!(Return, "return"),
            case!(True, "true"),
            case!(Semicolon, ";"),
            case!(RBrace, "}"),
            case!(Else, "else"),
            case!(LBrace, "{"),
            case!(Return, "return"),
            case!(False, "false"),
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
            case!(Null, "null"),
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
            case!(Delete, "delete"),
            case!(Ident, "foo"),
            case!(Semicolon, ";"),
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
