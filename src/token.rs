use std::{
    fmt::{Debug, Display},
    ops::Add,
};

use strum::Display;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }

    pub fn move_forward(&mut self) {
        self.col += 1;
    }

    pub fn new_line(&mut self) {
        self.col = 0;
        self.row += 1;
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.row)
    }
}

impl Add for Position {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            col: self.col + rhs.col,
            row: self.row + rhs.row,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Token {
    pub position: Position,
    pub tok_type: TokenType,
    pub tok_lit: String,
}

impl Token {
    pub fn new(tok_type: TokenType, tok_lit: String, position: Position) -> Self {
        Self {
            position,
            tok_type,
            tok_lit,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.tok_type, self.tok_lit)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Display, Debug)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum TokenType {
    // Misc
    Illegal,
    Eol,

    // Literals
    Ident,
    IntLiteral,
    StrLiteral,
    CharLiteral,
    FloatLiteral,

    // Binary Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    // Equality Operators
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,

    // Bitwise Operators
    Caret,
    BitAnd,
    BitOr,
    Shr,
    Shl,

    // Binary Operators
    And,
    Or,

    // Rest Symbols
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Dot,
    Range,
    Scope,

    // Keywords
    Function,
    Var,
    True,
    False,
    If,
    Else,
    Return,
    Const,
    Null,
    While,
    For,
    In,
    Class,
    New,
    Import,
    As,
    Break,
    Continue,
    Delete,
}

fn get_keywords(ident: &str) -> Option<TokenType> {
    match ident {
        "fn" => Some(TokenType::Function),
        "var" => Some(TokenType::Var),
        "true" => Some(TokenType::True),
        "false" => Some(TokenType::False),
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "return" => Some(TokenType::Return),
        "const" => Some(TokenType::Const),
        "null" => Some(TokenType::Null),
        "while" => Some(TokenType::While),
        "for" => Some(TokenType::For),
        "in" => Some(TokenType::In),
        "class" => Some(TokenType::Class),
        "new" => Some(TokenType::New),
        "import" => Some(TokenType::Import),
        "as" => Some(TokenType::As),
        "break" => Some(TokenType::Break),
        "continue" => Some(TokenType::Continue),
        "delete" => Some(TokenType::Delete),
        _ => None,
    }
}

pub fn lookup_ident(ident: &str) -> TokenType {
    get_keywords(ident).map_or(TokenType::Ident, |tok_type| tok_type)
}
