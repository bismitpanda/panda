use std::fmt::Debug;

use strum::Display;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Token {
    pub tok_type: Kind,
    pub tok_lit: String,
}

impl Token {
    pub fn new(tok_type: Kind, tok_lit: String) -> Self {
        Self { tok_type, tok_lit }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.tok_type, self.tok_lit)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Display, Debug)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum Kind {
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
    Modulo,

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

    // Boolean Operators
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
    Nil,
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

fn get_keywords(ident: &str) -> Option<Kind> {
    match ident {
        "fn" => Some(Kind::Function),
        "var" => Some(Kind::Var),
        "true" => Some(Kind::True),
        "false" => Some(Kind::False),
        "if" => Some(Kind::If),
        "else" => Some(Kind::Else),
        "return" => Some(Kind::Return),
        "const" => Some(Kind::Const),
        "nil" => Some(Kind::Nil),
        "while" => Some(Kind::While),
        "for" => Some(Kind::For),
        "in" => Some(Kind::In),
        "class" => Some(Kind::Class),
        "new" => Some(Kind::New),
        "import" => Some(Kind::Import),
        "as" => Some(Kind::As),
        "break" => Some(Kind::Break),
        "continue" => Some(Kind::Continue),
        "delete" => Some(Kind::Delete),
        _ => None,
    }
}

pub fn lookup_ident(ident: &str) -> Kind {
    get_keywords(ident).map_or(Kind::Ident, |tok_type| tok_type)
}
