use crate::token::TokenType;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, strum::Display)]
pub enum Precedence {
    Lowest,
    Range,
    Or,
    And,
    Comp,
    BitOr,
    BitXor,
    BitAnd,
    Shift,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
    Method,
    Assign,
}

pub fn precedences(t: TokenType) -> Precedence {
    match t {
        TokenType::Range => Precedence::Range,
        TokenType::Or => Precedence::Or,
        TokenType::And => Precedence::And,
        TokenType::Eq
        | TokenType::NotEq
        | TokenType::Lt
        | TokenType::LtEq
        | TokenType::Gt
        | TokenType::GtEq => Precedence::Comp,
        TokenType::BitOr => Precedence::BitOr,
        TokenType::Caret => Precedence::BitXor,
        TokenType::BitAnd => Precedence::BitAnd,
        TokenType::Shr | TokenType::Shl => Precedence::Shift,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Slash | TokenType::Asterisk => Precedence::Product,
        TokenType::LParen => Precedence::Call,
        TokenType::LBracket => Precedence::Index,
        TokenType::Dot | TokenType::Scope => Precedence::Method,
        TokenType::Assign => Precedence::Assign,
        _ => Precedence::Lowest,
    }
}
