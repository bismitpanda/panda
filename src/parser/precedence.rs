use crate::token::Kind;

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
    Modulo,
    Prefix,
    Call,
    Index,
    Method,
    Assign,
}

pub const fn precedences(t: Kind) -> Precedence {
    match t {
        Kind::Range => Precedence::Range,
        Kind::Or => Precedence::Or,
        Kind::And => Precedence::And,
        Kind::Eq | Kind::NotEq | Kind::Lt | Kind::LtEq | Kind::Gt | Kind::GtEq => Precedence::Comp,
        Kind::BitOr => Precedence::BitOr,
        Kind::Caret => Precedence::BitXor,
        Kind::BitAnd => Precedence::BitAnd,
        Kind::Shr | Kind::Shl => Precedence::Shift,
        Kind::Plus | Kind::Minus => Precedence::Sum,
        Kind::Slash | Kind::Asterisk => Precedence::Product,
        Kind::Modulo => Precedence::Modulo,
        Kind::LParen => Precedence::Call,
        Kind::LBracket => Precedence::Index,
        Kind::Dot | Kind::Scope => Precedence::Method,
        Kind::Assign => Precedence::Assign,
        _ => Precedence::Lowest,
    }
}
