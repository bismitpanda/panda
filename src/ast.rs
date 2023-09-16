use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

use num_bigint::BigInt;

use crate::token::Position;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}-{})", self.start, self.end)
    }
}

pub type BlockStatement = Vec<Statement>;
pub type Ident = String;

#[derive(Clone, PartialEq, Debug)]
pub enum Node {
    Program {
        span: Span,
        statements: BlockStatement,
    },
    Stmt(Statement),
    Expr(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program { statements, .. } => {
                for stmt in statements {
                    write!(f, "{stmt}")?;
                }
            }

            Self::Stmt(stmt) => write!(f, "{stmt}")?,

            Self::Expr(expr) => write!(f, "{expr}")?,
        }

        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Declaration {
    pub span: Span,
    pub name: Ident,
    pub mutable: bool,
    pub value: Option<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Return {
    pub span: Span,
    pub return_value: Expression,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Delete {
    pub span: Span,
    pub delete_ident: Ident,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ExpressionStmt {
    pub span: Span,
    pub returns: bool,
    pub expression: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub span: Span,
    pub ident: Ident,
    pub parameters: Vec<Ident>,
    pub body: BlockStatement,
}

#[derive(Clone, PartialEq, Debug)]
pub struct While {
    pub span: Span,
    pub condition: Expression,
    pub body: BlockStatement,
}

#[derive(Clone, PartialEq, Debug)]
pub struct For {
    pub span: Span,
    pub ident: Ident,
    pub iterator: Expression,
    pub body: BlockStatement,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassDecl {
    pub span: Span,
    pub ident: Ident,
    pub initializers: Vec<Ident>,
    pub body: Vec<ClassStatement>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Import {
    pub span: Span,
    pub path: Ident,
    pub alias: Option<Ident>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ClassStatement {
    Declaration(Declaration),
    Function(Function),
}

impl ClassStatement {
    pub fn to_statement(&self) -> Statement {
        match self {
            Self::Declaration(ast_node) => Statement::Declaration(ast_node.clone()),
            Self::Function(ast_node) => Statement::Function(ast_node.clone()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Declaration(Declaration),
    Return(Return),
    Delete(Delete),
    ExpressionStmt(ExpressionStmt),
    Function(Function),
    While(While),
    For(For),
    ClassDecl(ClassDecl),
    Import(Import),
    Break(Span),
    Continue(Span),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declaration(Declaration {
                name,
                mutable,
                value,
                ..
            }) => write!(
                f,
                "{} {}{};",
                if *mutable { "var" } else { "const" },
                name,
                value
                    .clone()
                    .map_or_else(String::new, |value| format!("= {value}"))
            ),

            Self::ClassDecl(ClassDecl {
                ident,
                initializers,
                body,
                ..
            }) => write!(
                f,
                "class({}) {} {}",
                initializers.join(", "),
                ident,
                body.iter()
                    .map(|stmt| stmt.to_statement().to_string())
                    .collect::<String>()
            ),

            Self::ExpressionStmt(ExpressionStmt {
                returns,
                expression,
                ..
            }) => write!(f, "{}{}", expression, if *returns { "" } else { ";" }),

            Self::For(For {
                ident,
                iterator,
                body,
                ..
            }) => write!(
                f,
                "for ({} in {}) {}",
                ident,
                iterator,
                body.iter().map(ToString::to_string).collect::<String>()
            ),

            Self::Function(Function {
                ident,
                parameters,
                body,
                ..
            }) => write!(
                f,
                "fn {}({}) {}",
                ident,
                parameters.join(", "),
                body.iter().map(ToString::to_string).collect::<String>()
            ),

            Self::Import(Import { path, alias, .. }) => write!(
                f,
                "import \"{}\"{}",
                path,
                alias
                    .clone()
                    .map_or_else(String::new, |alias| format!(" as {alias}"))
            ),

            Self::Return(Return { return_value, .. }) => write!(
                f,
                "return {};",
                if matches!(
                    return_value,
                    Expression::Literal(Literal { lit: Lit::Null, .. })
                ) {
                    String::new()
                } else {
                    return_value.to_string()
                }
            ),

            Self::While(While {
                condition, body, ..
            }) => write!(
                f,
                "while ({}) {}",
                condition,
                body.iter().map(ToString::to_string).collect::<String>()
            ),

            Self::Break(_) => write!(f, "break"),

            Self::Continue(_) => write!(f, "continue"),

            Self::Delete(Delete { delete_ident, .. }) => write!(f, "delete {delete_ident};"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Method {
    pub span: Span,
    pub left: Box<Expression>,
    pub method: Ident,
    pub arguments: Option<Vec<Expression>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Constructor {
    pub span: Span,
    pub constructable: Constructable,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Range {
    pub span: Span,
    pub start: Box<Expression>,
    pub stop: Box<Expression>,
    pub step: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Identifier {
    pub span: Span,
    pub value: Ident,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Assign {
    pub span: Span,
    pub to: Assignable,
    pub value: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Prefix {
    pub span: Span,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Infix {
    pub span: Span,
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct If {
    pub span: Span,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Lambda {
    pub span: Span,
    pub parameters: Vec<Ident>,
    pub body: BlockStatement,
    pub name: Ident,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Call {
    pub span: Span,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Index {
    pub span: Span,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal {
    pub span: Span,
    pub lit: Lit,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    pub span: Span,
    pub module: Ident,
    pub member: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Method(Method),
    Constructor(Constructor),
    Range(Range),
    Identifier(Identifier),
    Assign(Assign),
    Prefix(Prefix),
    Infix(Infix),
    If(If),
    Lambda(Lambda),
    Call(Call),
    Index(Index),
    Literal(Literal),
    Scope(Scope),
}

impl Expression {
    pub fn get_span(&self) -> Span {
        match self {
            Self::Method(node) => node.span,
            Self::Constructor(node) => node.span,
            Self::Range(node) => node.span,
            Self::Identifier(node) => node.span,
            Self::Assign(node) => node.span,
            Self::Prefix(node) => node.span,
            Self::Infix(node) => node.span,
            Self::If(node) => node.span,
            Self::Lambda(node) => node.span,
            Self::Call(node) => node.span,
            Self::Index(node) => node.span,
            Self::Literal(node) => node.span,
            Self::Scope(node) => node.span,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(Assign { to, value, .. }) => write!(f, "{to} = {value};"),

            Self::Call(Call {
                function,
                arguments,
                ..
            }) => write!(
                f,
                "{}({})",
                function,
                arguments
                    .iter()
                    .map(ToString::to_string)
                    .collect::<String>()
            ),

            Self::Constructor(Constructor { constructable, .. }) => {
                write!(f, "new {constructable};")
            }

            Self::Lambda(Lambda {
                parameters,
                body,
                name,
                ..
            }) => write!(
                f,
                "{}fn({}) {}",
                if name.is_empty() {
                    String::new()
                } else {
                    format!("<{name}>")
                },
                parameters.join(", "),
                body.iter().map(ToString::to_string).collect::<String>()
            ),

            Self::Identifier(Identifier { value, .. }) => write!(f, "{value}"),

            Self::If(If {
                condition,
                consequence,
                alternative,
                ..
            }) => write!(
                f,
                "if {} {}{}",
                condition,
                consequence
                    .iter()
                    .map(ToString::to_string)
                    .collect::<String>(),
                alternative.as_ref().map_or_else(String::new, |alt| format!(
                    "else {}",
                    alt.iter().map(ToString::to_string).collect::<String>()
                ))
            ),

            Self::Index(Index { left, index, .. }) => write!(f, "({left}[{index}])"),

            Self::Infix(Infix {
                left,
                operator,
                right,
                ..
            }) => write!(f, "({left} {operator} {right})"),

            Self::Literal(Literal { lit, .. }) => write!(f, "{lit}"),

            Self::Method(Method {
                left,
                method,
                arguments,
                ..
            }) => write!(
                f,
                "{}.{}{}",
                left,
                method,
                arguments
                    .as_ref()
                    .map_or_else(String::new, |arguments| format!(
                        "({})",
                        arguments
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    ))
            ),

            Self::Prefix(Prefix {
                operator, right, ..
            }) => {
                write!(f, "({operator}{right})")
            }

            Self::Range(Range {
                start, stop, step, ..
            }) => write!(
                f,
                "{start}..{stop}{}",
                step.as_ref()
                    .map_or_else(String::new, |step| format!("..{step}"))
            ),

            Self::Scope(Scope { module, member, .. }) => write!(f, "{module}::{member}"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Lit {
    Int {
        value: BigInt,
    },
    Float {
        value: f64,
    },
    Bool {
        value: bool,
    },
    Null,
    Str {
        value: String,
    },
    Char {
        value: char,
    },
    Array {
        elements: Vec<Expression>,
    },
    Hash {
        pairs: Vec<(Expression, Expression)>,
    },
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array { elements } => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Bool { value } => write!(f, "{value}"),

            Self::Char { value } => write!(f, "{value}"),

            Self::Float { value } => write!(f, "{value}"),

            Self::Hash { pairs } => write!(
                f,
                "{{{}}}",
                pairs
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Int { value } => write!(f, "{value}"),

            Self::Null => write!(f, "null"),

            Self::Str { value } => write!(f, "{value}"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Operator {
    Eq,
    NotEq,
    And,
    Or,
    Bang,
    Add,
    Sub,
    Mul,
    Div,
    BitXor,
    BitAnd,
    BitOr,
    Shr,
    Shl,
    Gt,
    Lt,
    GtEq,
    LtEq,
}

impl FromStr for Operator {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let op = match s {
            "==" => Self::Eq,
            "!=" => Self::NotEq,
            "&&" => Self::And,
            "||" => Self::Or,
            "!" => Self::Bang,
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "^" => Self::BitXor,
            "&" => Self::BitAnd,
            "|" => Self::BitOr,
            ">>" => Self::Shr,
            "<<" => Self::Shl,
            ">" => Self::Gt,
            "<" => Self::Lt,
            ">=" => Self::GtEq,
            "<=" => Self::LtEq,
            _ => return Err(format!("invalid operator: {s}")),
        };

        Ok(op)
    }
}

impl TryFrom<String> for Operator {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::from_str(&value)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::And => "&&",
            Self::Or => "||",
            Self::Bang => "!",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::BitXor => "^",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::Shr => ">>",
            Self::Shl => "<<",
            Self::Gt => ">",
            Self::Lt => "<",
            Self::GtEq => ">=",
            Self::LtEq => "<=",
        };
        write!(f, "{out}")
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Constructable {
    Identifier(Identifier),
    Call(Call),
    Scope(Scope),
}

impl Display for Constructable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Call(Call {
                function,
                arguments,
                ..
            }) => write!(
                f,
                "{}({})",
                function,
                arguments
                    .iter()
                    .map(ToString::to_string)
                    .collect::<String>()
            ),

            Self::Scope(Scope { module, member, .. }) => write!(f, "{module}::{member}"),

            Self::Identifier(Identifier { value, .. }) => write!(f, "{value}"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Assignable {
    Identifier(Identifier),
    Method(Method),
    Index(Index),
}

impl Display for Assignable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Method(Method {
                left,
                method,
                arguments,
                ..
            }) => write!(
                f,
                "{}.{}{}",
                left,
                method,
                arguments
                    .as_ref()
                    .map_or_else(String::new, |arguments| format!(
                        "({})",
                        arguments
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    ))
            ),

            Self::Index(Index { left, index, .. }) => write!(f, "{left}[{index}]"),

            Self::Identifier(Identifier { value, .. }) => write!(f, "{value}"),
        }
    }
}
