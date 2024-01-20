use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

pub type BlockStatement = Vec<Statement>;
pub type Ident = String;

#[derive(Clone, PartialEq, Debug)]
pub enum Node {
    Program { statements: BlockStatement },
    Stmt(Statement),
    Expr(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program { statements } => {
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
    pub name: Ident,
    pub mutable: bool,
    pub value: Option<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Return {
    pub return_value: Expression,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Delete {
    pub delete_ident: Ident,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ExpressionStmt {
    pub returns: bool,
    pub expression: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub ident: Ident,
    pub parameters: Vec<Ident>,
    pub body: BlockStatement,
}

#[derive(Clone, PartialEq, Debug)]
pub struct While {
    pub condition: Expression,
    pub body: BlockStatement,
}

#[derive(Clone, PartialEq, Debug)]
pub struct For {
    pub ident: Ident,
    pub iterator: Expression,
    pub body: BlockStatement,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassDecl {
    pub ident: Ident,
    pub initializers: Vec<Ident>,
    pub body: Vec<ClassStatement>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Import {
    pub path: String,
    pub class: bool,
    pub alias: Option<Ident>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassVariable {
    pub value: Option<Expression>,
    pub name: String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassMethod {
    pub name: Ident,
    pub parameters: Vec<Ident>,
    pub body: BlockStatement,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ClassStatement {
    Variable(ClassVariable),
    Method(ClassMethod),
}

impl ToString for ClassStatement {
    fn to_string(&self) -> String {
        String::new()
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
    Break,
    Continue,
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
                "class {}({}) {}",
                initializers.join(", "),
                ident,
                body.iter().map(ToString::to_string).collect::<String>()
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

            Self::Import(Import { path, alias, class }) => write!(
                f,
                "import{} \"{}\"{}",
                if *class { " class" } else { "" },
                path,
                alias
                    .clone()
                    .map_or_else(String::new, |alias| format!(" as {alias}"))
            ),

            Self::Return(Return { return_value }) => write!(
                f,
                "return {};",
                if matches!(
                    return_value,
                    Expression::Literal(Literal { lit: Lit::Null })
                ) {
                    String::new()
                } else {
                    return_value.to_string()
                }
            ),

            Self::While(While { condition, body }) => write!(
                f,
                "while ({}) {}",
                condition,
                body.iter().map(ToString::to_string).collect::<String>()
            ),

            Self::Break => write!(f, "break"),

            Self::Continue => write!(f, "continue"),

            Self::Delete(Delete { delete_ident }) => write!(f, "delete {delete_ident};"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Method {
    pub left: Box<Expression>,
    pub name: Ident,
    pub arguments: Option<Vec<Expression>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Constructor {
    pub constructable: Constructable,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Range {
    pub start: Box<Expression>,
    pub end: Box<Expression>,
    pub step: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Identifier {
    pub value: Ident,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Assign {
    pub to: Assignable,
    pub value: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Prefix {
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct If {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Lambda {
    pub parameters: Vec<Ident>,
    pub body: BlockStatement,
    pub name: Ident,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Index {
    pub left: Box<Expression>,
    pub expr: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal {
    pub lit: Lit,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
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

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(Assign { to, value }) => write!(f, "{to} = {value};"),

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

            Self::Constructor(Constructor { constructable }) => {
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

            Self::Identifier(Identifier { value }) => write!(f, "{value}"),

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

            Self::Index(Index { left, expr: index }) => write!(f, "({left}[{index}])"),

            Self::Infix(Infix {
                left,
                operator,
                right,
                ..
            }) => write!(f, "({left} {operator} {right})"),

            Self::Literal(Literal { lit }) => write!(f, "{lit}"),

            Self::Method(Method {
                left,
                name: method,
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

            Self::Prefix(Prefix { operator, right }) => {
                write!(f, "({operator}{right})")
            }

            Self::Range(Range { start, end, step }) => write!(
                f,
                "{start}..{end}{}",
                step.as_ref()
                    .map_or_else(String::new, |step| format!("..{step}"))
            ),

            Self::Scope(Scope { module, member }) => write!(f, "{module}::{member}"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Lit {
    Int {
        value: isize,
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
    Dict {
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

            Self::Dict { pairs } => write!(
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

            Self::Scope(Scope { module, member }) => write!(f, "{module}::{member}"),

            Self::Identifier(Identifier { value }) => write!(f, "{value}"),
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
                name: method,
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

            Self::Index(Index { left, expr: index }) => write!(f, "{left}[{index}]"),

            Self::Identifier(Identifier { value }) => write!(f, "{value}"),
        }
    }
}
