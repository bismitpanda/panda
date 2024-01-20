use crate::{
    ast::{Expression, Import, Lit, Node, Statement},
    lexer::Lexer,
    parser::Parser,
};

trait Formatter {
    fn formatter(&self, level: usize) -> String;
}

impl Formatter for Node {
    fn formatter(&self, level: usize) -> String {
        match self {
            Self::Program { statements, .. } => {
                let mut out = String::new();

                for stmt in statements {
                    out.push_str(&stmt.formatter(0));
                }

                out
            }

            Self::Stmt(stmt) => stmt.formatter(level),

            Self::Expr(expr) => expr.formatter(level),
        }
    }
}

impl Formatter for Statement {
    fn formatter(&self, level: usize) -> String {
        match self {
            Self::Break => format!("{}break;", "    ".repeat(level)),
            Self::Continue => format!("{}continue;", "    ".repeat(level)),
            Self::Import(Import { path, alias, .. }) => {
                let mut out = format!("{}import \"{path}\"", "    ".repeat(level));

                if let Some(alias) = alias {
                    out.push_str(&format!(" as {alias}"));
                }
                out.push(';');
                out
            }
            _ => todo!(),
        }
    }
}

impl Formatter for Expression {
    fn formatter(&self, _: usize) -> String {
        match self {
            Self::Literal(lit) => lit.lit.formatter(0),
            _ => todo!(),
        }
    }
}

impl Formatter for Lit {
    fn formatter(&self, _: usize) -> String {
        match self {
            Self::Str { value } => format!(r#""{value}""#),
            Self::Char { value } => format!("'{value}'"),
            Self::Bool { value } => value.to_string(),
            Self::Null => "null".to_string(),
            Self::Int { value } => value.to_string(),
            Self::Float { value } => value.to_string(),
            Self::Array { elements } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Dict { pairs } => format!(
                "{{{}}}",
                pairs
                    .iter()
                    .map(|(key, val)| format!("{key}: {val}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

pub fn formatter(file_name: &str) -> Result<String, String> {
    let input = std::fs::read_to_string(file_name).map_err(|err| err.to_string())?;

    let mut lexer = Lexer::new(&input);
    let mut parser = Parser::new(&mut lexer);

    let program = parser
        .parse_program()
        .ok_or_else(|| "parse_program() returned None.".to_string())?;

    if !parser.errors.is_empty() {
        println!("parser errors:");
        for msg in &parser.errors {
            println!("\t{msg}");
        }
        return Err("aborting due to parser error".to_string());
    }

    Ok(program.formatter(0))
}
