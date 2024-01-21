mod precedence;
#[cfg(test)]
mod tests;

use precedence::{precedences, Precedence};

use crate::{
    ast::{
        Assign, Assignable, BlockStatement, Call, ClassDecl, ClassMethod, ClassStatement,
        ClassVariable, Constructable, Constructor, Declaration, Delete, Expression, ExpressionStmt,
        For, Function, Identifier, If, Import, Index, Infix, Lambda, Lit, Literal, Method, Node,
        Prefix, Range, Return, Scope, Statement, While,
    },
    lexer::Lexer,
    token::{Kind, Token},
};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,

    cur_tok: Token,
    peek_tok: Token,

    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let mut p = Self {
            lexer,
            cur_tok: Token::new(Kind::Illegal, String::new()),
            peek_tok: Token::new(Kind::Illegal, String::new()),
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_tok = self.peek_tok.clone();
        self.peek_tok = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Node> {
        let mut statements = Vec::new();

        loop {
            let tok = &self.cur_tok;
            if tok.tok_type == Kind::Eol {
                break;
            }

            let statement = self.parse_statement();
            if let Some(statement) = statement {
                statements.push(statement);
            } else {
                return None;
            }

            self.next_token();
        }

        Some(Node::Program { statements })
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let stmt = match self.cur_tok.tok_type {
            Kind::Var | Kind::Const => self.parse_assign_statement(),
            Kind::Return => self.parse_return_statement(),
            Kind::While => self.parse_while_statement(),
            Kind::For => self.parse_for_statement(),
            Kind::Class => self.parse_class_statement(),
            Kind::Import => self.parse_import_statement(),
            Kind::Delete => self.parse_delete_statement(),
            Kind::Break => Some(Statement::Break),
            Kind::Continue => Some(Statement::Continue),
            Kind::Function if self.peek_token_is(Kind::Ident) => self.parse_function_statement(),
            _ => self.parse_expression_statement(),
        };

        if self.peek_token_is(Kind::Semicolon) {
            self.next_token();
        }

        stmt
    }

    fn parse_assign_statement(&mut self) -> Option<Statement> {
        let mutable = self.cur_token_is(Kind::Var);

        if !self.expect_peek(Kind::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();

        let name = ident_tok.tok_lit;

        let value = if self.peek_token_is(Kind::Assign) {
            self.next_token();
            self.next_token();

            let mut expr = self.parse_expression(Precedence::Lowest)?;

            if let Expression::Lambda(node) = expr {
                expr = Expression::Lambda(Lambda {
                    name: name.clone(),
                    ..node
                });
            }

            Some(expr)
        } else {
            None
        };

        Some(Statement::Declaration(Declaration {
            name,
            mutable,
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Return(Return { return_value }))
    }

    fn parse_delete_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(Kind::Ident) {
            return None;
        }

        let delete_ident = self.cur_tok.tok_lit.clone();

        Some(Statement::Delete(Delete { delete_ident }))
    }

    fn parse_function_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if !self.cur_token_is(Kind::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let ident = ident_tok.tok_lit;

        if !self.expect_peek(Kind::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(Kind::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Statement::Function(Function {
            ident,
            parameters,
            body,
        }))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(Kind::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Kind::RParen) {
            return None;
        }

        if !self.expect_peek(Kind::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Statement::While(While { condition, body }))
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(Kind::LParen) {
            return None;
        }

        self.next_token();

        if !self.cur_token_is(Kind::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let ident = ident_tok.tok_lit;

        if !self.expect_peek(Kind::In) {
            return None;
        }

        self.next_token();

        let iterator = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Kind::RParen) {
            return None;
        }

        if !self.expect_peek(Kind::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Statement::For(For {
            ident,
            iterator,
            body,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        let returns = !self.peek_token_is(Kind::Semicolon);

        Some(Statement::ExpressionStmt(ExpressionStmt {
            returns,
            expression,
        }))
    }

    fn parse_class_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(Kind::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let ident = ident_tok.tok_lit;

        let initializers = if self.peek_token_is(Kind::LParen) {
            self.next_token();
            self.parse_function_parameters()?
        } else {
            Vec::new()
        };

        if !self.expect_peek(Kind::LBrace) {
            return None;
        }

        let body = self.parse_class_block();

        Some(Statement::ClassDecl(ClassDecl {
            ident,
            initializers,
            body,
        }))
    }

    fn parse_expression(&mut self, prec: Precedence) -> Option<Expression> {
        let mut left_exp = match self.cur_tok.tok_type {
            Kind::Ident => Some(self.parse_identifier()),
            Kind::IntLiteral => self.parse_integer_literal(),
            Kind::FloatLiteral => self.parse_float_literal(),
            Kind::False | Kind::True => Some(Expression::Literal(Literal {
                lit: Lit::Bool {
                    value: self.cur_token_is(Kind::True),
                },
            })),
            Kind::Nil => Some(Expression::Literal(Literal { lit: Lit::Nil })),
            Kind::Bang | Kind::Minus => self.parse_prefix_expression(),
            Kind::LParen => self.parse_grouped_expression(),
            Kind::If => self.parse_if_expression(),
            Kind::Function => self.parse_function_literal(),
            Kind::StrLiteral => Some(self.parse_string_literal()),
            Kind::CharLiteral => self.parse_char_literal(),
            Kind::LBracket => self.parse_array_literal(),
            Kind::LBrace => self.parse_dict_literal(),
            Kind::New => self.parse_constructor_expression(),
            Kind::Semicolon => return Some(Expression::Literal(Literal { lit: Lit::Nil })),
            _ => {
                self.no_prefix_parse_error(self.cur_tok.tok_type);
                return None;
            }
        }?;

        while !self.peek_token_is(Kind::Semicolon) && prec < self.peek_precedence() {
            left_exp = match self.peek_tok.clone().tok_type {
                Kind::Assign => {
                    self.next_token();
                    self.parse_assign_expression(left_exp)
                }
                Kind::Plus
                | Kind::Minus
                | Kind::Slash
                | Kind::Asterisk
                | Kind::Modulo
                | Kind::Eq
                | Kind::NotEq
                | Kind::Lt
                | Kind::LtEq
                | Kind::Gt
                | Kind::GtEq
                | Kind::Caret
                | Kind::BitAnd
                | Kind::BitOr
                | Kind::Shr
                | Kind::Shl
                | Kind::And
                | Kind::Or => {
                    self.next_token();
                    self.parse_infix_expression(left_exp)
                }
                Kind::LParen => {
                    self.next_token();
                    self.parse_call_expression(left_exp)
                }
                Kind::LBracket => {
                    self.next_token();
                    self.parse_index_expression(left_exp)
                }
                Kind::Dot => {
                    self.next_token();
                    self.parse_method_expression(left_exp)
                }
                Kind::Scope => {
                    self.next_token();
                    self.parse_scope_expression(left_exp)
                }
                Kind::Range => {
                    self.next_token();
                    self.parse_range_expression(left_exp)
                }
                _ => return Some(left_exp),
            }?;
        }

        Some(left_exp)
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(Kind::RBrace) && !self.cur_token_is(Kind::Eol) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            };
            self.next_token();
        }

        statements
    }

    fn parse_class_block(&mut self) -> Vec<ClassStatement> {
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(Kind::RBrace) && !self.cur_token_is(Kind::Eol) {
            if let Some(class_stmt) = self.parse_class_member() {
                statements.push(class_stmt);
            };

            self.next_token();
        }

        statements
    }

    fn parse_class_member(&mut self) -> Option<ClassStatement> {
        let name = self.cur_tok.tok_lit.clone();

        if self.peek_token_is(Kind::LParen) {
            self.next_token();
            let parameters = self.parse_function_parameters()?;

            if !self.expect_peek(Kind::LBrace) {
                return None;
            }

            let body = self.parse_block_statement();

            Some(ClassStatement::Method(ClassMethod {
                name,
                parameters,
                body,
            }))
        } else {
            self.next_token();

            if self.cur_token_is(Kind::Assign) {
                self.next_token();

                let value = self.parse_expression(Precedence::Lowest)?;

                Some(ClassStatement::Variable(ClassVariable {
                    value: Some(value),
                    name,
                }))
            } else {
                None
            }
        }
    }

    fn parse_import_statement(&mut self) -> Option<Statement> {
        let class = if self.peek_token_is(Kind::Class) {
            self.next_token();
            true
        } else {
            false
        };

        if !self.expect_peek(Kind::StrLiteral) {
            return None;
        }

        let path = self.cur_tok.clone().tok_lit;

        let alias = if self.peek_token_is(Kind::As) {
            self.next_token();
            if !self.expect_peek(Kind::Ident) {
                return None;
            }

            Some(self.cur_tok.clone().tok_lit)
        } else {
            None
        };

        Some(Statement::Import(Import { path, class, alias }))
    }
}

impl Parser<'_> {
    fn parse_identifier(&mut self) -> Expression {
        let token = self.cur_tok.clone();

        Expression::Identifier(Identifier {
            value: token.tok_lit,
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let Ok(value) = token.tok_lit.parse() else {
            self.errors
                .push(format!("couldn't parse '{}' as integer", token.tok_lit));
            return None;
        };

        Some(Expression::Literal(Literal {
            lit: Lit::Int { value },
        }))
    }

    fn parse_float_literal(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let Ok(value) = token.tok_lit.parse() else {
            self.errors
                .push(format!("couldn't parse '{}' as float", token.tok_lit));
            return None;
        };

        Some(Expression::Literal(Literal {
            lit: Lit::Float { value },
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(Prefix {
            operator: token.tok_type.try_into().ok()?,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let prec = self.cur_precedence();
        self.next_token();

        let right = self.parse_expression(prec)?;

        Some(Expression::Infix(Infix {
            left: Box::new(left),
            operator: token.tok_type.try_into().ok()?,
            right: Box::new(right),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Kind::RParen) {
            return None;
        }

        Some(expr)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(Kind::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Kind::RParen) {
            return None;
        }

        if !self.expect_peek(Kind::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(Kind::Else) {
            self.next_token();

            if !self.expect_peek(Kind::LBrace) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Some(Expression::If(If {
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<String>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(Kind::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident_tok = self.cur_tok.clone();
        identifiers.push(ident_tok.tok_lit);

        while self.peek_token_is(Kind::Comma) {
            self.next_token();
            self.next_token();

            let ident_token = self.cur_tok.clone();
            identifiers.push(ident_token.tok_lit);
        }

        if !self.expect_peek(Kind::RParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(Kind::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(Kind::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Expression::Lambda(Lambda {
            parameters,
            body,
            name: String::new(),
        }))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let arguments = self.parse_expression_list(Kind::RParen)?;

        Some(Expression::Call(Call {
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_expression_list(&mut self, end: Kind) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(*Box::new(self.parse_expression(Precedence::Lowest)?));

        while self.peek_token_is(Kind::Comma) {
            self.next_token();
            self.next_token();

            list.push(*Box::new(self.parse_expression(Precedence::Lowest)?));
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn parse_string_literal(&mut self) -> Expression {
        let token = self.cur_tok.clone();

        Expression::Literal(Literal {
            lit: Lit::Str {
                value: token.tok_lit,
            },
        })
    }

    fn parse_char_literal(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let Some(value) = parse_char(&token.tok_lit) else {
            self.errors
                .push(format!("cannot parse \"{}\" as char", token.tok_lit));
            return None;
        };

        Some(Expression::Literal(Literal {
            lit: Lit::Char { value },
        }))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let elements = self.parse_expression_list(Kind::RBracket)?;

        Some(Expression::Literal(Literal {
            lit: Lit::Array { elements },
        }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Kind::RBracket) {
            return None;
        }

        Some(Expression::Index(Index {
            left: Box::new(left),
            expr: Box::new(index),
        }))
    }

    fn parse_dict_literal(&mut self) -> Option<Expression> {
        let mut pairs = Vec::new();

        while !self.peek_token_is(Kind::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;

            if !self.expect_peek(Kind::Colon) {
                return None;
            }

            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;

            pairs.push((key, value));

            if !self.peek_token_is(Kind::RBrace) && !self.expect_peek(Kind::Comma) {
                return None;
            }
        }

        if !self.expect_peek(Kind::RBrace) {
            return None;
        }

        Some(Expression::Literal(Literal {
            lit: Lit::Dict { pairs },
        }))
    }

    fn parse_assign_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let to = match left {
            Expression::Identifier(ast_node) => Assignable::Identifier(ast_node),
            Expression::Method(ast_node) => Assignable::Method(ast_node),
            Expression::Index(ast_node) => Assignable::Index(ast_node),
            _ => {
                self.errors.push(format!("{left} cannot be assigned to."));
                return None;
            }
        };

        let value = self.parse_expression(Precedence::Lowest)?;

        Some(Expression::Assign(Assign {
            to,
            value: Box::new(value),
        }))
    }

    fn parse_method_expression(&mut self, left: Expression) -> Option<Expression> {
        if !self.expect_peek(Kind::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let method = ident_tok.tok_lit;

        let arguments = if self.peek_token_is(Kind::LParen) {
            self.next_token();
            Some(self.parse_expression_list(Kind::RParen)?)
        } else {
            None
        };

        Some(Expression::Method(Method {
            left: Box::new(left),
            name: method,
            arguments,
        }))
    }

    fn parse_scope_expression(&mut self, left: Expression) -> Option<Expression> {
        let Expression::Identifier(Identifier { value: module }) = left else {
            self.errors.push("expected IDENT".to_string());
            return None;
        };

        self.next_token();
        let member = self.parse_expression(Precedence::Lowest)?;

        Some(Expression::Scope(Scope {
            module,
            member: Box::new(member),
        }))
    }

    fn parse_constructor_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        let constructable = match expr {
            Expression::Identifier(ast_node) => Constructable::Identifier(ast_node),
            Expression::Scope(ast_node) => Constructable::Scope(ast_node),
            Expression::Call(ast_node) => Constructable::Call(ast_node),
            _ => {
                self.errors.push("cannot construct class".to_string());
                return None;
            }
        };

        Some(Expression::Constructor(Constructor { constructable }))
    }

    fn parse_range_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let mut last = self.parse_expression(Precedence::Lowest)?;
        let mut step = None;

        if let Expression::Range(Range {
            start: last_start,
            end: last_end,
            step: last_step,
            ..
        }) = last
        {
            step = Some(last_end);

            last = *last_start;

            if last_step.is_some() {
                self.errors
                    .push("range cannot have more than 3 parts.".to_string());
                return None;
            }
        }

        Some(Expression::Range(Range {
            start: Box::new(left),
            end: Box::new(last),
            step,
        }))
    }
}

fn parse_char(ch: &str) -> Option<char> {
    let mut chars = ch.chars();
    let c = match ch.parse() {
        Ok(ch) => {
            chars.next();
            Some(ch)
        }
        Err(_) => match ch.len() {
            0 => None,
            _ => match chars.next() {
                Some('\\') => match chars.next() {
                    Some('x') => {
                        let data = String::from_iter(&[chars.next()?, chars.next()?]);
                        char::from_u32(u32::from_str_radix(&data, 16).ok()?)
                    }
                    Some('u') => {
                        let data = String::from_iter(&[
                            chars.next()?,
                            chars.next()?,
                            chars.next()?,
                            chars.next()?,
                        ]);
                        char::from_u32(u32::from_str_radix(&data, 16).ok()?)
                    }
                    Some('n') => Some('\n'),
                    Some('r') => Some('\r'),
                    Some('t') => Some('\t'),
                    Some('\\') => Some('\\'),
                    Some('0') => Some('\0'),
                    Some('\'') => Some('\''),
                    _ => None,
                },
                _ => None,
            },
        },
    };

    if chars.next().is_some() {
        return None;
    }

    c
}

impl Parser<'_> {
    fn cur_token_is(&self, t: Kind) -> bool {
        self.cur_tok.tok_type == t
    }

    fn peek_token_is(&self, t: Kind) -> bool {
        self.peek_tok.tok_type == t
    }

    fn expect_peek(&mut self, t: Kind) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn peek_precedence(&self) -> Precedence {
        precedences(self.peek_tok.clone().tok_type)
    }

    fn cur_precedence(&self) -> Precedence {
        precedences(self.cur_tok.tok_type)
    }
}

impl Parser<'_> {
    fn peek_error(&mut self, t: Kind) {
        self.errors.push(format!(
            "expected next token to be {}, got {} instead.",
            t, self.peek_tok.tok_type
        ));
    }

    fn no_prefix_parse_error(&mut self, t: Kind) {
        self.errors
            .push(format!("no prefix parse function for {t} found"));
    }
}
