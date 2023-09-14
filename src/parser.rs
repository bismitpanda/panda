mod precedence;
#[cfg(test)]
mod tests;

use crate::ast::{
    AssignAst, Assignable, BlockStatement, CallAst, ClassDeclAst, ClassStatement, Constructable,
    ConstructorAst, DeclarationAst, DeleteAst, Expression, ExpressionStmtAst, ForAst, FunctionAst,
    IdentifierAst, IfAst, ImportAst, IndexAst, InfixAst, LambdaAst, Literal, LiteralAst, MethodAst,
    Node, PrefixAst, RangeAst, ReturnAst, ScopeAst, Span, Statement, WhileAst,
};
use crate::lexer::Lexer;
use crate::token::{Position, Token, TokenType};
use precedence::{precedences, Precedence};

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
            cur_tok: Token::new(TokenType::Illegal, String::new(), Position::new(0, 0)),
            peek_tok: Token::new(TokenType::Illegal, String::new(), Position::new(0, 0)),
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
            if tok.tok_type == TokenType::Eol {
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

        Some(Node::Program {
            statements,
            span: Span {
                start: Position::new(0, 0),
                end: self.cur_tok.position,
            },
        })
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let stmt = match self.cur_tok.tok_type {
            TokenType::Var | TokenType::Const => self.parse_assign_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::For => self.parse_for_statement(),
            TokenType::Class => self.parse_class_statement(),
            TokenType::Import => self.parse_import_statement(),
            TokenType::Delete => self.parse_delete_statement(),
            TokenType::Break => {
                let pos = self.cur_tok.position;
                Some(Statement::Break(Span {
                    start: pos,
                    end: pos + Position::new(0, 5),
                }))
            }
            TokenType::Continue => {
                let pos = self.cur_tok.position;
                Some(Statement::Continue(Span {
                    start: pos,
                    end: pos + Position::new(0, 8),
                }))
            }
            TokenType::Function if self.peek_token_is(TokenType::Ident) => {
                self.parse_function_statement()
            }
            _ => self.parse_expression_statement(),
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        stmt
    }

    fn parse_assign_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        let mutable = self.cur_token_is(TokenType::Var);

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();

        let name = ident_tok.tok_lit;

        let value = if self.peek_token_is(TokenType::Assign) {
            self.next_token();
            self.next_token();

            let mut expr = self.parse_expression(Precedence::Lowest)?;

            if let Expression::Lambda(node) = expr {
                expr = Expression::Lambda(LambdaAst {
                    name: name.clone(),
                    ..node
                });
            }

            Some(expr)
        } else {
            None
        };

        let end = if let Some(value) = &value {
            value.get_span().end
        } else {
            self.cur_tok.position + Position::new(0, name.len())
        };

        let span = Span { start, end };

        Some(Statement::Declaration(DeclarationAst {
            span,
            name,
            mutable,
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        let end = return_value.get_span().end;
        let span = Span { start, end };

        Some(Statement::Return(ReturnAst { span, return_value }))
    }

    fn parse_delete_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let delete_ident = self.cur_tok.tok_lit.clone();

        let end = self.cur_tok.position + Position::new(0, delete_ident.len());
        let span = Span { start, end };

        Some(Statement::Delete(DeleteAst { span, delete_ident }))
    }

    fn parse_function_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        self.next_token();

        if !self.cur_token_is(TokenType::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let ident = ident_tok.tok_lit;

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        let end = self.cur_tok.position;
        let span = Span { start, end };

        Some(Statement::Function(FunctionAst {
            span,
            ident,
            parameters,
            body,
        }))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        let end = self.cur_tok.position;
        let span = Span { start, end };

        Some(Statement::While(WhileAst {
            span,
            condition,
            body,
        }))
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();

        if !self.cur_token_is(TokenType::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let ident = ident_tok.tok_lit;

        if !self.expect_peek(TokenType::In) {
            return None;
        }

        self.next_token();

        let iterator = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        let end = self.cur_tok.position;
        let span = Span { start, end };

        Some(Statement::For(ForAst {
            span,
            ident,
            iterator,
            body,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        let expression = self.parse_expression(Precedence::Lowest)?;

        let returns = !self.peek_token_is(TokenType::Semicolon);

        let span = Span {
            start,
            end: expression.get_span().end,
        };

        Some(Statement::ExpressionStmt(ExpressionStmtAst {
            span,
            returns,
            expression,
        }))
    }

    fn parse_class_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let ident = ident_tok.tok_lit;

        let initializers = if self.peek_token_is(TokenType::LParen) {
            self.next_token();
            self.parse_function_parameters()?
        } else {
            Vec::new()
        };

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_class_block_statement()?;

        let end = self.cur_tok.position;
        let span = Span { start, end };

        Some(Statement::ClassDecl(ClassDeclAst {
            span,
            ident,
            initializers,
            body,
        }))
    }

    fn parse_expression(&mut self, prec: Precedence) -> Option<Expression> {
        let start = self.cur_tok.position;
        let mut left_exp = match self.cur_tok.tok_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::IntLiteral => self.parse_integer_literal(),
            TokenType::FloatLiteral => self.parse_float_literal(),
            TokenType::False | TokenType::True => Some(Expression::Literal(LiteralAst {
                span: Span {
                    start,
                    end: start + Position::new(0, self.cur_tok.tok_lit.len()),
                },
                lit: Literal::Bool {
                    value: self.cur_token_is(TokenType::True),
                },
            })),
            TokenType::Null => Some(Expression::Literal(LiteralAst {
                span: Span {
                    start,
                    end: start + Position::new(0, 4),
                },
                lit: Literal::Null,
            })),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
            TokenType::StrLiteral => self.parse_string_literal(),
            TokenType::CharLiteral => self.parse_char_literal(),
            TokenType::LBracket => self.parse_array_literal(),
            TokenType::LBrace => self.parse_hash_literal(),
            TokenType::New => self.parse_constructor_expression(),
            TokenType::Semicolon => {
                return Some(Expression::Literal(LiteralAst {
                    span: Span {
                        start,
                        end: self.cur_tok.position + Position::new(0, 1),
                    },
                    lit: Literal::Null,
                }))
            }
            _ => {
                self.no_prefix_parse_error(self.cur_tok.tok_type);
                return None;
            }
        }?;

        while !self.peek_token_is(TokenType::Semicolon) && prec < self.peek_precedence() {
            left_exp = match self.peek_tok.clone().tok_type {
                TokenType::Assign => {
                    self.next_token();
                    self.parse_assign_expression(left_exp)
                }
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::LtEq
                | TokenType::Gt
                | TokenType::GtEq
                | TokenType::Caret
                | TokenType::BitAnd
                | TokenType::BitOr
                | TokenType::Shr
                | TokenType::Shl
                | TokenType::And
                | TokenType::Or => {
                    self.next_token();
                    self.parse_infix_expression(left_exp)
                }
                TokenType::LParen => {
                    self.next_token();
                    self.parse_call_expression(left_exp)
                }
                TokenType::LBracket => {
                    self.next_token();
                    self.parse_index_expression(left_exp)
                }
                TokenType::Dot => {
                    self.next_token();
                    self.parse_method_expression(left_exp)
                }
                TokenType::Scope => {
                    self.next_token();
                    self.parse_scope_expression(left_exp)
                }
                TokenType::Range => {
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

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eol) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            };
            self.next_token();
        }

        statements
    }

    fn parse_class_block_statement(&mut self) -> Option<Vec<ClassStatement>> {
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eol) {
            if let Some(stmt) = self.parse_statement() {
                match stmt {
                    Statement::Declaration(decl) => {
                        statements.push(ClassStatement::Declaration(decl));
                    }
                    Statement::Function(func) => statements.push(ClassStatement::Function(func)),
                    _ => {
                        self.errors
                            .push("invalid statements inside class".to_string());
                        return None;
                    }
                }
            };
            self.next_token();
        }

        Some(statements)
    }

    fn parse_import_statement(&mut self) -> Option<Statement> {
        let start = self.cur_tok.position;
        if !self.expect_peek(TokenType::StrLiteral) {
            return None;
        }

        let path = self.cur_tok.clone().tok_lit;

        let alias = if self.peek_token_is(TokenType::As) {
            self.next_token();
            if !self.expect_peek(TokenType::Ident) {
                return None;
            }

            Some(self.cur_tok.clone().tok_lit)
        } else {
            None
        };

        let span = Span {
            start,
            end: self.cur_tok.position
                + Position::new(
                    0,
                    if let Some(alias) = &alias {
                        alias.len()
                    } else {
                        path.len()
                    },
                ),
        };

        Some(Statement::Import(ImportAst { span, path, alias }))
    }
}

impl Parser<'_> {
    fn parse_identifier(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let span = Span {
            start: token.position,
            end: token.position + Position::new(0, token.tok_lit.len()),
        };

        Some(Expression::Identifier(IdentifierAst {
            span,
            value: token.tok_lit,
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let span = Span {
            start: token.position,
            end: token.position + Position::new(0, token.tok_lit.len()),
        };

        let Ok(value) = token.tok_lit.parse() else {
            self.errors
                .push(format!("couldn't parse '{}' as integer", token.tok_lit));
            return None;
        };

        Some(Expression::Literal(LiteralAst {
            span,
            lit: Literal::Int { value },
        }))
    }

    fn parse_float_literal(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let span = Span {
            start: token.position,
            end: token.position + Position::new(0, token.tok_lit.len()),
        };

        let Ok(value) = token.tok_lit.parse() else {
            self.errors
                .push(format!("couldn't parse '{}' as float", token.tok_lit));
            return None;
        };

        Some(Expression::Literal(LiteralAst {
            span,
            lit: Literal::Float { value },
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        let span = Span {
            start: token.position,
            end: right.get_span().end,
        };

        Some(Expression::Prefix(PrefixAst {
            span,
            operator: token.tok_lit.try_into().ok()?,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let prec = self.cur_precedence();
        self.next_token();

        let right = self.parse_expression(prec)?;

        let span = Span {
            start: left.get_span().start,
            end: right.get_span().end,
        };

        Some(Expression::Infix(InfixAst {
            span,
            left: Box::new(left),
            operator: token.tok_lit.try_into().ok()?,
            right: Box::new(right),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        Some(expr)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let start = self.cur_tok.position;

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        let span = Span {
            start,
            end: self.cur_tok.position,
        };

        Some(Expression::If(IfAst {
            span,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<String>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident_tok = self.cur_tok.clone();
        identifiers.push(ident_tok.tok_lit);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let ident_token = self.cur_tok.clone();
            identifiers.push(ident_token.tok_lit);
        }

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let start = self.cur_tok.position;
        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        let span = Span {
            start,
            end: self.cur_tok.position,
        };

        Some(Expression::Lambda(LambdaAst {
            span,
            parameters,
            body,
            name: String::new(),
        }))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let start = function.get_span().start;
        let arguments = self.parse_expression_list(TokenType::RParen)?;

        let span = Span {
            start,
            end: self.cur_tok.position,
        };

        Some(Expression::Call(CallAst {
            span,
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(*Box::new(self.parse_expression(Precedence::Lowest)?));

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            list.push(*Box::new(self.parse_expression(Precedence::Lowest)?));
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        Some(Expression::Literal(LiteralAst {
            span: Span {
                start: token.position,
                end: token.position + Position::new(0, token.tok_lit.len()),
            },
            lit: Literal::Str {
                value: token.tok_lit,
            },
        }))
    }

    fn parse_char_literal(&mut self) -> Option<Expression> {
        let token = self.cur_tok.clone();

        let Some(value) = parse_char(&token.tok_lit) else {
            self.errors
                .push(format!("cannot parse \"{}\" as char", token.tok_lit));
            return None;
        };

        let span = Span {
            start: token.position,
            end: token.position + Position::new(0, token.tok_lit.len()),
        };

        Some(Expression::Literal(LiteralAst {
            span,
            lit: Literal::Char { value },
        }))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let start = self.cur_tok.position;
        let elements = self.parse_expression_list(TokenType::RBracket)?;

        let span = Span {
            start,
            end: self.cur_tok.position,
        };

        Some(Expression::Literal(LiteralAst {
            span,
            lit: Literal::Array { elements },
        }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let start = left.get_span().start;
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RBracket) {
            return None;
        }

        let span = Span {
            start,
            end: self.cur_tok.position,
        };

        Some(Expression::Index(IndexAst {
            span,
            left: Box::new(left),
            index: Box::new(index),
        }))
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let start = self.cur_tok.position;
        let mut pairs = Vec::new();

        while !self.peek_token_is(TokenType::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;

            if !self.expect_peek(TokenType::Colon) {
                return None;
            }

            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;

            pairs.push((key, value));

            if !self.peek_token_is(TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
                return None;
            }
        }

        if !self.expect_peek(TokenType::RBrace) {
            return None;
        }

        let span = Span {
            start,
            end: self.cur_tok.position,
        };

        Some(Expression::Literal(LiteralAst {
            span,
            lit: Literal::Hash { pairs },
        }))
    }

    fn parse_assign_expression(&mut self, left: Expression) -> Option<Expression> {
        let start = left.get_span().start;
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

        let span = Span {
            start,
            end: value.get_span().end,
        };

        Some(Expression::Assign(AssignAst {
            span,
            to,
            value: Box::new(value),
        }))
    }

    fn parse_method_expression(&mut self, left: Expression) -> Option<Expression> {
        let start = left.get_span().start;
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let ident_tok = self.cur_tok.clone();
        let method = ident_tok.tok_lit;

        let arguments = if self.peek_token_is(TokenType::LParen) {
            self.next_token();
            Some(self.parse_expression_list(TokenType::RParen)?)
        } else {
            None
        };

        let span = Span {
            start,
            end: if arguments.is_none() {
                self.cur_tok.position
            } else {
                self.cur_tok.position + Position::new(0, method.len())
            },
        };

        Some(Expression::Method(MethodAst {
            span,
            left: Box::new(left),
            method,
            arguments,
        }))
    }

    fn parse_scope_expression(&mut self, left: Expression) -> Option<Expression> {
        let start = left.get_span().start;

        let Expression::Identifier(IdentifierAst { value: module, .. }) = left else {
            self.errors.push("expected IDENT".to_string());
            return None;
        };

        self.next_token();
        let member = self.parse_expression(Precedence::Lowest)?;

        let span = Span {
            start,
            end: member.get_span().end,
        };

        Some(Expression::Scope(ScopeAst {
            span,
            module,
            member: Box::new(member),
        }))
    }

    fn parse_constructor_expression(&mut self) -> Option<Expression> {
        let start = self.cur_tok.position;
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;
        let end = expr.get_span().end;

        let constructable = match expr {
            Expression::Identifier(ast_node) => Constructable::Identifier(ast_node),
            Expression::Scope(ast_node) => Constructable::Scope(ast_node),
            Expression::Call(ast_node) => Constructable::Call(ast_node),
            _ => {
                self.errors.push("cannot construct class".to_string());
                return None;
            }
        };

        let span = Span { start, end };

        Some(Expression::Constructor(ConstructorAst {
            span,
            constructable,
        }))
    }

    fn parse_range_expression(&mut self, left: Expression) -> Option<Expression> {
        let start = left.get_span().start;
        self.next_token();

        let mut stop = self.parse_expression(Precedence::Lowest)?;
        let mut step = None;

        if let Expression::Range(RangeAst {
            start: end_start,
            stop: end_stop,
            step: end_step,
            ..
        }) = stop
        {
            step = Some(end_stop);

            stop = *end_start;

            if end_step.is_some() {
                self.errors
                    .push("range cannot have more than 3 parts.".to_string());
                return None;
            }
        }

        let span = Span {
            start,
            end: self.cur_tok.position,
        };

        Some(Expression::Range(RangeAst {
            span,
            start: Box::new(left),
            stop: Box::new(stop),
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
    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_tok.tok_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_tok.tok_type == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
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
    fn peek_error(&mut self, t: TokenType) {
        self.errors.push(format!(
            "expected next token to be {}, got {} instead.",
            t, self.peek_tok.tok_type
        ));
    }

    fn no_prefix_parse_error(&mut self, t: TokenType) {
        self.errors
            .push(format!("no prefix parse function for {t} found"));
    }
}
