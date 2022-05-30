use std::fmt::{Display, format};
use crate::ast::{Assign, Binary, Call, Class, ElseIf, Expression, For, Function, FunctionKind, Get, If, IndexCall, Let, Literal, Logical, Set, Statement, Unary, While};
use crate::tokens::Token;
use crate::TokenType;

enum VarKind {
	Variable,
	Property,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Context {
	Global,
	Class,
	Function,
	Loop,
	For,
	While,
	If,
	Block,
}

impl Display for VarKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			VarKind::Variable => write!(f, "variable"),
			VarKind::Property => write!(f, "property"),
		}
	}
}

#[derive(Clone, Debug)]
pub struct ParseError {
	message: String,
	token: Token,
	context: Context,

}

pub struct Parser {
	tokens: Vec<Token>,
	current: usize,
	errors: Vec<ParseError>,
	context: Context,
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Parser {
		Parser {
			tokens,
			current: 0,
			errors: Vec::new(),
			context: Context::Global,
		}
	}

	pub fn parse(&mut self) -> Result<Vec<Statement>, Vec<ParseError>> {
		let mut statements = Vec::new();

		while !self.is_at_end() {
			match self.declaration() {
				Ok(statement) => statements.push(statement),
				Err(e) => {
					self.errors.push(e);

					self.synchronize();
				}
			}
		}

		if self.errors.is_empty() {
			Ok(statements)
		} else {
			Err(self.errors.clone())
		}
	}

	fn declaration(&mut self) -> Result<Statement, ParseError> {
		if self.match_token(TokenType::Fn) {
			let function = self.function(FunctionKind::Function)?;
			Ok(Statement::Function(Box::new(function)))
		} else if self.match_token(TokenType::Class) {
			self.class_declaration()
		} else if self.match_token(TokenType::Let) {
			self.var_declaration(VarKind::Variable)
		} else {
			self.statement()
		}
	}

	fn class_declaration(&mut self) -> Result<Statement, ParseError> {
		self.context = Context::Class;

		let name = self.consume_identifier("Expect class name.")?;

		let mut superclass = None;
		if self.match_token(TokenType::Less) {
			superclass = Some(self.consume_identifier("Expect superclass name.")?);
		}

		self.consume_token(TokenType::LeftBrace, "Expect '{' before class body.")?;

		let mut methods = Vec::new();
		let mut properties = Vec::new();
		let mut index_getter = None;
		let mut index_setter = None;
		let mut initializer = None;

		while !self.check(TokenType::RightBrace) && !self.is_at_end() {
			self.context = Context::Class;
			if self.match_token(TokenType::Init) {
				initializer = Some(self.function(FunctionKind::Initializer)?);
			} else if self.match_token(TokenType::Get) {
				if self.match_token(TokenType::Index) {
					index_getter = Some(self.function(FunctionKind::IndexGetter)?);
				} else {
					let function = self.function(FunctionKind::Getter)?;
					if function.name == "get" {
						self.errors.push(self.error(format!("Cannot override getter in class {}", name)));
					} else {
						self.errors.push(self.error(format!("Cannot override getter for {} in class {}", function.name, name)));
					}
				}
			} else if self.match_token(TokenType::Set) {
				if self.match_token(TokenType::Index) {
					index_setter = Some(self.function(FunctionKind::IndexSetter)?);
				} else {
					let function = self.function(FunctionKind::Setter)?;
					if function.name == "set" {
						self.errors.push(self.error(format!("Cannot override setter in class {}", name)));
					} else {
						self.errors.push(self.error(format!("Cannot override setter for {} in class {}", function.name, name)));
					}
				}
			} else if self.peek_next().token_type == TokenType::LeftParen {
				methods.push(self.function(FunctionKind::Method)?);
			} else {
				properties.push(self.var_declaration(VarKind::Property)?);
			}
		}

		self.consume_token(TokenType::RightBrace, "Expect '}' after class body.")?;

		Ok(Statement::Class(Box::new(Class {
			name: name,
			superclass,
			methods,
			properties,
			index_setter,
			index_getter,
			init: initializer,
		})))
	}


	fn function(&mut self, kind: FunctionKind) -> Result<Function, ParseError> {
		self.context = Context::Function;
		let mut name = "".to_string();
		if kind == FunctionKind::Initializer {
			name = "init".to_string();
		} else if kind == FunctionKind::IndexGetter {
			name = "getIndex".to_string();
		} else if kind == FunctionKind::IndexSetter {
			name = "setIndex".to_string();
		} else if kind == FunctionKind::Getter {
			name = "get".to_string();
			if self.check_identifier() {
				name = self.consume_identifier("Expect property name after 'get'.")?;
			}
		} else if kind == FunctionKind::Setter {
			name = "set".to_string();
			if self.check_identifier() {
				name = self.consume_identifier("Expect property name after 'set'.")?;
			}
		} else {
			name = self.consume_identifier(format!("Expect {} name.", kind))?;
		}

		let mut left_paren_type = TokenType::LeftParen;
		let mut right_paren_type = TokenType::RightParen;
		if kind == FunctionKind::IndexGetter || kind == FunctionKind::IndexSetter {
			left_paren_type = TokenType::LeftBracket;
			right_paren_type = TokenType::RightBracket;
		}

		self.consume_token(left_paren_type.clone(), format!("Expect '{}' after {} name.", left_paren_type, kind))?;

		let mut params = Vec::new();

		if !self.check(right_paren_type.clone()) {
			loop {
				params.push(self.consume_identifier("Expect parameter name.")?);

				if self.match_token(TokenType::Comma) {
					continue;
				}

				break;
			}
		}

		self.consume_token(right_paren_type.clone(), format!("Expect '{}' after parameters.", right_paren_type))?;

		self.consume_token(TokenType::LeftBrace, "Expect '{' before function body.")?;

		let mut body = self.block()?;

		Ok(Function {
			name,
			kind,
			params,
			body,
		})
	}

	fn var_declaration(&mut self, kind: VarKind) -> Result<Statement, ParseError> {
		let name = self.consume_identifier("Expect variable name.")?;

		let mut initializer = None;
		if self.match_token(TokenType::Equal) {
			initializer = Some(self.expression()?);
		}

		self.consume_token(TokenType::Semicolon, "Expect ';' after variable declaration.")?;

		Ok(Statement::Let(Let {
			name,
			value: initializer,
		}))
	}

	fn statement(&mut self) -> Result<Statement, ParseError> {
		if self.match_token(TokenType::If) {
			self.if_statement()
		} else if self.match_token(TokenType::While) {
			self.while_statement()
		} else if self.match_token(TokenType::For) {
			self.for_statement()
		} else if self.match_token(TokenType::Loop) {
			self.loop_statement()
		} else if self.match_token(TokenType::Print) {
			self.print_statement()
		} else if self.match_token(TokenType::Return) {
			self.return_statement()
		} else if self.check(TokenType::Break) {
			self.break_statement()
		}else if self.check(TokenType::Continue) {
			self.continue_statement()
		} else if self.match_token(TokenType::LeftBrace) {
			self.block()
		} else {
			self.expression_statement()
		}
	}

	fn loop_statement(&mut self) -> Result<Statement, ParseError> {
		let body = self.block()?;
		Ok(Statement::Loop(Box::new(body)))
	}

	fn if_statement(&mut self) -> Result<Statement, ParseError> {
		let condition = self.expression()?;
		self.consume_token(TokenType::LeftBrace, "Expect '{' after if condition.")?;
		let then_branch = self.block()?;
		let else_ifs = self.else_ifs()?;
		let mut else_branch = None;
		if self.match_token(TokenType::Else) {
			else_branch = Some(self.block()?);
		}

		Ok(Statement::If(Box::new(If {
			condition,
			then: then_branch,
			else_if: else_ifs,
			else_: else_branch,
		})))
	}

	fn else_ifs(&mut self) -> Result<Vec<ElseIf>, ParseError> {
		let mut else_ifs = Vec::new();
		while self.peek().token_type == TokenType::Else && self.peek_next().token_type == TokenType::If {
			self.consume_token(TokenType::Else, "Expect 'else' in else if.")?;
			self.consume_token(TokenType::If, "Expect 'if' after else.")?;
			let condition = self.expression()?;
			self.consume_token(TokenType::LeftBrace, "Expect '{' after else if condition.")?;
			let then_branch = self.block()?;
			else_ifs.push(ElseIf {
				condition,
				then: then_branch,
			});
		}

		Ok(else_ifs)
	}

	fn while_statement(&mut self) -> Result<Statement, ParseError> {
		let condition = self.expression()?;
		self.consume_token(TokenType::LeftBrace, "Expect '{' after while condition.")?;
		let body = self.block()?;

		Ok(Statement::While(Box::new(While {
			condition,
			body,
		})))
	}

	fn for_statement(&mut self) -> Result<Statement, ParseError> {
		self.consume_token(TokenType::LeftParen, "Expect '(' after 'for'.")?;
		let mut initializer = None;
		if !self.check(TokenType::Semicolon) {
			initializer = Some(self.statement()?);
		}

		self.consume_token(TokenType::Semicolon, "Expect ';' after for-loop initializer.")?;

		let mut condition = None;
		if !self.check(TokenType::Semicolon) {
			condition = Some(self.expression()?);
		}

		self.consume_token(TokenType::Semicolon, "Expect ';' after for-loop condition.")?;

		let mut update = None;
		if !self.check(TokenType::RightParen) {
			update = Some(self.statement()?);
		}

		self.consume_token(TokenType::RightParen, "Expect ')' after for-loop increment.")?;

		self.consume_token(TokenType::LeftBrace, "Expect '{' before for-loop body.")?;
		let body = self.block()?;

		Ok(Statement::For(Box::new(For {
			init: initializer,
			condition,
			update,
			body,
		})))
	}

	fn print_statement(&mut self) -> Result<Statement, ParseError> {
		let value = self.expression()?;
		self.consume_token(TokenType::Semicolon, "Expect ';' after value.")?;

		Ok(Statement::Print(value))
	}

	fn return_statement(&mut self) -> Result<Statement, ParseError> {
		let value = if self.check(TokenType::Semicolon) {
			None
		} else {
			Some(self.expression()?)
		};

		self.consume_token(TokenType::Semicolon, "Expect ';' after return value.")?;

		Ok(Statement::Return(value))
	}

	fn break_statement(&mut self) -> Result<Statement, ParseError> {
		if !self.in_loop() {
			return Err(self.error("Cannot use 'break' outside of loop."));
		}

		self.consume_token(TokenType::Break, "Expected break statement.")?;
		self.consume_token(TokenType::Semicolon, "Expect ';' after break.")?;

		Ok(Statement::Break)
	}

	fn continue_statement(&mut self) -> Result<Statement, ParseError> {
		if !self.in_loop() {
			return Err(self.error("Cannot use 'continue' outside of loop."));
		}

		self.consume_token(TokenType::Continue, "Expected continue statement.")?;
		self.consume_token(TokenType::Semicolon, "Expect ';' after continue.")?;

		Ok(Statement::Continue)
	}

	fn block(&mut self) -> Result<Statement, ParseError> {
		let mut statements = Vec::new();
		while !self.check(TokenType::RightBrace) && !self.is_at_end() {
			statements.push(self.declaration()?);
		}

		self.consume_token(TokenType::RightBrace, "Expect '}' after block.")?;

		Ok(Statement::Block(statements))
	}

	fn expression_statement(&mut self) -> Result<Statement, ParseError> {
		let expr = self.expression()?;
		self.consume_token(TokenType::Semicolon, "Expect ';' after expression.")?;

		Ok(Statement::Expression(expr))
	}

	fn expression(&mut self) -> Result<Expression, ParseError> {
		self.assignment()
	}

	fn assignment(&mut self) -> Result<Expression, ParseError> {
		let expr = self.or()?;
		if self.match_token(TokenType::Equal) {
			let equals = self.previous();
			let value = self.assignment()?;
			return if let Expression::Variable(name) = expr {
				Ok(Expression::Assign(Assign {
					name,
					value: Box::new(value),
				}))
			} else if let Expression::Get(get) = expr {
				Ok(Expression::Set(Set {
					object: get.object,
					name: get.name,
					value: Box::new(value),
				}))
			} else {
				Err(self.error(format!("Invalid assignment target: {}", equals.token_type)))
			}
		}

		Ok(expr)
	}

	fn or(&mut self) -> Result<Expression, ParseError> {
		let mut expr = self.and()?;
		while self.match_token(TokenType::PipePipe) {
			let operator = self.previous();
			let right = self.and()?;
			expr = Expression::Logical(Logical {
				left: Box::new(expr),
				operator: operator.token_type,
				right: Box::new(right),
			});
		}

		Ok(expr)
	}

	fn and(&mut self) -> Result<Expression, ParseError> {
		let mut expr = self.equality()?;
		while self.match_token(TokenType::AmpersandAmpersand) {
			let operator = self.previous();
			let right = self.equality()?;
			expr = Expression::Logical(Logical {
				left: Box::new(expr),
				operator: operator.token_type,
				right: Box::new(right),
			});
		}

		Ok(expr)
	}

	fn equality(&mut self) -> Result<Expression, ParseError> {
		let mut expr = self.comparison()?;
		while self.match_token(TokenType::EqualEqual)
			|| self.match_token(TokenType::BangEqual) {
			let operator = self.previous();
			let right = self.comparison()?;
			expr = Expression::Binary(Binary {
				left: Box::new(expr),
				operator: operator.token_type,
				right: Box::new(right),
			});
		}

		Ok(expr)
	}

	fn comparison(&mut self) -> Result<Expression, ParseError> {
		let mut expr = self.addition()?;
		while self.match_token(TokenType::Greater)
			|| self.match_token(TokenType::GreaterEqual)
			|| self.match_token(TokenType::Less)
			|| self.match_token(TokenType::LessEqual) {
			let operator = self.previous();
			let right = self.addition()?;
			expr = Expression::Binary(Binary {
				left: Box::new(expr),
				operator: operator.token_type,
				right: Box::new(right),
			});
		}

		Ok(expr)
	}

	fn addition(&mut self) -> Result<Expression, ParseError> {
		let mut expr = self.multiplication()?;
		while self.match_token(TokenType::Plus)
			|| self.match_token(TokenType::Minus) {
			let operator = self.previous();
			let right = self.multiplication()?;
			expr = Expression::Binary(Binary {
				left: Box::new(expr),
				operator: operator.token_type,
				right: Box::new(right),
			});
		}

		Ok(expr)
	}

	fn multiplication(&mut self) -> Result<Expression, ParseError> {
		let mut expr = self.unary()?;
		while self.match_token(TokenType::Star)
			|| self.match_token(TokenType::Slash) {
			let operator = self.previous();
			let right = self.unary()?;
			expr = Expression::Binary(Binary {
				left: Box::new(expr),
				operator: operator.token_type,
				right: Box::new(right),
			});
		}

		Ok(expr)
	}

	fn unary(&mut self) -> Result<Expression, ParseError> {
		if self.match_token(TokenType::Bang) {
			return Ok(Expression::Unary(Unary {
				operator: self.previous().token_type,
				right: Box::new(self.unary()?),
			}));
		}

		self.call()
	}

	fn call(&mut self) -> Result<Expression, ParseError> {
		let mut expr = self.primary()?;
		loop {
			if self.match_token(TokenType::LeftParen) {
				expr = self.finish_call(expr)?;
			} else if self.match_token(TokenType::LeftBracket) {
				expr = self.finish_index(expr)?;
			} else if self.match_token(TokenType::Dot) {
				let name = self.consume_identifier("Expected property name after '.'")?;
				expr = Expression::Get(Get {
					object: Box::new(expr),
					name,
				});
			} else {
				break;
			}
		}

		Ok(expr)
	}

	fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
		let mut arguments = Vec::new();
		if !self.check(TokenType::RightParen) {
			loop {
				arguments.push(self.expression()?);
				if !self.match_token(TokenType::Comma) {
					break;
				}
			}
		}

		self.consume_token(TokenType::RightParen, "Expect ')' after arguments.")?;
		Ok(Expression::Call(Call {
			callee: Box::new(callee),
			args: arguments,
		}))
	}

	fn finish_index(&mut self, left: Expression) -> Result<Expression, ParseError> {
		let index = self.expression()?;
		self.consume_token(TokenType::RightBracket, "Expect ']' after index.")?;
		Ok(Expression::Index(IndexCall {
			object: Box::new(left),
			index: Box::new(index),
		}))
	}

	fn primary(&mut self) -> Result<Expression, ParseError> {
		if self.match_token(TokenType::False) {
			return Ok(Expression::Literal(Literal::Boolean(false)));
		}

		if self.match_token(TokenType::True) {
			return Ok(Expression::Literal(Literal::Boolean(true)));
		}

		if self.match_token(TokenType::Nil) {
			return Ok(Expression::Literal(Literal::Nil));
		}

		if self.check_number() {
			return Ok(Expression::Literal(Literal::Number(self.consume_number()?)));
		}

		if self.check_string() {
			return Ok(Expression::Literal(Literal::String(self.consume_string()?)));
		}

		if self.match_token(TokenType::LeftBracket) {
			return Ok(self.array()?);
		}

		if self.match_token(TokenType::Super) {
			return Ok(Expression::Super);
		}

		if self.match_token(TokenType::This) {
			return Ok(Expression::This);
		}

		if self.match_token(TokenType::LeftParen) {
			let expr = self.expression()?;
			self.consume_token(TokenType::RightParen, "Expect ')' after expression.")?;
			return Ok(Expression::Grouping(Box::new(expr)));
		}

		if self.check_identifier() {
			return Ok(Expression::Variable(self.consume_identifier("Expect identifier.")?));
		}

		Err(self.error("Expect expression."))
	}

	fn array(&mut self) -> Result<Expression, ParseError> {
		let mut expressions = Vec::new();
		if !self.check(TokenType::RightBracket) {
			loop {
				expressions.push(self.expression()?);
				if !self.match_token(TokenType::Comma) {
					break;
				}
			}
		}

		self.consume_token(TokenType::RightBracket, "Expect ']' after array.")?;
		Ok(Expression::Array(expressions))
	}

	fn consume_token<S: ToString>(&mut self, token_type: TokenType, error_message: S) -> Result<(), ParseError> {
		if self.check(token_type) {
			self.advance();
			return Ok(());
		}

		Err(self.error(error_message.to_string()))
	}

	fn match_token(&mut self, token_type: TokenType) -> bool {
		if self.check(token_type) {
			self.advance();
			return true;
		}

		false
	}

	fn check(&self, token_type: TokenType) -> bool {
		self.peek().token_type == token_type
	}

	fn advance(&mut self) -> Token {
		self.current += 1;
		self.previous()
	}

	fn previous(&self) -> Token {
		self.tokens[self.current - 1].clone()
	}

	fn error<S: ToString>(&self, message: S) -> ParseError {
		ParseError {
			message: message.to_string(),
			token: self.peek().clone(),
			context: self.context,
		}
	}

	fn peek(&self) -> Token {
		self.tokens[self.current].clone()
	}

	fn peek_next(&self) -> Token {
		self.tokens[self.current + 1].clone()
	}

	fn check_next(&self, token_type: TokenType) -> bool {
		self.peek_next().token_type == token_type
	}

	fn synchronize(&mut self) {
		self.advance();

		while !self.is_at_end() {
			if self.previous().token_type == TokenType::Class {
				return;
			}

			if self.previous().token_type == TokenType::Fn {
				return;
			}

			if self.previous().token_type == TokenType::Let {
				return;
			}

			if self.previous().token_type == TokenType::For {
				return;
			}

			if self.previous().token_type == TokenType::If {
				return;
			}

			if self.previous().token_type == TokenType::While {
				return;
			}

			if self.previous().token_type == TokenType::Print {
				return;
			}

			if self.previous().token_type == TokenType::Return {
				return;
			}

			self.advance();
		}
	}

	fn is_at_end(&self) -> bool {
		self.peek().token_type == TokenType::Eof
	}


	fn check_identifier(&mut self) -> bool {
		match self.peek().token_type {
			TokenType::Identifier(_) => {
				true
			},
			TokenType::Get => {
				true
			},
			TokenType::Set => {
				true
			},
			_ => false,
		}
	}

	fn check_number(&mut self) -> bool {
		match self.peek().token_type {
			TokenType::Number(_) => {
				true
			},
			_ => false,
		}
	}

	fn check_string(&mut self) -> bool {
		match self.peek().token_type {
			TokenType::String(_) => {
				true
			},
			_ => false,
		}
	}

	fn peek_identifier(&mut self) -> Result<String, ParseError> {
		match self.peek().token_type {
			TokenType::Identifier(ref s) => {
				Ok(s.clone())
			},
			TokenType::Get => {
				Ok("get".to_string())
			},
			TokenType::Set => {
				Ok("set".to_string())
			},
			_ => Err(self.error("Expect identifier.")),
		}
	}

	fn consume_identifier<S: ToString>(&mut self, error_message: S) -> Result<String, ParseError> {
		match self.peek().token_type {
			TokenType::Identifier(identifier) => {
				self.advance();
				Ok(identifier.clone())
			},
			TokenType::Get => {
				self.advance();
				Ok("get".to_string())
			},
			TokenType::Set => {
				self.advance();
				Ok("set".to_string())
			},
			_ => Err(self.error(error_message)),
		}
	}

	fn consume_number(&mut self) -> Result<f64, ParseError> {
		match self.peek().token_type {
			TokenType::Number(number) => {
				self.advance();
				Ok(number)
			},
			_ => Err(self.error("Expect number.")),
		}
	}

	fn consume_string(&mut self) -> Result<String, ParseError> {
		match self.peek().token_type {
			TokenType::String(string) => {
				self.advance();
				Ok(string.clone())
			},
			_ => Err(self.error("Expect string.")),
		}
	}

	fn in_loop(&self) -> bool {
		self.context == Context::Loop || self.context == Context::While || self.context == Context::For
	}
}